{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module API where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Except       (throwE)
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.Int                         (Int64)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Proxy                       (Proxy (..))
import           Data.Semigroup                   ((<>))
import           Data.Text                        (Text)
import           Data.Text.Encoding               (decodeUtf8)
import           Network.Wai                      (Request, requestHeaders)
import           Network.Wai.Handler.Warp         (run)
import           Servant                          (throwError)
import           Servant.API
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Client
import           Servant.Server
import           Servant.Server.Experimental.Auth
import           Web.Cookie                       (parseCookies)

import           Database                         (AuthToken, MongoInfo,
                                                   RedisInfo, Username)
import qualified Database                         as DB
import qualified Recommendation.MatchExecutor     as Recommender
import           FrontendTypes
import           Schema


{---------------------------------------------------------------------}
{-                                API                                -}
{---------------------------------------------------------------------}
-- | The API.
type DatingAPI = UserAPI :<|> AuthAPI :<|> MessageAPI :<|> QuestionAPI

type UserAPI =
        -- Create User
  "users" :> ReqBody '[JSON] CreateUserDTO
          :> Post '[JSON] LoggedInDTO
  :<|>  -- Fetch User
  "users" :> AuthProtect "cookie-auth"
          :> Capture "username" Username
          :> Get '[JSON] UserDTO
  :<|>  -- Fetch Users
  "users" :> AuthProtect "cookie-auth"
          :> Capture "offset" Int
          :> Capture "limit" Int
          :> Get '[JSON] [UserDTO]
  :<|>  -- Fetch matching users
  "match" :> AuthProtect "cookie-auth"
          :> Capture "offset" Int
          :> Capture "limit" Int
          :> Get '[JSON] [UserWithScoreDTO]
  :<|>  -- Does Username Exist?
  "users" :> "exists"
          :> Capture "username" Username
          :> Get '[JSON] Bool
  :<|>  -- Edit User
  "edit"  :> AuthProtect "cookie-auth"
          :> ReqBody '[JSON] EditUserDTO
          :> Post '[JSON] LoggedInDTO

type AuthAPI =
        -- Login
  "login"  :> ReqBody '[JSON] CredentialDTO
           :> Post '[JSON] LoggedInDTO
  :<|>  --Logout
  "logout" :> AuthProtect "cookie-auth"
           :> Post '[JSON] ()

type MessageAPI =
        -- Create Message
  "messages" :> AuthProtect "cookie-auth"
             :> Capture "username" Username
             :> ReqBody '[JSON] CreateMessageDTO
             :> Post '[JSON] ()
  :<|>  -- Fetch Message Previews
  "messages" :> AuthProtect "cookie-auth"
             :> Get '[JSON] [ConversationPreviewDTO]
  :<|>  -- Fetch Conversation and some messages
  "messages" :> AuthProtect "cookie-auth"
             :> Capture "username" Username
             :> Capture "offset" Int
             :> Capture "limit" Int
             :> Get '[JSON] ConversationDTO

type QuestionAPI =
        -- Get Questions
  "questions" :> AuthProtect "cookie-auth"
              :> Get '[JSON] [QuestionDTO]
  :<|>  -- Post Answer
  "questions" :> AuthProtect "cookie-auth"
              :> ReqBody '[JSON] AnswerDTO
              :> Post '[JSON] ()


-- | A proxy for the API. Technical detail.
datingAPI :: Proxy DatingAPI
datingAPI = Proxy :: Proxy DatingAPI


{-----------------------------------------------------------------------------}
{-                                  USERS                                    -}
{-----------------------------------------------------------------------------}

-- | Creates a user in the db.
createUserHandler :: MongoInfo -> CreateUserDTO -> Handler LoggedInDTO
createUserHandler mongoInfo newUser = do
  maybeCreated <- liftIO $ DB.createUser mongoInfo newUser
  case maybeCreated of
    Right loggedInDTO -> return loggedInDTO
    Left text         -> Handler $ throwE text

-- | Fetches a user by username.
fetchUserHandler :: MongoInfo -> Username -> Username -> Handler UserDTO
fetchUserHandler mongoInfo _ username = do
  maybeUser <- liftIO $ DB.fetchUser mongoInfo username
  case maybeUser of
    Just user -> return user

    Nothing -> Handler $ throwE $ err404 { errBody = "The user does not exist"}

-- | Fetches all users from db.
fetchUsersHandler :: MongoInfo -> Username -> Int -> Int -> Handler [UserDTO]
fetchUsersHandler mongoInfo username offset limit = liftIO $ DB.fetchUsers mongoInfo username offset limit

-- | Fetches users that matches current user
fetchMatchingUsersHandler :: MongoInfo -> Username -> Int -> Int -> Handler [UserWithScoreDTO]
fetchMatchingUsersHandler mongoInfo username offset limit = do
  maybeTimeToPredict <- liftIO $ DB.timeToPredict mongoInfo username
  case maybeTimeToPredict of
    Right shouldPredict ->
      if shouldPredict then do
        _ <- liftIO $ DB.updatePredictionStatus mongoInfo username True
        _ <- liftIO $ Recommender.createMatchesForUser username
        res <- liftIO $ DB.fetchMatchesForUser mongoInfo offset limit username
        _ <- liftIO $ DB.updatePredictionStatus mongoInfo username False
        return res
      else
        liftIO $ DB.fetchMatchesForUser mongoInfo offset limit username
    Left err      -> Handler $ throwE err

-- | Ask if username exists.
fetchUserExists :: MongoInfo -> Username -> Handler Bool
fetchUserExists mongoInfo username = liftIO $ DB.fetchUserExists mongoInfo username

-- | Edit user from UserDTO

updateUserHandler :: MongoInfo -> Username -> EditUserDTO -> Handler LoggedInDTO
updateUserHandler mongoInfo username user = do
  maybeEdited <- liftIO $ DB.updateUser mongoInfo username user
  case maybeEdited of
    Right loggedInDTO -> return loggedInDTO
    Left err          -> Handler $ throwE err

{-----------------------------------------------------------------------------}
{-                             AUTHENTICATION                                -}
{-----------------------------------------------------------------------------}


-- | Returns an LoggedInDTO when given correct credentials
loginHandler :: MongoInfo -> CredentialDTO -> Handler LoggedInDTO
loginHandler mongoInfo credentials = do
  maybeUser <- liftIO $ DB.fetchUserByCredentials mongoInfo credentials
  case maybeUser of
    Just user -> return user
    Nothing   -> throwError (err401 {errBody = "Invalid credentials"})


-- | Logs a user out
logoutHandler :: MongoInfo -> Username -> Handler ()
logoutHandler mongoInfo username =
  liftIO $ DB.removeAuthToken mongoInfo username



-- | Specifies the data returned after authentication.
type instance AuthServerData (AuthProtect "cookie-auth") = Username


-- | The context is sort of the state, being authenticated or not. Starts empty.
datingServerContext :: MongoInfo -> Context (AuthHandler Request Username ': '[])
datingServerContext mongoInfo = authHandler mongoInfo :. EmptyContext


-- | The handler which is called whenever a protected endpoint is visited.
authHandler :: MongoInfo -> AuthHandler Request Username
authHandler mongoInfo = mkAuthHandler handler
  where
    throw401 :: LBS.ByteString -> Handler Username
    throw401 msg = throwError $ err401 { errBody = msg }
    handler req = either throw401 (lookupByAuthToken mongoInfo) $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "Auth-Token" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ decodeUtf8 <$> lookup "dating-auth-cookie" (parseCookies cookie)


-- | Given an AuthToken it returns either the Username or throws and 403 error.
lookupByAuthToken :: MongoInfo -> Text -> Handler Username
lookupByAuthToken mongoInfo authToken = do
  maybeUsername <- liftIO $ DB.fetchUsernameByAuthToken mongoInfo authToken
  case maybeUsername of
    Nothing -> throwError (err403 { errBody = "Invalid authentication token" })
    Just username -> return username

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right


{-----------------------------------------------------------------------------}
{-                              CONVERSATIONS                                -}
{-----------------------------------------------------------------------------}

-- | Creates a new message between two users
createMessageHandler :: MongoInfo -> Username -> Username -> CreateMessageDTO -> Handler ()
createMessageHandler mongoInfo ownUsername otherUsername msgDTO =
  if ownUsername == otherUsername then
    Handler $ throwE $ err409 { errBody = "Cannot chat with yourself" }
  else
    liftIO $ DB.createMessage mongoInfo ownUsername otherUsername msgDTO

-- | Fetches all messages between two users.
fetchMessagesBetweenHandler :: MongoInfo -> Username -> Username -> Int -> Int -> Handler ConversationDTO
fetchMessagesBetweenHandler mongoInfo ownUsername otherUsername offset limit =
  if ownUsername == otherUsername then
    Handler $ throwE $ err409 { errBody = "Cannot chat with yourself"}
  else
    liftIO $ DB.fetchConversation mongoInfo ownUsername otherUsername offset limit

-- | Fetches an overview of conversations for one user.
fetchConversationPreviewsHandler :: MongoInfo -> Username -> Handler [ConversationPreviewDTO]
fetchConversationPreviewsHandler mongoInfo ownUsername =
  liftIO $ DB.fetchConversationPreviews mongoInfo ownUsername


{-----------------------------------------------------------------------------}
{-                                 Questions                                 -}
{-----------------------------------------------------------------------------}

fetchQuestionsHandler :: MongoInfo -> Username -> Handler [QuestionDTO]
fetchQuestionsHandler mongoInfo username =
  liftIO $ DB.fetchQuestions mongoInfo username


createAnswerHandler :: MongoInfo -> Username -> AnswerDTO -> Handler ()
createAnswerHandler mongoInfo username answer = do
  maybePostAnswer <- liftIO $ DB.createAnswer mongoInfo username answer
  case maybePostAnswer of
    Right a  -> return ()
    Left err -> Handler $ throwE err


{-----------------------------------------------------------------------------}
{-                          COMBINATION OF PARTS                             -}
{-----------------------------------------------------------------------------}

-- | Specifies the handler functions for each endpoint. Has to be in the right order.
datingServer :: MongoInfo -> RedisInfo -> Server DatingAPI
datingServer mongoInfo redisInfo = userHandlers :<|> authHandlers :<|> messageHandlers :<|> questionHandlers
  where

    authHandlers =       loginHandler mongoInfo
                    :<|> logoutHandler mongoInfo

    userHandlers =       createUserHandler mongoInfo
                    :<|> fetchUserHandler mongoInfo
                    :<|> fetchUsersHandler mongoInfo
                    :<|> fetchMatchingUsersHandler mongoInfo
                    :<|> fetchUserExists mongoInfo
                    :<|> updateUserHandler mongoInfo

    messageHandlers =    createMessageHandler mongoInfo
                    :<|> fetchConversationPreviewsHandler mongoInfo
                    :<|> fetchMessagesBetweenHandler mongoInfo

    questionHandlers =   fetchQuestionsHandler mongoInfo
                    :<|> createAnswerHandler mongoInfo

-- | Serves the API
runServer :: IO ()
runServer = do
  mongoInfo <- DB.fetchMongoInfo
  redisInfo <- DB.fetchRedisInfo
  run port $ serveWithContext datingAPI (datingServerContext mongoInfo) (datingServer mongoInfo redisInfo)
  where
    port = 1234
