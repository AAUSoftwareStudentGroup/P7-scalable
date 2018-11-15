{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module API where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Except       (throwE)
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.Generics.Product            (getField)
import           Data.Int                         (Int64)
import           Data.Map                         (Map, fromList)
import qualified Data.Map                         as Map
import           Data.Proxy                       (Proxy (..))
import           Data.Semigroup                   ((<>))
import           Data.Text                        (Text)
import           Data.Text.Encoding               (encodeUtf8, decodeUtf8, encodeUtf16BE)
import           Database.Persist                 (Entity, Key, update)
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
import           FrontendTypes
import           Schema





-----------------------------------------------------------------------
--                                API                                --
-----------------------------------------------------------------------
-- | The API.
type DatingAPI = UserAPI :<|> AuthAPI :<|> MessageAPI

type UserAPI =
       "users" :> ReqBody '[JSON] CreateUserDTO :> Post '[JSON] LoggedInDTO                       -- Create User
  :<|> "users" :> AuthProtect "cookie-auth" :> Capture "username" Username :> Get '[JSON] UserDTO -- Fetch User
  :<|> "users" :> AuthProtect "cookie-auth" :> Capture "offset" Int :> Capture "limit" Int :> Get '[JSON] [UserDTO]                              -- Fetch Users

type AuthAPI =
       "login"  :> ReqBody '[JSON] CredentialDTO :> Post '[JSON] LoggedInDTO    -- Login
  :<|> "logout" :> ReqBody '[JSON] Text :> Post '[JSON] ()                      -- Logout


type MessageAPI =
       "messages" :> AuthProtect "cookie-auth" :> Capture "username" Username :> ReqBody '[JSON] CreateMessageDTO :> Post '[JSON] () -- Create Msg
  :<|> "messages" :> AuthProtect "cookie-auth" :> Get '[JSON] [ConversationPreviewDTO]                                         -- Fetch previews
  :<|> "messages" :> AuthProtect "cookie-auth" :> Capture "username" Username :> Capture "offset" Int :> Capture "limit" Int :> Get '[JSON] ConversationDTO                   -- Fetch Convo

-- | A proxy for the API. Technical detail.
datingAPI :: Proxy DatingAPI
datingAPI = Proxy :: Proxy DatingAPI


-------------------------------------------------------------------------------
--                                  USERS                                    --
-------------------------------------------------------------------------------

-- | Creates a user in the db.
createUserHandler :: MongoInfo -> CreateUserDTO -> Handler LoggedInDTO
createUserHandler mongoInfo newUser = do
  maybeCreated <- liftIO $ DB.createUser mongoInfo newUser
  case maybeCreated of
    Right loggedInDTO -> return loggedInDTO
    Left text -> Handler $ throwE $ err409 {errBody = text }

-- | Fetches a user by username.
fetchUserHandler :: MongoInfo -> Username -> Username -> Handler UserDTO
fetchUserHandler mongoInfo _ username = do
  maybeUser <- liftIO $ DB.fetchUser mongoInfo username
  case maybeUser of
    Just user -> return user

    Nothing -> Handler $ throwE $ err401 { errBody = "The user does not exist."}

-- | Fetches all users from db.
fetchAllUsersHandler :: MongoInfo -> Username -> Int -> Int -> Handler [UserDTO]
fetchAllUsersHandler mongoInfo _ offset limit = liftIO $ DB.fetchAllUsers mongoInfo offset limit


-------------------------------------------------------------------------------
--                             AUTHENTICATION                                --
-------------------------------------------------------------------------------


-- | Returns an LoggedInDTO when given correct credentials
loginHandler :: MongoInfo -> CredentialDTO -> Handler LoggedInDTO
loginHandler mongoInfo credentials = do
  maybeUser <- liftIO $ DB.fetchUserByCredentials mongoInfo credentials
  case maybeUser of
    Just user -> return user
    Nothing   -> throwError (err403 {errBody = "Invalid credentials."})


-- | Logs a user out
logoutHandler :: MongoInfo -> AuthToken -> Handler ()
logoutHandler mongoInfo token =
  liftIO $ DB.removeAuthToken mongoInfo token



-- | Specifies the data returned after authentication.
type instance AuthServerData (AuthProtect "cookie-auth") = Username


-- | The context is sort of the state, being authenticated or not. Starts empty.
datingServerContext :: MongoInfo -> Context (AuthHandler Request Username ': '[])
datingServerContext mongoInfo = authHandler mongoInfo :. EmptyContext

-- err401 ::
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
    Nothing -> throwError (err403 { errBody = "Invalid authentication token." })
    Just username -> return username

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right


-------------------------------------------------------------------------------
--                              CONVERSATIONS                                --
-------------------------------------------------------------------------------

-- | Creates a new message between two users
createMessageHandler :: MongoInfo -> Username -> Username -> CreateMessageDTO -> Handler ()
createMessageHandler mongoInfo ownUsername otherUsername msgDTO =
  liftIO $ DB.createMessage mongoInfo ownUsername otherUsername msgDTO

-- | Fetches all messages between two users.
fetchMessagesBetweenHandler :: MongoInfo -> Username -> Username -> Int -> Int -> Handler ConversationDTO
fetchMessagesBetweenHandler mongoInfo ownUsername otherUsername offset limit =
  liftIO $ DB.fetchConversation mongoInfo ownUsername otherUsername offset limit

-- | Fetches an overview of conversations for one user.
fetchConversationPreviewsHandler :: MongoInfo -> Username -> Handler [ConversationPreviewDTO]
fetchConversationPreviewsHandler mongoInfo ownUsername =
  liftIO $ DB.fetchConversationPreviews mongoInfo ownUsername



-------------------------------------------------------------------------------
--                          COMBINATION OF PARTS                             --
-------------------------------------------------------------------------------

-- | Specifies the handler functions for each endpoint. Has to be in the right order.
datingServer :: MongoInfo -> RedisInfo -> Server DatingAPI
datingServer mongoInfo redisInfo = userHandlers :<|> authHandlers :<|> messageHandlers
  where

    authHandlers =       loginHandler mongoInfo
                    :<|> logoutHandler mongoInfo

    userHandlers =       createUserHandler mongoInfo
                    :<|> fetchUserHandler mongoInfo
                    :<|> fetchAllUsersHandler mongoInfo

    messageHandlers =    createMessageHandler mongoInfo
                    :<|> fetchConversationPreviewsHandler mongoInfo
                    :<|> fetchMessagesBetweenHandler mongoInfo


-- | Serves the API on port 1234
runServer :: IO ()
runServer = do
  mongoInfo <- DB.fetchMongoInfo
  redisInfo <- DB.fetchRedisInfo
  run port $ serveWithContext datingAPI (datingServerContext mongoInfo) (datingServer mongoInfo redisInfo)
  where
    port = 1234
