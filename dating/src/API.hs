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
import           Data.ByteString                  (ByteString)
import           Data.Int                         (Int64)
import           Data.Map                         (Map, fromList)
import qualified Data.Map                         as Map
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text)
import           Database.Persist                 (Entity, Key)
import           Network.Wai                      (Request, requestHeaders)
import           Network.Wai.Handler.Warp         (run)
import           Servant                          (throwError)
import           Servant.API
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Client
import           Servant.Server
import           Servant.Server.Experimental.Auth
import           Web.Cookie                       (parseCookies)

import           Database                         (MongoInfo, Username)
import qualified Database                         as DB
import           FrontendTypes
import           Schema





-----------------------------------------------------------------------
--                                API                                --
-----------------------------------------------------------------------
-- | The API.
type DatingAPI = UserAPI -- :<|> AuthAPI :<|> MessageAPI

type UserAPI =
       "users" :> ReqBody '[JSON] CreateUserDTO :> Post '[JSON] LoggedInDTO                       -- Create User
  :<|> "users" :> AuthProtect "cookie-auth" :> Capture "username" Username :> Get '[JSON] UserDTO -- Fetch User
  :<|> "users" :> AuthProtect "cookie-auth" :> Get '[JSON] [UserDTO]                              -- Fetch Users

type AuthAPI = "login" :> ReqBody '[JSON] CredentialDTO :> Post '[JSON] LoggedInDTO

type MessageAPI =
       "messages" :> AuthProtect "cookie-auth" :> Capture "username" Username :> ReqBody '[JSON] MessageDTO :> Post '[JSON] () -- Create Msg
  :<|> "messages" :> AuthProtect "cookie-auth" :> Get '[JSON] [ConversationPreviewDTO]                                         -- Fetch previews
  :<|> "messages" :> AuthProtect "cookie-auth" :> Capture "username" Username :> Get '[JSON] [ConversationDTO]                 -- Fetch Convo

-- | A proxy for the API. Technical detail.
datingAPI :: Proxy DatingAPI
datingAPI = Proxy :: Proxy DatingAPI


-------------------------------------------------------------------------------
--                                  USERS                                    --
-------------------------------------------------------------------------------

-- | Creates a user in the db.
createUserHandler :: MongoInfo -> CreateUserDTO -> Handler LoggedInDTO
createUserHandler mongoInfo newUser = liftIO $ DB.createUser mongoInfo newUser

-- | Fetches a user by username.
fetchUserHandler :: MongoInfo -> Username -> Username -> Handler UserDTO
fetchUserHandler mongoInfo _ username = do
  maybeUser <- liftIO $ DB.fetchUser mongoInfo username
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ throwE $ err401 { errBody = "The user '" <> username <> "' does not exist."}

-- | Fetches all users from db.
fetchAllUsersHandler :: MongoInfo -> Username -> Handler [UserDTO]
fetchAllUsersHandler mongoInfo _ = liftIO $ DB.fetchAllUsers mongoInfo


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


-- | Specifies the data returned after authentication.
type instance AuthServerData (AuthProtect "cookie-auth") = Username


-- | The context is sort of the state, being authenticated or not. Starts empty.
datingServerContext :: MongoInfo -> Context (AuthHandler Request Username ': '[])
datingServerContext mongoInfo = authHandler mongoInfo :. EmptyContext


-- | The handler which is called whenever a protected endpoint is visited.
authHandler :: MongoInfo -> AuthHandler Request Username
authHandler mongoInfo = mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 { errBody = msg }
    handler req = either throw401 (lookupByAuthToken mongoInfo) $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "Auth-Token" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "dating-auth-cookie" $ parseCookies cookie


-- | Given an AuthToken it returns either the Username or throws and 403 error.
lookupByAuthToken :: MongoInfo -> Text -> Handler Username
lookupByAuthToken mongoInfo authToken = do
  maybeUsername <- liftIO $ DB.fetchUsernameByAuthToken mongoInfo authToken
  case maybeUsername of
    Nothing -> throwError (err403 { errBody = "Invalid authentication token." })
    Just username -> return username


maybeToEither e = maybe (Left e) Right


-------------------------------------------------------------------------------
--                              CONVERSATIONS                                --
-------------------------------------------------------------------------------

-- | Creates a new message between two users
createMessageHandler :: MongoInfo -> Username -> Username -> CreateMessageDTO -> Handler ()
createMessageHandler mongoInfo ownUsername otherUsername msgDTO = 
  liftIO $ DB.createMessage mongoInfo ownUsername otherUsername msgDTO

-- | Fetches all messages between two users.
fetchMessagesBetweenHandler :: MongoInfo -> Username -> Username -> Handler ConversationDTO
fetchMessagesBetweenHandler mongoInfo ownUsername otherUsername = 
  liftIO $ DB.fetchConversation mongoInfo ownUsername otherUsername

-- | Fetches an overview of conversations for one user.
fetchConversationPreviewsHandler :: MongoInfo -> Username -> Handler [ConversationPreviewDTO]
fetchConversationPreviewsHandler mongoInfo ownUsername = 
  liftIO $ DB.fetchConversationPreviews mongoInfo ownUsername



-------------------------------------------------------------------------------
--                          COMBINATION OF PARTS                             --
-------------------------------------------------------------------------------

-- | Specifies the handler functions for each endpoint. Has to be in the right order.
datingServer :: MongoInfo -> RedisInfo -> Server DatingAPI
datingServer mongoInfo redisInfo = authHandlers
  where

    authHandlers =       loginHandler mongoInfo

    userHandlers =       fetchUserHandler mongoInfo redisInfo
                    :<|> fetchAllUsersHandler mongoInfo
                    :<|> createUserHandler mongoInfo

    messageHandlers =    createMessageHandler mongoInfo
                    :<|> fetchConversationPreviewDTOsHandler mongoInfo
                    :<|> fetchMessagesBetweenHandler mongoInfo


-- | Serves the API on port 1234
runServer :: IO ()
runServer = do
  mongoInfo <- fetchMongoInfo
  run port $ serveWithContext datingAPI (datingServerContext mongoInfo) (datingServer mongoInfo redisInfo)
  where
    port = 1234
