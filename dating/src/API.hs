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

import           Database
import           FrontendTypes
import           Schema

type Username = Text

-- | The API.
type DatingAPI = AuthAPI -- :<|> UserAPI :<|> MessageAPI

type AuthAPI = "login" :> ReqBody '[JSON] CredentialDTO :> Post '[JSON] LoggedInDTO

type UserAPI =
       "users" :> ReqBody '[JSON] CreateUserDTO :> Post '[JSON] LoggedInDTO                       -- Create User
  :<|> "users" :> AuthProtect "cookie-auth" :> Capture "username" Username :> Get '[JSON] UserDTO -- Fetch User
  :<|> "users" :> AuthProtect "cookie-auth" :> Get '[JSON] [UserDTO]                              -- Fetch Users

type MessageAPI =
       "messages" :> AuthProtect "cookie-auth" :> Capture "username" Username :>
                                ReqBody '[JSON] MessageDTO :> Post '[JSON] ()                 -- Create Msg
  :<|> "messages" :> AuthProtect "cookie-auth" :> Get '[JSON] [ConversationPreviewDTO]        -- Fetch previews
  :<|> "messages" :> AuthProtect "cookie-auth" :> Capture "username" Username :> Get '[JSON] [ConversationDTO] -- Fetch Convo


-- | A proxy for the API. Technical detail.
datingAPI :: Proxy DatingAPI
datingAPI = Proxy :: Proxy DatingAPI


-- | Fetches a user by id. First it tries redis, then postgres. It saves to cache if it goes to the db.
fetchUserHandler :: MongoInfo -> Username -> Username -> Handler UserDTO
fetchUserHandler mongoInfo _ username = do
  maybeUser <- liftIO $ fetchUser mongoInfo username
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ throwE $ err401 { errBody = "Could not find user with that username"}

-- | Fetches all users from db if you are authenticated.
fetchAllUsersHandler :: MongoInfo -> Username -> Handler [UserDTO]
fetchAllUsersHandler pgInfo _ = liftIO $ fetchAllUsersPG pgInfo


-- | Creates a user in the db.
createUserHandler :: PGInfo -> User -> Handler LoggedInDTO
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user

-- | Creates a new message between two users
createMessageHandler :: PGInfo -> UserId -> Int64 -> Message -> Handler ()
createMessageHandler pgInfo _ otherUserId msg = liftIO $ createMessagePG pgInfo otherUserId msg

-- | Fetches all messages between two users.
fetchMessagesBetweenHandler :: PGInfo -> UserId -> Int64 -> Handler [MessageDTO]
fetchMessagesBetweenHandler pgInfo ownUserId otherUserId = liftIO $
  fetchMessagesBetweenPG pgInfo ownUserId otherUserId

fetchConversationPreviewDTOsHandler :: PGInfo -> UserId -> Handler [ConversationPreviewDTO]
fetchConversationPreviewDTOsHandler pgInfo ownUserId = liftIO $ fetchConversationPreviewDTOsListPG pgInfo ownUserId


-- | Returns an LoggedInDTO when given correct credentials
loginHandler :: MongoInfo -> CredentialDTO -> Handler LoggedInDTO
loginHandler mongoInfo credentials = do
  maybeUser <- liftIO $ fetchUserByCredentials pgInfo credentials
  case maybeUser of
    Just user -> return user
    Nothing   -> throwError (err403 {errBody = "Invalid credentials"})

-- | Given an AuthToken it returns either the UserId or throws and 403 error.
lookupByAuthToken :: PGInfo -> ByteString -> Handler UserId
lookupByAuthToken pgInfo authToken = do
  maybeUserId <- liftIO $ fetchUserIdByAuthTokenPG pgInfo authToken
  case maybeUserId of
    Nothing -> throwError (err403 { errBody = "Invalid authentication token" })
    Just userId -> return userId


-- | The handler which is called whenever a protected endpoint is visited.
authHandler :: PGInfo -> AuthHandler Request UserId
authHandler pgInfo = mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 { errBody = msg }
    handler req = either throw401 (lookupByAuthToken pgInfo) $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "Auth-Token" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "dating-auth-cookie" $ parseCookies cookie

maybeToEither e = maybe (Left e) Right

-- | Specifies the data returned after authentication.
type instance AuthServerData (AuthProtect "cookie-auth") = UserId


-- | Specifies the handler functions for each endpoint. Has to be in the right order.
datingServer :: PGInfo -> RedisInfo -> Server DatingAPI
datingServer pgInfo redisInfo = authHandlers
  where

    authHandlers =       loginHandler pgInfo

    userHandlers =       fetchUserHandler pgInfo redisInfo
                    :<|> fetchAllUsersHandler pgInfo
                    :<|> createUserHandler pgInfo

    messageHandlers =    createMessageHandler pgInfo
                    :<|> fetchConversationPreviewDTOsHandler pgInfo
                    :<|> fetchMessagesBetweenHandler pgInfo

-- | The context is sort of the state, being authenticated or not. Starts empty.
datingServerContext :: PGInfo -> Context (AuthHandler Request UserId ': '[])
datingServerContext pgInfo = authHandler pgInfo :. EmptyContext


-- | Serves the API on port 1234
runServer :: IO ()
runServer = do
  mongoInfo <- fetchMongoInfo
  run port $ serveWithContext datingAPI (datingServerContext mongoInfo) (datingServer mongoInfo redisInfo)
  where
    port = 1234


-- Client functions for testing. Not updated to use authentication.

-- fetchUserClient :: Int64 -> ClientM User
-- fetchAllUsersClient :: ClientM [User]
-- createUserClient :: User -> ClientM Int64
-- (fetchUserClient :<|> fetchAllUsersClient :<|> createUserClient) = client (Proxy :: Proxy DatingAPI)
