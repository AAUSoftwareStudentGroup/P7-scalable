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
import           Data.Int                         (Int64)
import           Data.Proxy                       (Proxy (..))
import           Database.Persist                 (Entity, Key)
import           Network.Wai                      (Request, requestHeaders)
import           Network.Wai.Handler.Warp         (run)
import           Servant                          (throwError)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Data.ByteString                  (ByteString)
import           Data.Map                         (Map, fromList)
import qualified Data.Map                         as Map
import           Data.Text                        (Text)
import           Database                         (Conversation, Credentials,
                                                   PGInfo, RedisInfo,
                                                   createMessagePG,
                                                   createUserPG,
                                                   createUserRedis,
                                                   fetchAllUsersPG,
                                                   fetchAuthTokenByCredentialsPG,
                                                   fetchConversationsPG,
                                                   fetchMessagesBetweenPG,
                                                   fetchPostgresConnection,
                                                   fetchRedisConnection,
                                                   fetchUserIdByAuthTokenPG,
                                                   fetchUserPG, fetchUserRedis)
import           Schema
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server.Experimental.Auth
import           Web.Cookie                       (parseCookies)


-- | The API.
type DatingAPI = AuthAPI :<|> UserAPI :<|> MessageAPI

type UserAPI =
       "users" :> AuthProtect "cookie-auth" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> AuthProtect "cookie-auth" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

type AuthAPI = "login" :> ReqBody '[JSON] Credentials :> Post '[JSON] Text

type MessageAPI =
       "messages" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] Message :> Post '[JSON] ()
  :<|> "messages" :> AuthProtect "cookie-auth" :> Get '[JSON] [Conversation]
  :<|> "messages" :> AuthProtect "cookie-auth" :> Capture "userid" Int64 :> Get '[JSON] [Message]


-- | A proxy for the API. Technical detail.
datingAPI :: Proxy DatingAPI
datingAPI = Proxy :: Proxy DatingAPI


-- | Fetches a user by id. First it tries redis, then postgres. It saves to cache if it goes to the db.
fetchUserHandler :: PGInfo -> RedisInfo -> UserId -> Int64 -> Handler User
fetchUserHandler pgInfo redisInfo _ uid = do
  maybeCachedUser <- liftIO $ fetchUserRedis redisInfo uid
  case maybeCachedUser of
    Just user -> return user
    Nothing -> do
      maybeUser <- liftIO $ fetchUserPG pgInfo uid
      case maybeUser of
        Just user -> liftIO (createUserRedis redisInfo uid user) >> return user
        Nothing -> Handler $ throwE $ err401 { errBody = "Could not find user with that ID"}

-- | Fetches all users from db if you are authenticated.
fetchAllUsersHandler :: PGInfo -> UserId -> Handler [User]
fetchAllUsersHandler pgInfo _ = liftIO $ fetchAllUsersPG pgInfo


-- | Creates a user in the db.
createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user

-- | Creates a new message between two users
createMessageHandler :: PGInfo -> User -> Message -> Handler ()
createMessageHandler pgInfo _ msg = liftIO $ createMessagePG pgInfo msg

-- | Fetches all messages between two users.
fetchMessagesBetweenHandler :: PGInfo -> User -> Int64 -> Handler [Message]
fetchMessagesBetweenHandler pgInfo user otherUserId = liftIO $
  fetchMessagesBetweenPG pgInfo user otherUserId

fetchConversationsHandler :: PGInfo -> User -> Handler [Conversation]
fetchConversationsHandler pgInfo user = liftIO $ fetchConversationsPG pgInfo user


-- | Returns an authToken when given correct credentials
loginHandler :: PGInfo -> Credentials -> Handler Text
loginHandler pgInfo credentials = do
  maybeAuthToken <- liftIO $ fetchAuthTokenByCredentialsPG pgInfo credentials
  case maybeAuthToken of
    Just token -> return token
    Nothing    -> throwError (err403 {errBody = "Invalid credentials"})

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
datingServer pgInfo redisInfo = authHandlers :<|> userHandlers :<|> messageHandlers
  where
    authHandlers = loginHandler pgInfo
    userHandlers = fetchUserHandler pgInfo redisInfo :<|>
                   fetchAllUsersHandler pgInfo :<|>
                   createUserHandler pgInfo
    messageHandlers = createMessageHandler :<|>
                      fetchConversationsHandler :<|>
                      fetchMessagesBetweenHandler

-- | The context is sort of the state, being authenticated or not. Starts empty.
datingServerContext :: PGInfo -> Context (AuthHandler Request UserId ': '[])
datingServerContext pgInfo = authHandler pgInfo :. EmptyContext


-- | Serves the API on port 1234
runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  run port $ serveWithContext datingAPI (datingServerContext pgInfo) (datingServer pgInfo redisInfo)
  where
    port = 1234


-- Client functions for testing. Not updated to use authentication.

-- fetchUserClient :: Int64 -> ClientM User
-- fetchAllUsersClient :: ClientM [User]
-- createUserClient :: User -> ClientM Int64
-- (fetchUserClient :<|> fetchAllUsersClient :<|> createUserClient) = client (Proxy :: Proxy DatingAPI)
