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
import           Database                         (Credentials, PGInfo,
                                                   RecentMessage, RedisInfo,
                                                   createMessagePG,
                                                   userId,
                                                   createUserPG,
                                                   createUserRedis,
                                                   fetchAllUsersPG,
                                                   fetchMessagesBetweenPG,
                                                   fetchRecentMessagesListPG,
                                                   fetchPostgresConnection,
                                                   fetchRedisConnection,
                                                   fetchUserByCredentialsPG,
                                                   fetchUserIdByAuthTokenPG,
                                                   fetchUserPG, fetchUserRedis)
import           Schema
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server.Experimental.Auth
import           Web.Cookie                       (parseCookies)


-- | The API.
type DatingAPI = "login" :> ReqBody '[JSON] Credentials :> Post '[JSON] (Entity User)
  :<|> "users" :> AuthProtect "cookie-auth" :> Capture "userid" Int64 :> Get '[JSON] (Entity User)
  :<|> "users" :> AuthProtect "cookie-auth" :> Get '[JSON] [(Entity User)]
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "messages" :> AuthProtect "cookie-auth" :> Capture "userid" Int64 :> ReqBody '[JSON] Message :> Post '[JSON] ()
  :<|> "messages" :> AuthProtect "cookie-auth" :> Get '[JSON] [RecentMessage]
  :<|> "messages" :> AuthProtect "cookie-auth" :> Capture "userid" Int64 :> Get '[JSON] [Message]


-- | A proxy for the API. Technical detail.
datingAPI :: Proxy DatingAPI
datingAPI = Proxy :: Proxy DatingAPI


-- | Fetches a user by id. First it tries redis, then postgres. It saves to cache if it goes to the db.
fetchUserHandler :: PGInfo -> RedisInfo -> UserId -> Int64 -> Handler (Entity User)
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
fetchAllUsersHandler :: PGInfo -> UserId -> Handler [Entity User]
fetchAllUsersHandler pgInfo _ = liftIO $ fetchAllUsersPG pgInfo


-- | Creates a user in the db.
createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user

-- | Creates a new message between two users
createMessageHandler :: PGInfo -> UserId -> Int64 -> Message -> Handler ()
createMessageHandler pgInfo _ otherUserId msg = liftIO $ createMessagePG pgInfo otherUserId msg

-- | Fetches all messages between two users.
fetchMessagesBetweenHandler :: PGInfo -> UserId -> Int64 -> Handler [Message]
fetchMessagesBetweenHandler pgInfo ownUserId otherUserId = liftIO $
  fetchMessagesBetweenPG pgInfo ownUserId otherUserId

fetchRecentMessagesHandler :: PGInfo -> UserId -> Handler [RecentMessage]
fetchRecentMessagesHandler pgInfo ownUserId = liftIO $ fetchRecentMessagesListPG pgInfo ownUserId


-- | Returns an authToken when given correct credentials
loginHandler :: PGInfo -> Credentials -> Handler (Entity User)
loginHandler pgInfo credentials = do
  maybeUser <- liftIO $ fetchUserByCredentialsPG pgInfo credentials
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
datingServer pgInfo redisInfo = loginHandler pgInfo
                                 :<|> fetchUserHandler pgInfo redisInfo
                                 :<|> fetchAllUsersHandler pgInfo
                                 :<|> createUserHandler pgInfo
                                 :<|> createMessageHandler pgInfo
                                 :<|> fetchRecentMessagesHandler pgInfo
                                 :<|> fetchMessagesBetweenHandler pgInfo

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
