{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchUserPG, createUserPG, fetchPostgresConnection, PGInfo, RedisInfo,
                           fetchUserRedis, createUserRedis, fetchRedisConnection)
import           Schema


type UsersAPI =
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

usersApi :: Proxy UsersAPI
usersApi = Proxy :: Proxy UsersAPI

fetchUserHandler :: PGInfo -> RedisInfo -> Int64 -> Handler User
fetchUserHandler pgInfo redisInfo uid = do
  maybeCachedUser <- liftIO $ fetchUserRedis redisInfo uid
  case maybeCachedUser of
    Just user -> return user
    Nothing -> do
      maybeUser <- liftIO $ fetchUserPG pgInfo uid
      case maybeUser of
        Just user -> liftIO (createUserRedis redisInfo uid user) >> return user
        Nothing -> Handler $ throwE $ err401 { errBody = "Could not find user with that ID"}


createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user


usersServer :: PGInfo -> RedisInfo -> Server UsersAPI
usersServer pgInfo redisInfo =
  fetchUserHandler pgInfo redisInfo :<|>
  createUserHandler pgInfo

runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  run port $ simpleCors $ serve usersApi $ usersServer pgInfo redisInfo
  where
    port = 1234


fetchUserClient :: Int64 -> ClientM User
createUserClient :: User -> ClientM Int64
(fetchUserClient :<|> createUserClient) = client (Proxy :: Proxy UsersAPI)
