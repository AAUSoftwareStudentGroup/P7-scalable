{-# LANGUAGE OverloadedStrings #-}
module Database where

import           Control.Monad (void)
import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT, LogLevel(..), filterLogger)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Database.Persist (get, insert, delete, entityVal, Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT)
import           Database.Redis (ConnectInfo, connect, Redis, runRedis, defaultConnectInfo, setex, del, connectHost)
import qualified Database.Redis as Redis

import           Schema

type PGInfo = ConnectionString
type RedisInfo = ConnectInfo

localConnString :: PGInfo
localConnString = "host=postgres port=5432 user=postgres dbname=postgres"

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

-- This is IO since in a real application we'd want to configure it.
fetchPostgresConnection :: IO PGInfo
fetchPostgresConnection = return localConnString

fetchRedisConnection :: IO RedisInfo
fetchRedisConnection = return $ defaultConnectInfo {connectHost = "redis"}

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)


-- | DATABASE

-- User

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid



-- | REDIS

runRedisAction :: RedisInfo -> Redis a -> IO a
runRedisAction redisInfo action = do
  connection <- connect redisInfo
  runRedis connection action


-- User

createUserRedis :: RedisInfo -> Int64 -> User -> IO ()
createUserRedis redisInfo uid user = runRedisAction redisInfo
  $ void $ setex (pack . show $ uid) 3600 (pack . show $ user)

fetchUserRedis :: RedisInfo -> Int64 -> IO (Maybe User)
fetchUserRedis redisInfo uid = runRedisAction redisInfo $ do
  result <- Redis.get (pack . show $ uid)
  case result of
    Right (Just userString) -> return $ Just (read . unpack $ userString)
    _ -> return Nothing

deleteUserRedis :: RedisInfo -> Int64 -> IO ()
deleteUserRedis redisInfo uid = do
  connection <- connect redisInfo
  runRedis connection $ do
    _ <- del [pack . show $ uid]
    return ()
