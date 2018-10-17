{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LogLevel (..), LoggingT,
                                              MonadLogger, filterLogger,
                                              runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Data.Aeson.Types            (FromJSON, ToJSON)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       (pack, unpack)
import           Data.Int                    (Int64)
import           Data.Maybe                  (listToMaybe)
import qualified Data.Text                   as T (unpack, pack)
import Data.Text (Text)
import qualified Data.Text.Encoding          (decodeUtf8)
import           Database.Esqueleto
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT,
                                              runMigration, withPostgresqlConn)
import           Database.Redis              (ConnectInfo, Redis, connect,
                                              connectHost, defaultConnectInfo,
                                              del, runRedis, setex)
import qualified Database.Redis              as Redis
import           Elm                         (ElmType)
import           GHC.Generics                (Generic)
import qualified System.Random               as Random

import           Schema

type PGInfo = ConnectionString
type RedisInfo = ConnectInfo

data Credentials = Credentials { username :: Text, password :: Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)

data Conversation = Conversation { userId :: Int64, receiverUsername :: Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)

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
migrateDB pgInfo = runAction pgInfo (runMigration migrateAll)

deleteEverythingInDB :: PGInfo -> IO ()
deleteEverythingInDB pgInfo = runAction pgInfo deleteAction
  where 
    deleteAction :: SqlPersistT (LoggingT IO) ()
    deleteAction = do
      delete $
        from $ \(message :: SqlExpr (Entity Message)) ->
        return ()
      delete $ 
        from $ \(user :: SqlExpr (Entity User)) ->
        return ()


-- -- DATABASE

-- User

fromEntity = (fmap . fmap) entityVal

createUserPG :: PGInfo -> User -> IO Int64
createUserPG pgInfo user = do 
  g <- Random.newStdGen
  let authToken = T.pack $ take 64 $ Random.randomRs ('a', 'z') g
  let user' = user { userAuthToken = authToken }
  fromSqlKey <$> runAction pgInfo (insert user')

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG pgInfo uid = fromEntity $ runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe (Entity User))
    selectAction = do
      usersFound <- select $
                    from $ \user -> do
                    where_ (user ^. UserId ==. valkey uid)
                    return user
      return $ listToMaybe $ usersFound

fetchAllUsersPG :: PGInfo -> IO [User]
fetchAllUsersPG pgInfo = (fmap . fmap) entityVal $ runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) [Entity User]
    selectAction = do
      users <- select $ from $ \user -> return user
      return users


deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG pgInfo uid = runAction pgInfo $ do
  delete $
    from $ \user -> do
    where_ (user ^. UserId ==. valkey uid)


fetchUserIdByAuthTokenPG :: PGInfo -> ByteString -> IO (Maybe UserId)
fetchUserIdByAuthTokenPG pgInfo authToken = runAction pgInfo selectAction
  where
    authToken' = Data.Text.Encoding.decodeUtf8 authToken
    selectAction :: SqlPersistT (LoggingT IO) (Maybe (Key User))
    selectAction = do
      userIdsFound <-
        select $
        from $ \user -> do
        where_ (user ^. UserAuthToken ==. val authToken')
        return (user ^. UserId)
      return $ listToMaybe $ fmap unValue userIdsFound


-- Authentication

fetchAuthTokenByCredentialsPG :: PGInfo -> Credentials -> IO (Maybe Text)
fetchAuthTokenByCredentialsPG pgInfo (Credentials {username = usr, password = psw}) = runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe Text)
    selectAction = do
      tokensFound <- select $
                     from $ \user -> do
                       where_ (user ^. UserUsername ==. val usr &&. user ^. UserPassword ==. val psw)
                       return (user ^. UserAuthToken)
      return $ unValue <$> listToMaybe tokensFound

-- Messages

createMessagePG :: PGInfo -> Message -> IO ()
createMessagePG pgInfo message = void $ runAction pgInfo $ insert message

-- -- TODO: 2nd should be user.
fetchMessagesBetweenPG :: PGInfo -> User -> Int64 -> IO [Message]
fetchMessagesBetweenPG pgInfo userOne userTwoId = runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) [Message]
    selectAction = do
      maybeUserOneId <- ((fmap unValue) . listToMaybe) <$>
        (select . from $ \(user :: SqlExpr (Entity User)) -> do
            where_ (user ^. UserUsername ==. val (userUsername userOne))
            return (user ^. UserId))
      case maybeUserOneId of
        Nothing -> return []
        Just userOneId -> (fmap entityVal) <$> (select . from $ \(msg :: SqlExpr (Entity Message)) -> do
                                                where_ (msg ^. MessageSender ==. val userOneId
                                                        &&. msg ^. MessageReceiver ==. valkey userTwoId ||.
                                                        msg ^. MessageSender ==. valkey userTwoId
                                                        &&. msg ^. MessageReceiver ==. val userOneId)
                                                return msg)


fetchConversationsPG :: PGInfo -> User -> IO [Conversation]
fetchConversationsPG pgInfo userId = runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) [Conversation]
    selectAction = do
      ids <- (select $ from $ \msg -> do
                 where_ (msg ^. MessageReceiver ==. valkey userId)
                 return (msg ^. MessageReceiver))
             ++.
             (select $ from $ \msg -> do
                 where_ (msg ^. MessageSender ==. valkey userId)
                 return (msg ^. MsgSender))
      return ids

fetchUserIdByUser :: PGInfo -> User -> IO (Maybe Int64)
fetchUserIdByUser pgInfo user = runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe Int64)


-- -- REDIS

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
    _                       -> return Nothing

deleteUserRedis :: RedisInfo -> Int64 -> IO ()
deleteUserRedis redisInfo uid = do
  connection <- connect redisInfo
  runRedis connection $ do
    _ <- del [pack . show $ uid]
    return ()
