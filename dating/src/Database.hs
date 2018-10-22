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
import           Control.Monad.Reader        (runReaderT, ReaderT)
import           Data.Aeson.Types            (FromJSON, ToJSON)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       (pack, unpack)
import           Data.Int                    (Int64)
import           Data.List                   (intersperse, sort, sortBy)
import           Data.Maybe                  (listToMaybe)
import           Data.Ord                    (comparing)
import           Data.Text                   (Text)
import qualified Data.Text                   as T (pack, unpack)
import qualified Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Calendar          (fromGregorian)
import           Data.Time.Clock             (UTCTime (..), secondsToDiffTime)
import           Database.Esqueleto
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT,
                                              runMigration, withPostgresqlConn)
import qualified Database.Persist.Sql as P
import           Database.Redis              (ConnectInfo, Redis, connect,
                                              connectHost, defaultConnectInfo,
                                              del, runRedis, setex)
import qualified Database.Redis              as Redis
import           Elm                         (ElmType)
import           GHC.Generics                (Generic)
import           Schema
import qualified System.Random               as Random

type PGInfo = ConnectionString
type RedisInfo = ConnectInfo

data Credentials = Credentials { username :: Text, password :: Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)

data RecentMessage = RecentMessage {convoWith :: Text, imLastAuthor :: Bool, body :: Text, timeStamp :: UTCTime}
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ElmType)

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

createUserPG :: PGInfo -> User -> IO Int64
createUserPG pgInfo user = do
  g <- Random.newStdGen
  let authToken = T.pack $ take 64 $ Random.randomRs ('a', 'z') g
  let user' = user { userAuthToken = authToken }
  fromSqlKey <$> runAction pgInfo (insert user')

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe (Entity User))
fetchUserPG pgInfo uid = runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe (Entity User))
    selectAction = do
      usersFound <- select $
                    from $ \user -> do
                    where_ (user ^. UserId ==. valkey uid)
                    return user
      return $ listToMaybe $ usersFound

fetchAllUsersPG :: PGInfo -> IO [Entity User]
fetchAllUsersPG pgInfo = runAction pgInfo selectAction
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

fetchUserByCredentialsPG :: PGInfo -> Credentials -> IO (Maybe (Entity User))
fetchUserByCredentialsPG pgInfo (Credentials {username = usr, password = psw}) = runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe (Entity User))
    selectAction = do
      userFound <- select $
                     from $ \user -> do
                       where_ (user ^. UserUsername ==. val usr &&. user ^. UserPassword ==. val psw)
                       return user
      return $ listToMaybe userFound


-- Conversations

createConversationPG :: PGInfo -> Int64 -> Int64 -> IO ConversationId
createConversationPG pgInfo ownUserId otherUserId = runAction pgInfo $ insert conversation
  where
    members = toMembersList [ownUserId, otherUserId]
    conversation = Conversation members

fetchConversationByUserIdsPG :: PGInfo -> Int64 -> Int64 -> IO (Maybe (Entity Conversation))
fetchConversationByUserIdsPG pgInfo user1 user2 = runAction pgInfo selectAction
  where
    members = toMembersList [user1, user2]
    selectAction = listToMaybe <$> (select . from $ (\conv -> do
                                       where_ (conv ^. ConversationMemberIds ==. val members)
                                       return conv))

toMembersList :: [Int64] -> [UserId]
toMembersList = map toSqlKey . sort


fetchRecentMessagesListPG :: PGInfo -> UserId -> IO [RecentMessage]
fetchRecentMessagesListPG pgInfo ownUserId = fmap toRecentMessage <$> runAction pgInfo selectAction
  where
    ownUserIdInt64 = fromSqlKey ownUserId
    toRecentMessage :: (Single Text, Single Bool, Single Text, Single UTCTime) -> RecentMessage
    toRecentMessage (convoWith, imLastAuthor, body, timeStamp) = RecentMessage (unSingle convoWith) (unSingle imLastAuthor) (unSingle body) (unSingle timeStamp)

    selectAction :: MonadIO m => ReaderT SqlBackend m [(Single Text, Single Bool, Single Text, Single UTCTime)]
    selectAction = P.rawSql (T.pack stmt) (replicate 3 (PersistInt64 ownUserIdInt64))
    stmt = "SELECT u.username convoWith, m.authorId = ? imLastAuthor, m.body, m.timeStamp" ++
           "FROM conversations c JOIN" ++
           "messages m" ++
           "ON c.conversationId = m.conversationId" ++
           "and ? = any (c.memberIds)" ++
           "INNER JOIN (" ++
           "  SELECT conversationId, MAX(timeStamp) maxtstamp" ++
           "  FROM messages" ++
           "  GROUP BY conversationId" ++
           ") temp" ++
           "ON m.conversationId = temp.conversationId AND m.timeStamp = temp.maxtstamp" ++
           "JOIN users u" ++
           "ON u.userId != ? AND u.userId = ANY (c.memberIds);"


fetchMessagesBetweenPG :: PGInfo -> UserId -> Int64 -> IO [Message]
fetchMessagesBetweenPG pgInfo ownUserId otherUserId = fmap entityVal <$> runAction pgInfo selectAction
  where
    ownUserIdInt64 = fromSqlKey ownUserId
    selectAction :: MonadIO m => ReaderT SqlBackend m [(Entity Message)]
    selectAction = P.rawSql (T.pack stmt) []
    stmt = "SELECT ??" ++
           "FROM conversations c JOIN messages m ON c.cid = m.convoid" ++
           "WHERE ? = ANY (c.members) AND ? = ANY (c.members)" ++
           "ORDER BY m.tstamp DESC;"

getOther :: Text -> [Text] -> Text
getOther self [] = self
getOther self (u:[]) = self
getOther self (u1:u2:rest) = if self == u1 then u2 else u1

userId :: Entity User -> Int64
userId (Entity id _) = fromSqlKey id

-- Messages

createMessagePG :: PGInfo -> Int64 -> Message -> IO ()
createMessagePG pgInfo otherUserId message = void $ runAction pgInfo insertAndMaybeCreateConvo
  where
    ownUserId = (fromSqlKey . messageAuthorId) message
    getAndMaybeCreateConvoId = do
      maybeConvo <- fetchConversationByUserIdsPG pgInfo ownUserId otherUserId
      case maybeConvo of
        Nothing ->
          createConversationPG pgInfo ownUserId otherUserId
        Just (Entity cid _) ->
          return cid

    insertAndMaybeCreateConvo = do
      convoId <- liftIO $ getAndMaybeCreateConvoId
      let message' = message {messageConversationId = convoId}
      insert message'



fetchUserIdByUser :: PGInfo -> User -> IO (Maybe Int64)
fetchUserIdByUser pgInfo user = runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe Int64)
    selectAction = ((fmap (fromSqlKey . unValue)) . listToMaybe) <$>
        (select . from $ \(dbUser :: SqlExpr (Entity User)) -> do
            where_ (dbUser ^. UserUsername ==. val (userUsername user))
            return (dbUser ^. UserId))


-- -- REDIS

runRedisAction :: RedisInfo -> Redis a -> IO a
runRedisAction redisInfo action = do
  connection <- connect redisInfo
  runRedis connection action


-- User

createUserRedis :: RedisInfo -> Int64 -> Entity User -> IO ()
createUserRedis redisInfo uid user = runRedisAction redisInfo
  $ void $ setex (pack . show $ uid) 3600 (pack . show $ user)

fetchUserRedis :: RedisInfo -> Int64 -> IO (Maybe (Entity User))
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
