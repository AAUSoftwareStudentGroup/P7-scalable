{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Database where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LogLevel (..), LoggingT,
                                              MonadLogger, filterLogger,
                                              runStdoutLoggingT)
import           Control.Monad.Reader        (ReaderT, runReaderT)
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
import qualified Database.Persist.Sql        as P
import           Database.Redis              (ConnectInfo, Redis, connect,
                                              connectHost, defaultConnectInfo,
                                              del, runRedis, setex)
import qualified Database.Redis              as Redis
import           Elm                         (ElmType)
import           FrontendTypes
import           GHC.Generics                (Generic)
import           Schema
import qualified System.Random               as Random

type PGInfo = ConnectionString
type RedisInfo = ConnectInfo


localConnString :: PGInfo
localConnString = "host=postgres port=5432 dbname=datingdb user=datingdbuser password=datingdbpassword"

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

fetchUserDataPG :: PGInfo -> Int64 -> IO (Maybe UserData)
fetchUserDataPG pgInfo uid = runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe UserData)
    selectAction = do
      userPartsFound <- select $
                    from $ \user -> do
                    where_ (user ^. UserId ==. valkey uid)
                    return (user ^. UserUsername, user ^. UserId, user ^. UserGender, user ^. UserBirthday, user ^. UserTown, user ^. UserProfileText)
      return $ (fmap toUserData . listToMaybe) userPartsFound

    toUserData (username, id, gender, birthday, town, pText)
      = UserData (unValue username) (unValKey id) (unValue gender) (unValue town) (unValue pText)

unValKey = fromSqlKey . unValue

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

fetchLoggedInDataByCredentialDataPG :: PGInfo -> CredentialData -> IO (Maybe LoggedInData)
fetchLoggedInDataByCredentialDataPG pgInfo (CredentialData {username = usr, password = psw})
  = runAction pgInfo selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe LoggedInData)
    selectAction = do
      userPartsFound <- select . from $ \user -> do
                       where_ (user ^. UserUsername ==. val usr &&. user ^. UserPassword ==. val psw)
                       return (user ^. UserUsername, user ^. UserId, user ^. UserAuthToken)
      return $ (\(uName, uId, uToken) -> LoggedInData (unValue uName) (fromSqlKey . unValue $ uId) (unValue uToken)) <$> listToMaybe userParsFound


-- Conversations

createConversationPG :: PGInfo -> Int64 -> Int64 -> IO ConversationId
createConversationPG pgInfo ownUserId otherUserId = runAction pgInfo $ insert conversation
  where
    members = toMembersTuple ownUserId otherUserId
    conversation = uncurry Conversation $ members

fetchConversationByUserIdsPG :: PGInfo -> Int64 -> Int64 -> IO (Maybe (Entity Conversation))
fetchConversationByUserIdsPG pgInfo user1 user2 = runAction pgInfo selectAction
  where
    membersTuple = toMembersTuple user1 user2
    selectAction = listToMaybe <$> (select . from $ (\conv -> do
                                       where_ (conv ^. ConversationUserOneId ==. val (fst membersTuple) &&.
                                               conv ^. ConversationUserTwoId ==. val (snd membersTuple))
                                       return conv))


fetchRecentMessagesListPG :: PGInfo -> UserId -> IO [RecentMessage]
fetchRecentMessagesListPG pgInfo ownUserId = fmap toRecentMessage <$> runAction pgInfo selectAction
  where
    ownUserIdInt64 = fromSqlKey ownUserId
    toRecentMessage :: (Single Text, Single Int64, Single Bool, Single Text, Single UTCTime) -> RecentMessage
    toRecentMessage (convoWithUsername, convoWithId, imLastAuthor, body, timeStamp) = RecentMessage (unSingle convoWithUsername) (unSingle convoWithId) (unSingle imLastAuthor) (unSingle body) (unSingle timeStamp)

    selectAction :: MonadIO m => ReaderT SqlBackend m [(Single Text, Single Int64, Single Bool, Single Text, Single UTCTime)]
    selectAction = P.rawSql (T.pack stmt) (replicate 4 (PersistInt64 ownUserIdInt64))
    stmt = "SELECT users.username convoWithUsername, users.id convoWithId, messages.author_id = ? im_last_author, messages.body, messages.time_stamp " ++
           "FROM conversations JOIN " ++
           "messages " ++
           "ON conversations.id = messages.conversation_id " ++
           "AND (? = conversations.user_one_id OR ? = conversations.user_two_id) " ++
           "INNER JOIN ( " ++
           "  SELECT conversation_id, MAX(time_stamp) maxtstamp " ++
           "  FROM messages" ++
           "  GROUP BY conversation_id " ++
           ") temp " ++
           "ON messages.conversation_id = temp.conversation_id AND messages.time_stamp = temp.maxtstamp " ++
           "JOIN users " ++
           "ON users.id != ? AND (users.id = conversations.user_one_id OR users.id = conversations.user_two_id);"


fetchMessagesBetweenPG :: PGInfo -> UserId -> Int64 -> IO [Message]
fetchMessagesBetweenPG pgInfo ownUserId otherUserIdInt64 = fmap entityVal <$> runAction pgInfo selectAction
  where
    ownUserIdInt64 = fromSqlKey ownUserId
    selectAction :: MonadIO m => ReaderT SqlBackend m [(Entity Message)]
    selectAction = P.rawSql (T.pack stmt) (PersistInt64 <$> sort [ownUserIdInt64, otherUserIdInt64])
    stmt = "SELECT ?? " ++
           "FROM conversations JOIN messages ON conversations.id = messages.conversation_id " ++
           "JOIN users ON users.id = messages.author_id " ++
           "WHERE ? = conversations.user_one_id AND ? = conversations.user_two_id " ++
           "ORDER BY messages.time_stamp DESC; "

getOther :: Text -> [Text] -> Text
getOther self []           = self
getOther self (u:[])       = self
getOther self (u1:u2:rest) = if self == u1 then u2 else u1

userId :: Entity User -> Int64
userId (Entity id _) = fromSqlKey id


toMembersTuple :: Int64 -> Int64 -> (UserId, UserId)
toMembersTuple userOneId userTwoId = if userOneId <= userTwoId then (userOneId', userTwoId') else (userTwoId', userOneId')
  where
    userOneId' = toSqlKey userOneId
    userTwoId' = toSqlKey userTwoId

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

createUserDataRedis :: RedisInfo -> Int64 -> UserData -> IO ()
createUserDataRedis redisInfo uid user = runRedisAction redisInfo
  $ void $ setex (pack . show $ uid) 3600 (pack . show $ user)

fetchUserDataRedis :: RedisInfo -> Int64 -> IO (Maybe UserData)
fetchUserDataRedis redisInfo uid = runRedisAction redisInfo $ do
  result <- Redis.get (pack . show $ uid)
  case result of
    Right (Just userString) -> return $ Just (read . unpack $ userString)
    _                       -> return Nothing

deleteUserDataRedis :: RedisInfo -> Int64 -> IO ()
deleteUserDataRedis redisInfo uid = do
  connection <- connect redisInfo
  runRedis connection $ do
    _ <- del [pack . show $ uid]
    return ()
