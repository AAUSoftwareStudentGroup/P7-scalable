{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Database where

import           Control.Lens
import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger       (LogLevel (..), LoggingT,
                                             MonadLogger, filterLogger,
                                             runStdoutLoggingT)
import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack, unpack)
import           Data.Generics.Product      (getField, super, field)
import           Data.Int                   (Int64)
import           Data.List                  (intersperse, sort, sortBy)
import           Data.Maybe                 (listToMaybe)
import           Data.Ord                   (comparing)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack, unpack)
import qualified Data.Text.Encoding         (decodeUtf8)
import           Data.Time.Calendar         (fromGregorian)
import           Data.Time.Clock            (UTCTime (..), secondsToDiffTime)
import           Database.Persist
import           Database.Persist.MongoDB
import           Database.Persist.TH
import           Database.Redis             (ConnectInfo, Redis, connect,
                                             connectHost, defaultConnectInfo,
                                             del, runRedis, setex)
import qualified Database.Redis             as Redis
import           Elm                        (ElmType)
import           GHC.Generics               (Generic)
import qualified Database.MongoDB as Mongo
import           Language.Haskell.TH.Syntax
import           Network                    (PortID (PortNumber))
import qualified System.Random              as Random

import           FrontendTypes
import           Schema

localMongoConf :: MongoConf
localMongoConf = conf { mgAuth = Just $ MongoAuth "datingdbuser" "datingdbpassword"
                      , mgHost = "mongodb"
                      }
  where
    conf = defaultMongoConf "datingdb"


runAction mongoConf action = withConnection mongoConf $ 
  \pool -> runMongoDBPoolDef action pool

createUser :: MongoConf -> CreateUserDTO -> IO LoggedInDTO
createUser mongoConf createUserDTO = runAction mongoConf action
  where
    action :: Action IO LoggedInDTO
    action = do
      authToken <- liftIO mkAuthToken
      let newUser = User
            { userEmail = getField @"email" createUserDTO
            , userPassword = getField @"password" createUserDTO -- TODO: Hash
            , userUsername = getField @"username" createUserDTO
            , userGender = getField @"gender" createUserDTO
            , userBirthday = getField @"birthday" createUserDTO
            , userTown = getField @"town" createUserDTO
            , userProfileText = getField @"profileText" createUserDTO
            , userAuthToken = authToken
            }
      userId <- insert newUser
      return $ LoggedInDTO (getField @"username" createUserDTO) (keyToText . toBackendKey $ userId) authToken


addAuthTokenToUser :: MongoConf -> CredentialDTO -> IO (Maybe LoggedInDTO)
addAuthTokenToUser mongoConf credentials = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO (Maybe LoggedInDTO)
    fetchAction = do
      maybeUserEnt <- getBy $ UniqueUsername (getField @"username" credentials)
      case maybeUserEnt of
        Nothing -> return Nothing
        Just (Entity userId user) -> do 
          authToken <- liftIO mkAuthToken
          update userId [UserAuthToken =. authToken]
          if getField @"userPassword" user == getField @"password" credentials
            then return . Just $ LoggedInDTO (userUsername user) (keyToText . toBackendKey $ userId) authToken
            else return Nothing

{-
deleteAuthTokenFromUser :: MongoConf -> Text -> IO ()
deleteAuthTokenFromUser mongoConf token = runAction mongoConf deleteAction
  where
    deleteAction :: Action IO ()
    deleteAction = do
      maybeUserEnt <- getBy $ UniqueAuthToken token
      case maybeUserEnt of
        Nothing -> return ()
        Just (Entity userAuthToken user) -> do
-}


mkUserDTOFromUserEntity :: Entity User -> UserDTO
mkUserDTOFromUserEntity (Entity userId user) = userDTO
  where 
    userDTO = UserDTO 
      { username = userUsername user
      , userId = (keyToText . toBackendKey) userId
      , gender = userGender user
      , birthday = userBirthday user
      , town = userTown user
      , profileText = userProfileText user
      }

mkAuthToken :: IO Text
mkAuthToken = do
  generator <- Random.newStdGen
  return . T.pack . take 32 $ Random.randomRs ('a', 'z') generator

fetchAllUsers :: MongoConf -> IO [UserDTO]
fetchAllUsers mongoConf = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO [UserDTO]
    fetchAction = map mkUserDTOFromUserEntity <$> selectList [] []

fetchUserDTOByUsername :: MongoConf -> Text -> IO (Maybe UserDTO)
fetchUserDTOByUsername mongoConf username = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO (Maybe UserDTO)
    fetchAction = do
      maybeUserEnt <- getBy $ UniqueUsername username
      case maybeUserEnt of
        Nothing -> return Nothing
        Just user -> return . Just $ mkUserDTOFromUserEntity user




-- type PGInfo = ConnectionString
-- type RedisInfo = ConnectInfo


-- localConnString :: PGInfo
-- localConnString = "host=postgres port=5432 dbname=datingdb user=datingdbuser password=datingdbpassword"

-- logFilter :: a -> LogLevel -> Bool
-- logFilter _ LevelError     = True
-- logFilter _ LevelWarn      = True
-- logFilter _ LevelInfo      = True
-- logFilter _ LevelDebug     = False
-- logFilter _ (LevelOther _) = False

-- -- This is IO since in a real application we'd want to configure it.
-- fetchPostgresConnection :: IO PGInfo
-- fetchPostgresConnection = return localConnString

-- fetchRedisConnection :: IO RedisInfo
-- fetchRedisConnection = return $ defaultConnectInfo {connectHost = "redis"}

-- runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
-- runAction connectionString action =
--   runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
--     runReaderT action backend

-- migrateDB :: PGInfo -> IO ()
-- migrateDB pgInfo = runAction pgInfo (runMigration migrateAll)

-- deleteEverythingInDB :: PGInfo -> IO ()
-- deleteEverythingInDB pgInfo = runAction pgInfo deleteAction
--   where
--     deleteAction :: SqlPersistT (LoggingT IO) ()
--     deleteAction = do
--       delete $
--         from $ \(message :: SqlExpr (Entity Message)) ->
--         return ()
--       delete $
--         from $ \(user :: SqlExpr (Entity User)) ->
--         return ()


-- -- -- DATABASE

-- -- User

-- createUserPG :: PGInfo -> User -> IO Int64
-- createUserPG pgInfo user = do
--   g <- Random.newStdGen
--   let authToken = T.pack $ take 64 $ Random.randomRs ('a', 'z') g
--   let user' = user { userAuthToken = authToken }
--   fromSqlKey <$> runAction pgInfo (insert user')

-- fetchUserDTOPG :: PGInfo -> Int64 -> IO (Maybe UserDTO)
-- fetchUserDTOPG pgInfo uid = runAction pgInfo selectAction
--   where
--     selectAction :: SqlPersistT (LoggingT IO) (Maybe UserDTO)
--     selectAction = do
--       userPartsFound <- select $
--                     from $ \user -> do
--                     where_ (user ^. UserId ==. valkey uid)
--                     return (user ^. UserUsername, user ^. UserId, user ^. UserGender, user ^. UserBirthday, user ^. UserTown, user ^. UserProfileText)
--       return $ (fmap toUserDTO . listToMaybe) userPartsFound

--     toUserDTO (username, id, gender, birthday, town, pText)
--       = UserDTO (unValue username) (unValKey id) (unValue gender) (unValue town) (unValue pText)

-- unValKey = fromSqlKey . unValue

-- fetchAllUsersPG :: PGInfo -> IO [Entity User]
-- fetchAllUsersPG pgInfo = runAction pgInfo selectAction
--   where
--     selectAction :: SqlPersistT (LoggingT IO) [Entity User]
--     selectAction = do
--       users <- select $ from $ \user -> return user
--       return users


-- deleteUserPG :: PGInfo -> Int64 -> IO ()
-- deleteUserPG pgInfo uid = runAction pgInfo $ do
--   delete $
--     from $ \user -> do
--     where_ (user ^. UserId ==. valkey uid)


-- fetchUserIdByAuthTokenPG :: PGInfo -> ByteString -> IO (Maybe UserId)
-- fetchUserIdByAuthTokenPG pgInfo authToken = runAction pgInfo selectAction
--   where
--     authToken' = Data.Text.Encoding.decodeUtf8 authToken
--     selectAction :: SqlPersistT (LoggingT IO) (Maybe (Key User))
--     selectAction = do
--       userIdsFound <-
--         select $
--         from $ \user -> do
--         where_ (user ^. UserAuthToken ==. val authToken')
--         return (user ^. UserId)
--       return $ listToMaybe $ fmap unValue userIdsFound

-- fetchLoggedInDTOByCredentialDTOPG :: PGInfo -> CredentialDTO -> IO (Maybe LoggedInDTO)
-- fetchLoggedInDTOByCredentialDTOPG pgInfo (CredentialDTO {username = usr, password = psw})
--   = runAction pgInfo selectAction
--   where
--     selectAction :: SqlPersistT (LoggingT IO) (Maybe LoggedInDTO)
--     selectAction = do
--       userPartsFound <- select . from $ \user -> do
--                        where_ (user ^. UserUsername ==. val usr &&. user ^. UserPassword ==. val psw)
--                        return (user ^. UserUsername, user ^. UserId, user ^. UserAuthToken)
--       return $ (\(uName, uId, uToken) -> LoggedInDTO (unValue uName) (fromSqlKey . unValue $ uId) (unValue uToken)) <$> listToMaybe userParsFound


-- -- Conversations

-- createConversationPG :: PGInfo -> Int64 -> Int64 -> IO ConversationId
-- createConversationPG pgInfo ownUserId otherUserId = runAction pgInfo $ insert conversation
--   where
--     members = toMembersTuple ownUserId otherUserId
--     conversation = uncurry Conversation $ members

-- fetchConversationByUserIdsPG :: PGInfo -> Int64 -> Int64 -> IO (Maybe (Entity Conversation))
-- fetchConversationByUserIdsPG pgInfo user1 user2 = runAction pgInfo selectAction
--   where
--     membersTuple = toMembersTuple user1 user2
--     selectAction = listToMaybe <$> (select . from $ (\conv -> do
--                                        where_ (conv ^. ConversationUserOneId ==. val (fst membersTuple) &&.
--                                                conv ^. ConversationUserTwoId ==. val (snd membersTuple))
--                                        return conv))


-- fetchConversationPreviewDTOsListPG :: PGInfo -> UserId -> IO [ConversationPreviewDTO]
-- fetchConversationPreviewDTOsListPG pgInfo ownUserId = fmap toConversationPreviewDTO <$> runAction pgInfo selectAction
--   where
--     ownUserIdInt64 = fromSqlKey ownUserId
--     toConversationPreviewDTO :: (Single Text, Single Int64, Single Bool, Single Text, Single UTCTime) -> ConversationPreviewDTO
--     toConversationPreviewDTO (convoWithUsername, convoWithId, imLastAuthor, body, timeStamp) = ConversationPreviewDTO (unSingle convoWithUsername) (unSingle convoWithId) (unSingle imLastAuthor) (unSingle body) (unSingle timeStamp)

--     selectAction :: MonadIO m => ReaderT SqlBackend m [(Single Text, Single Int64, Single Bool, Single Text, Single UTCTime)]
--     selectAction = P.rawSql (T.pack stmt) (replicate 4 (PersistInt64 ownUserIdInt64))
--     stmt = "SELECT users.username convoWithUsername, users.id convoWithId, messages.author_id = ? im_last_author, messages.body, messages.time_stamp " ++
--            "FROM conversations JOIN " ++
--            "messages " ++
--            "ON conversations.id = messages.conversation_id " ++
--            "AND (? = conversations.user_one_id OR ? = conversations.user_two_id) " ++
--            "INNER JOIN ( " ++
--            "  SELECT conversation_id, MAX(time_stamp) maxtstamp " ++
--            "  FROM messages" ++
--            "  GROUP BY conversation_id " ++
--            ") temp " ++
--            "ON messages.conversation_id = temp.conversation_id AND messages.time_stamp = temp.maxtstamp " ++
--            "JOIN users " ++
--            "ON users.id != ? AND (users.id = conversations.user_one_id OR users.id = conversations.user_two_id);"


-- fetchMessagesBetweenPG :: PGInfo -> UserId -> Int64 -> IO [Message]
-- fetchMessagesBetweenPG pgInfo ownUserId otherUserIdInt64 = fmap entityVal <$> runAction pgInfo selectAction
--   where
--     ownUserIdInt64 = fromSqlKey ownUserId
--     selectAction :: MonadIO m => ReaderT SqlBackend m [(Entity Message)]
--     selectAction = P.rawSql (T.pack stmt) (PersistInt64 <$> sort [ownUserIdInt64, otherUserIdInt64])
--     stmt = "SELECT ?? " ++
--            "FROM conversations JOIN messages ON conversations.id = messages.conversation_id " ++
--            "JOIN users ON users.id = messages.author_id " ++
--            "WHERE ? = conversations.user_one_id AND ? = conversations.user_two_id " ++
--            "ORDER BY messages.time_stamp DESC; "

-- getOther :: Text -> [Text] -> Text
-- getOther self []           = self
-- getOther self (u:[])       = self
-- getOther self (u1:u2:rest) = if self == u1 then u2 else u1

-- userId :: Entity User -> Int64
-- userId (Entity id _) = fromSqlKey id


-- toMembersTuple :: Int64 -> Int64 -> (UserId, UserId)
-- toMembersTuple userOneId userTwoId = if userOneId <= userTwoId then (userOneId', userTwoId') else (userTwoId', userOneId')
--   where
--     userOneId' = toSqlKey userOneId
--     userTwoId' = toSqlKey userTwoId

-- -- Messages

-- createMessagePG :: PGInfo -> Int64 -> Message -> IO ()
-- createMessagePG pgInfo otherUserId message = void $ runAction pgInfo insertAndMaybeCreateConvo
--   where
--     ownUserId = (fromSqlKey . messageAuthorId) message
--     getAndMaybeCreateConvoId = do
--       maybeConvo <- fetchConversationByUserIdsPG pgInfo ownUserId otherUserId
--       case maybeConvo of
--         Nothing ->
--           createConversationPG pgInfo ownUserId otherUserId
--         Just (Entity cid _) ->
--           return cid

--     insertAndMaybeCreateConvo = do
--       convoId <- liftIO $ getAndMaybeCreateConvoId
--       let message' = message {messageConversationId = convoId}
--       insert message'



-- fetchUserIdByUser :: PGInfo -> User -> IO (Maybe Int64)
-- fetchUserIdByUser pgInfo user = runAction pgInfo selectAction
--   where
--     selectAction :: SqlPersistT (LoggingT IO) (Maybe Int64)
--     selectAction = ((fmap (fromSqlKey . unValue)) . listToMaybe) <$>
--         (select . from $ \(dbUser :: SqlExpr (Entity User)) -> do
--             where_ (dbUser ^. UserUsername ==. val (userUsername user))
--             return (dbUser ^. UserId))


-- -- -- REDIS

-- runRedisAction :: RedisInfo -> Redis a -> IO a
-- runRedisAction redisInfo action = do
--   connection <- connect redisInfo
--   runRedis connection action


-- -- User

-- createUserDTORedis :: RedisInfo -> Int64 -> UserDTO -> IO ()
-- createUserDTORedis redisInfo uid user = runRedisAction redisInfo
--   $ void $ setex (pack . show $ uid) 3600 (pack . show $ user)

-- fetchUserDTORedis :: RedisInfo -> Int64 -> IO (Maybe UserDTO)
-- fetchUserDTORedis redisInfo uid = runRedisAction redisInfo $ do
--   result <- Redis.get (pack . show $ uid)
--   case result of
--     Right (Just userString) -> return $ Just (read . unpack $ userString)
--     _                       -> return Nothing

-- deleteUserDTORedis :: RedisInfo -> Int64 -> IO ()
-- deleteUserDTORedis redisInfo uid = do
--   connection <- connect redisInfo
--   runRedis connection $ do
--     _ <- del [pack . show $ uid]
--     return ()
