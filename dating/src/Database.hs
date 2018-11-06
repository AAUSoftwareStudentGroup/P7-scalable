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
import qualified Crypto.Hash.SHA512         as SHA512    
import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack, unpack)
import           Data.Bson                  (val, fval, typed, Document, Value)
import           Data.Generics.Product      (field, getField, super)
import           Data.Int                   (Int64)
import           Data.List                  (intersperse, sort, sortBy)
import           Data.Maybe                 (listToMaybe)
import           Data.Semigroup             ((<>))
import           Data.Ord                   (comparing)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack, unpack)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf16BE, encodeUtf16LE, decodeUtf16LE)
import           Data.Time.Calendar         (fromGregorian)
import           Data.Either                (rights)
import qualified Database.MongoDB           as Mongo
import           Database.MongoDB.Query     (find, select, rest)   
import           Data.Time.Clock            (UTCTime (..), secondsToDiffTime, getCurrentTime)
import           Database.Persist
import           Database.Persist.MongoDB
import           Database.Persist.TH
import           Database.Redis             (ConnectInfo, Redis, connect,
                                             connectHost, defaultConnectInfo,
                                             del, runRedis, setex)
import qualified Database.Redis             as Redis
import           Elm                        (ElmType)
import           GHC.Generics               (Generic)
import           Language.Haskell.TH.Syntax
import           Network                    (PortID (PortNumber))
import qualified System.Random              as Random
import           System.IO.Unsafe           (unsafePerformIO)

import           FrontendTypes
import           Schema


-------------------------------------------------------------------------------
--                            CONNECTION INFO                                --
-------------------------------------------------------------------------------

type MongoInfo = MongoConf

-- | Should probably be placed in a file instead
localMongoConf :: MongoInfo
localMongoConf = conf { mgAuth = Just $ MongoAuth "datingdbuser" "datingdbpassword"
                      , mgHost = "mongodb"
                      }
  where
    conf = defaultMongoConf "datingdb"

-- | Has type IO because it should fetch the connection string from a file
fetchMongoInfo :: IO MongoInfo
fetchMongoInfo = return localMongoConf

type RedisInfo = ConnectInfo

-- | Has IO for same reason as fetchMongoInfo
fetchRedisInfo :: IO RedisInfo
fetchRedisInfo = return $ defaultConnectInfo {connectHost = "redis"}


-------------------------------------------------------------------------------
--                                   USERS                                   --
-------------------------------------------------------------------------------

-- | Create a user if the username is not taken
createUser :: MongoConf -> CreateUserDTO -> IO LoggedInDTO
createUser mongoConf createUserDTO = runAction mongoConf action
  where
    action :: Action IO LoggedInDTO
    action = do
      authToken <- liftIO mkAuthToken
      salt <- liftIO mkAuthToken
      let newUser = User
            { userEmail       = getField @"email"       createUserDTO
            , userPassword    = hashPassword (getField @"password"    createUserDTO) salt -- TODO: Hash
            , userUsername    = getField @"username"    createUserDTO
            , userGender      = getField @"gender"      createUserDTO
            , userBirthday    = getField @"birthday"    createUserDTO
            , userTown        = getField @"town"        createUserDTO
            , userProfileText = getField @"profileText" createUserDTO
            , userAuthToken   = authToken
            , userSalt        = salt
            }
      userId <- insert newUser
      return $ LoggedInDTO (getField @"username" createUserDTO) authToken


      -- | Generate a random authtoken.
mkAuthToken :: IO Text
mkAuthToken = do
  generator <- Random.newStdGen
  return . T.pack . take 32 $ Random.randomRs ('a', 'z') generator


-- | Fetch a user by username
fetchUser :: MongoConf -> Username -> IO (Maybe UserDTO)
fetchUser mongoConf username = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO (Maybe UserDTO)
    fetchAction = fmap userEntityToUserDTO <$> getBy (UniqueUsername username)


-- | Fetch a list of users
fetchAllUsers :: MongoConf -> IO [UserDTO]
fetchAllUsers mongoConf = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO [UserDTO]
    fetchAction = fmap userEntityToUserDTO <$> selectList [] []


-------------------------------------------------------------------------------
--                             AUTHENTICATION                                --
-------------------------------------------------------------------------------

fetchUserByCredentials :: MongoConf -> CredentialDTO -> IO (Maybe LoggedInDTO)
fetchUserByCredentials mongoConf credentials = runAction mongoConf fetchAction
  where
    username = getField @"username" credentials
    password = getField @"password" credentials
    
    fetchAction :: Action IO (Maybe LoggedInDTO)
    fetchAction = do 
      maybeEntUser <- selectFirst [UserUsername ==. username] []
      case maybeEntUser of
        Nothing -> return Nothing
        Just (Entity _ user) -> 
            if (hashPassword password $ getField @"userSalt" user) == getField @"userPassword" user 
              then 
                return $ Just LoggedInDTO
                { username  = getField @"userUsername"  user
                , authToken = getField @"userAuthToken" user
                }
              else
                return Nothing      



fetchUsernameByAuthToken :: MongoConf -> AuthToken -> IO (Maybe Username)
fetchUsernameByAuthToken mongoConf authToken = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO (Maybe Username)
    fetchAction = do
      
      maybeEntUser <- getBy (UniqueAuthToken authToken)
      case maybeEntUser of
        Nothing -> return Nothing
        Just (Entity _ user) -> return . Just $ getField @"userUsername" user


-------------------------------------------------------------------------------
--                              CONVERSATIONS                                --
-------------------------------------------------------------------------------

createMessage :: MongoConf -> Username -> Username -> CreateMessageDTO -> IO ()
createMessage mongoConf from to messageDTO = runAction mongoConf action
  where
    action :: Action IO ()
    action = do
      msgToInsert <- liftIO $ mkMessage from body
      maybeConvo <- selectFirst [ConversationMembers `anyEq` from, ConversationMembers `anyEq` to] []
      case maybeConvo of
        Nothing -> void $ insert (mkConversation from to msgToInsert)
        Just (Entity conversationId convo) -> 
          update conversationId [ConversationMessages `push` msgToInsert]

    body :: Text
    body = getField @"body" messageDTO

    mkConversation :: Username -> Username -> Message -> Conversation
    mkConversation from to msg = conversation
      where
        conversation = Conversation
          { conversationMembers = [from, to]
          , conversationMessages = [msg]
          }
          
    mkMessage :: Username -> Text -> IO Message
    mkMessage from body = do 
      currentTime <- getCurrentTime
      return  Message
          { messageAuthorUsername = from
          , messageTimeStamp = currentTime
          , messageBody = body
          }

fetchConversation :: MongoConf -> Username -> Username -> IO ConversationDTO
fetchConversation mongoConf ownUsername otherUsername = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO ConversationDTO
    fetchAction = do
       maybeConvo <- selectFirst [ConversationMembers `anyEq` ownUsername, ConversationMembers `anyEq` otherUsername] []
       case maybeConvo of
        Nothing -> return emptyConvoDTO
        Just convo -> return $ convoEntityToConversationDTO convo otherUsername
    
    emptyConvoDTO :: ConversationDTO
    emptyConvoDTO = ConversationDTO otherUsername mempty


fetchConversationPreviews :: MongoConf -> Username -> IO [ConversationPreviewDTO]
fetchConversationPreviews mongoConf ownUsername = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO [ConversationPreviewDTO]
    fetchAction = do
      cursor <- find ( select 
        [ "members.username" =: (ownUsername::Text)
        , "messages" =: 
            [ "$slice" =: (-1::Int)
            ]
        ] "conversations")
      docList <- rest cursor
      return $ fmap (conversationEntityToConversationPreviewDTO ownUsername) . rights . fmap docToEntityEither $ docList
      --return $ fmap (conversationEntityToConversationPreviewDTO ownUsername) (fmap docToEntityEither (rest cursor))



-------------------------------------------------------------------------------
--                                 HELPERS                                   --
-------------------------------------------------------------------------------

runAction mongoConf action = withConnection mongoConf $
  \pool -> runMongoDBPoolDef action pool

{-
toConversation :: Document -> Conversation
toConversation document = convo
  where
    convo = Conversation
      { conversationMembers = []
      , conversationMessages = [toMsgFromField $ look "messages" document]
      }

toMsgFromField :: [Value] -> Message
toMsgFromField values = msg
  where
    msg = Message
      { messageAuthorUsername =  typed values[0]
      , messageTimeStamp      =  typed values[1]
      , messageBody           =  typed values[2]
      }
-}
conversationEntityToConversationPreviewDTO :: Text -> Entity Conversation -> ConversationPreviewDTO
conversationEntityToConversationPreviewDTO username (Entity _ convo) = conversationPreview
  where
    members = getField @"conversationMembers" convo
    message = last $ getField @"conversationMessages" convo
    conversationPreview = ConversationPreviewDTO
      { convoWithUsername = if (head members == username) then last members else head members
      , isLastAuthor = (username == getField @"messageAuthorUsername" message)
      , body = getField @"messageBody" message
      , timeStamp = getField @"messageTimeStamp" message
      }


userEntityToUserDTO :: Entity User -> UserDTO
userEntityToUserDTO (Entity _ user) = userDTO
  where
    userDTO = UserDTO
      { username    = userUsername user
      , gender      = userGender user
      , birthday    = userBirthday user
      , town        = userTown user
      , profileText = userProfileText user
      }

userEntityToLoggedInDTO :: Entity User -> LoggedInDTO
userEntityToLoggedInDTO (Entity _ user) = LoggedInDTO
  { username  = getField @"userUsername"  user
  , authToken = getField @"userAuthToken" user}

convoEntityToConversationDTO :: Entity Conversation -> Text -> ConversationDTO
convoEntityToConversationDTO (Entity _ convo) username = conversationDTO
  where
    conversationDTO = ConversationDTO
      { convoWithUsername = username
      , messages = map messageToMessageDTO (conversationMessages convo)
      }
  
messageToMessageDTO :: Message -> MessageDTO
messageToMessageDTO message = messageDTO
  where
    messageDTO = MessageDTO
      { authorUsername = messageAuthorUsername message
      , body = messageBody message
      , timeStamp = messageTimeStamp message
      }


hashPassword :: Text -> Text -> Text
hashPassword password salt = decodeUtf16LE hash
  where
    hash :: ByteString
    hash = SHA512.finalize ctx
    ctx = SHA512.update iCtx $ encodeUtf16LE $ (password <> salt)
    iCtx = SHA512.init

-------------------------------------------------------------------------------
--                                   TYPES                                   --
-------------------------------------------------------------------------------

type Username = Text
type AuthToken = Text



-------------------------------------------------------------------------------
--                                    OLD                                    --
-------------------------------------------------------------------------------

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
