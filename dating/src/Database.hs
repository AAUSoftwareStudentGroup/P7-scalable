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
import           Data.Generics.Product      (field, getField, super)
import           Data.Int                   (Int64)
import           Data.List                  (intersperse, sort, sortBy)
import           Data.Maybe                 (listToMaybe)
import           Data.Ord                   (comparing)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack, unpack)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf16BE)
import           Data.Time.Calendar         (fromGregorian)
import           Data.Time.Clock            (UTCTime (..), getCurrentTime,
                                             secondsToDiffTime)
import qualified Database.MongoDB           as Mongo
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
import           System.IO.Unsafe           (unsafePerformIO)
import qualified System.Random              as Random

import           FrontendTypes
import           Schema


-------------------------------------------------------------------------------
--                                   TYPES                                   --
-------------------------------------------------------------------------------

type Username = Text
type AuthToken = Text


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
      let newUser = User
            { userEmail       = getField @"email"       createUserDTO
            , userPassword    = getField @"password"    createUserDTO -- TODO: Hash
            , userUsername    = getField @"username"    createUserDTO
            , userGender      = getField @"gender"      createUserDTO
            , userBirthday    = getField @"birthday"    createUserDTO
            , userTown        = getField @"town"        createUserDTO
            , userProfileText = getField @"profileText" createUserDTO
            , userAuthToken   = authToken
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
    fetchAction = fmap userEntityToLoggedInDTO <$>
      selectFirst [UserUsername ==. username, UserPassword ==. password] []



fetchUsernameByAuthToken :: MongoConf -> AuthToken -> IO (Maybe Username)
fetchUsernameByAuthToken mongoConf authToken = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO (Maybe Username)
    fetchAction = do

      maybeEntUser <- getBy (UniqueAuthToken authToken)
      case maybeEntUser of
        Nothing              -> return Nothing
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
fetchConversationPreviews mongoConf ownUsername = undefined -- TODO


-------------------------------------------------------------------------------
--                                 HELPERS                                   --
-------------------------------------------------------------------------------

runAction mongoConf action = withConnection mongoConf $
  \pool -> runMongoDBPoolDef action pool


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



