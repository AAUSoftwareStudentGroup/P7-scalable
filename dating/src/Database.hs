{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Database where

import           Control.Lens
import           Control.Monad              (void, sequence, foldM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger       (LogLevel (..), LoggingT,
                                             MonadLogger, filterLogger,
                                             runStdoutLoggingT)
import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Crypto.Hash
import           Codec.Picture              (saveJpgImage, readJpeg)
import           Codec.Picture.Jpg          (encodeJpeg, decodeJpeg)
import           Codec.Picture.Types
import           Codec.Picture.Saving
import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.Bson                  (Document, Value, fval, typed, val)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Base64     as Base64
import           Data.ByteString.Char8      (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Either                (rights, fromRight)
import           Data.Generics.Product      (field, getField, super)
import           Data.Int                   (Int64)
import           Data.List                  (intersperse, sort, sortBy, zip)
import           Data.Maybe                 (listToMaybe)
import           Data.Ord                   (comparing)
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack, unpack)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Time.Calendar         (fromGregorian)
import           Data.Time.Clock            (UTCTime (..), getCurrentTime,
                                             secondsToDiffTime)
import qualified Database.MongoDB           as Mongo
import           Database.MongoDB.Admin     as Mongo.Admin
import           Database.MongoDB.Query     (modify, find, findOne, rest, select, project, limit)
import           Database.Persist
import           Database.Persist.MongoDB
import           Database.Persist.Types     (unHaskellName)
import           Database.Persist.TH
import           Database.Redis             (ConnectInfo, Redis, connect,
                                             connectHost, defaultConnectInfo,
                                             del, runRedis, setex)
import qualified Database.Redis             as Redis
import           GHC.Generics               (Generic)
import           Language.Haskell.TH.Syntax
import           Network                    (PortID (PortNumber))
import           System.IO.Unsafe           (unsafePerformIO)
import qualified System.Random              as Random
import           Servant.Server.Internal.ServantErr
import           FrontendTypes
import           Schema


-------------------------------------------------------------------------------
--                                   TYPES                                   --
-------------------------------------------------------------------------------

type Username = Text
type AuthToken = Text


type MongoInfo = MongoConf
type RedisInfo = ConnectInfo


userIndex = (Mongo.Admin.index "users" ["username" =: (1::Int)]) {iUnique = True, iDropDups = True}

-------------------------------------------------------------------------------
--                            CONNECTION INFO                                --
-------------------------------------------------------------------------------


-- | Should probably be placed in a file instead
localMongoInfo :: MongoInfo
localMongoInfo = conf { mgAuth = Just $ MongoAuth "datingdbuser" "datingdbpassword"
                      , mgHost = "mongodb"
                      }
  where
    conf = defaultMongoConf "datingdb"

-- | Has type IO because it should fetch the connection string from a file
fetchMongoInfo :: IO MongoInfo
fetchMongoInfo = return localMongoInfo


-- | Has IO for same reason as fetchMongoInfo
fetchRedisInfo :: IO RedisInfo
fetchRedisInfo = return $ defaultConnectInfo {connectHost = "redis"}


-------------------------------------------------------------------------------
--                                   USERS                                   --
-------------------------------------------------------------------------------

-- | Create a user if the username is not taken
createUser :: MongoConf -> CreateUserDTO -> IO (Either ServantErr LoggedInDTO)
createUser mongoConf createUserDTO = runAction mongoConf action
  where
    action :: Action IO (Either ServantErr LoggedInDTO)
    action = do
      authToken <- liftIO mkAuthToken
      salt <- liftIO mkAuthToken
      let newUser = User
            { userEmail       = getField @"email"       createUserDTO
            , userPassword    = hashPassword (getField @"password" createUserDTO) salt
            , userUsername    = getField @"username"    createUserDTO
            , userGender      = getField @"gender"      createUserDTO
            , userBirthday    = getField @"birthday"    createUserDTO
            , userTown        = getField @"town"        createUserDTO
            , userProfileText = getField @"profileText" createUserDTO
            , userImage       = ("/img/users/" <> salt <> ".jpg")
            , userAuthToken   = authToken
            , userSalt        = salt
            }
      canNotBeInserted <- checkUnique newUser
      case canNotBeInserted of
        Just a ->
          if ((unHaskellName $ fst $ head $ persistUniqueToFieldNames a) == "username") then
            return $ Left $ err409 { errBody = ("A user named \"" <> (LBS.fromStrict $ encodeUtf8 (getField @"username" createUserDTO)) <> "\" already exists") }
          else
            return $ Left $ err409 { errBody = ("A user with email \"" <> (LBS.fromStrict $ encodeUtf8 (getField @"email" createUserDTO)) <> "\" already exists") }
        Nothing -> do
          case (urlFromBase64EncodedImage (getField @"imageData" createUserDTO) salt) of
            Left a -> return $ Left $ err415 { errBody = a }
            Right img -> do
              getImg <- liftIO img
              userId <- insert newUser
              addIndex <- Mongo.Admin.ensureIndex userIndex
              return $ Right $ LoggedInDTO (getField @"username" createUserDTO) authToken


urlFromBase64EncodedImage :: Text -> Text -> Either LBS.ByteString (IO())
urlFromBase64EncodedImage img salt =
    case (decodeJpeg $ fromRight "" $ Base64.decode $ encodeUtf8 img) of
      Left _ -> Left ("Invalid image, must be a Jpg") --fromRight 
      Right image -> Right $ saveJpgImage 90 (T.unpack ("/app/user/frontend/img/users/" <> salt <> ".jpg")) image


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


-- | Fetch all users
fetchAllUsers :: MongoConf -> Int -> Int -> IO [UserDTO]
fetchAllUsers mongoConf offset limit = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO [UserDTO]
    fetchAction = fmap userEntityToUserDTO <$> selectList [] [OffsetBy offset, LimitTo limit]


-- | Return "True" or "False" if user exists
fetchUserExists :: MongoConf -> Username -> IO Bool
fetchUserExists mongoConf username = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO Bool
    fetchAction = do
      mUser <- getBy (UniqueUsername username)
      case mUser of
        Just _ -> return  True
        Nothing -> return False


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
        Just (Entity id user) ->
          if hashPassword password (getField @"userSalt" user) == getField @"userPassword" user
            then do
              token <- liftIO mkAuthToken
              temp <- update id [UserAuthToken =. token]
              return $ Just LoggedInDTO
                { username  = getField @"userUsername"  user
                , authToken = token
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
        Nothing              -> return Nothing
        Just (Entity _ user) -> return . Just $ getField @"userUsername" user


removeAuthToken :: MongoConf -> AuthToken -> IO ()
removeAuthToken mongoConf token = runAction mongoConf action
  where
    action :: Action IO ()
    action = do
      maybeEntUser <- getBy (UniqueAuthToken token)
      case maybeEntUser of
        Nothing               -> return ()
        Just (Entity id user) -> void $ update id [UserAuthToken =. ""]


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
          { messageAuthor = from
          , messageTime = currentTime
          , messageText = body
          }

fetchConversation :: MongoConf -> Username -> Username -> Int -> Int -> IO ConversationDTO
fetchConversation mongoConf ownUsername otherUsername offset limit = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO ConversationDTO
    fetchAction = do
      maybeDoc <- findOne 
        ( ( select [ "members" =: (ownUsername::Text), "members" =: (otherUsername::Text) ] "conversations") 
          { project = [ "messages" =: [ "$slice" =: [ (offset::Int) , (limit::Int) ] ] ] }
        )
      case maybeDoc of
        Just doc -> do
          case docToEntityEither doc of
            Left _ -> return emptyConvoDTO
            Right convo -> return $ convoEntityToConversationDTO convo otherUsername
        Nothing -> return emptyConvoDTO
      

    emptyConvoDTO :: ConversationDTO
    emptyConvoDTO = ConversationDTO otherUsername mempty


fetchConversationPreviews :: MongoConf -> Username -> IO [ConversationPreviewDTO]
fetchConversationPreviews mongoConf ownUsername = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO [ConversationPreviewDTO]
    fetchAction = do
      cursor <- find 
        ( ( select [ "members" =: (ownUsername::Text) ] "conversations") 
          { project = [ "messages" =: [ "$slice" =: (-1::Int) ] ] }
        )
      docList <- rest cursor
      return $
        fmap (conversationEntityToConversationPreviewDTO ownUsername) . rights . fmap docToEntityEither
        $ docList



-------------------------------------------------------------------------------
--                                 Questions                                 --
-------------------------------------------------------------------------------
        

fetchQuestions :: MongoConf -> Username -> IO [QuestionDTO]
fetchQuestions mongoConf username = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO [QuestionDTO]
    fetchAction = do
      cursor <- find 
        ( ( select ["user_answers.username" =: ["$ne" =: username]] "questions")
          { project = ["survey_answers" =: (0::Int), "user_answers" =: (0::Int)]
          , limit = 10
          }
        )  
      docList <- rest cursor
      return $ fmap questionToQuestionDTO . rights . fmap docToEntityEither $ docList
    

postAnswer :: MongoConf -> Username -> AnswerDTO -> IO (Either ServantErr Text)
postAnswer mongoConf username (AnswerDTO id response) = runAction mongoConf postAction
  where
    postAction :: Action IO (Either ServantErr Text)
    postAction = do
      answerToInsert <- liftIO $ answerFromAnswerInfo username response
      case readMayObjectId id of
        Just oId -> do
          updatingQuestion <- modify 
            ( select ["_id" =: oId, "user_answers.username" =: [ "$ne" =: (username::Text)]] "questions")
            ["$push" =: ["user_answers" =: ((recordToDocument $ answerToInsert)::Document)]] 
          return $ Right $ "Successfully inserted"
        Nothing -> return $ Left $ err406 { errBody = "No such ID" }

    answerFromAnswerInfo :: Username -> Text -> IO UserAnswer
    answerFromAnswerInfo name body = do
      currentTime <- getCurrentTime
      return UserAnswer
          { userAnswerUsername = name
          , userAnswerScore = body
          , userAnswerTime = currentTime
          }


-------------------------------------------------------------------------------
--                                 HELPERS                                   --
-------------------------------------------------------------------------------

runAction mongoConf action = withConnection mongoConf $
  \pool -> runMongoDBPoolDef action pool

        
conversationEntityToConversationPreviewDTO :: Text -> Entity Conversation -> ConversationPreviewDTO
conversationEntityToConversationPreviewDTO username (Entity _ convo) = conversationPreview
  where
    members = getField @"conversationMembers" convo
    message = last $ getField @"conversationMessages" convo
    conversationPreview = ConversationPreviewDTO
      { convoWithUsername = if head members == username then last members else head members
      , isLastAuthor = username == getField @"messageAuthor" message
      , body = getField @"messageText" message
      , timeStamp = getField @"messageTime" message
      }


userEntityToUserDTO :: Entity User -> UserDTO
userEntityToUserDTO (Entity _ user) = UserDTO
  { username    = userUsername user
  , gender      = userGender user
  , birthday    = userBirthday user
  , town        = userTown user
  , profileText = userProfileText user
  , imageUrl    = userImage user
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
      { authorUsername = messageAuthor message
      , timeStamp = messageTime message
      , body = messageText message
      }

questionToQuestionDTO :: Entity Question -> QuestionDTO
questionToQuestionDTO (Entity key q) = QuestionDTO
  { id = keyToText $ toBackendKey key
  , question = getField @"questionText" q
  }
        
hashPassword :: Text -> Text -> Text
hashPassword password salt = T.pack $ show hashed
  where
    passPlusSalt = encodeUtf8 (password <> salt)
    hashed = hash passPlusSalt :: Digest SHA3_512
