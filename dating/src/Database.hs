{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Database where

import Codec.Picture (saveJpgImage)
import Codec.Picture.Jpg (decodeJpeg)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash
import Data.Bson (Document)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (rights)
import Data.Foldable (traverse_)
import Data.Generics.Product (getField)
import qualified Data.Maybe as Maybe
import qualified Data.List  as List
import qualified Data.List.Extra as ListE
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time.Clock as Clock (getCurrentTime, nominalDay, diffUTCTime, UTCTime)
import qualified Database.MongoDB.Admin as Mongo.Admin
import qualified Database.MongoDB.Query as Mongo.Query
import Database.Persist
import Database.Persist.MongoDB (Action, (=:))
import qualified Database.Persist.MongoDB as Persist.Mongo
import Database.Persist.Types (unHaskellName)
import qualified Database.Redis as Redis
import Servant.Server.Internal.ServantErr
import System.Directory
import qualified System.Random as Random

import FrontendTypes
import Schema


{-----------------------------------------------------------------------------}
{-                                   TYPES                                   -}
{-----------------------------------------------------------------------------}
type Username = Text

type AuthToken = Text

type MongoInfo = Persist.Mongo.MongoConf

type RedisInfo = Redis.ConnectInfo

userIndex :: Mongo.Admin.Index
userIndex =
  (Mongo.Admin.index "users" ["username" =: (1 :: Int)])
    {Mongo.Admin.iUnique = True, Mongo.Admin.iDropDups = True}

{-----------------------------------------------------------------------------}
{-                            CONNECTION INFO                                -}
{-----------------------------------------------------------------------------}
-- | Should probably be placed in a file instead
localMongoInfo :: MongoInfo
localMongoInfo =
  conf
    { Persist.Mongo.mgAuth =
        Just $ Persist.Mongo.MongoAuth "datingdbuser" "datingdbpassword"
    , Persist.Mongo.mgHost = "mongodb"
    }
  where
    conf = Persist.Mongo.defaultMongoConf "datingdb"

-- | Has type IO because it should fetch the connection string from a file
fetchMongoInfo :: IO MongoInfo
fetchMongoInfo = return localMongoInfo

-- | Has IO for same reason as fetchMongoInfo
fetchRedisInfo :: IO RedisInfo
fetchRedisInfo = return $ Redis.defaultConnectInfo {Redis.connectHost = "redis"}

{-----------------------------------------------------------------------------}
{-                                   USERS                                   -}
{-----------------------------------------------------------------------------}
-- | Create a user if the username is not taken
createUser :: MongoInfo -> CreateUserDTO -> IO (Either ServantErr LoggedInDTO)
createUser mongoConf createUserDTO = runAction mongoConf action
  where
    action :: Action IO (Either ServantErr LoggedInDTO)
    action = do
      authToken' <- liftIO mkAuthToken
      salt <- liftIO mkAuthToken
      image <- liftIO mkAuthToken
      let newUser =
            User
              { userEmail = getField @"email" createUserDTO
              , userPassword =
                  hashPassword (getField @"password" createUserDTO) salt
              , userUsername = getField @"username" createUserDTO
              , userGender = getField @"gender" createUserDTO
              , userBirthday = getField @"birthday" createUserDTO
              , userTown = getField @"town" createUserDTO
              , userProfileText = getField @"profileText" createUserDTO
              , userImage = "/img/users/" <> image <> ".jpg"
              , userAuthToken = authToken'
              , userSalt = salt
              , userBeingPredicted = False
              }
      canNotBeInserted <- checkUnique newUser
      case canNotBeInserted of
        Just a ->
          if (unHaskellName . fst . head . persistUniqueToFieldNames $ a) ==
             "username"
            then return $
                 Left $
                 err409
                   { errBody =
                       "A user named \"" <>
                       (LBS.fromStrict . encodeUtf8 $
                        getField @"username" createUserDTO) <>
                       "\" already exists"
                   }
            else return $
                 Left $
                 err409
                   { errBody =
                       "A user with email \"" <>
                       (LBS.fromStrict . encodeUtf8 $
                        getField @"email" createUserDTO) <>
                       "\" already exists"
                   }
        Nothing ->
          case urlFromBase64EncodedImage
                 (getField @"imageData" createUserDTO)
                 salt of
            Left a -> return $ Left $ err415 {errBody = a}
            Right img -> do
              liftIO img
              _ <- Persist.Mongo.insert newUser
              _ <- Mongo.Admin.ensureIndex userIndex
              answerToInsert <- liftIO $ answerFromAnswerInfo (getField @"username" createUserDTO) 0 True
              _ <- Mongo.Query.modify
                ( Mongo.Query.select [] "questions")
                ["$push" =: ["answers" =: (Persist.Mongo.recordToDocument answerToInsert :: Document)]]
              return $ Right $ LoggedInDTO (getField @"username" createUserDTO) authToken' True



urlFromBase64EncodedImage :: Text -> Text -> Either LBS.ByteString (IO())
urlFromBase64EncodedImage img salt =
  case (Base64.decode . encodeUtf8 $ img) >>= decodeJpeg of
    Left _ -> Left "Invalid image, must be a Jpg"
    Right image ->
      Right $
      saveJpgImage
        90
        (T.unpack ("/app/user/frontend/img/users/" <> salt <> ".jpg"))
        image

-- | Generate a random authtoken.
mkAuthToken :: IO Text
mkAuthToken = do
  generator <- Random.newStdGen
  return . T.pack . take 32 $ Random.randomRs ('a', 'z') generator

-- | Fetch a user by username
fetchUser :: MongoInfo -> Username -> IO (Maybe UserDTO)
fetchUser mongoConf username' = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO (Maybe UserDTO)
    fetchAction = fmap userEntityToUserDTO <$> getBy (UniqueUsername username')

fetchAllUsers :: MongoInfo -> IO [Username]
fetchAllUsers mongoConf = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO [Username]
    fetchAction = fmap usernameFromUser <$> selectList [] []

    usernameFromUser :: Entity User -> Username
    usernameFromUser (Entity _ user) = getField @"userUsername" user

-- | Fetch all users
fetchUsers :: MongoInfo -> Username -> Int -> Int -> IO [UserDTO]
fetchUsers mongoConf username' offset askedLimit =
  runAction mongoConf fetchAction
  where
    limit =
      if askedLimit > 30
        then 30
        else askedLimit
    fetchAction :: Action IO [UserDTO]
    fetchAction =
      fmap userEntityToUserDTO <$>
      selectList [UserUsername !=. username'] [OffsetBy offset, LimitTo limit]

-- | Fetch user matches
fetchMatchesForUser :: MongoInfo -> Int -> Int -> Username -> IO [UserWithScoreDTO]
fetchMatchesForUser mongoConf offset limit username' = runAction mongoConf fetchAction
  where
    userMatchEntityToUsername :: Username -> Entity UserMatches -> (Username, Double)
    userMatchEntityToUsername username'' (Entity _ match') =
      let
        (u1:u2:_) = getField @"userMatchesMatch" match'
        score' = getField @"userMatchesCorrelation" match'
      in
        if u1 == username'' then (u2, score') else (u1, score')

    fetchAction :: Action IO [UserWithScoreDTO]
    fetchAction = do
      matches <- selectList [UserMatchesMatch `Persist.Mongo.anyEq` username'] [Desc UserMatchesCorrelation, OffsetBy offset, LimitTo limit]
      let pairs = fmap (userMatchEntityToUsername username') matches
      userDTOs <- liftIO $ fmap Maybe.catMaybes . mapM (fetchUser mongoConf . fst) $ pairs
      return . map (uncurry UserWithScoreDTO) $ zip userDTOs (map snd pairs)

    onOtherUsername :: Username -> Entity UserMatches -> Text
    onOtherUsername username'' (Entity _ match'') =
      let (u1:u2:_) = getField @"userMatchesMatch" match''
      in if username'' == u1 then u2 else u1
    
    ownUsername :: UserDTO -> Text
    ownUsername = getField @"username"

    matchScore :: (Entity UserMatches, UserDTO) -> Double
    matchScore (Entity _ uMatch,_) = getField @"userMatchesCorrelation" uMatch


-- | Is it time to predict?
timeToPredict :: MongoInfo -> Username -> IO (Either ServantErr Bool)
timeToPredict mongoConf username = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO (Either ServantErr Bool)
    fetchAction = do
      mUser <- getBy (UniqueUsername username)
      case mUser of
        Just (Entity _ user) ->
          if getField @"userBeingPredicted" user then
            return $ Left $ err409 { errBody = "Already predicting for this user" }
          else do
            answeredEnough <- haveWeAnsweredEnough
            if answeredEnough then do
              timeOfLastPrediction <- fetchTimeOfPredictedAnswer
              allQuestionsAnswered <- noQuestionsRemain 
              result <- questionsAnsweredSinceLastPrediction timeOfLastPrediction
      
              if List.null result && allQuestionsAnswered then return $ Right False
              else if allQuestionsAnswered || List.length result == 5 then return $ Right True
              else return $ Right False
            else
              return $ Left $ err412 { errBody = "Need to answer more questions" }
        Nothing ->
          return $ Left $ err404 { errBody = "User not found, this should be impossible" }


    haveWeAnsweredEnough :: Action IO Bool
    haveWeAnsweredEnough = do
      docList <- Mongo.Query.find
        ( (Mongo.Query.select 
            ["answers" =: 
              ["$elemMatch" =: ["answerer" =: username, "ispredicted" =: False]]
            ]
          "questions")
          { Mongo.Query.project = ["answers" =: (0::Int)]
          , Mongo.Query.limit = 10
          }
        )
      haveAnsweredEnough <- Mongo.Query.rest docList
      return $ List.length haveAnsweredEnough == 10

    noQuestionsRemain :: Action IO Bool
    noQuestionsRemain = do
      mQuestion <- Mongo.Query.findOne
        ( (Mongo.Query.select 
            ["answers" =: 
              ["$elemMatch" =: 
                ["answerer" =: username, "ispredicted" =: True]
              ]
            ]
          "questions")
          { Mongo.Query.project = ["answers" =: (0::Int)] }
        )
      case mQuestion of
        Just _ -> return False
        Nothing -> return True
    
    questionsAnsweredSinceLastPrediction :: Clock.UTCTime -> Action IO [Document]
    questionsAnsweredSinceLastPrediction timeOfLastAnswer = do
      docList <- Mongo.Query.find
        ( (Mongo.Query.select 
            ["answers" =: 
              ["$elemMatch" =: 
                ["answerer" =: username, "timestamp" =: ["$gt" =: timeOfLastAnswer], "ispredicted" =: False]
              ]
            ]
          "questions")
          { Mongo.Query.project = ["answers" =: (0::Int)]
          , Mongo.Query.limit = 5
          }
        )
      Mongo.Query.rest docList


    fetchTimeOfPredictedAnswer :: Action IO Clock.UTCTime
    fetchTimeOfPredictedAnswer = do
      mDoc <- Mongo.Query.findOne
        ( (Mongo.Query.select 
          [ "answers" =: 
            [ "$elemMatch" =: 
              [ "answerer" =: username, "ispredicted" =: True] 
            ] 
          ]
          "questions"
          )
          { Mongo.Query.project = 
            [ "answers" =: 
              [ "$elemMatch" =: 
                [ "answerer" =: username, "ispredicted" =: True] 
              ] 
            , "index" =: (1::Int)
            , "text" =: (1::Int)
            ] 
          }
        )
      case mDoc of
        Just doc ->
          case Persist.Mongo.docToEntityEither doc of
            Right question -> return $ getField @"answerTimestamp" $ extractAnswer question
            Left _ -> liftIO Clock.getCurrentTime
        Nothing -> liftIO Clock.getCurrentTime
          
    
    extractAnswer :: Entity Question -> Answer
    extractAnswer (Entity _ q) = head $ getField @"questionAnswers" q

    answerTime :: Answer -> Clock.UTCTime
    answerTime = getField @"answerTimestamp"

-- | Return "True" or "False" if user exists
fetchUserExists :: MongoInfo -> Username -> IO Bool
fetchUserExists mongoConf username' = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO Bool
    fetchAction = do
      mUser <- getBy (UniqueUsername username')
      case mUser of
        Just _ -> return True
        Nothing -> return False

-- | Edit user
updateUser ::
     MongoInfo -> Username -> EditUserDTO -> IO (Either ServantErr LoggedInDTO)
updateUser mongoConf username newFields = runAction mongoConf editAction
  where
    editAction :: Action IO (Either ServantErr LoggedInDTO)
    editAction = do
      dbEntry <- getBy (UniqueUsername username)
      case dbEntry of
        Just (Entity key user) ->
          case getField @"imageData" newFields of
            Just base64 ->
              case urlFromBase64EncodedImage base64 (getField @"userSalt" user) of
                Left txt -> return $ Left $ err415 {errBody = txt}
                Right img -> do
                  liftIO img
                  _ <- liftIO $ removeSingleFile (T.unpack ("frontend" <> getField @"userImage" user))
                  newImg <- liftIO mkAuthToken
                  _ <- update key ([UserImage =. "/img/users/" <> newImg <> ".jpg"] <> updatedValues newFields (getField @"userSalt" user))
                  return . Right $ userEntityToLoggedInDTO (Entity key user)
            Nothing -> do
              _ <-
                update key $ updatedValues newFields (getField @"userSalt" user)
              return . Right $ userEntityToLoggedInDTO (Entity key user)
        Nothing ->
          return . Left $ err409 {errBody = "This user does not exist in the database"}
    updatedValues :: EditUserDTO -> Text -> [Update User]
    updatedValues fields' salt = updates
      where
        password = (\newpass -> UserPassword =. hashPassword newpass salt) <$> getField @"password" fields'
        gender = (UserGender =.) <$> getField @"gender" fields'
        birthday = (UserBirthday =.) <$> getField @"birthday" fields'
        town = (UserTown =. ) <$> getField @"town" fields'
        profileText = (UserProfileText =.) <$> getField @"profileText" fields'
        updates = Maybe.catMaybes [password, gender, birthday, town, profileText]

{-----------------------------------------------------------------------------}
{-                             AUTHENTICATION                                -}
{-----------------------------------------------------------------------------}

fetchUserByCredentials :: MongoInfo -> CredentialDTO -> IO (Maybe LoggedInDTO)
fetchUserByCredentials mongoConf credentials = runAction mongoConf fetchAction
  where
    username' = getField @"username" credentials
    password' = getField @"password" credentials
    fetchAction :: Action IO (Maybe LoggedInDTO)
    fetchAction = do
      maybeEntUser <- selectFirst [UserUsername ==. username'] []
      case maybeEntUser of
        Nothing -> return Nothing
        Just (Entity id' user) ->
          if hashPassword password' (getField @"userSalt" user) ==
             getField @"userPassword" user
            then do
              token <- liftIO mkAuthToken
              _ <- update id' [UserAuthToken =. token]
              return $ Just LoggedInDTO
                { username  = getField @"userUsername"  user
                , authToken = token
                , firstLogIn = False
                }
            else
              return Nothing



fetchUsernameByAuthToken :: MongoInfo -> AuthToken -> IO (Maybe Username)
fetchUsernameByAuthToken mongoConf authToken' = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO (Maybe Username)
    fetchAction = do
      maybeEntUser <- getBy (UniqueAuthToken authToken')
      case maybeEntUser of
        Nothing -> return Nothing
        Just (Entity _ user) -> return . Just $ getField @"userUsername" user

removeAuthToken :: MongoInfo -> Username -> IO ()
removeAuthToken mongoConf username = runAction mongoConf action
  where
    action :: Action IO ()
    action = do
      maybeEntUser <- getBy (UniqueUsername username)
      case maybeEntUser of
        Nothing -> return ()
        Just (Entity id' _) -> void $ update id' [UserAuthToken =. ""]

{-----------------------------------------------------------------------------}
{-                              CONVERSATIONS                                -}
{-----------------------------------------------------------------------------}

createMessage :: MongoInfo -> Username -> Username -> CreateMessageDTO -> IO ()
createMessage mongoConf from to messageDTO = runAction mongoConf action
  where
    action :: Action IO ()
    action = do
      msgToInsert <- liftIO $ mkMessage from body'
      maybeConvo <-
        selectFirst
          [ ConversationMembers `Persist.Mongo.anyEq` from
          , ConversationMembers `Persist.Mongo.anyEq` to
          ]
          []
      case maybeConvo of
        Nothing -> do
          user' <- getBy (UniqueUsername to)
          case user' of
            Just _ -> void $ insert (mkConversation from to msgToInsert)
            Nothing -> return ()
        Just (Entity conversationId _) ->
          update
            conversationId
            [ConversationMessages `Persist.Mongo.push` msgToInsert]
    body' :: Text
    body' = getField @"body" messageDTO
    mkConversation :: Username -> Username -> Message -> Conversation
    mkConversation from' to' msg = conversation
      where
        conversation =
          Conversation
            {conversationMembers = [from', to'], conversationMessages = [msg]}
    mkMessage :: Username -> Text -> IO Message
    mkMessage from' body'' = do
      currentTime <- Clock.getCurrentTime
      return
        Message
          { messageAuthor = from'
          , messageTimestamp = currentTime
          , messageBody = body''
          }

fetchConversation ::
     MongoInfo -> Username -> Username -> Int -> Int -> IO ConversationDTO
fetchConversation mongoConf ownUsername otherUsername offset askedLimit =
  runAction mongoConf fetchAction
  where
    limit =
      if askedLimit > 100
        then 100
        else askedLimit
    fetchAction :: Action IO ConversationDTO
    fetchAction = do
      maybeDoc <-
        Mongo.Query.findOne
          ((Mongo.Query.select
              [ "$and" =: 
                [ ["members" =: (ownUsername :: Text)]
                , ["members" =: (otherUsername :: Text)]
                ]
              ]
              "conversations")
             { Mongo.Query.project =
                 ["messages" =: ["$slice" =: [offset :: Int, limit :: Int]]]
             })
      case maybeDoc of
        Just doc ->
          case Persist.Mongo.docToEntityEither doc of
            Left _ -> return emptyConvoDTO
            Right convo ->
              return $ convoEntityToConversationDTO convo otherUsername
        Nothing -> return emptyConvoDTO
    emptyConvoDTO :: ConversationDTO
    emptyConvoDTO = ConversationDTO otherUsername mempty

fetchConversationPreviews ::
     MongoInfo -> Username -> IO [ConversationPreviewDTO]
fetchConversationPreviews mongoConf ownUsername =
  runAction mongoConf fetchAction
  where
    fetchAction :: Action IO [ConversationPreviewDTO]
    fetchAction = do
      cursor <-
        Mongo.Query.find
          ((Mongo.Query.select
              ["members" =: (ownUsername :: Text)]
              "conversations")
             {Mongo.Query.project = ["messages" =: ["$slice" =: (-1 :: Int)]]})
      docList <- Mongo.Query.rest cursor
      return $
        fmap (conversationEntityToConversationPreviewDTO ownUsername) .
        rights . fmap Persist.Mongo.docToEntityEither $
        docList

{-----------------------------------------------------------------------------}
{-                                 Questions                                 -}
{-----------------------------------------------------------------------------}

fetchQuestions :: MongoInfo -> Username -> IO [QuestionDTO]
fetchQuestions mongoConf username' = runAction mongoConf fetchAction
  where
    fetchAction :: Action IO [QuestionDTO]
    fetchAction = do
      docList <- 
        Mongo.Query.aggregate "questions" 
          [
            [ "$match" =: 
              [ "answers" =: 
                [ "$not" =: 
                  [ "$elemMatch" =: 
                    [ "answerer" =: username', "ispredicted" =: False]
                  ]
                ]
              ]
            ]
          , [ "$project" =: 
              [ "answers" =: (0::Int)]
            ]
          , [ "$sample" =: 
              [ "size" =: (10::Int)]
            ]
          ]
      let questions = fmap questionToQuestionDTO . rights . fmap Persist.Mongo.docToEntityEither $ List.nub docList
      if List.length questions > 5 then
        return questions
      else
        fetchFewRemainingQuestions
    -- There is no guarantee that the $sample aggregate operation returns unique documents, so we remove duplicates
    -- with List.nub, then in case there are too few questions to return, we just fetch the first 10 questions like
    -- previously. $sample works fine on a single machine apparently, so could be a problem with sharding  
    fetchFewRemainingQuestions :: Action IO [QuestionDTO]
    fetchFewRemainingQuestions = do
      cursor <- Mongo.Query.find
        ( (Mongo.Query.select 
            [ "answers" =: 
              [ "$not" =: 
                [ "$elemMatch" =: 
                  [ "answerer" =: username', "ispredicted" =: False] 
                ] 
              ] 
            ]
            "questions"
          )
          { Mongo.Query.project = ["answers" =: (0::Int)]
          , Mongo.Query.limit = 10
          }
        )
      docList <- Mongo.Query.rest cursor
      return $
        fmap questionToQuestionDTO .
        rights . fmap Persist.Mongo.docToEntityEither $
        docList

createAnswer ::
     MongoInfo -> Username -> AnswerDTO -> IO (Either ServantErr Text)
createAnswer mongoConf username' (AnswerDTO id' response) =
  runAction mongoConf postAction
  where
    postAction :: Action IO (Either ServantErr Text)
    postAction =
      if response > 5 || response < 1
        then return . Left $
             err416 {errBody = "answer must be an integer between 1 and 5"}
        else do
          answerToInsert <-
            liftIO $ answerFromAnswerInfo username' (fromIntegral response) False
          case Persist.Mongo.readMayObjectId id' of
            Just oId -> do
              _ <-
                Mongo.Query.modify
                  (Mongo.Query.select
                     [ "_id" =: oId
                     , "answers" =:
                       [ "$elemMatch" =:
                         ["answerer" =: username', "ispredicted" =: True]
                       ]
                     ]
                     "questions")
                  [ "$set" =:
                    [ "answers.$" =:
                      (Persist.Mongo.recordToDocument answerToInsert :: Document)
                    ]
                  ]
              return . Right $ "Successfully inserted"
            Nothing -> return . Left $ err406 {errBody = "No such ID"}

answerFromAnswerInfo :: Username -> Double -> Bool -> IO Answer
answerFromAnswerInfo name score' isActualAnswer = do
  currentTime <- Clock.getCurrentTime
  return
    Answer
      { answerAnswerer = name
      , answerScore = score'
      , answerTimestamp = currentTime
      , answerIspredicted = isActualAnswer
      }

createEmbeddings :: MongoInfo -> EmbeddingsDTO -> IO ()
createEmbeddings mongoInfo embeddingsDTO = runAction mongoInfo insertAction
  where
    insertAction :: Action IO ()
    insertAction = do
      currentTime <- liftIO Clock.getCurrentTime
      let embeddings =
            Embeddings currentTime kValue' mse' iterations' userEmb' itemEmb'
      void $ Persist.Mongo.insert embeddings
    kValue' = kValue embeddingsDTO
    mse' = mse embeddingsDTO
    iterations' = iterations embeddingsDTO
    userEmb' = userEmb embeddingsDTO
    itemEmb' = itemEmb embeddingsDTO

  
fetchNonPredictedAnswers :: MongoInfo -> Username -> IO [AnswerWithIndexDTO]
fetchNonPredictedAnswers mongoConf username = runAction mongoConf fetchAction
  where
    questionEntityToAnswerWithIndexDTO :: Entity Question -> AnswerWithIndexDTO
    questionEntityToAnswerWithIndexDTO (Entity _ question) = answerWithIndexDTO
      where
        answer = head $ getField @"questionAnswers" question
        answerWithIndexDTO = AnswerWithIndexDTO 
          { questionIndex = getField @"questionIndex" question
          , score = getField @"answerScore" answer
          }

    fetchAction :: Action IO [AnswerWithIndexDTO]
    fetchAction = do
      cursor <- Mongo.Query.find
        ( (Mongo.Query.select 
            [ "answers" =: 
              [ "$elemMatch" =: 
                [ "answerer" =: username, "ispredicted" =: False] 
              ] 
            ]
            "questions"
          )
          { Mongo.Query.project = 
            [ "answers" =: 
              [ "$elemMatch" =: 
                [ "answerer" =: username, "ispredicted" =: False] 
              ] 
            , "index" =: (1::Int)
            , "text" =: (1::Int)
            ] 
          }
        )
      docList <- Mongo.Query.rest cursor
      return $
        fmap questionEntityToAnswerWithIndexDTO .
        rights . fmap Persist.Mongo.docToEntityEither $
        docList

fetchOtherUsersAndAnswers :: MongoInfo -> Username -> IO [(Username, [AnswerWithIndexDTO])]
fetchOtherUsersAndAnswers mongoInfo username = runAction mongoInfo fetchAction
  where
    fetchAction :: Action IO [(Username, [AnswerWithIndexDTO])]
    fetchAction = do
      results <- fmap toUsernameAndAnswersPair <$> selectList [] []
      return . ListE.groupSort . List.filter elementsToNotInclude . List.concat $ results -- [[(Username, AnswerWithIndexDTO)]]
      
    toUsernameAndAnswersPair :: Entity Question -> [(Username, AnswerWithIndexDTO)]
    toUsernameAndAnswersPair (Entity _ q) = fmap (getAnswers (getField @"questionIndex" q)) $ getField @"questionAnswers" q

    elementsToNotInclude :: (Username, AnswerWithIndexDTO) -> Bool
    elementsToNotInclude (u, AnswerWithIndexDTO {score=a}) =
      not (u == username || (0::Double) == a)

    getAnswers :: Int -> Answer -> (Username, AnswerWithIndexDTO)
    getAnswers index answer = 
      ( getField @"answerAnswerer" answer
      , AnswerWithIndexDTO index (getField @"answerScore" answer)
      )
    
    byUsername :: (Username, AnswerWithIndexDTO) -> Username
    byUsername (fst, _) = fst

fetchBestEmbeddings :: MongoInfo -> IO (Maybe Embeddings)
fetchBestEmbeddings mongoInfo = runAction mongoInfo fetchAction
  where
    fetchAction :: Action IO (Maybe Embeddings)
    fetchAction = fmap entityVal <$> Persist.Mongo.selectFirst [] [Asc EmbeddingsMse]


fetchBestItemEmbedding :: MongoInfo -> IO (Maybe [[Double]])
fetchBestItemEmbedding mongoInfo = fmap embeddingsItemEmb <$> fetchBestEmbeddings mongoInfo


-- TODO: Make it take a single pair
updatePredictedAnswers :: MongoInfo -> [(Username, [AnswerWithIndexDTO])] -> IO ()
updatePredictedAnswers mongoConf usernamesWithAnswers = 
  runAction mongoConf updateAction
  where
    updateAction :: Action IO ()
    updateAction = do
      curTime <- liftIO Clock.getCurrentTime
      _ <- Mongo.Query.updateAll "questions" $ List.concatMap (updateSingleUsername curTime) usernamesWithAnswers
      return ()

    updateSingleUsername :: Clock.UTCTime -> (Username, [AnswerWithIndexDTO]) -> [(Mongo.Query.Selector, Document, [Mongo.Query.UpdateOption])]
    updateSingleUsername curTime' (username, answers) = fmap (updateSingleAnswer username curTime') answers
    
    updateSingleAnswer :: Username -> Clock.UTCTime -> AnswerWithIndexDTO -> (Mongo.Query.Selector, Mongo.Query.Modifier, [Mongo.Query.UpdateOption])
    updateSingleAnswer username' curTime'' AnswerWithIndexDTO {questionIndex=indexToUpdate, score=scoreToUpdate} =
      ( ["index" =: indexToUpdate, "answers" =: ["$elemMatch" =: ["answerer" =: username', "ispredicted" =: True ]]]
      , ["$set" =: ["answers.$" =: (Persist.Mongo.recordToDocument (newAnswer username' curTime'' scoreToUpdate) :: Document)]]::Mongo.Query.Modifier
      , []
      ) 
    newAnswer :: Username -> Clock.UTCTime -> Double -> Answer
    newAnswer username'' currenttime score'' = Answer
      { answerAnswerer = username''
      , answerScore = score''
      , answerTimestamp = currenttime
      , answerIspredicted = True
      }

{-----------------------------------------------------------------------------}
{-                                 HELPERS                                   -}
{-----------------------------------------------------------------------------}

--runAction :: MonadIO m => MongoInfo -> Action m b -> m b
runAction mongoConf action =
  Persist.Mongo.withConnection mongoConf $ \pool ->
    Persist.Mongo.runMongoDBPoolDef action pool

conversationEntityToConversationPreviewDTO ::
     Text -> Entity Conversation -> ConversationPreviewDTO
conversationEntityToConversationPreviewDTO username (Entity _ convo) =
  conversationPreview
  where
    members = getField @"conversationMembers" convo
    message = last $ getField @"conversationMessages" convo
    conversationPreview =
      ConversationPreviewDTO
        { convoWithUsername =
            if head members == username
              then last members
              else head members
        , isLastAuthor = username == getField @"messageAuthor" message
        , body = getField @"messageBody" message
        , timeStamp = getField @"messageTimestamp" message
        }

userEntityToUserDTO :: Entity User -> UserDTO
userEntityToUserDTO (Entity _ user) =
  UserDTO
    { username = userUsername user
    , gender = userGender user
    , birthday = userBirthday user
    , town = userTown user
    , profileText = userProfileText user
    , imageUrl = userImage user
    }

userEntityToLoggedInDTO :: Entity User -> LoggedInDTO
userEntityToLoggedInDTO (Entity _ user) = LoggedInDTO
  { username  = getField @"userUsername"  user
  , authToken = getField @"userAuthToken" user
  , firstLogIn = False
  }

convoEntityToConversationDTO :: Entity Conversation -> Text -> ConversationDTO
convoEntityToConversationDTO (Entity _ convo) username = conversationDTO
  where
    conversationDTO =
      ConversationDTO
        { convoWithUsername = username
        , messages = map messageToMessageDTO (conversationMessages convo)
        }

messageToMessageDTO :: Message -> MessageDTO
messageToMessageDTO message = messageDTO
  where
    messageDTO =
      MessageDTO
        { authorUsername = messageAuthor message
        , timeStamp = messageTimestamp message
        , body = messageBody message
        }

questionToQuestionDTO :: Entity Question -> QuestionDTO
questionToQuestionDTO (Entity key q) =
  QuestionDTO
    { id = Persist.Mongo.keyToText $ Persist.Mongo.toBackendKey key
    , question = getField @"questionText" q
    }

hashPassword :: Text -> Text -> Text
hashPassword password salt = T.pack $ show hashed
  where
    passPlusSalt = encodeUtf8 (password <> salt)
    hashed = hash passPlusSalt :: Digest SHA3_512

getQuestions :: IO [Question]
getQuestions = runAction localMongoInfo action
  where
    entityQuestionToQuestion :: Entity Question -> Question
    entityQuestionToQuestion (Entity _ q) = q
    action :: Action IO [Question]
    action = fmap entityQuestionToQuestion <$> selectList [] []


saveMatchesToDb :: MongoInfo -> [(Username, Username, Double)] -> IO ()
saveMatchesToDb mongoConf matches = runAction mongoConf saveAction
  where
    updateOption = [Mongo.Query.Upsert]
    saveAction :: Action IO ()
    saveAction = do
      _ <- Mongo.Query.updateAll "user_matches" $ fmap query matches
      return ()
    query :: (Username, Username, Double) -> (Mongo.Query.Selector, Document, [Mongo.Query.UpdateOption]) --maybeAlreadySaved <- selectFirst [UserMatchesMatch `Persist.Mongo.anyEq` username, UserMatchesMatch `Persist.Mongo.anyEq` matchingUser] []
    query (ownUsername, otherUsername, predictionValue) =
      ( [ "$and" =: 
          [ ["match" =: (ownUsername :: Text)]
          , ["match" =: (otherUsername :: Text)]
          ]
        ]::Mongo.Query.Selector
      , Persist.Mongo.recordToDocument $ UserMatches [ownUsername, otherUsername] predictionValue
      , updateOption
      )

deleteEverything :: IO ()
deleteEverything = do
  _ <- deleteEverythingInDB
  content <- listDirectory "frontend/img/users"
  traverse_ removeSingleFile (map ("frontend/img/users/" ++) content)
  return ()

removeSingleFile :: FilePath -> IO ()
removeSingleFile path =
  if path == "frontend/img/users/.gitignore"
    then return ()
    else removeFile path

deleteEverythingInDB :: IO ()
deleteEverythingInDB = runAction localMongoInfo action
  where
    action :: Action IO ()
    action = do
      _ <- Mongo.Query.delete $ Mongo.Query.select [] "users"
      _ <- Mongo.Query.delete $ Mongo.Query.select [] "conversations"
      _ <- Mongo.Query.delete $ Mongo.Query.select [] "questions"
      return ()

convertQuestionsFromOldToNew :: MongoInfo -> IO String
convertQuestionsFromOldToNew mongoConf = runAction mongoConf convertAction
  where
    convertAction :: Action IO String
    convertAction = do
      newQs <- fmap oldQuestionToNew <$> selectList [] []
      _ <- Mongo.Query.delete $ Mongo.Query.select [] "questions"
      _ <- Mongo.Query.insertMany_ "questions" $ map Persist.Mongo.recordToDocument newQs
      _ <- Mongo.Query.modify
        ( Mongo.Query.select [] "users")
        ["$set" =: ["being_predicted" =: False]]
      return "inserted" 
      --return qs
    oldQuestionToNew :: Entity OldQuestion -> Question
    oldQuestionToNew (Entity _ oldQ) = Question
      { questionIndex = getField @"oldQuestionIndex" oldQ
      , questionText = getField @"oldQuestionText" oldQ
      , questionAnswers = oldAnswersToNew <$> getField @"oldQuestionAnswers" oldQ
      }
    oldAnswersToNew :: OldAnswer -> Answer
    oldAnswersToNew oldA = Answer
      { answerAnswerer = getField @"oldAnswerAnswerer" oldA
      , answerScore = fromIntegral $ getField @"oldAnswerScore" oldA
      , answerTimestamp = getField @"oldAnswerTimestamp" oldA
      , answerIspredicted = not $ getField @"oldAnswerIspredicted" oldA
      }

updatePredictionStatus :: MongoInfo -> Username -> Bool -> IO ()
updatePredictionStatus mongoConf username status = runAction mongoConf updateAction
  where
    updateAction :: Action IO()
    updateAction = do
      mUser <- getBy (UniqueUsername username)
      case mUser of
        Just (Entity id' _) -> void $ update id' [UserBeingPredicted =. status]
        Nothing -> return ()