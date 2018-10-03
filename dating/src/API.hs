{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module API where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Except       (throwE)
import           Data.Int                         (Int64)
import           Data.Proxy                       (Proxy (..))
import           Database.Persist                 (Entity, Key)
import           Network.Wai                      (Request, requestHeaders)
import           Network.Wai.Handler.Warp         (run)
import           Servant                          (throwError)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Data.ByteString                  (ByteString)
import           Data.Map                         (Map, fromList)
import qualified Data.Map                         as Map
import           Data.Text                        (Text)
import           Database                         (PGInfo, RedisInfo,
                                                   createUserPG,
                                                   createUserRedis,
                                                   fetchAllUsersPG,
                                                   fetchPostgresConnection,
                                                   fetchRedisConnection,
                                                   fetchUserPG, fetchUserRedis)
import           Schema
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server.Experimental.Auth
import           Web.Cookie                       (parseCookies)


-- | The API.
type DatingAPI =
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> AuthProtect "cookie-auth" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

-- | A proxy for the API. Technical detail.
datingAPI :: Proxy DatingAPI
datingAPI = Proxy :: Proxy DatingAPI


-- TEMPORARY START
newtype AuthToken = AuthToken { getToken :: Text }

database :: Map ByteString AuthToken
database = fromList [ ("key1", AuthToken "Kasper")
                    , ("key2", AuthToken "Anton")
                    , ("key3", AuthToken "Jonatan")
                    ]
-- TEMPORARY END

-- | Fetches a user by id. First it tries redis, then postgres. It saves to cache if it goes to the db.
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

-- | Fetches all users from db if you are authenticated.
fetchAllUsersHandler :: PGInfo -> AuthToken -> Handler [User]
fetchAllUsersHandler pgInfo (AuthToken _) = liftIO $ fetchAllUsersPG pgInfo


-- | Creates a user in the db.
createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user


-- | Given a "password" it returns either an 403 error or the AuthToken.
lookupAuthToken :: ByteString -> Handler AuthToken
lookupAuthToken key = case Map.lookup key database of
  Nothing  -> throwError (err403 { errBody = "Invalid Password"} )
  Just usr -> return usr


-- | The handler which is called whenever a protected endpoint is visited.
authHandler :: AuthHandler Request AuthToken
authHandler = mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 { errBody = msg }
    handler req = either throw401 lookupAuthToken $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "dating-auth-cookie" $ parseCookies cookie

maybeToEither e = maybe (Left e) Right

-- | Specifies the data returned after authentication.
type instance AuthServerData (AuthProtect "cookie-auth") = AuthToken


-- | Specifies the handler functions for each endpoint. Has to be in the right order.
datingServer :: PGInfo -> RedisInfo -> Server DatingAPI
datingServer pgInfo redisInfo =
  fetchUserHandler pgInfo redisInfo :<|>
  fetchAllUsersHandler pgInfo :<|>
  createUserHandler pgInfo


-- | The context is sort of the state, being authenticated or not. Starts empty.
datingServerContext :: Context (AuthHandler Request AuthToken ': '[])
datingServerContext = authHandler :. EmptyContext


-- | Serves the API on port 1234
runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  run port $ serveWithContext datingAPI datingServerContext (datingServer pgInfo redisInfo)
  where
    port = 1234


-- Client functions for testing. Not updated to use authentication.

-- fetchUserClient :: Int64 -> ClientM User
-- fetchAllUsersClient :: ClientM [User]
-- createUserClient :: User -> ClientM Int64
-- (fetchUserClient :<|> fetchAllUsersClient :<|> createUserClient) = client (Proxy :: Proxy DatingAPI)
