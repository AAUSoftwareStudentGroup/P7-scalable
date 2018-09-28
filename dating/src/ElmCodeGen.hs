{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ElmCodeGen where

import           API
import           Data.Semigroup         ((<>))
import           Data.Text              (Text, pack)
import           Elm                    (Spec (Spec), specsToDir,
                                         toElmDecoderSource, toElmEncoderSource,
                                         toElmTypeSource)
import           Elm.Export.Persist.Ent
import           Schema
import           Servant.Elm            (ElmOptions, ElmType, Proxy (Proxy),
                                         UrlPrefix (Static), defElmImports,
                                         defElmOptions, generateElmForAPIWith,
                                         urlPrefix)


myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static "http://localhost:8080" }

myDefElmImports :: Text
myDefElmImports = defElmImports <> pack "\nimport Models"

userSpec :: Spec
userSpec = Spec ["Generated", "UsersApi"]
      (myDefElmImports
       : toElmTypeSource (Proxy :: Proxy (EntId User))
       : toElmDecoderSource (Proxy :: Proxy (EntId User))
       : toElmEncoderSource (Proxy :: Proxy (EntId User))
       : generateElmForAPIWith myElmOpts (Proxy :: Proxy UsersAPI))

genUsersApiCode :: IO ()
genUsersApiCode = specsToDir [userSpec] "frontend/src"
