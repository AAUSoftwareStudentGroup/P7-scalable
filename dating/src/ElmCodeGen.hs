{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module ElmCodeGen where

import           API
import           Data.Semigroup                   ((<>))
import           Data.Text                        (Text, pack)
import           Elm                              (ElmDatatype, ElmType (..),
                                                   Spec (Spec), specsToDir,
                                                   toElmDecoderSource,
                                                   toElmEncoderSource,
                                                   toElmType, toElmTypeSource)
import           Servant.Elm                      (ElmOptions (..), ElmType,
                                                   UrlPrefix (Static),
                                                   defElmImports, defElmOptions,
                                                   generateElmForAPIWith)

import           Elm.Export.Persist.Ent
import           GHC.TypeLits                     (KnownSymbol)
import           Schema
import           Servant
import           Servant.Elm                      (ElmOptions, ElmType,
                                                   Proxy (Proxy),
                                                   UrlPrefix (Static),
                                                   defElmImports, defElmOptions,
                                                   generateElmForAPIWith,
                                                   urlPrefix)
import           Servant.Elm.Internal.Foreign     (LangElm, getEndpoints)
import           Servant.Foreign                  (Arg (..), Foreign,
                                                   GenerateList, HasForeign,
                                                   HasForeignType,
                                                   HeaderArg (..),
                                                   PathSegment (..), Req (..),
                                                   foreignFor, generateList,
                                                   typeFor)
import           Servant.Server.Experimental.Auth


instance (KnownSymbol sym, HasForeign lang ftype sublayout)
    => HasForeign lang ftype (AuthProtect sym :> sublayout) where
    type Foreign ftype (AuthProtect sym :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy req =
      foreignFor lang ftype (Proxy :: Proxy sublayout) req

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static "http://localhost:8080" }

myDefElmImports :: Text
myDefElmImports = defElmImports <> pack "\nimport GenHelpers"

userSpec :: Spec
userSpec = Spec ["Generated", "DatingApi"]
      (myDefElmImports
       : toElmTypeSource (Proxy :: Proxy (EntId User))
       : toElmDecoderSource (Proxy :: Proxy (EntId User))
       : toElmEncoderSource (Proxy :: Proxy (EntId User))
       : generateElmForAPIWith myElmOpts (Proxy :: Proxy DatingAPI ))

genUsersApiCode :: IO ()
genUsersApiCode = specsToDir [userSpec] "frontend/src"
