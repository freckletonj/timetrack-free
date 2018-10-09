{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-} -- type-level strings
{-# LANGUAGE PolyKinds  #-} -- kind polymorphism
{-# LANGUAGE TypeFamilies  #-} -- type-level functions
{-# LANGUAGE FlexibleInstances  #-} -- type-class programming
{-# LANGUAGE ScopedTypeVariables #-} -- type annotations to guide the type checker
{-# LANGUAGE TypeOperators #-} -- type-level infix operators :<|>, :>
{-# LANGUAGE OverloadedStrings #-}

module App where

import Data.Aeson
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Servant
import Servant.Auth.Server
import Network.Wai.Handler.Warp

import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Trans.Except
import Control.Monad.Except
import qualified Data.HashMap.Lazy as HM
import Data.String.Conversions (cs)
import Data.Text

import Util.OAuth2.Servant
import Util.OAuth2
import Api
import Db
import PersistentType

startApp :: IO ()
startApp = do

  -- Config
  cfg <- C.load [C.Required "resource/config.cfg"]
  oauthcfg <- C.load [C.Required ".secret/oauth.cfg"]
  
  -- Variables
  env <- C.require cfg "environment"
  githuboa <- oauth2 oauthcfg "github"
  bitbucketoa <- oauth2 oauthcfg "bitbucket"

  -- JWT
  keyPath <- C.require cfg "jwtKey"
  jwtKey <- mkJWK keyPath
  
  let jwtCfg = defaultJWTSettings jwtKey
      serverCfg = defaultCookieSettings :. jwtCfg :. EmptyContext 

  -- Resources
  pool <- makePool (read env) cfg -- TODO: read == unsafe
  runSqlPool (runMigration migrateAll) pool

  -- Run
  Network.Wai.Handler.Warp.run 8080
    $ serveWithContext mainApi serverCfg
    $ mainServer
    pool
    defaultCookieSettings
    jwtCfg
    githuboa
    bitbucketoa

