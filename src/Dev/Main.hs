{-

# Dev.Main

    A dynamic place to futz around with things

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Dev.Main where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Servant
import Network.Wai.Handler.Warp
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Trans.Except
import Control.Monad.Except
import qualified Data.HashMap.Lazy as HM

import OAuth2
import OAuthAPI
import Api
import PersistentType


--------------------
-- OAuth

type OAuthAPI = "github" :> AuthAPI
           :<|> "bitbucket" :> AuthAPI

api :: Proxy OAuthAPI
api = Proxy

oauthServer :: OAuth2
            -> OAuth2
            -> Server OAuthAPI
oauthServer github bitbucket = (authEndpoint github)
                               :<|> (authEndpoint bitbucket)


--------------------
-- DevApi

type DevApi =  MyApi
               :<|> OAuthAPI

devServer :: OAuth2
          -> OAuth2
          -> ConnectionPool
          -> ServerT DevApi (ExceptT ServantErr IO)
devServer githuboa bitbucketoa pool = server pool
                    :<|>  authEndpoint githuboa
                    :<|>  authEndpoint bitbucketoa

devApi :: Proxy DevApi
devApi = Proxy

main :: IO ()
main = do

  -- Config
  cfg <- C.load [C.Required "resource/config.cfg"]
  env <- C.require cfg "environment"
  oauthcfg <- C.load [C.Required ".secret/oauth.cfg"]

  -- Resources
  pool <- makePool (read env) cfg -- read == unsafe
  runSqlPool (runMigration migrateAll) pool

  -- OAuth
  githuboa <- oauth2 oauthcfg "github"
  bitbucketoa <- oauth2 oauthcfg "bitbucket"

  -- Run
  Network.Wai.Handler.Warp.run 8080
    $ serve devApi
    $ devServer githuboa bitbucketoa pool
