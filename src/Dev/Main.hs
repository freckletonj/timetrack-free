{-# LANGUAGE AllowAmbiguousTypes #-}
{-

# Dev.Main

    A dynamic place to futz around with things

-}

{-# LANGUAGE DataKinds #-} -- type-level strings
{-# LANGUAGE PolyKinds  #-} -- kind polymorphism
{-# LANGUAGE TypeFamilies  #-} -- type-level functions
{-# LANGUAGE FlexibleInstances  #-} -- type-class programming
{-# LANGUAGE ScopedTypeVariables #-} -- type annotations to guide the type checker
{-# LANGUAGE TypeOperators #-} -- type-level infix operators :<|>, :>
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

type Authorize = Get '[JSON] String
type Callback a = QueryParam "code" String :> Get '[JSON] a

authorize :: OAuth2 -> Server Authorize
authorize oa = return $ authUri oa

callback :: (Maybe String -> Handler a) -> Server (Callback a)
callback f = (\code -> f code >>= return)

--callbackFn :: (MonadIO m)=> OAuth2 -> Maybe String -> m String
callbackFn :: OAuth2 -> Maybe String -> Handler String
callbackFn oa mcode = do
  liftIO . putStrLn $ "mcode: " ++ show mcode
  case mcode of
    Nothing -> return "no code"
    Just code -> do
      mtoken <- liftIO $ getAccessToken oa code
      case mtoken of
        Left error -> return error
        Right token -> return token


type OAuthAPI = "github" :> (
  "authorize" :> Authorize
  :<|> "authorized" :> Callback String)
  
api :: Proxy OAuthAPI
api = Proxy

oauthServer :: OAuth2
            -> Server OAuthAPI
oauthServer
  githuboa
  = authorize githuboa
    :<|> callback (callbackFn githuboa)




--------------------
-- DevApi

type DevApi =  MyApi
               :<|> OAuthAPI

devServer :: OAuth2
          -> ConnectionPool
          -> ServerT DevApi (ExceptT ServantErr IO)
devServer
  githuboa
  pool
  = server pool
    :<|>  oauthServer githuboa
    

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
    $ devServer
    githuboa
    pool
