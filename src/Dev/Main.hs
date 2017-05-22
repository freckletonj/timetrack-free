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
import Data.String.Conversions (cs)
import Data.Text

import Util.OAuth2.Servant
import Util.OAuth2
import Api
import PersistentType


-- | Return a redirect to the OAuth provider's required URI
authorizeFn :: OAuth2
            -> Maybe Text -- ^ email
            -> Handler String
authorizeFn oa = (\memail -> case memail of
                     Nothing -> throwError err400 -- TODO: which error
                                                  -- appropriate?
                     Just email -> throwError err301 -- redirect (not error)
                       { errHeaders = [("Location",
                                        cs $ authUri oa
                                         ++ "?email="
                                         ++ cs email)] })

-- | Accept a temporary code from a provider, exchange it for a token,
-- persist it, and redirect the api consumer to the appropriate page.
callbackFn :: ConnectionPool
           -> OAuth2
           -> Maybe String -- ^ temporary code
           -> Maybe String -- ^ email
           -> Handler String
callbackFn pool oa mcode memail = do
  case mcode of
    Nothing -> return "no code"
    Just code ->
      case memail of
        Nothing -> return "no email"
        Just email -> do

          -- exchange temporary code for token
          mtoken <- liftIO $ getAccessToken oa code
          case mtoken of
            Left error -> return error
            Right token -> do

               -- TODO: remove
              liftIO . putStrLn . cs $ email
              liftIO . putStrLn . cs $ code
              liftIO . putStrLn . cs $ token

              -- TODO: persist token
              return token


--------------------
-- Generic OAuthAPI

type OAuthAPI = "github" :> ProviderAPI
                :<|> "bitbucket" :> ProviderAPI
  
api :: Proxy OAuthAPI
api = Proxy

oauthServer :: ConnectionPool
            -> OAuth2 -- github
            -> OAuth2 -- bitbucket
            -> Server OAuthAPI
oauthServer
  pool
  githuboa
  bitbucketoa
  = -- Github
  (authorize (authorizeFn githuboa)
    :<|> callback (callbackFn pool githuboa))

  -- Bitbucket
  :<|> (authorize (authorizeFn bitbucketoa)
        :<|> callback (callbackFn pool bitbucketoa))

--------------------
-- DevApi

type DevApi =  MyApi
               :<|>
               OAuthAPI

devServer :: ConnectionPool
          -> OAuth2
          -> OAuth2
          -> ServerT DevApi (ExceptT ServantErr IO)
devServer
  pool
  githuboa
  bitbucketoa
  =
  server pool
  :<|>
  oauthServer pool githuboa bitbucketoa
    

devApi :: Proxy DevApi
devApi = Proxy

main :: IO ()
main = do

  -- Config
  cfg <- C.load [C.Required "resource/config.cfg"]
  oauthcfg <- C.load [C.Required ".secret/oauth.cfg"]

  -- Variables
  env <- C.require cfg "environment"
  githuboa <- oauth2 oauthcfg "github"
  bitbucketoa <- oauth2 oauthcfg "bitbucket"

  -- Resources
  pool <- makePool (read env) cfg -- TODO: read == unsafe
  runSqlPool (runMigration migrateAll) pool

  -- Run
  Network.Wai.Handler.Warp.run 8080
    $ serve devApi
    $ devServer
    pool
    githuboa
    bitbucketoa

