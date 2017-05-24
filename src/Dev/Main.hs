{-# LANGUAGE FlexibleContexts #-}
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
import Servant.Auth.Server
import Network.Wai.Handler.Warp
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Trans.Except
import Control.Monad.Except
import qualified Data.HashMap.Lazy as HM
import Data.String.Conversions (cs)
import Data.Text
import Crypto.JOSE.JWK (JWK, fromRSA)
import Data.X509 (PrivKey (..))
import Data.X509.File (readKeyFile)

import Util.OAuth2.Servant
import Util.OAuth2
import Api
import Authenticate
import PersistentType

-- | insert any supported OAuth Provider
insertProvider :: (MonadIO m, MonadError ServantErr m)
  => ConnectionPool -> OAuthCred -> m ()
insertProvider pool (OGh x) = runDb pool $ insert x >> return ()
insertProvider pool (OBb x) = runDb pool $ insert x >> return ()
insertProvider _ ONone = throwError err400

-- | Return a redirect to the OAuth provider's required URI
authorizeFn :: OAuth2
            -> AuthResult User
            -> Handler String
authorizeFn oa = (\auser -> case auser of
                     (Authenticated (User email)) ->
                       throwError err301 -- redirect (not error)
                       { errHeaders = [("Location",
                                         cs $ authUri oa
                                         ++ "?email="
                                         ++ cs email)] }
                     _ -> throwError err400 -- TODO: which error
                                            -- appropriate?
                 )


-- | Accept a temporary code from a provider, exchange it for a token,
-- persist it, and redirect the api consumer to the appropriate page.
-- TODO: security risk with black hats calling this endpoint?
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
              let providerName = oauthName oa
                  providerCons = providerMap $ cs providerName

              -- persist user's token
              (insertProvider pool
                (providerCons
                  (UserKey $ cs email)
                  (cs token)))

              return token


--------------------
-- Generic OAuthAPI

type OAuthAPI auths = "github" :> ProviderAPI auths
                :<|> "bitbucket" :> ProviderAPI auths
  
api :: Proxy (OAuthAPI auths)
api = Proxy

oauthServer :: ConnectionPool
            -> OAuth2 -- github
            -> OAuth2 -- bitbucket
            -> Server (OAuthAPI auths)
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

type DevApi auths =  MyApi
               :<|> OAuthAPI auths
               :<|> "login" :> LoginRoute
               :<|> "signup" :> SignupRoute
               :<|> "authtest" :> TestAuthRoute auths
               
devServer :: ConnectionPool
          -> CookieSettings
          -> JWTSettings
          -> OAuth2
          -> OAuth2
          -> ServerT (DevApi '[JWT, Cookie]) (ExceptT ServantErr IO)
devServer
  pool
  cs
  jwts
  githuboa
  bitbucketoa
  =
  server pool
  :<|> oauthServer pool githuboa bitbucketoa
  :<|> loginRoute cs jwts pool
  :<|> signupRoute cs jwts pool
  :<|> testAuthRoute
    

devApi :: Proxy (DevApi '[JWT, Cookie])
devApi = Proxy

mkJWK :: FilePath -> IO JWK
mkJWK keypath = do
  maybePk <- readKeyFile keypath
  case  maybePk of
    [] -> error $ "no valid keys at: " ++ keypath
    (PrivKeyRSA pk):_ -> pure $ fromRSA pk
    _ -> error "not a valid RSA key"


main :: IO ()
main = do

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
    $ serveWithContext devApi serverCfg
    $ devServer
    pool
    defaultCookieSettings
    jwtCfg
    githuboa
    bitbucketoa

