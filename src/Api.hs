{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Control.Monad.Operational  as O
import Control.Monad.Operational  hiding (view)
import Control.Monad.Reader       (ask)
import Control.Monad.Writer
import Control.Monad.Trans.Except
import Control.Monad.Except

import DSL
import Data.Aeson
import qualified Data.Foldable as F
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text.Lens
import Debug.Trace 
import qualified Network.Wai.Handler.Warp
import PersistentType
import Servant hiding (throw)
import Servant.Auth.Server
import Control.Exception.Safe hiding (throw, Handler)
import Crypto.JOSE.JWK (JWK, fromRSA)
import Data.X509 (PrivKey (..))
import Data.X509.File (readKeyFile)
import Network.HTTP.Simple hiding (Proxy)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Types

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.HashMap.Lazy as HM

import Type
import Db
import Util.OAuth2
import Authenticate
import Util.OAuth2.Servant



--------------------------------------------------
-- Serving the API

type CrudApi = "user" :> CRUD User
               :<|> "clock"   :> CRUD Clock
               :<|> "session" :> CRUD Session
               :<|> "ghcredential" :> CRUD GhCredential

crudApi :: Proxy CrudApi
crudApi = Proxy

server :: ConnectionPool -> ServerT CrudApi (ExceptT ServantErr IO)
server pool = runCrud pool -- user
             :<|> runCrud pool -- clock
             :<|> runCrud pool -- session
             :<|> runCrud pool -- ghcredential


--------------------------------------------------
-- Security API

type SecurityApi auths =  CrudApi
               :<|> OAuthAPI auths
               :<|> "login" :> LoginRoute
               :<|> "signup" :> SignupRoute
               :<|> "authtest" :> TestAuthRoute auths
               :<|> "ghtest" :> TestGhTokenRoute auths
               
securityServer :: ConnectionPool
          -> CookieSettings
          -> JWTSettings
          -> OAuth2
          -> OAuth2
          -> ServerT (SecurityApi '[JWT, Cookie]) (ExceptT ServantErr IO)
securityServer
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
  :<|> ghRepoList pool
    

devProxy :: Proxy (SecurityApi '[JWT, Cookie])
devProxy = Proxy


--------------------------------------------------


-- TODO: "security server" needs to be broken apart
mainApi = devProxy
mainServer = securityServer

--------------------------------------------------

-- | Persist user's OAuth token
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

-- | A List of auth'd User's repos on Github
ghRepoList :: ConnectionPool -> AuthResult User -> Handler [Object]
ghRepoList pool (Authenticated (User email)) = do
  mghcred <- runDb pool $ get (GhCredentialKey (UserKey email))
  case mghcred of
    Nothing -> throwError $ err400 {errBody="not authd with provider yet"}
    Just (GhCredential _ token) -> do
      request <- parseRequest "GET https://api.github.com/user/repos"
      let request' = 
                addRequestHeader "Accept" "application/json"
                $ addRequestHeader "Accept" "application/vnd.github.v3+json"
                $ addRequestHeader "User-Agent" "timetrack" -- name of github oauth
                $ addRequestHeader "Authorization" (cs $ "token " ++ cs token)
                $ request
      response <- httpJSONEither request'
      case getResponseBody response of
        Left e -> throwError $ err400 {errBody = cs . show $ e}
        Right body -> return body
ghRepoList _ _ = throwError err400 { errBody = "not authenticated" }

type GhReposRoute = Get '[JSON] [Object]
type TestGhTokenRoute auths = Auth auths User :> GhReposRoute


--------------------
-- OAuth Providers

type OAuthAPI auths = "github" :> ProviderAPI auths
                :<|> "bitbucket" :> ProviderAPI auths

oauthProxy :: Proxy (OAuthAPI auths)
oauthProxy = Proxy

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
-- Keys

-- | Load a JSON-signing key from a file
mkJWK :: FilePath -> IO JWK
mkJWK keypath = do
  maybePk <- readKeyFile keypath
  case  maybePk of
    [] -> error $ "no valid keys at: " ++ keypath
    (PrivKeyRSA pk):_ -> pure $ fromRSA pk
    _ -> error "not a valid RSA key"
