{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module OAuthAPI where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.HashMap.Lazy as HM
import Servant
import OAuth2


-- | Type of OAuth2's endpoints
type AuthAPI = "authorize" :> Get '[HTML] HTML
               :<|> "authorized" :> QueryParam "code" String :> Get '[HTML] HTML

-- | Construct endpoints for a given OAuth2
authEndpoints :: OAuth2 -> Server AuthAPI
authEndpoints oa = (getAuthorize oa)
                   :<|> (getAuthorized oa)

----------

type OAuthAPI = "github" :> AuthAPI
           :<|> "bitbucket" :> AuthAPI

api :: Proxy OAuthAPI
api = Proxy

oauthServer :: (HM.HashMap CT.Name CT.Value) -> Maybe (Server OAuthAPI)
oauthServer cfg = (:<|>)
             <$> f "github"
             <*> f "bitbucket"
  where f = (authEndpoints <$>) . oauth2 cfg

----------

-- main = do
--   putStrLn "Visit http://localhost:8080/github/authorize"
--   cfg <- C.getMap =<< C.load [ C.Required ".secrets/app.cfg" ]
--   let mserver' = server cfg
--   case mserver' of
--     Just server' -> run 8080 (serve api server')
--     Nothing -> error "bad config"


