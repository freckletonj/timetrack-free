{-# LANGUAGE OverloadedStrings #-}

module Util.OAuth2 where

import Data.Aeson
import Data.Aeson.Types (parseEither, parse)

import Data.String.Conversions (cs)
import Data.List

import Control.Monad
import Control.Monad.IO.Class

import Servant
import Data.Proxy

import Network.HTTP.Simple hiding (Proxy)

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.HashMap.Lazy as HM


{-
TODO: include `state` in github request, security feature
TODO: tests, integration tests?
-}

-- | Type for holding OAuth2 configuration per Service
data OAuth2 = OAuth2 { oauthClientId :: String
                     , oauthClientSecret :: String
                     , oauthOAuthorizeEndpoint :: String
                     , oauthAccessTokenEndpoint :: String
                     , oauthCallback :: String
                     } deriving (Show, Eq)

-- | Generate a URL query string from [(key, value)]
-- ex [("a", "A"), ("b", "B")] -> "?a=A&b=B"
queryString :: [(String, String)] -> String
queryString xs = '?' : intercalate "&" (map f xs)
  where f (k,v) = k ++ '=':v

-- | Generate the URL for GETing a temporary code
authUri :: OAuth2 -> String
authUri oa = (oauthOAuthorizeEndpoint oa)
                  ++ queryString [("client_id", oauthClientId oa)
                                 ,("response_type", "code")
                                 ,("redirect_uri", oauthCallback oa)]

-- | Generate the URL for POSTing a code to trade for a token
tokenUri :: String -> OAuth2 -> String
tokenUri code oa = (oauthAccessTokenEndpoint oa)
                        ++ queryString [("client_id", oauthClientId oa)
                                       ,("client_secret", oauthClientSecret oa)
                                       ,("code", code)]

-- | Exchange a temporary code for possibly an auth token
getAccessToken :: OAuth2 -> String -> IO (Either String String)
getAccessToken oa code = do
  let endpoint = tokenUri code oa
  request' <- parseRequest endpoint
  let request = setRequestMethod "POST"
                $ addRequestHeader "Accept" "application/json"
                $ setRequestBasicAuth (cs . oauthClientId $ oa) (cs . oauthClientSecret $ oa)
                $ setRequestBodyURLEncoded [("grant_type", "authorization_code") -- < TODO: bb specific?
                                           , ("code", cs $ code)
                                           , ("redirect_uri", cs $ (oauthCallback oa))]
                $ request'
  response <- httpJSONEither request -- only Left if response not JSON
  return $ case ((getResponseBody response) :: Either JSONException Object) of
             Left exc -> Left . show $ exc
             Right obj -> parseResAccessToken obj

-- |
parseResAccessToken :: Object -> Either String String
parseResAccessToken obj = case HM.lookup "access_token" obj of
                                        Just x -> case x of
                                          String x' -> Right . cs $ x'
                                          _ -> Left "access_token wasn't a string"
                                        _ -> Left "didn't have access_token"

-- | Try to construct an OAuth2 given a Config, and a String that
-- defines the name of the config record where this provider's
-- information lives. EG `github { ... }` 

oauth2 :: CT.Config -> String -> IO OAuth2
oauth2 c name = OAuth2
  <$> f "clientId"
  <*> f "clientSecret"
  <*> f "oAuthorizeEndpoint"
  <*> f "accessTokenEndpoint"
  <*> f "callback"
  where
    f v = (C.require c (cs (name ++ '.' : v)))


