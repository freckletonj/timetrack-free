{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE Arrows #-}


module OAuth2 where

import Data.Aeson
import Data.Aeson.Types (parseEither, parse)

import Data.String.Conversions (cs)
import Data.List

import Control.Monad
import Control.Monad.IO.Class

import Servant
import Data.Proxy
import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//), (/:))

import Network.HTTP.Simple hiding (Proxy)

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.HashMap.Lazy as HM


{-

TODO: include `state` in github request
TODO: break up fns
TODO: tests, integration tests?
TODO: eventually, break out modules
TODO: make into an OAuth server?
TODO: use yesod authenticate? or is it OAuth1? look for inspiration?
TODO: hoauth2 - https://hackage.haskell.org/package/hoauth2
-}

--------------------------------------------------

-- | Type for holding OAuth2 configuration per Service
data OAuth2 = OAuth2 { oauthClientId :: String
                     , oauthClientSecret :: String
                     , oauthOAuthorizeEndpoint :: String
                     , oauthAccessTokenEndpoint :: String
                     , oauthCallback :: String
                     } deriving (Show, Eq)

----------

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

----------

-- | Step 1. Take user to the service's auth page
getAuthorize :: OAuth2 -> String
getAuthorize oa = authUri oa

-- | Step 2. Accept a temporary code from the service, and exchange
-- for an auth token 
getAuthorized :: OAuth2 -> Maybe String -> Handler HTML
getAuthorized oa mcode = do
  case mcode of
    Nothing -> error "You must pass in a code as a parameter"
    Just code -> do
      token <- liftIO $ getAccessToken oa code
      return $ case token of
        Left e ->  "<h1>Error Fetching Token</h1>" ++ "<p>" ++ e ++ "</p>"
        Right t -> concat ["<h1>Your Token Is:</h1>"
                         , "<h3>" , t , "</h3>"]

-- | Step 3. Exchange code for auth token
getAccessToken :: OAuth2 -> String -> IO (Either String String)
getAccessToken oa code = do
  let endpoint = tokenUri code oa
  request' <- parseRequest endpoint
  let request = setRequestMethod "POST"
                $ addRequestHeader "Accept" "application/json"
                $ setRequestBasicAuth (cs . oauthClientId $ oa) (cs . oauthClientSecret $ oa)
                $ setRequestBodyURLEncoded [("grant_type", "authorization_code") -- < TODO: bb specific?
                                           ,("code", cs $ code)
                                           ,("redirect_uri", cs $ (oauthCallback oa))]
                $ request'
  response <- httpJSONEither request -- only Left if response not JSON
  -- putStrLn . show $ response -- TODO: debug, remove
  
  return $ case ((getResponseBody response) :: Either JSONException Object) of
             Left exc -> Left . show $ exc -- < TODO: never triggered, always fails through Right... a useful failure for testing is try to redeem the same code twice

             Right obj -> parseResAccessToken obj

parseResAccessToken :: Object -> Either String String
parseResAccessToken obj = case HM.lookup "access_token" obj of
                                        Just x -> case x of
                                          String x' -> Right . cs $ x'
                                          _ -> Left "access_token wasn't a string"
                                        _ -> Left "didn't have access_token"

eitherLookup :: String -> Object -> Either String Value
eitherLookup s obj = case HM.lookup (cs s) obj of
                    Just x -> Right x
                    _ -> Left (s ++ " not found in object")

valueToString :: Value -> Either String String
valueToString v = case v of
                    String x -> Right . cs $ x
                    _ -> Left "not a string"

-- | HTML Content Type for returning Strings as HTML
type HTML = String
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML String where
  mimeRender _ = cs
  

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


