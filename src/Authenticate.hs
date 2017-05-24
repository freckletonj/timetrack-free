{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Authenticate where

import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics (Generic)

import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import Control.Monad.IO.Class
import Data.Text
import Data.String.Conversions (cs)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Types

import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import PersistentType
import Api

-- TODO: belongs in config
hashIterations = 12         -- 15 =~ 6 sec

-- data Authy val = NotAuthenticated
--                     | Authenticated val

-- type TokenRoute = ReqBody '[JSON] ClearCredentials
--                 :> Post '[JSON] Text

-- type SignupRoute = ReqBody '[JSON] ClearCredentials
--                  :> Post '[JSON] NoContent

type Authed authable obj route =
  (Auth authable obj :> route)

class Authable a where -- TODO: can I generalize ConnectionPool?
  isAuthenticated :: (MonadIO m) => ConnectionPool -> a -> m Bool


----------
-- Data-Model-Specific Implementation

data ClearCredentials = ClearCredentials { ccEmail :: Key Password
                                         , ccClearPass :: Text}
                 -- deriving (Show) -- TODO: remove Show

instance FromJSON ClearCredentials where
  parseJSON (Object v) = ClearCredentials 
                         <$> v .: "email"
                         <*> v .: "password"
  
-- TODO: clean up nesting
-- TODO: clean up cs, Text, B.pack
instance Authable ClearCredentials where
  isAuthenticated pool (ClearCredentials email clearpass) = do
    mPassword <- runDb pool $ get $ email
    case mPassword of
      Nothing -> return False
      Just (Password _ hash) -> do
            return $ validatePassword
              (B.pack $ cs clearpass)
              (B.pack $ cs hash)


-- type TestProtected =  (Get '[JSON] Bool)
-- type TestAuthRoute =  Authed '[ClearCredentials] User TestProtected

type TestProtected = Get '[JSON] Bool
type TestAuthRoute auths = Auth auths User :> TestProtected

testAuthRoute :: AuthResult User -> Server TestProtected
testAuthRoute (Authenticated u) = return True
testAuthRoute _ = return False

instance FromJWT User where
instance ToJWT User where

----------

type LoginRoute = ReqBody '[JSON] ClearCredentials
                  :> Post '[JSON] String

loginRoute :: CookieSettings
  -> JWTSettings
  -> ConnectionPool
  -> ClearCredentials
  -> Handler String
loginRoute cookieSettings jwtSettings pool cc = do
  valid <- isAuthenticated pool cc
  -- etoken <- makeJWT 
  case valid of
    True -> return "hi"
    False -> return "nope"
  
----------

type SignupRoute = ReqBody '[JSON] ClearCredentials
                   :> Post '[JSON] String

signupRoute :: CookieSettings
  -> JWTSettings
  -> ConnectionPool
  -> ClearCredentials
  -> Handler String
signupRoute cookieSettings
  jwtSettings
  pool
  cc@(ClearCredentials kemail pass) = do
  let cemail = fromPersistValue <$> keyToValues kemail
      testemail = unPasswordKey kemail
  hash <- hashPassword hashIterations $ B.pack . cs $ pass

  -- kemail is a `Key Password`, in order to get a value out, convert
  -- to a list of PersistValues (for if it's a multi-valued key), and
  -- convert those to `Either <Haskell Value>`s
  case cemail of
    Right email:[] -> do
      u <- runDb pool $ insert (User email)
      p <- runDb pool $ insert (Password u (cs . B.unpack $ hash))
      return "hi"
 
-- Set Headers
-- PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
--                                         , Header "Set-Cookie" SetCookie]
--                                        NoContent)
