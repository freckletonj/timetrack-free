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

import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import PersistentType
import Api

-- TODO: belongs in config
hashIterations = 12         -- 15 =~ 6 sec

-- data Authy val = NotAuthenticated
--                     | Authenticated val

type TokenRoute = ReqBody '[JSON] ClearCredentials
                :> Post '[JSON] Text

type SignupRoute = ReqBody '[JSON] ClearCredentials
                 :> Post '[JSON] NoContent

type Authed authable obj route =
  (Auth authable obj :> route)

class Authable a where -- TODO: can I generalize ConnectionPool?
  isAuthenticated :: (MonadIO m) => ConnectionPool -> a -> m Bool


----------
-- Data-Model-Specific Implementation

data ClearCredentials = ClearCredentials { ccEmail :: Key Password
                                         , ccClearPass :: Text}
                 deriving (Show) -- TODO: remove Show

instance FromJSON ClearCredentials where
  parseJSON (Object v) = ClearCredentials 
                         <$> v .: "email"
                         <*> v .: "password"
  
-- TODO: clean up nesting
-- TODO: clean up cs, Text, B.pack
instance Authable ClearCredentials where
  isAuthenticated pool (ClearCredentials email clearpass) = do
    liftIO $ putStrLn $ show email ++ show clearpass
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

type LoginRoute = ReqBody '[JSON] ClearCredentials
                  :> Post '[JSON] String

loginRoute :: CookieSettings
  -> JWTSettings
  -> ConnectionPool
  -> ClearCredentials
  -> Handler String
loginRoute cookieSettings jwtSettings pool cc = do
  valid <- isAuthenticated pool cc
  case valid of
    True -> return "hi"
    False -> return "nope"
  

-- Set Headers
-- PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
--                                         , Header "Set-Cookie" SetCookie]
--                                        NoContent)
