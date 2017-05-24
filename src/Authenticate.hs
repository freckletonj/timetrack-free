{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Authenticate where

import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Crypto.JOSE as Jose
import Crypto.JWT (NumericDate (NumericDate), _claimExp, createJWSJWT)

import Data.Aeson
import Data.Aeson.TH
import Data.Time              (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX  (posixSecondsToUTCTime)

import GHC.Generics (Generic)

import Servant
import Servant.Auth.Server hiding (makeJWT)

import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Text (Text)
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
tokenDuration = 60*60*24*30 -- one month, TODO: make revokable,
                            -- expiration works, but can't be revoked
                            -- that way


----------
-- Data-Model-Specific Implementation

data ClearCredentials = ClearCredentials { ccEmail :: Key Password
                                         , ccClearPass :: Text}

instance FromJSON ClearCredentials where
  parseJSON (Object v) = ClearCredentials 
                         <$> v .: "email"
                         <*> v .: "password"
  
authenticate :: ConnectionPool -> ClearCredentials -> Handler (Maybe User)
authenticate pool (ClearCredentials email clearpass) = do
    mPassword <- runDb pool $ get $ email
    case mPassword of
      Nothing -> return Nothing
      Just (Password _ hash) -> do
            let v = validatePassword
                    (B.pack $ cs clearpass)
                    (B.pack $ cs hash)
                (UserKey email') = unPasswordKey email
            if v
              then return . Just $ User email'
              else return Nothing

type TestProtected = Get '[JSON] (Maybe User)
type TestAuthRoute auths = Auth auths User :> TestProtected

testAuthRoute :: AuthResult User -> Server TestProtected
testAuthRoute (Authenticated u) = return $ Just u
testAuthRoute _ = return Nothing

instance FromJWT User where
instance ToJWT User where

----------

type LoginRoute = ReqBody '[JSON] ClearCredentials
                  :> Post '[JSON] String

-- | Make an expiring JWT using RS256
-- https://hackage.haskell.org/package/jose-0.6.0.1/docs/src/Crypto-JWT.html#claimExp
makeJWT :: ToJWT a => a
  -> JWTSettings
  -> Maybe UTCTime
  -> IO (Either Jose.Error BL.ByteString)
makeJWT value cfg expiry = runExceptT $ do
  ejwt <- createJWSJWT (key cfg)
          (Jose.newJWSHeader (Jose.Protected, Jose.RS256))
          (addExp $ encodeJWT value)
  Jose.encodeCompact ejwt
  where addExp claims = case expiry of
          Nothing -> claims
          Just e -> claims { _claimExp = Just $ NumericDate e }
          
loginRoute :: CookieSettings
  -> JWTSettings
  -> ConnectionPool
  -> ClearCredentials
  -> Handler String
loginRoute cookieSettings jwtSettings pool cc = do
  muser <- authenticate pool cc
  case muser of
    Nothing -> return "nope"
    Just u -> do
      now <- liftIO $ getCurrentTime
      let expiry = addUTCTime tokenDuration now
      etoken <- liftIO $ makeJWT u jwtSettings $ Just expiry
      case etoken of
        Left e -> return $ show e
        Right v -> return . B.unpack . BL.toStrict $ v
  
----------

type SignupRoute = ReqBody '[JSON] ClearCredentials
                   :> Post '[JSON] User

signupRoute :: CookieSettings
  -> JWTSettings
  -> ConnectionPool
  -> ClearCredentials
  -> Handler User
signupRoute cookieSettings
  jwtSettings
  pool
  (ClearCredentials kemail pass) = do
  let (UserKey email) = unPasswordKey kemail
  hash <- hashPassword hashIterations $ B.pack . cs $ pass
  u@(UserKey e) <- runDb pool $ insert (User email)
  p <- runDb pool $ insert (Password u (cs . B.unpack $ hash))
  return (User e)
 
