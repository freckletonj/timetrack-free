{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveGeneric #-}


module Authenticate where

import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics (Generic)
import Servant
import Database.Persist


data ClearCredentials = ClearCredentials { email :: String
                                         , clearPass :: String}
                 deriving (Generic, FromJSON)

type AuthenticateAPI = "token" :> ReqBody '[JSON] ClearCredentials
                       :> Post '[JSON] (String)
                       
                       :<|> "signup" :> ReqBody '[JSON] ClearCredentials
                       :> Post '[JSON] NoContent
