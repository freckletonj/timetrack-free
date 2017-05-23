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

module PersistentType where

import Control.Lens
import Data.Int
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Class
import Database.Persist.TH
import Data.String.Conversions (cs)

import Text.Read (readMaybe)
import Data.Text

import Data.Aeson
import GHC.Generics (Generic)
import Web.HttpApiData
import Data.Text
import qualified Data.Text as T
import Control.Arrow
import Web.PathPieces (PathPiece(..))

import Type


share [ mkPersist sqlSettings { mpsGenerateLenses = True } , mkMigrate "migrateAll"]
      [persistLowerCase|

Clock json
  userEmail UserId
  timein CUTCTime
  timeout CUTCTime
  deriving Show

User json
  email Text
  Primary email
  deriving Show

------------------------------
-- Security

Password
  userEmail UserId
  hash Text
  Primary userEmail
  -- no `deriving Show` to help eliminate printing, IE to logs

Session json
  userId UserId
  token Text
  deriving Show

------------------------------
-- OAuth Providers
  
GhCredential json
  userEmail UserId
  token Text
  Primary userEmail
  deriving Show
  
BbCredential json
  userEmail UserId
  token Text
  Primary userEmail
  deriving Show

|]
