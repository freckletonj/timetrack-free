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
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.String.Conversions (cs)
import Text.Read (readMaybe)
import Data.Aeson
import GHC.Generics (Generic)
import Web.HttpApiData
import Data.Text
import qualified Data.Text as T
import Control.Arrow
import Data.UUID

import Type

--------------------------------------------------
-- Person

share [ mkPersist sqlSettings { mpsGenerateLenses = True } , mkMigrate "migrateAll"]
      [persistLowerCase|

User json
  -- UUID sqltype=uuid default=uuid_generate_v4()
  email Text
  deriving Show

Clock json
  userId User
  timein CUTCTime
  timeout CUTCTime
  deriving Show
  
Session json
  userId User
  token Text
  deriving Show
  
GHCredential json
  userId User
  secret Text

|]

        
--------------------------------------------------
-- MKey
--
--   iso to Persistent Key's, but you can write new instances
        
newtype MKey a = MKey { getMKey :: Int64 }
  deriving (Generic, FromJSON, ToJSON)

_MKey :: ToBackendKey SqlBackend a => Iso' (MKey a) (Key a)
_MKey = iso (toSqlKey . getMKey) (MKey . fromSqlKey)

parseMKey :: Text -> Either Text (MKey a)
parseMKey t = case readMaybe . cs $ t of
              Just x -> Right . MKey $ x
              Nothing -> Left "couldn't parse MKey"

leftAppend :: Text -> Either Text a -> Either Text a
leftAppend = left . flip T.append

instance FromHttpApiData (MKey a) where
  parseUrlPiece = leftAppend ": UrlPiece" . parseMKey
  parseHeader = leftAppend ": Header" . parseMKey . cs
  parseQueryParam = leftAppend ":QueryParam" . parseMKey

