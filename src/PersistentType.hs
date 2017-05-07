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

import Type



--------------------------------------------------
-- Person

share [ mkPersist sqlSettings { mpsGenerateLenses = True } , mkMigrate "migrateAll"]
      [persistLowerCase|
Person json
    name Text
    isAdmin Bool
    UniqueName name
    deriving Show
BlogPost json
    title Text
    content Text
    deriving Show
PostRights json
    person PersonId
    post BlogPostId
    access AccessType
    UniqueRight person post
    deriving Show
|]


--------------------------------------------------
-- MKey

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
