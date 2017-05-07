{-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Type where

import GHC.Generics (Generic)
import Data.Aeson

import Database.Persist
import Database.Persist.Sql

import Data.String.Conversions (cs)
import Text.Read (readMaybe)

--import Database.Persist.TH

--------------------------------------------------
-- AccessType

data AccessType = NoAccess
                | ReadOnly
                | ReadWrite
                | Owner
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

instance PersistField AccessType where
  toPersistValue = toPersistValue . show
  fromPersistValue (PersistText t) = maybe
                                     (Left . cs $ "Couldn't read AccessType")
                                     Right
                                     $ readMaybe . cs $ t

instance PersistFieldSql AccessType where
  sqlType _ = SqlString
