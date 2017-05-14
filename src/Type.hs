{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Type where

import GHC.Generics (Generic)
import Data.Aeson

import Database.Persist
import Database.Persist.Sql

import Data.UUID

import Data.String.Conversions (cs)
import Text.Read (readMaybe)

import Data.Time (UTCTime(..), picosecondsToDiffTime, diffTimeToPicoseconds)
import Web.PathPieces (PathPiece(..))


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
                                     (Left "Couldn't read AccessType")
                                     Right
                                     $ readMaybe . cs $ t

instance PersistFieldSql AccessType where
  sqlType _ = SqlString


--------------------------------------------------
-- Custom UTCTime, since the stock one does weirdness with milliseconds being there, or not

newtype CUTCTime = CUTCTime UTCTime
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance PersistField CUTCTime where
  toPersistValue (CUTCTime (UTCTime d t)) = toPersistValue (UTCTime d (removeMs t))
    where
      removeMs =  fromInteger . floor
  fromPersistValue (PersistUTCTime x) = Right $ CUTCTime x
  
instance PersistFieldSql CUTCTime where
  sqlType _ = SqlDayTime


--------------------------------------------------
-- UUID

-- JSON Instances

instance ToJSON UUID where
  toJSON = String . toText
instance FromJSON UUID where
  parseJSON = withText "UUID" $ (\x -> case fromText x of
                                    Just y -> return y
                                    Nothing -> fail "not a uuid")

-- Persist Instances
--   reference :
--     - http://bitemyapp.com/posts/2016-06-15-uuids-with-persistent-yesod.html
--     - http://michaelxavier.net/posts/2015-04-14-Adding-a-UUID-Column-to-a-Persistent-Table.html

instance PersistField UUID where
  toPersistValue u = PersistDbSpecific . toASCIIBytes $ u
  fromPersistValue (PersistDbSpecific t) =
    case fromASCIIBytes t of
      Just x -> Right  x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"
  
instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

-- Yesod-related cruft, but needed for UUIDs to work
--   just converts to/from text for yesod's url paths
instance PathPiece UUID where
  toPathPiece = toText
  fromPathPiece = fromText
