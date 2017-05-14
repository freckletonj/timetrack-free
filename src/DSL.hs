{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module DSL where

import Control.Monad
import qualified Control.Monad.Operational  as O
import Control.Monad.Operational  hiding (view)
import Servant hiding (throw)
import Database.Persist
import Database.Persist.Sql

import PersistentType
import Type


--------------------------------------------------
-- Persistence DSL


----------
-- `(...)` - a tuple lifted to the type level
-- `~` - this syntax is native to the compiler, means the two types are equal
-- (a,b) ::  Constraint
-- u ~ u :: Constraint
-- val :: *
-- PersistEntity :: * -> Constraint

-- _kind of_ like a "Constraint synonym"
type PC val = (PersistEntityBackend val ~ SqlBackend, PersistEntity val)

type PersistenceService = Program PersistenceAction
data PersistenceAction a where
    Throw :: ServantErr               -> PersistenceAction a
    Get   :: PC val => Key val        -> PersistenceAction (Maybe val)
    Del   :: PC val => Key val        -> PersistenceAction ()
    GetBy :: PC val => Unique val     -> PersistenceAction (Maybe (Entity val))
    New   :: PC val =>            val -> PersistenceAction (Key val)
    Upd   :: PC val => Key val -> val -> PersistenceAction ()

-- throws an error
throw :: ServantErr -> PersistenceService a
throw = singleton . Throw

-- dual of `persistent`'s `get`
mget :: PC val => Key val -> PersistenceService (Maybe val)
mget = singleton . Get

-- dual of `persistent`'s `getBy`
mgetBy :: PC val => Unique val ->  PersistenceService (Maybe (Entity val))
mgetBy = singleton . GetBy

-- dual of `persistent`'s `insert`
mnew :: PC val => val ->  PersistenceService (Key val)
mnew = singleton . New

-- dual of `persistent`'s `update`
mupd :: PC val => Key val -> val -> PersistenceService ()
mupd k v = singleton (Upd k v)

-- dual of `persistent`'s `delete`
mdel :: PC val => Key val -> PersistenceService ()
mdel = singleton . Del

-- like `mget` but throws a 404 if it could not find the corresponding record
mgetOr404 :: PC val => Key val -> PersistenceService val
mgetOr404 = mget >=> maybe (throw err404) return

-- like `mgetBy` but throws a 404 if it could not find the corresponding record
mgetByOr404 :: PC val => Unique val -> PersistenceService (Entity val)
mgetByOr404 = mgetBy >=> maybe (throw err404) return
