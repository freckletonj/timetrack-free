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
import PersistentType
import Type

--------------------------------------------------
-- Permission Checking DSL

type PermProgram = Program PermCheck
data PermCheck a where
    IsAdmin       :: PermCheck Bool
    BlogPostRight :: Key BlogPost -> PermCheck AccessType

-- checks that the current user is an administrator
isAdmin :: PermProgram Bool
isAdmin = singleton IsAdmin

-- returns the access right of the current user on a particular blog post
blogPostRight :: Key BlogPost -> PermProgram AccessType
blogPostRight = singleton . BlogPostRight

-- checks that an `AccessType` is at least `ReadOnly`
ro :: PermProgram AccessType -> PermProgram Bool
ro = fmap (>= ReadOnly)

-- checks that an `AccessType` is at least `ReadWrite`
rw :: PermProgram AccessType -> PermProgram Bool
rw = fmap (>= ReadWrite)

-- checks that an `AccessType` is at least `Owner`
owner :: PermProgram AccessType -> PermProgram Bool
owner = fmap (>= Owner)

-- helper function for actions that everyone can perform
always :: PermProgram Bool
always = pure True

-- operator for "or-ing" two permission checking actions
(.||) :: PermProgram Bool -> PermProgram Bool -> PermProgram Bool
(.||) = liftM2 (||)

-- operator for "and-ing" two permission checking actions
(.&&) :: PermProgram Bool -> PermProgram Bool -> PermProgram Bool
(.&&) = liftM2 (&&)

-- isAdmin .|| (ro (blogPostRight postid) .&& owner (commentRight commentid))
