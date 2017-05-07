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

module Api where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Control.Monad.Operational  as O
import Control.Monad.Operational  hiding (view)
import Control.Monad.Reader       (ask)
import Control.Monad.Trans.Except
import DSL
import Data.Aeson
import qualified Data.Foldable as F
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text.Lens
import Database.Persist.Sqlite
import Database.Persist.TH
import Debug.Trace 
import qualified Network.Wai.Handler.Warp
import PersistentType
import Servant hiding (throw)
import Type

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.HashMap.Lazy as HM


{-

# TODO

## Errors
- return appropriate errors

## Transactions
- http://stackoverflow.com/questions/31359894/catching-an-exception-from-rundb
- transaction errors log + return (exception-lifted?)
- runSqlConn(?) keeps things in a transaction
- transactionSave / transactionUndo?

## Configuration

## Auth
- access control
- credentials add appropriate dn?
- goog/facebook auth? <- lowest risk/compliance + easy for customers?
- sessions and tokens


-}


type CRUD a = DN :> (ReqBody '[JSON] a :> Post '[JSON] (MKey a) -- create
                     :<|> Capture "id" (MKey a) :> Get '[JSON] a -- read
                     :<|> Capture "id" (MKey a)
                      :> ReqBody '[JSON] a :> Put '[JSON] () -- update
                     :<|> Capture "id" (MKey a) :> Delete '[JSON] () -- delete
                    )

type DN = Header "dn" Text



--------------------------------------------------
-- Interpreting the Permissions Checking DSL

-- Given the current user, runs a `PermProgram` in the `WebService` monad.
checkPerms :: Entity Person -> PermProgram a -> WebService a
checkPerms ent cnd = eval (O.view cnd)
    where
        usr = entityVal ent
        userkey = entityKey ent
        eval :: ProgramView PermCheck a -> WebService a
        eval (Return a) = return a
        eval (IsAdmin :>>= nxt) =
              checkPerms ent (nxt (_personIsAdmin usr))
        eval (BlogPostRight k :>>= nxt) =
              mgetBy (UniqueRight userkey k)
                  >>= checkPerms ent
                    . nxt
                    . maybe NoAccess (_postRightsAccess . entityVal)


--------------------------------------------------
-- Interpreting the Web DSL

type ServantIO a = SqlPersistT (LoggingT (ExceptT ServantErr IO)) a

runServant :: WebService a -> ServantIO a
runServant ws = case O.view ws of
                  Return a -> return a
                  a :>>= f -> runM a f

-- connRollback conn (getStmtConn conn)

-- conn <- ask
-- liftIO $ connRollback conn (getStmtConn conn)

runM :: WebAction a -> (a -> WebService b) -> ServantIO b
runM x f = case x of
    Throw rr@(ServantErr code reason body headers) -> do
      conn <- ask
      -- r <- liftIO . try $ ( connRollback conn (getStmtConn conn))
      -- case r of
      --   Left x -> throwError $ x { errBody = "couldn't rollback" }
      --   Right x -> logOtherNS
      --              "Rollback" LevelInfo "successfully rolled back"
      logOtherNS "WS" LevelError (show (code,reason) ^. packed)
      throwError rr
    Get k    -> get k       >>= tsf
    New v    -> insert v    >>= tsf
    Del v    -> delete v    >>= tsf
    GetBy u  -> getBy u     >>= tsf
    Upd k v  -> replace k v >>= tsf
  where
      tsf = runServant . f


--------------------------------------------------
-- Implementing the API

data PermsFor a = PermsFor { _newPerms :: PermProgram Bool
                           , _getPerms :: Key a -> PermProgram Bool
                           , _updPerms :: Key a -> PermProgram Bool
                           , _delPerms :: Key a -> PermProgram Bool
                           }

adminOnly :: PermsFor a
adminOnly = PermsFor isAdmin (const isAdmin) (const isAdmin) (const isAdmin)

runCrud :: (PersistEntity a, ToBackendKey SqlBackend a, PC b)
        => ConnectionPool -- ^ Connection pool
        -> PermsFor a -- ^ Permission checking record
        -> Maybe (Key Person -> Key a -> AccessType -> b)
           -- ^ Extra actions after creation
        -> Maybe (Key a -> WebService ()) -- ^ Extra actions after deletion
        -> (Maybe Text ->
            (a -> ExceptT ServantErr IO (MKey a))
            :<|> ((MKey a      -> ExceptT ServantErr IO a)
            :<|> ((MKey a -> a -> ExceptT ServantErr IO ())
            :<|> (MKey a      -> ExceptT ServantErr IO ())))
           )
runCrud pool (PermsFor pnew pget pupd pdel) rightConstructor predelete =
  (\dn -> runnew dn
          :<|> runget dn
          :<|> runupd dn
          :<|> rundel dn)
    where
        auth Nothing _ = throw err401
        auth (Just dn) perm = do
            user <- mgetBy (UniqueName dn) >>= maybe (throw err403) return
            check <- checkPerms user perm
            unless check (throw err403)
            return user
        runnew dn val = runQuery $ do
            usr <- auth dn pnew
            k <- mnew val
            F.mapM_ (\c -> mnew (c (entityKey usr) k Owner)) rightConstructor
            return (k ^. from _MKey)
        runget dn mk = runQuery $ do
            let k = mk ^. _MKey
            void $ auth dn (pget k) -- void :: Functor f => f a -> f ()
            mgetOr404 k
        runupd dn mk val = runQuery $ do
            let k = mk ^. _MKey
            void $ auth dn (pupd k)
            mupd k val
        rundel dn mk = runQuery $ do
            let k = mk ^. _MKey
            void $ auth dn (pdel k)
            F.mapM_ ($ k) predelete
            mdel k
        runQuery :: WebService a -> ExceptT ServantErr IO a
        runQuery ws = runStderrLoggingT $ runSqlPool (runServant ws) pool

-- A default action for when you need not run additional actions after creation
noCreateRightAdjustment :: Maybe (Key Person -> Key a -> AccessType -> PostRights)
noCreateRightAdjustment = Nothing


--------------------------------------------------
-- Problem

type Problem = Get '[JSON] [Key BlogPost]

problem :: ConnectionPool -> ExceptT ServantErr IO [Key BlogPost]
problem pool = runQuery $ do
  k1 <- mnew (BlogPost "666" "AAA")
  k2 <- mnew (BlogPost "666" (cs . show $ k1))
  throw err303
  k3 <- mnew (BlogPost "666" (cs . show $ k2))

  return $ [k1, k2, k3 ]
      where
        runQuery :: WebService a -> ExceptT ServantErr IO a
        runQuery ws = runStderrLoggingT $ runSqlPool (runServant ws) pool

--------------------------------------------------
-- Serving the API

type MyApi = "person" :> CRUD Person
        :<|> "post"   :> CRUD BlogPost
        :<|> "problem" :> Problem

myApi :: Proxy MyApi
myApi = Proxy

server :: ConnectionPool -> ServerT MyApi (ExceptT ServantErr IO) -- Server MyApi
server pool =
      runCrud pool adminOnly noCreateRightAdjustment Nothing
 :<|> defaultCrud blogPostRight PostRights Nothing
 :<|> problem pool
   where
     editRights c cid = rw (c cid) .|| isAdmin
     delRights c cid = owner (c cid) .|| isAdmin
     defaultPermissions c =
          PermsFor always -- create
                  (const always) -- read
                  (editRights c) -- update
                  (delRights c) -- delete
     defaultCrud c r d = runCrud pool (defaultPermissions c)
                                 (Just r) d


--------------------------------------------------
-- Main

main :: IO ()
main = do

  -- Config
  cfg <- C.load [C.Required "resource/config.cfg"]
  dbPath <- C.require cfg "sqlite.path"


  -- Resources
  pool <- runStderrLoggingT $ do
    p <- createSqlitePool dbPath 1
    runSqlPool (runMigration migrateAll) p
    return p

  -- Run
  Network.Wai.Handler.Warp.run 8080 (serve myApi (server pool))
