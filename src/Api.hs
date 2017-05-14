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
import Control.Monad.Writer
import Control.Monad.Trans.Except
import Control.Monad.Except

import DSL
import Data.Aeson
import qualified Data.Foldable as F
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text.Lens
import Database.Persist.Postgresql
import Database.Persist.TH
import Debug.Trace 
import qualified Network.Wai.Handler.Warp
import PersistentType
import Servant hiding (throw)
import Control.Exception.Safe hiding (throw)
import Type

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.HashMap.Lazy as HM


{-

# TODO

## Errors
- return appropriate errors
- safe-exceptions
- ExceptT
- ServantError
- 

## Transactions
- http://stackoverflow.com/questions/31359894/catching-an-exception-from-rundb
- transaction errors log + return (exception-lifted?)
- runSqlConn(?) keeps things in a transaction
- transactionSave / transactionUndo?

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
-- Interpreting the Persistence DSL

type ServantIO a = SqlPersistT (LoggingT (ExceptT ServantErr IO)) a

runPersistence :: PersistenceService a -> ServantIO a
runPersistence ps = case O.view ps of
                  Return a -> return a
                  a :>>= f -> runM a f

-- connRollback conn (getStmtConn conn)

-- conn <- ask
-- liftIO $ connRollback conn (getStmtConn conn)

runM :: PersistenceAction a -> (a -> PersistenceService b) -> ServantIO b
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
      tsf = runPersistence . f


--------------------------------------------------
-- Interpreting Persistence DSL for testing

-- newtype PersistenceLogIO a = PersistenceLogIO {
--   runPersistenceLogIO :: WriterT [String] (ExceptT ServantErr IO) a
--   } deriving (Functor, Applicative, Monad, MonadWriter [String], MonadError ServantErr)

-- runWebLog :: PersistenceService a -> PersistenceLogIO a
-- runWebLog ps = case O.view ps of
--                  Return a -> return a
--                  a :>>= f -> runL a f

-- runL :: PersistenceAction a -> (a -> PersistenceService b) -> PersistenceLogIO b
-- runL x f = case x of
--   Throw e -> do
--     tell ["error"]
--     throwError e
--   Get k -> tell ["get"] >> tsf Nothing
--   where
--     tsf = runWebLog . f



--------------------------------------------------
-- Implementing the API

-- | Create an AccessType for 1. a record on 2. a key within 3. a rights-tracking Entity
createRight :: (PersistEntityBackend val ~ SqlBackend, PersistEntity val) =>
     Entity record                             -- ex: Key Person
     -> t                                      -- ex: Key BlogPost
     -> (Key record -> t -> AccessType -> val) -- ex: PostRights
     -> PersistenceService (Key val)
createRight usr k constructor =  mnew (constructor (entityKey usr) k Owner)

runCrud :: (PersistEntity a, ToBackendKey SqlBackend a) -- PC b
        => ConnectionPool -- ^ Connection pool
        -> (Maybe Text ->
            (a -> ExceptT ServantErr IO (MKey a))
            :<|> ((MKey a      -> ExceptT ServantErr IO a)
            :<|> ((MKey a -> a -> ExceptT ServantErr IO ())
            :<|> (MKey a      -> ExceptT ServantErr IO ())))
           )
runCrud pool =
  (\dn -> runnew dn
          :<|> runget dn
          :<|> runupd dn
          :<|> rundel dn)
    where
        auth Nothing _ = throw err401
        auth (Just dn) perm = do
            user <- mgetBy dn >>= maybe (throw err403) return
            -- check <- checkPerms user perm
            -- unless check (throw err403)
            return user
        runnew dn val = runQuery $ do
            -- usr <- auth dn pnew
            k <- mnew val
            -- monadic map over the traversable Maybe
            -- F.mapM_ (createRight usr k) rightConstructor
            return (k ^. from _MKey)
        runget dn mk = runQuery $ do
            let k = mk ^. _MKey
            -- void $ auth dn (pget k) -- void :: Functor f => f a -> f ()
            mgetOr404 k
        runupd dn mk val = runQuery $ do
            let k = mk ^. _MKey
            -- void $ auth dn (pupd k)
            mupd k val
        rundel dn mk = runQuery $ do
            let k = mk ^. _MKey
            -- void $ auth dn (pdel k)
            -- F.mapM_ ($ k) predelete
            mdel k
        runQuery :: PersistenceService a -> ExceptT ServantErr IO a
        runQuery ps = runStderrLoggingT $ runSqlPool (runPersistence ps) pool


--------------------------------------------------
-- Serving the API

type MyApi = "user" :> CRUD User
        :<|> "clock"   :> CRUD Clock
        :<|> "session" :> CRUD Session
        :<|> "ghcredential" :> CRUD GHCredential

myApi :: Proxy MyApi
myApi = Proxy

server :: ConnectionPool -> ServerT MyApi (ExceptT ServantErr IO) -- Server MyApi
server pool = runCrud pool
              :<|> runCrud pool
              :<|> runCrud pool
              :<|> runCrud pool


--------------------------------------------------
-- Database

data Env = Development
         | Testing
         | Production
         deriving (Read, Eq, Show)

connStr :: MonadIO m => CT.Config -> m ConnectionString
connStr cfg = do
  host <- f "postgres.host"
  dbname <- f "postgres.dbname"
  port <- f "postgres.port"
  user <- f "postgres.user"
  pass <- f "postgres.pass"
  return . cs $ concat ["host=", host
                       , " dbname=", dbname
                       , " port=", port
                       , " user=", user
                       , " password=", pass]
    where
      f = liftIO . C.require cfg

makePool :: Env -> CT.Config -> IO ConnectionPool
makePool Development cfg = do
  c <- (connStr cfg)
  runStdoutLoggingT $ createPostgresqlPool c 2
  
makePool Testing     cfg = undefined
makePool Production  cfg = undefined


--------------------------------------------------
-- Main

main :: IO ()
main = do

  -- Config
  cfg <- C.load [C.Required "resource/config.cfg"]
  env <- C.require cfg "environment"

  -- Resources
  pool <- makePool (read env) cfg -- read == unsafe
  runSqlPool (runMigration migrateAll) pool
  

  -- Run
  Network.Wai.Handler.Warp.run 8080 (serve myApi (server pool))
