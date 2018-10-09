{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Db where

import Control.Monad.Logger
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class
import Servant
import Data.Text (Text)
import qualified Data.Configurator as C -- TODO: shouldn't need configuration
import qualified Data.Configurator.Types as CT -- TODO: shouldn't need configuration
import Data.String.Conversions (cs)

runDb :: (MonadIO m) => ConnectionPool -> SqlPersistT IO a -> m a
runDb pool query = liftIO $ runSqlPool query pool

runCrud :: Persistable a
  => ConnectionPool
  -> Server (CRUD a)
runCrud pool = (\mdn ->
                  crudNew pool mdn
                  :<|> crudGet pool mdn
                  :<|> crudUpdate pool mdn
                  :<|> crudDelete pool mdn)

crudNew :: Persistable a => ConnectionPool -> Maybe Text -> a -> Handler (Key a)
crudNew pool mdn a = do
  newKey <- runDb pool (insert a)
  return $  newKey

crudGet :: Persistable a => ConnectionPool -> Maybe Text -> Key a -> Handler a
crudGet pool mdn k = do
  mbA <- runDb pool $ get k
  case mbA of
    Nothing -> throwError err404
    Just a -> return $ a -- Entity k a

crudDelete :: Persistable a => ConnectionPool -> Maybe Text -> Key a -> Handler NoContent
crudDelete pool mdn k = do
  runDb pool $ delete k
  return NoContent

crudUpdate :: Persistable a => ConnectionPool -> Maybe Text -> Key a -> a -> Handler NoContent
crudUpdate pool mdn k a = do
  exists <- runDb pool $ get k
  case exists of
    Nothing -> throwError err404
    Just _ -> runDb pool $ replace k a >> return NoContent


type DN = Header "dn" Text -- todo: replace DN header with some sort
                           -- of appropriate session header

type CRUD a = DN :> (ReqBody '[JSON] a :> Post '[JSON] (Key a) -- create
                     :<|> Capture "id" (Key a) :> Get '[JSON] a -- read
                     :<|> Capture "id" (Key a)
                      :> ReqBody '[JSON] a :> Put '[JSON] NoContent -- update
                     :<|> Capture "id" (Key a) :> Delete '[JSON] NoContent -- delete
                    )

type Persistable val = (PersistEntityBackend val ~ SqlBackend
                       , PersistEntity val
                       -- , ToBackendKey SqlBackend val -- TODO: why
                       -- did ppl include this?
                       )


--------------------------------------------------
-- Database

data Env = Development
         | Testing
         | Production
         deriving (Read, Eq, Show)

connStr :: MonadIO m => CT.Config -> m ConnectionString
connStr cfg = do
  host <- f "host"
  dbname <- f "dbname"
  port <- f "port"
  user <- f "user"
  pass <- f "pass"
  return . cs $ concat ["host=", host
                       , " dbname=", dbname
                       , " port=", port
                       , " user=", user
                       , " password=", pass]
    where
      f = liftIO . C.require cfg . cs . ("postgres." ++)

makePool :: Env -> CT.Config -> IO ConnectionPool
makePool Development cfg = do
  c <- (connStr cfg)
  runStdoutLoggingT $ createPostgresqlPool c 2
  
makePool Testing cfg = undefined
makePool Production cfg = undefined
