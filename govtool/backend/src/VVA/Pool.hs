{-# LANGUAGE FlexibleContexts #-}
module VVA.Pool where

import Data.Has (Has, getter)
import Data.Pool (Pool, takeResource, putResource)
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)

type ConnectionPool = Pool Connection

withPool
  :: (Has ConnectionPool r, MonadReader r m, MonadIO m)
  => (Connection -> m a)
  -> m a
withPool f = do
  pool <- asks getter
  (conn,localPool) <- liftIO $ takeResource pool
  result <- f conn
  liftIO $ putResource localPool conn
  return result
