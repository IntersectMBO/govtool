{-# LANGUAGE FlexibleContexts #-}

module VVA.Pool where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)

import           Data.Has                   (Has, getter)
import           Data.Pool                  (Pool, putResource, takeResource)

import           Database.PostgreSQL.Simple (Connection)

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
