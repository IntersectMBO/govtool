{-# LANGUAGE FlexibleContexts #-}

module VVA.Pool where

import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Reader        (MonadReader, asks)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Has                    (Has, getter)
import Data.Pool                   (Pool, withResource)

import Database.PostgreSQL.Simple  (Connection)

type ConnectionPool = Pool Connection

withPool
  :: (Has ConnectionPool r, MonadReader r m, MonadIO m, MonadBaseControl IO m)
  => (Connection -> m a)
  -> m a
withPool f = do
  pool <- asks getter
  withResource pool f
