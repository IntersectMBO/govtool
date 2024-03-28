{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module VVA.Epoch where

import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader

import           Data.Aeson                 (Value)
import           Data.ByteString            (ByteString)
import           Data.FileEmbed             (embedFile)
import           Data.Has                   (Has)
import           Data.String                (fromString)
import           Data.Text                  (Text, unpack)
import qualified Data.Text.Encoding         as Text

import qualified Database.PostgreSQL.Simple as SQL

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

getCurrentEpochParamsSql :: SQL.Query
getCurrentEpochParamsSql = sqlFrom $(embedFile "sql/get-current-epoch-params.sql")

getCurrentEpochParams ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  m (Maybe Value)
getCurrentEpochParams = withPool $ \conn -> do
  result <- liftIO $ SQL.query_ conn getCurrentEpochParamsSql
  case result of
    []                  -> return Nothing
    (SQL.Only params:_) -> return $ Just params
