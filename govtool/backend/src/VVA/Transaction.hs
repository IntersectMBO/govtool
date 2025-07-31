{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module VVA.Transaction where

import           Control.Exception          (throw)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader

import           Data.Aeson                 (Value)
import           Data.ByteString            (ByteString)
import           Data.FileEmbed             (embedFile)
import           Data.Has                   (Has)
import           Data.String                (fromString)
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text
import           Data.Maybe                 (fromMaybe)

import qualified Database.PostgreSQL.Simple as SQL

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types                  (AppError (..), TransactionStatus (..))

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

getTransactionStatusSql :: SQL.Query
getTransactionStatusSql = sqlFrom $(embedFile "sql/get-transaction-status.sql")

getTransactionStatus ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m)
  => Text
  -> m (Maybe TransactionStatus)
getTransactionStatus transactionId = withPool $ \conn -> do
  result <- liftIO $ SQL.query conn getTransactionStatusSql (transactionId, transactionId, transactionId)
  case result of
    [(transactionConfirmed, votingProcedure, drepRegistration)] -> do
      return $ Just $ TransactionStatus transactionConfirmed votingProcedure drepRegistration
    _ -> return Nothing