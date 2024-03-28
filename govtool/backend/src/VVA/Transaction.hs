{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module VVA.Transaction where


import Data.ByteString (ByteString)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader
import Data.FileEmbed (embedFile)
import Data.Text (Text, unpack, pack)
import Data.String (fromString)
import qualified Data.Text.Encoding as Text
import qualified Database.PostgreSQL.Simple as SQL
import VVA.Config
import Data.Aeson (Value)
import Data.Has (Has)
import VVA.Pool (ConnectionPool, withPool)
import VVA.Types (TransactionStatus(..), AppError(..))
import Control.Exception (throw)

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs


getTransactionStatusSql :: SQL.Query
getTransactionStatusSql = sqlFrom $(embedFile "sql/get-transaction-status.sql")

getTransactionStatus ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m)
  => Text
  -> m TransactionStatus
getTransactionStatus transactionId = withPool $ \conn -> do
  result <- liftIO $ SQL.query conn getTransactionStatusSql (SQL.Only transactionId)
  case result of
    [SQL.Only True] -> return TransactionConfirmed
    [SQL.Only False] -> return TransactionUnconfirmed
    x -> throwError $ CriticalError ("Expected exactly one result from get-transaction-status.sql but got " <> pack (show (length x)) <> " of them. This should never happen")
