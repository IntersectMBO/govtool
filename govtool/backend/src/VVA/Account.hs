{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module VVA.Account where

import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader       (MonadIO, MonadReader, liftIO)

import           Data.ByteString            (ByteString)
import           Data.FileEmbed             (embedFile)
import           Data.Has                   (Has)
import           Data.String                (fromString)
import           Data.Text                  (Text, unpack)
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text

import qualified Database.PostgreSQL.Simple as SQL

import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types                  (AccountInfo (..), AppError (..))

sqlFrom :: ByteString -> SQL.Query
sqlFrom = fromString . unpack . Text.decodeUtf8

accountInfoSql :: SQL.Query
accountInfoSql = sqlFrom $(embedFile "sql/get-account-info.sql")

accountInfo ::
  (Has ConnectionPool r, MonadReader r m, MonadIO m, MonadError AppError m) =>
  Text ->
    m AccountInfo
accountInfo stakeKey = withPool $ \conn -> do
  result <- liftIO $ SQL.query conn accountInfoSql (SQL.Only stakeKey)
  case result of
    [(id, view, is_registered, is_script_based)] ->
      return $ AccountInfo id view is_registered is_script_based
    _ -> throwError $ CriticalError "Could not query the account info."
