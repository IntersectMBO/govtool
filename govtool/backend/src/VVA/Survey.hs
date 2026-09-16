{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module VVA.Survey where

import           Control.Monad.Except        (MonadError, throwError)
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.ByteString             (ByteString)
import           Data.FileEmbed              (embedFile)
import           Data.Has                    (Has)
import           Data.String                 (fromString)
import           Data.Text                   (Text, unpack)
import qualified Data.Text.Encoding          as Text

import qualified Database.PostgreSQL.Simple  as SQL

import           VVA.Pool                    (ConnectionPool, withPool)
import           VVA.Types                   (AppError (..))

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

getSurveyDefinitionSql :: SQL.Query
getSurveyDefinitionSql = sqlFrom $(embedFile "sql/get-survey-definition.sql")

getSurveyDefinition ::
  (Has ConnectionPool r, MonadReader r m, MonadIO m, MonadBaseControl IO m, MonadError AppError m) =>
  Text ->
  m Text
getSurveyDefinition txId = withPool $ \conn -> do
  result <- liftIO $ SQL.query conn getSurveyDefinitionSql (SQL.Only txId)
  case result of
    [SQL.Only payload] -> pure payload
    _ -> throwError $ NotFoundError $
      "No metadata label 17 found for transaction " <> txId
