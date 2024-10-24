{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module VVA.Proposal where

import           Control.Exception          (throw)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Aeson.Types           (Parser, parseMaybe)
import           Data.ByteString            (ByteString)
import           Data.FileEmbed             (embedFile)
import           Data.Foldable              (fold)
import           Data.Has                   (Has)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Sum (..), getSum)
import           Data.Scientific
import           Data.String                (fromString)
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text
import           Data.Time

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Types as PG
import           Database.PostgreSQL.Simple.ToField (ToField(..))
import           Database.PostgreSQL.Simple.ToRow (ToRow(..))

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types                  (AppError (..), Proposal (..))

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

listProposalsSql :: SQL.Query
listProposalsSql = sqlFrom $(embedFile "sql/list-proposals.sql")

newtype TextArray = TextArray [Text]

instance ToRow TextArray where
  toRow (TextArray texts) = map toField texts

listProposals ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
  Maybe Text -> m [Proposal]
listProposals mSearch = getProposals (fmap (:[]) mSearch)

getProposal ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
  Text ->
  Integer ->
  m Proposal
getProposal txHash index = do
  let proposalId = txHash <> "#" <> pack (show index)
  result <- getProposals (Just [proposalId])
  case result of
    [] -> throwError $ NotFoundError ("Proposal with id: " <> proposalId <> " not found")
    [a] -> return a
    _ -> throwError $ CriticalError ("Multiple proposals found for id: " <> proposalId <> ". This should never happen")

getProposals ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
  Maybe [Text] -> m [Proposal]
getProposals mSearchTerms = withPool $ \conn -> do
  let searchParam = maybe "" head mSearchTerms
  liftIO $ SQL.query conn listProposalsSql 
    ( searchParam
    , "%" <> searchParam <> "%"
    , "%" <> searchParam <> "%"
    , "%" <> searchParam <> "%"
    , "%" <> searchParam <> "%"
    , "%" <> searchParam <> "%"
    )
