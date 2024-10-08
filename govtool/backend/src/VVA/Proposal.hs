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

import qualified GHC.Generics               as SQL

import           Text.Read                  (readMaybe)

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types                  (AppError (..), Proposal (..))

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

listProposalsSql :: SQL.Query
listProposalsSql = sqlFrom $(embedFile "sql/list-proposals.sql")

listProposals ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
  m [Proposal]
listProposals = getProposals Nothing

getProposal ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
  Text ->
  Integer ->
  m Proposal
getProposal txHash index = do
  result <- getProposals (Just [txHash <> "#" <> pack (show index)])
  case result of
    [] -> throwError $ NotFoundError ("Proposal with id: " <> txHash <> "#" <> pack (show index) <> " not found")
    [a] -> return a
    _ -> throwError $ CriticalError ("Multiple proposal found for id: " <> txHash <> "#" <> pack (show index) <> ". This should never happen")

getProposals ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
  Maybe [Text] ->
  m [Proposal]
getProposals mProposalIds = withPool $ \conn ->
  liftIO $ case mProposalIds of
    Nothing          -> SQL.query @(Bool, SQL.In [Text]) conn listProposalsSql (False, SQL.In [])
    Just proposalIds -> SQL.query conn listProposalsSql (True, SQL.In proposalIds)
