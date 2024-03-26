{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module VVA.Proposal where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Foldable (fold)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..), getSum)
import Data.Scientific
import Data.String (fromString)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time
import qualified Database.PostgreSQL.Simple as SQL
import qualified GHC.Generics as SQL
import VVA.Config
import Data.Aeson (Value)
import Text.Read (readMaybe)
import Data.Has (Has)
import VVA.Pool (ConnectionPool, withPool)
import Control.Exception (throw)
import VVA.Types (Proposal(..), AppError(..))

import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import Data.Text (Text)

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
getProposals mProposalIds = withPool $ \conn -> do
  proposalResults <- liftIO $ case mProposalIds of
    Nothing          -> SQL.query @(Bool, SQL.In [Text]) conn listProposalsSql (False, SQL.In [])
    Just proposalIds -> SQL.query conn listProposalsSql (True, SQL.In proposalIds)

  timeZone <- liftIO getCurrentTimeZone
  return $ map
            ( \( id'
               , txHash'
               , index'
               , type'
               , details'
               , expiryDate'
               , expiryEpochNo'
               , createdDate'
               , createdEpochNo'
               , url'
               , docHash'
               , title'
               , about'
               , motivation'
               , rationale'
               , yesVotes'
               , noVotes'
               , abstainVotes'
               ) ->
              let eDate = localTimeToUTC timeZone <$> expiryDate'
                  cDate = localTimeToUTC timeZone createdDate'
              in
                Proposal
                  id'
                  txHash'
                  (floor @Scientific index')
                  type'
                  details'
                  eDate
                  expiryEpochNo'
                  cDate
                  createdEpochNo'
                  url'
                  docHash'
                  title'
                  about'
                  motivation'
                  rationale'
                  (floor @Scientific yesVotes')
                  (floor @Scientific noVotes')
                  (floor @Scientific abstainVotes')
            )
            proposalResults