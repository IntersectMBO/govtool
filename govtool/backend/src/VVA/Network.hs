{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module VVA.Network where

import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader

import           Data.Aeson                 (Value)
import           Data.ByteString            (ByteString)
import           Data.FileEmbed             (embedFile)
import           Data.Has                   (Has)
import           Data.String                (fromString)
import           Data.Text                  (Text, unpack)
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock

import qualified Database.PostgreSQL.Simple as SQL

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

networkInfoSql :: SQL.Query
networkInfoSql = sqlFrom $(embedFile "sql/get-network-info.sql")

networkInfo ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m) =>
  m NetworkInfo
networkInfo = withPool $ \conn -> do
  result <- liftIO $ SQL.query_ conn networkInfoSql
  current_time <- liftIO getCurrentTime
  case result of 
    [( network_epoch
     , network_block_no
     , network_name
     )] -> return $ NetworkInfo
              current_time
              network_epoch
              network_block_no
              network_name
    _ -> throwError $ CriticalError "Could not query the network info. This should never happen."

networkTotalStakeSql :: SQL.Query
networkTotalStakeSql = sqlFrom $(embedFile "sql/get-network-total-stake.sql")

networkTotalStake ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m) =>
  m NetworkTotalStake
networkTotalStake = withPool $ \conn -> do
  result <- liftIO $ SQL.query_ conn networkTotalStakeSql
  case result of
    [( total_stake_controlled_by_dreps
     , total_stake_controlled_by_spos
     , always_abstain_voting_power
      , always_no_confidence_voting_power
      )] -> return $ NetworkTotalStake
              total_stake_controlled_by_dreps
              total_stake_controlled_by_spos
              always_abstain_voting_power
              always_no_confidence_voting_power
    _ -> throwError $ CriticalError "Could not query the network total stake. This should never happen."

networkMetricsSql :: SQL.Query
networkMetricsSql = sqlFrom $(embedFile "sql/get-network-metrics.sql")

networkMetrics ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m) =>
  m NetworkMetrics
networkMetrics = withPool $ \conn -> do
  result <- liftIO $ SQL.query_ conn networkMetricsSql
  case result of
    [( unique_delegators
     , total_delegations
     , total_gov_action_proposals
     , total_drep_votes
     , total_registered_dreps
     , total_drep_distr
     , total_active_dreps
     , total_inactive_dreps
     , total_active_cip119_compliant_dreps
     , total_registered_direct_voters
     , no_of_committee_members
     , quorum_numerator
     , quorum_denominator
     )] -> return $ NetworkMetrics
            unique_delegators
            total_delegations
            total_gov_action_proposals
            total_drep_votes
            total_registered_dreps
            total_drep_distr
            total_active_dreps
            total_inactive_dreps
            total_active_cip119_compliant_dreps
            total_registered_direct_voters
            no_of_committee_members
            quorum_numerator
            quorum_denominator
    _ -> throwError $ CriticalError "Could not query the network metrics. This should never happen."
