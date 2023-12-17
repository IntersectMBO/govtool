{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module VVA.Network where


import Data.ByteString (ByteString)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader
import Data.FileEmbed (embedFile)
import Data.Text (Text, unpack)
import Data.String (fromString)
import qualified Data.Text.Encoding as Text
import qualified Database.PostgreSQL.Simple as SQL
import VVA.Config
import Data.Aeson (Value)
import Data.Has (Has)
import VVA.Pool (ConnectionPool, withPool)
import VVA.Types
import Data.Time.Clock

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

networkMetricsSql :: SQL.Query
networkMetricsSql = sqlFrom $(embedFile "sql/get-network-metrics.sql")

networkMetrics ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m) =>
  m NetworkMetrics
networkMetrics = withPool $ \conn -> do
  result <- liftIO $ SQL.query_ conn networkMetricsSql
  current_time <- liftIO $ getCurrentTime
  case result of
    [( epoch_no
     , block_no
     , unique_delegators
     , total_delegations
     , total_governance_actions
     , total_drep_votes
     , total_registered_dreps
     )] -> return $ NetworkMetrics
            current_time
            epoch_no
            block_no
            unique_delegators
            total_delegations
            total_governance_actions
            total_drep_votes
            total_registered_dreps
    _ -> throwError $ CriticalError "Could not query the network metrics. This should never happen."
