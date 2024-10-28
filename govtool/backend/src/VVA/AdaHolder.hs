{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module VVA.AdaHolder where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Crypto.Hash

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C
import           Data.FileEmbed             (embedFile)
import           Data.Has                   (Has)
import           Data.Scientific
import           Data.String                (fromString)
import           Data.Text                  (Text, unpack)
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text

import qualified Database.PostgreSQL.Simple as SQL

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

getCurrentDelegationSql :: SQL.Query
getCurrentDelegationSql = sqlFrom $(embedFile "sql/get-current-delegation.sql")

getCurrentDelegation ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  Text ->
  m (Maybe Delegation)
getCurrentDelegation stakeKey = withPool $ \conn -> do
  result <- liftIO $ SQL.query conn getCurrentDelegationSql (SQL.Only stakeKey)
  case result of
    []                                                 -> return Nothing
    [(mDRepHash, dRepView, isDRepScriptBased, txHash)] -> return $ Just $ Delegation mDRepHash dRepView isDRepScriptBased txHash
    _                                                  -> error ("multiple delegations for stake key: " <> unpack stakeKey)

getVotingPowerSql :: SQL.Query
getVotingPowerSql = sqlFrom $(embedFile "sql/get-stake-key-voting-power.sql")

getStakeKeyVotingPower ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m) =>
  Text ->
  m Integer
getStakeKeyVotingPower stakeKey = withPool $ \conn -> do
  result <- liftIO $ SQL.query @_ @(Scientific, Text) conn getVotingPowerSql (SQL.Only stakeKey)
  case result of
    [(votingPower,_)] -> return $ floor votingPower
    _ -> do
      liftIO $ Text.putStrLn ("couldn't fetch voting power for stake key: " <> stakeKey)
      return 0
