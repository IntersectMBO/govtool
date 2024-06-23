{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE BlockArguments    #-}

module VVA.Transaction where

import           Data.Time
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Network.WebSockets.Connection as WS
import           GHC.Conc (TVar, threadDelay, readTVar, readTVarIO, writeTVar, atomically)
import           Data.Maybe
import Data.UUID.V4 (nextRandom)
import           Network.WebSockets.Connection (Connection)
import qualified Database.Redis as Redis
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

import qualified Database.PostgreSQL.Simple as SQL

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types                  (AppError (..), AppEnv (..), WebsocketTvar,
                                             TransactionStatus (..), vvaWebSocketConnections)

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

timeoutStaleWebsocketConnections ::
  (Has AppEnv r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m)
  => WebsocketTvar
  -> m ()
timeoutStaleWebsocketConnections tvar = do
  currentTime <- liftIO getCurrentTime
  connections <- liftIO $ readTVarIO tvar
  websocketLifetimeSeconds <- getWebsocketLifetimeSeconds
  let staleConnections = Map.filter (\(_, time) -> diffUTCTime currentTime time > fromIntegral websocketLifetimeSeconds) connections
  forM_ (Map.keys staleConnections) $ \txHash -> do
    removeWebsocketConnection tvar txHash "Websocket timed out."
  liftIO $ threadDelay (30 * 1000000)
  timeoutStaleWebsocketConnections tvar

processTransactionStatuses ::
  (Has AppEnv r, Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m)
  => WebsocketTvar
  -> m ()
processTransactionStatuses tvar = do
  txs <- getWatchedTransactions
  forM_ txs $ \(txHash, uuid) -> do
    status <- getTransactionStatus txHash
    case status of
      TransactionConfirmed -> do
        connection <- getWebsocketConnection tvar uuid
        case connection of
          Just (conn, _) -> do
            liftIO $ WS.sendTextData conn ("{\"status\": \"confirmed\"}" :: Text)
            unWatchTransaciton tvar uuid
            removeWebsocketConnection tvar uuid "Tx confirmed. Closing connection."
          Nothing -> unWatchTransaciton tvar uuid
      TransactionUnconfirmed -> return ()

  liftIO $ threadDelay (20 * 1000000)
  processTransactionStatuses tvar


watchTransaction ::
  (Has AppEnv r, Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m)
  => WebsocketTvar
  -> Text
  -> Connection
  -> m ()
watchTransaction tVar txHash connection = do

  uuid <- (pack . show) <$> liftIO nextRandom

  port <- getRedisPort
  host <- getRedisHost
  conn <- liftIO $ Redis.checkedConnect $ Redis.defaultConnectInfo {Redis.connectHost = unpack host, Redis.connectPort = Redis.PortNumber $ fromIntegral port, Redis.connectDatabase = 1}
  websocketLifetimeSeconds <- getWebsocketLifetimeSeconds

  liftIO $ Redis.runRedis conn $ do
    _ <- Redis.set (Text.encodeUtf8 txHash) (Text.encodeUtf8 uuid)
    Redis.expire (Text.encodeUtf8 txHash) $ fromIntegral websocketLifetimeSeconds
    return ()

  setWebsocketConnection tVar uuid connection

unWatchTransaciton ::
  (Has AppEnv r, Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m)
  => WebsocketTvar
  -> Text
  -> m ()
unWatchTransaciton tVar uuid = do
  port <- getRedisPort
  host <- getRedisHost
  conn <- liftIO $ Redis.checkedConnect $ Redis.defaultConnectInfo {Redis.connectHost = unpack host, Redis.connectPort = Redis.PortNumber $ fromIntegral port, Redis.connectDatabase = 1}

  liftIO $ Redis.runRedis conn $ do
    _ <- Redis.del [Text.encodeUtf8 uuid]
    return ()


getWatchedTransactions ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadError AppError m)
  => m [(Text, Text)]
getWatchedTransactions = do
  port <- getRedisPort
  host <- getRedisHost
  conn <- liftIO $ Redis.checkedConnect $ Redis.defaultConnectInfo {Redis.connectHost = unpack host, Redis.connectPort = Redis.PortNumber $ fromIntegral port, Redis.connectDatabase = 1}
  keysResult <- liftIO $ Redis.runRedis conn $ Redis.keys "*"
  case keysResult of
    Left err -> do
      -- handle error, maybe throw an AppError
      throwError $ CriticalError $ "Error fetching keys: " <> (pack . show) err
    Right keys -> do
      keyValuePairs <- liftIO $ Redis.runRedis conn $ mapM getKeyValue keys
      let filteredPairs = catMaybes keyValuePairs
      pure filteredPairs


getKeyValue :: ByteString -> Redis.Redis (Maybe (Text, Text))
getKeyValue key = do
  valueResult <- Redis.get key
  return $ case valueResult of
    Left _ -> Nothing
    Right Nothing -> Nothing
    Right (Just value) -> Just (Text.decodeUtf8 key, Text.decodeUtf8 value)

setWebsocketConnection ::
  (MonadReader r m, MonadIO m, MonadError AppError m)
  => WebsocketTvar
  -> Text
  -> Connection
  -> m ()
setWebsocketConnection tvar txHash connection = do
  liftIO do
    currentTime <- getCurrentTime
    atomically $ do
      connections <- readTVar tvar
      writeTVar tvar $ Map.insert txHash (connection, currentTime) connections


getWebsocketConnection ::
  (MonadReader r m, MonadIO m, MonadError AppError m)
  => WebsocketTvar
  -> Text
  -> m (Maybe (Connection, UTCTime))
getWebsocketConnection tvar txHash = do
  connections <- liftIO $ readTVarIO tvar
  return $ Map.lookup txHash connections

removeWebsocketConnection ::
  (MonadReader r m, MonadIO m, MonadError AppError m)
  => WebsocketTvar
  -> Text
  -> Text
  -> m ()
removeWebsocketConnection tvar txHash message = liftIO $ do
  mConn <- atomically $ do
    connections <- readTVar tvar
    writeTVar tvar $ Map.delete txHash connections
    return $ Map.lookup txHash connections

  case mConn of
      Just (conn, _) -> liftIO $ WS.sendClose conn message
      Nothing -> return ()