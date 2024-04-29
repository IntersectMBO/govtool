{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module      : VVA.Config
--   Description : Configuration interface
--
-- This module provides data types and functions to work with external program
-- configuration.  It provides interface that hides details of the actual
-- configuration backend (Conferer) and ensures that the provided configuration
-- is valid and satisfies required invariants.
module VVA.Config
    ( -- * Data types and basic functions
      VVAConfig (..)
    , loadVVAConfig
      -- * Data type conversions
    , getDbSyncConnectionString
    , getServerHost
    , getServerPort
    , vvaConfigToText
    , getMetadataValidationHost
    , getMetadataValidationPort
    ) where

import           Conferer
import qualified Conferer.Source.Aeson    as JSON
import qualified Conferer.Source.Env      as Env

import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import           Data.ByteString          (ByteString, toStrict)
import           Data.Has                 (Has, getter)
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           GHC.Generics

import           VVA.CommandLine          (CommandLineConfig (..),
                                           clcConfigPath)
-- | PostgreSQL database access information.
data DBConfig
  = DBConfig
      { -- | URL of host running the database
        dBConfigHost     :: Text
        -- | Database name
      , dBConfigDbname   :: Text
        -- | User name
      , dBConfigUser     :: Text
        -- | Database password
      , dBConfigPassword :: Text
        -- | Port
      , dBConfigPort     :: Int
      }
  deriving (FromConfig, Generic, Show)

instance DefaultConfig DBConfig where
  configDef = DBConfig "localhost" "cexplorer" "postgres" "test" 9903

-- | Internal, backend-dependent representation of configuration for DEX.  This
-- data type should not be exported from this module.
data VVAConfigInternal
  = VVAConfigInternal
      { -- | db-sync database access.
        vVAConfigInternalDbsyncconfig           :: DBConfig
        -- | Server port.
      , vVAConfigInternalPort                   :: Int
        -- | Server host.
      , vVAConfigInternalHost                   :: Text
        -- | Request cache duration
      , vVaConfigInternalCacheDurationSeconds   :: Int
        -- | Sentry DSN
      , vVAConfigInternalSentrydsn              :: String
        -- | Metadata validation service host
      , vVAConfigInternalMetadataValidationHost :: Text
        -- | Metadata validation service port
      , vVAConfigInternalMetadataValidationPort :: Int
      }
  deriving (FromConfig, Generic, Show)

instance DefaultConfig VVAConfigInternal where
  configDef =
    VVAConfigInternal
      { vVAConfigInternalDbsyncconfig = configDef,
        vVAConfigInternalPort = 3000,
        vVAConfigInternalHost = "localhost",
        vVaConfigInternalCacheDurationSeconds = 20,
        vVAConfigInternalSentrydsn = "https://username:password@senty.host/id",
        vVAConfigInternalMetadataValidationHost = "localhost",
        vVAConfigInternalMetadataValidationPort = 3001
      }

-- | DEX configuration.
data VVAConfig
  = VVAConfig
      { -- | db-sync database credentials.
        dbSyncConnectionString :: Text
        -- | Server port.
      , serverPort             :: Int
        -- | Server host.
      , serverHost             :: Text
        -- | Request cache duration
      , cacheDurationSeconds   :: Int
        -- | Sentry DSN
      , sentryDSN              :: String
        -- | Metadata validation service host
      , metadataValidationHost :: Text
        -- | Metadata validation service port
      , metadataValidationPort :: Int
      }
  deriving (Generic, Show, ToJSON)

-- | Convert 'DBConfig' to a string required by PostgreSQL backend.
dbConfigToText :: DBConfig -> Text
dbConfigToText (DBConfig host dbname user password port) =
  Text.concat
    [ "host=",
      host,
      " ",
      "dbname=",
      dbname,
      " ",
      "user=",
      user,
      " ",
      "password=",
      password,
      " ",
      "port=",
      Text.pack $ show port
    ]

-- | Pretty-print 'DexConfig' as JSON.
vvaConfigToText :: VVAConfig -> Text
vvaConfigToText = decodeUtf8 . toStrict . AP.encodePretty

-- | Convert interal representation of configuration used by Conferer into a
-- representation used by application.  This function validates basic
-- correctness of configuration, throwing an error in case of failure.  In
-- particular 'convertConfig' ensures that when Testnet is specified as network
-- it also contains the required magic number.
convertConfig :: VVAConfigInternal -> VVAConfig
convertConfig VVAConfigInternal {..} =
  VVAConfig
    { dbSyncConnectionString = dbConfigToText vVAConfigInternalDbsyncconfig,
      serverPort = vVAConfigInternalPort,
      serverHost = vVAConfigInternalHost,
      cacheDurationSeconds = vVaConfigInternalCacheDurationSeconds,
      sentryDSN = vVAConfigInternalSentrydsn,
      metadataValidationHost = vVAConfigInternalMetadataValidationHost,
      metadataValidationPort = vVAConfigInternalMetadataValidationPort
    }

-- | Load configuration from a file specified on the command line.  Load from
-- 'example-config.json' if no configuration file is specified.  Settings in the
-- file can be overridden using environment variables prefixed with 'VVA_',
-- e.g. defining env variable 'VVA_NETWORK="Mainnet"' will override "Testnet"
-- setting from default configuration file.
loadVVAConfig :: Maybe FilePath -> IO VVAConfig
loadVVAConfig configFile = do
  config <- buildConfig
  convertConfig <$> fetch config
  where
    buildConfig :: IO Config
    buildConfig =
      Conferer.mkConfig'
        []
        [ Env.fromConfig "vva",
          JSON.fromFilePath (fromMaybe "example-config.json" configFile)
        ]

-- | Access db-sync database connection string component of the configuration.
getDbSyncConnectionString ::
  (Has VVAConfig r, MonadReader r m) =>
  m ByteString
getDbSyncConnectionString = asks (encodeUtf8 . dbSyncConnectionString . getter)

-- | Access server port.
getServerPort ::
  (Has VVAConfig r, MonadReader r m) =>
  m Int
getServerPort = asks (serverPort . getter)

-- | Access server host.
getServerHost ::
  (Has VVAConfig r, MonadReader r m) =>
  m Text
getServerHost = asks (serverHost . getter)

-- | Access MetadataValidationService host
getMetadataValidationHost ::
  (Has VVAConfig r, MonadReader r m) =>
  m Text
getMetadataValidationHost = asks (metadataValidationHost . getter)

-- | Access MetadataValidationService port
getMetadataValidationPort ::
  (Has VVAConfig r, MonadReader r m) =>
  m Int
getMetadataValidationPort = asks (metadataValidationPort . getter)
