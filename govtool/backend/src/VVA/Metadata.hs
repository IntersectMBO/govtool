{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module VVA.Metadata where

import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader

import           Data.Aeson                 (Value, decode, encode, object, (.=))
import           Data.Maybe                 (fromJust)
import           Data.ByteString            (ByteString)
import           Data.FileEmbed             (embedFile)
import           Data.Has                   (Has, getter)
import           Data.String                (fromString)
import           Data.Text                  (Text, unpack)
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock

import qualified Database.PostgreSQL.Simple as SQL

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson (encode, object, (.=))

validateMetadata
    :: (Has VVAConfig r, Has Manager r, MonadReader r m, MonadIO m, MonadError AppError m)
    => Text
    -> Text
    -> m Value
validateMetadata url hash = do
    metadataHost <- getMetadataValidationHost
    metadataPort <- getMetadataValidationPort
    manager <- asks getter
    let requestBody = encode $ object ["url" .= unpack url, "hash" .= unpack hash]
    initialRequest <- liftIO $ parseRequest (unpack metadataHost <> ":" <> show metadataPort <> "/validate")
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS requestBody
            , requestHeaders = [("Content-Type", "application/json")]
            }
    response <- liftIO $ httpLbs request manager
    return $ fromJust $ decode $ responseBody response
