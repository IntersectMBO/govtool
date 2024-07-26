{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VVA.Metadata where

import           Control.Exception          (SomeException, Exception, try)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader

import           Data.Aeson                 (Value (..), decode, encode, object, (.=))
import           Data.Aeson.KeyMap          (KeyMap, lookup)
import           Data.ByteString            (ByteString)
import           Data.FileEmbed             (embedFile)
import           Data.Has                   (Has, getter)
import           Data.Maybe                 (fromJust)
import           Data.String                (fromString)
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock
import           Data.Typeable              (Typeable)
import           Data.Vector                (toList)

import qualified Database.PostgreSQL.Simple as SQL

import           Network.HTTP.Client        (httpLbs, parseRequest, Request(..), RequestBody(..), Response, Manager, newManager, managerResponseTimeout, responseTimeoutMicro, defaultManagerSettings, responseBody)
import           Network.HTTP.Client.TLS

import           Prelude                    hiding (lookup)

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types


logException :: (MonadIO m) => Text -> SomeException -> m ()
logException url ex = liftIO $ putStrLn $ "Failed to validate metadata for URL: " ++ unpack url ++ " with error: " ++ show ex

validateMetadata
    :: (Has VVAConfig r, Has Manager r, MonadReader r m, MonadIO m, MonadError AppError m)
    => Text
    -> Text
    -> Maybe Text
    -> m (MetadataValidationResult Value)
validateMetadata url hash standard = do
    metadataEnabled <- getMetadataValidationEnabled
    if not metadataEnabled
      then return $ MetadataValidationResult True (Just "Metadata validation disabled") Nothing
      else do
        metadataHost <- getMetadataValidationHost
        metadataPort <- getMetadataValidationPort

        let timeout = responseTimeoutMicro 1000
        manager <- liftIO $ newManager $ tlsManagerSettings { managerResponseTimeout = timeout }

        let requestBody = encode $ object $
                ["url" .= url, "hash" .= hash] ++ maybe [] (\x -> ["standard" .= x]) standard
            requestUrl = unpack metadataHost ++ ":" ++ show metadataPort ++ "/validate"

        parsedRequestResult <- liftIO $ try $ parseRequest requestUrl
        case parsedRequestResult of
            Left (e :: SomeException) -> do
                logException url e
                return $ MetadataValidationResult False (Just "VALIDATION_FAILED") Nothing
            Right initialRequest -> do
                let request = initialRequest
                        { method = "POST"
                        , requestBody = RequestBodyLBS requestBody
                        , requestHeaders = [("Content-Type", "application/json")]
                        }

                responseResult <- liftIO $ try $ httpLbs request manager
                case responseResult of
                    Left (e :: SomeException) -> do
                        logException url e
                        return $ MetadataValidationResult False (Just "VALIDATION_FAILED") Nothing
                    Right response -> case decode (responseBody response) of
                        Nothing -> return $ MetadataValidationResult False (Just "VALIDATION_FAILED") Nothing
                        Just x  -> return $ MetadataValidationResult True (Just "VALIDATION_SUCCESS") (Just x)

getProposalMetadataValidationResult ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
    Text ->
    Text ->
    m (MetadataValidationResult ProposalMetadata)
getProposalMetadataValidationResult url hash = do
        result <- validateMetadata url hash (Just "CIP108")
        case result of
            MetadataValidationResult False status _ -> return $ MetadataValidationResult False status Nothing
            MetadataValidationResult True status (Just (Object r)) -> case go r of
                Nothing -> throwError $ InternalError "Failed to validate metadata"
                Just x  -> return $ MetadataValidationResult True status (Just x)
            _ -> return $ MetadataValidationResult False (Just "VALIDATION_FAILED") Nothing
    where
        go :: KeyMap Value -> Maybe ProposalMetadata
        go result = do
            (Bool valid) <- lookup "valid" result
            (Object metadata) <- lookup "metadata" result
            (String abstract) <- lookup "abstract" metadata
            (String motivation) <- lookup "motivation" metadata
            (String rationale) <- lookup "rationale" metadata
            (String title) <- lookup "title" metadata
            (Array references') <- lookup "references" metadata
            let references = map (\(String x) -> x) (toList references')
            return $ ProposalMetadata abstract motivation rationale title references

getDRepMetadataValidationResult ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
    Text ->
    Text ->
    m (MetadataValidationResult DRepMetadata)
getDRepMetadataValidationResult url hash = do
        result <- validateMetadata url hash (Just "CIPQQQ")
        case result of
            MetadataValidationResult False status _ -> return $ MetadataValidationResult False status Nothing
            MetadataValidationResult True status (Just (Object r)) -> case go r of
                Nothing -> throwError $ InternalError "Failed to validate metadata"
                Just x  -> return $ MetadataValidationResult True status (Just x)
            _ -> return $ MetadataValidationResult False (Just "VALIDATION_FAILED") Nothing
    where
        go :: KeyMap Value -> Maybe DRepMetadata
        go result = do
            (Bool valid) <- lookup "valid" result
            (Object metadata) <- lookup "metadata" result
            (String bio) <- lookup "bio" metadata
            (String dRepName) <- lookup "dRepName" metadata
            (String email) <- lookup "email" metadata
            (Array references') <- lookup "references" metadata
            let references = map (\(String x) -> x) (toList references')
            return $ DRepMetadata bio dRepName email references