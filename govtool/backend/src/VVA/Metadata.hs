{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VVA.Metadata where

import Prelude hiding (lookup)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader
import           Control.Exception          (try, Exception)

import           Data.Typeable              (Typeable)
import           Data.Vector                (toList)
import           Data.Aeson.KeyMap          (lookup)
import           Data.Aeson                 (Value(..), decode, encode, object, (.=))
import           Data.Maybe                 (fromJust)
import           Data.ByteString            (ByteString)
import           Data.FileEmbed             (embedFile)
import           Data.Has                   (Has, getter)
import           Data.String                (fromString)
import           Data.Text                  (Text, unpack, pack)
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
    -> Maybe Text
    -> m (Either Text Value)
validateMetadata url hash standard = do
    metadataHost <- getMetadataValidationHost
    metadataPort <- getMetadataValidationPort
    manager <- asks getter
    let requestBody = encode $ object (["url" .= unpack url, "hash" .= unpack hash] ++ maybe [] (\x -> ["standard" .= unpack x]) standard)
    initialRequest <- liftIO $ parseRequest (unpack metadataHost <> ":" <> show metadataPort <> "/validate")
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS requestBody
            , requestHeaders = [("Content-Type", "application/json")]
            }
    response <- liftIO $ try $ httpLbs request manager
    case response of
        Left (e :: HttpException) -> return $ Left (pack $ show e)
        Right r -> case decode $ responseBody r of
            Nothing -> throwError $ InternalError "Failed to validate metadata"
            Just x -> return $ Right x

getProposalMetadataValidationResult ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
    Text ->
    Text ->
    m (MetadataValidationResult ProposalMetadata)
getProposalMetadataValidationResult url hash = do
        result <- validateMetadata url hash (Just "CIP108")
        case result of
            Left e -> return $ MetadataValidationResult False (Just e) Nothing
            Right (Object r) -> case go r of
                Nothing -> throwError $ InternalError "Failed to validate metadata"
                Just x -> return x
    where
        go result = do
                (Bool valid) <- lookup "valid" result
                let status = case lookup "status" result of
                                Just (String s) -> Just s
                                _ -> Nothing
                let proposalMetadata = do
                    (Object m) <- lookup "metadata" result
                    let abstract = (\(String s) -> s) <$> lookup "abstract" m
                    let motivation = (\(String s) -> s) <$> lookup "motivation" m
                    let rationale = (\(String s) -> s) <$> lookup "rationale" m
                    let title = (\(String s) -> s) <$> lookup "title" m
                    let references = (\(Array references') -> map (\(String x) -> x) $ toList references') <$> lookup "references" m
                    ProposalMetadata <$> abstract <*> motivation <*> rationale <*> title <*> references
                return $ MetadataValidationResult valid status proposalMetadata



getDRepMetadataValidationResult ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
    Text ->
    Text ->
    m (MetadataValidationResult DRepMetadata)
getDRepMetadataValidationResult url hash = do
        result <- validateMetadata url hash (Just "CIPQQQ")
        case result of
            Left e -> return $ MetadataValidationResult False (Just e) Nothing
            Right (Object r) -> case go r of
                Nothing -> throwError $ InternalError "Failed to validate metadata"
                Just x -> return x
    where
        go result = do
                (Bool valid) <- lookup "valid" result
                let status = case lookup "status" result of
                                Just (String s) -> Just s
                                _ -> Nothing
                let proposalMetadata = do
                    (Object m) <- lookup "metadata" result
                    let bio = (\(String s) -> s) <$> lookup "bio" m
                    let dRepName = (\(String s) -> s) <$> lookup "dRepName" m
                    let email = (\(String s) -> s) <$> lookup "email" m
                    let references = (\(Array references') -> map (\(String x) -> x) $ toList references') <$> lookup "references" m
                    DRepMetadata <$> bio <*> dRepName <*> email <*> references
                return $ MetadataValidationResult valid status proposalMetadata