{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module VVA.Metadata where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (Exception, try)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader

import           Data.Aeson                 ( Value(..), decode, encode, object, (.=), encode, object, (.=) )
import           Data.Aeson.KeyMap          (lookup)
import           Data.ByteString            (ByteString)
import           Data.FileEmbed             (embedFile)
import           Data.Has                   (Has, getter)
import           Data.List                  (partition)
import           Data.Maybe                 (fromJust)
import           Data.Scientific
import           Data.String                (fromString)
import           Data.Text                  (Text, pack, unpack)
import           Data.Time.Clock
import qualified Data.Text.Encoding         as Text

import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.Redis as Redis

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Prelude                    hiding (lookup)

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import           VVA.Types

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

getVotingAnchorsSql :: SQL.Query
getVotingAnchorsSql = sqlFrom $(embedFile "sql/get-voting-anchors.sql")

getNewVotingAnchors ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m)
    => Integer
    -> m [VotingAnchor]
getNewVotingAnchors lastId = do
    anchors <- withPool $ \conn -> do
        liftIO $ SQL.query conn getVotingAnchorsSql $ SQL.Only (lastId :: Integer)
    return $ map (\(id, url, hash, type') -> VotingAnchor (floor @Scientific id) url hash type') anchors

startFetchProcess ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m)
    => m ()
startFetchProcess = go 0
    where
        go latestKnownId = do
            liftIO $ putStrLn "Fetching metadata..."

            anchors <- getNewVotingAnchors latestKnownId
            if null anchors
                then do
                    liftIO $ threadDelay (20 * 1000000)
                    go latestKnownId
                else do
                    (drepMetadata, proposalMetadata) <- processAnchors anchors
                    storeMetadata drepMetadata
                    storeMetadata proposalMetadata

                    let newId = maximum $ map votingAnchorId anchors

                    liftIO $ putStrLn ("Stored " <> show (length anchors) <> " voting anchors")

                    liftIO $ threadDelay (20 * 1000000)
                    go newId


processAnchors ::
   (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m)
   => [VotingAnchor]
   -> m ( [(Text, MetadataValidationResult DRepMetadata)]
        , [(Text, MetadataValidationResult ProposalMetadata)]
        )
processAnchors anchors = do
    let (drepAnchors, proposalAnchors) = partition ((== "other") . votingAnchorType) anchors
    drepMetadata <- mapM (\(VotingAnchor id url hash _) -> (url<>"#"<>hash, ) <$> getDRepMetadataValidationResult' url hash) drepAnchors
    proposalMetadata <- mapM (\(VotingAnchor id url hash _) -> (url<>"#"<>hash, ) <$> getProposalMetadataValidationResult' url hash) proposalAnchors
    return (drepMetadata, proposalMetadata)

storeMetadata ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m, ToJSON a)
    => [(Text, MetadataValidationResult a)]
    -> m ()
storeMetadata metadataResults = do
    port <- getRedisPort
    host <- getRedisHost
    pass <- fmap Text.encodeUtf8 <$> getRedisPassword
    conn <- liftIO $ Redis.checkedConnect $ Redis.defaultConnectInfo
        { Redis.connectHost = unpack host
        , Redis.connectPort = Redis.PortNumber $ fromIntegral port
        , Redis.connectAuth = pass
        }
    liftIO $ Redis.runRedis conn $ do
        forM metadataResults $ \(reddisId, metadataValidationResult) -> do
                _ <- Redis.set (Text.encodeUtf8 reddisId) (toStrict $ encode metadataValidationResult)
                return ()
    return ()

fetchMetadataValidationResult ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m, FromJSON a)
    => Text
    -> Text
    -> m (Maybe (MetadataValidationResult a))
fetchMetadataValidationResult url hash = do
    conn <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
    result <- liftIO $ Redis.runRedis conn $ Redis.get (Text.encodeUtf8 $ url<>"#"<>hash)
    case result of
        Left _ -> return Nothing
        Right (Just x) -> case decode $ fromStrict x of
            Nothing -> return Nothing
            Just x -> return $ Just x
        Right Nothing -> return Nothing

validateMetadata ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m)
    => Text
    -> Text
    -> Maybe Text
    -> m (Either Text Value)
validateMetadata url hash standard = do
    metadataEnabled <- getMetadataValidationEnabled
    metadataHost <- getMetadataValidationHost
    metadataPort <- getMetadataValidationPort
    manager <- asks getter
    (if metadataEnabled then (do
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
              Just x  -> return $ Right x) else return $ Right "")


getProposalMetadataValidationResult ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
    Text ->
    Text ->
    m (MetadataValidationResult ProposalMetadata)
getProposalMetadataValidationResult url hash = do
    result <- fetchMetadataValidationResult url hash
    case result of
        Just x -> return x
        Nothing -> getProposalMetadataValidationResult' url hash


getProposalMetadataValidationResult' ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
    Text ->
    Text ->
    m (MetadataValidationResult ProposalMetadata)
getProposalMetadataValidationResult' url hash = do
        result <- validateMetadata url hash (Just "CIP108")
        case result of
            Left e -> return $ MetadataValidationResult False (Just e) Nothing
            Right (Object r) -> case go r of
                Nothing -> throwError $ InternalError "Failed to validate metadata"
                Just x  -> return x
            Right "" -> return $ MetadataValidationResult True (Just "200") Nothing
    where
        go result = do
                (Bool valid) <- lookup "valid" result
                let status = case lookup "status" result of
                                Just (String s) -> Just s
                                _               -> Nothing
                (Object m) <- lookup "metadata" result
                let abstract = (\(String s) -> s) <$> lookup "abstract" m
                let motivation = (\(String s) -> s) <$> lookup "motivation" m
                let rationale = (\(String s) -> s) <$> lookup "rationale" m
                let title = (\(String s) -> s) <$> lookup "title" m
                let references = (\(Array references') -> map (\(String x) -> x) $ toList references') <$> lookup "references" m
                let proposalMetadata = ProposalMetadata <$> abstract <*> motivation <*> rationale <*> title <*> references
                return $ MetadataValidationResult valid status proposalMetadata

getDRepMetadataValidationResult ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
    Text ->
    Text ->
    m (MetadataValidationResult DRepMetadata)
getDRepMetadataValidationResult url hash = do
    result <- fetchMetadataValidationResult url hash
    case result of
        Just x -> return x
        Nothing -> getDRepMetadataValidationResult' url hash


getDRepMetadataValidationResult' ::
    (Has ConnectionPool r, Has Manager r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
    Text ->
    Text ->
    m (MetadataValidationResult DRepMetadata)
getDRepMetadataValidationResult' url hash = do
        result <- validateMetadata url hash (Just "CIPQQQ")
        case result of
            Left e -> return $ MetadataValidationResult False (Just e) Nothing
            Right (Object r) -> case go r of
                Nothing -> throwError $ InternalError "Failed to validate metadata"
                Just x  -> return x
            Right "" -> return $ MetadataValidationResult True (Just "200") Nothing
    where
        go result = do
                (Bool valid) <- lookup "valid" result
                let status = case lookup "status" result of
                                Just (String s) -> Just s
                                _               -> Nothing
                (Object m) <- lookup "metadata" result
                let bio = (\(String s) -> s) <$> lookup "bio" m
                let dRepName = (\(String s) -> s) <$> lookup "dRepName" m
                let email = (\(String s) -> s) <$> lookup "email" m
                let references = (\(Array references') -> map (\(String x) -> x) $ toList references') <$> lookup "references" m
                let proposalMetadata = DRepMetadata <$> bio <*> dRepName <*> email <*> references
                return $ MetadataValidationResult valid status proposalMetadata
