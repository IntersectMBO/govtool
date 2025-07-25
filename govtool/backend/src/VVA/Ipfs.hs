{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VVA.Ipfs (ipfsUpload) where

import           Control.Exception              (SomeException, try)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Aeson                     (FromJSON(parseJSON), withObject, (.:), eitherDecode)
import qualified Data.ByteString.Lazy           as LBS
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as TE
import           GHC.Generics                   (Generic)
import           Network.HTTP.Client            (newManager, parseRequest, httpLbs, method, requestHeaders, RequestBody(..), Request, responseBody, responseStatus)
import           Network.HTTP.Client.TLS        (tlsManagerSettings)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partBS, partFileRequestBody)
import           Network.HTTP.Types.Status      (statusIsSuccessful)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as T


data PinataData = PinataData
  { cid    :: Text
  , size     :: Int
  , created_at   :: Text
  , isDuplicate :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON PinataData

data PinataSuccessResponse = PinataSuccessResponse
  { pinataData :: PinataData
  } deriving (Show)

instance FromJSON PinataSuccessResponse where
  parseJSON = withObject "PinataSuccessResponse" $ \v -> PinataSuccessResponse
    <$> v .: "data"

ipfsUpload :: Maybe Text -> Text -> LBS.ByteString -> IO (Either String Text)
ipfsUpload maybeJwt fileName fileContent =
  case maybeJwt of
    Nothing -> pure $ Left "Backend is not configured to support ipfs upload"
    Just "" -> pure $ Left "Backend is not configured to support ipfs upload"
    Just jwt -> do
      manager <- newManager tlsManagerSettings
      initialRequest <- parseRequest "https://uploads.pinata.cloud/v3/files"
      let req = initialRequest
            { method = "POST"
            , requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 jwt)]
            }
      result <- try $ flip httpLbs manager =<< formDataBody
        [ partBS "network" "public"
        , partFileRequestBody "file" (T.unpack fileName) $ RequestBodyLBS fileContent
        ]
        req

      case result of
        Left (e :: SomeException) -> do
          let errMsg = show e
          liftIO $ putStrLn errMsg
          pure $ Left errMsg
        Right response -> do
          let body = responseBody response
          let status = responseStatus response
          if statusIsSuccessful status
            then case eitherDecode body of
              Left err -> do
                let errMsg = "Failed to decode Pinata API reponse: " <> err <> "\nResponse body: " <>   LBS8.unpack body
                liftIO $ putStrLn errMsg
                pure $ Left errMsg
              Right (res :: PinataSuccessResponse) -> pure $ Right $ cid $ pinataData res
            else do
              let errMsg = "Pinata API request failed with status: " <> show status <> "\nResponse body: " <> LBS8.unpack body
              liftIO $ putStrLn errMsg
              pure $ Left errMsg
