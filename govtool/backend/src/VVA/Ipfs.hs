{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VVA.Ipfs
    ( IpfsError (..)
    , ipfsUpload
    ) where

import           Control.Exception                     (SomeException, try)
import           Control.Monad.IO.Class                (liftIO)

import           Data.Aeson                            (FromJSON (parseJSON), ToJSON (..), eitherDecode,
                                                        encode, object, withObject, (.:), (.=))
import qualified Data.Aeson                            as A
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.ByteString.Lazy.Char8            as LBS8
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import qualified Data.Text.Lazy                        as TL
import qualified Data.Text.Lazy.Encoding               as TL

import           GHC.Generics                          (Generic)

import           Network.HTTP.Client                   (Request, RequestBody (..), httpLbs, method,
                                                        newManager, parseRequest, requestHeaders,
                                                        responseBody, responseStatus)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partBS, partFileRequestBody)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Network.HTTP.Types.Status             (Status, status400, status503, statusIsSuccessful)

import           Servant.Exception                     (Exception (..), ToServantErr (..))
import           Servant.Server                        (ServerError (errBody))


data PinataData
  = PinataData
      { cid         :: Text
      , size        :: Int
      , created_at  :: Text
      , isDuplicate :: Maybe Bool
      }
  deriving (Generic, Show)

instance FromJSON PinataData

newtype PinataSuccessResponse
  = PinataSuccessResponse { pinataData :: PinataData }
  deriving (Show)

instance FromJSON PinataSuccessResponse where
  parseJSON = withObject "PinataSuccessResponse" $ \v -> PinataSuccessResponse
    <$> v .: "data"

data IpfsError
  = PinataConnectionError String
  | PinataAPIError Status LBS.ByteString
  | PinataDecodingError String LBS.ByteString
  | IpfsUnconfiguredError
  | OtherIpfsError String
  deriving (Generic, Show)

instance ToJSON IpfsError where
  toJSON (PinataConnectionError msg) =
    object ["errorType" .= A.String "PinataConnectionError", "message" .= msg]

  toJSON (PinataAPIError status body) =
    object
      [ "errorType" .= A.String "PinataAPIError"
      , "message" .= ("Pinata API returned error status : " ++ show status)
      , "pinataResponse" .= object
          [ "status" .= show status
          , "body" .= TL.unpack (TL.decodeUtf8 body)
          ]
      ]

  toJSON (PinataDecodingError msg body) =
    object
      [ "errorType" .= A.String "PinataDecodingError"
      , "message" .= msg
      , "pinataResponse" .= object
          [ "status" .= ("unknown" :: String)
          , "body" .= TL.unpack (TL.decodeUtf8 body)
          ]
      ]

  toJSON IpfsUnconfiguredError =
    object ["errorType" .= A.String "IpfsUnconfiguredError", "message" .= ("Backend is not configured for upfs upload" :: String)]

  toJSON (OtherIpfsError msg) =
    object ["errorType" .= A.String "OtherIpfsError", "message" .= msg]


instance Exception IpfsError



instance ToServantErr IpfsError where
  status (OtherIpfsError _) = status400
  status _                  = status503

  message (PinataConnectionError msg) = T.pack ("Pinata service connection error: " <> msg)
  message (PinataAPIError status body) = T.pack ("Pinata API error: " <> show status <> " - " <> LBS8.unpack body)
  message (PinataDecodingError msg body) = T.pack ("Pinata decoding error: " <> msg <> " - " <> LBS8.unpack body)
  message IpfsUnconfiguredError = T.pack "Backend is not configured to support ipfs upload"
  message (OtherIpfsError msg) = T.pack msg

ipfsUpload :: Maybe Text -> Text -> LBS.ByteString -> IO (Either IpfsError Text)
ipfsUpload maybeJwt fileName fileContent =
  case maybeJwt of
    Nothing -> pure $ Left IpfsUnconfiguredError
    Just "" -> pure $ Left IpfsUnconfiguredError
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
          pure $ Left $ PinataConnectionError errMsg
        Right response -> do
          let body = responseBody response
          let status = responseStatus response
          if statusIsSuccessful status
            then case eitherDecode body of
              Left err -> do
                let errMsg = "Failed to decode Pinata API reponse: " <> err
                liftIO $ putStrLn errMsg
                pure $ Left $ PinataDecodingError errMsg body
              Right (res :: PinataSuccessResponse) -> pure $ Right $ cid $ pinataData res
            else do
              let errMsg = "Pinata API request failed with status: " <> show status
              liftIO $ putStrLn errMsg
              pure $ Left $ PinataAPIError status body
