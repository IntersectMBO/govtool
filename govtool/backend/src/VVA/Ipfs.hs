{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VVA.Ipfs (ipfsUpload, IpfsError(..)) where

import           Control.Exception              (SomeException, try)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Aeson                     as A
import           Data.Aeson                     (FromJSON(parseJSON), withObject, (.:), eitherDecode, ToJSON(..), encode,(.=),object)
import qualified Data.ByteString.Lazy           as LBS
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as TE
import           GHC.Generics                   (Generic)
import           Network.HTTP.Client            (newManager, parseRequest, httpLbs, method, requestHeaders, RequestBody(..), Request, responseBody, responseStatus)
import           Network.HTTP.Client.TLS        (tlsManagerSettings)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partBS, partFileRequestBody)
import           Network.HTTP.Types.Status      (statusIsSuccessful, Status, status503, status400)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as T
import           Servant.Server                 (ServerError (errBody))
import           Servant.Exception              (ToServantErr(..), Exception(..))


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

data IpfsError
  = PinataConnectionError String
  | PinataAPIError Status LBS.ByteString
  | PinataDecodingError String LBS.ByteString
  | IpfsUnconfiguredError 
  | OtherIpfsError String
  deriving (Show, Generic)

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
  status _ = status503

  message (PinataConnectionError msg) = T.pack ("Pinata service connection error: " <> msg)
  message (PinataAPIError status body) = T.pack ("Pinata API error: " <> show status <> " - " <> LBS8.unpack body)
  message (PinataDecodingError msg body) = T.pack ("Pinata decoding error: " <> msg <> " - " <> LBS8.unpack body)
  message IpfsUnconfiguredError = T.pack ("Backend is not configured to support ipfs upload")
  message (OtherIpfsError msg) = T.pack msg

ipfsUpload :: Maybe Text -> Text -> LBS.ByteString -> IO (Either IpfsError Text)
ipfsUpload maybeJwt fileName fileContent =
  case maybeJwt of
    Nothing -> pure $ Left $ IpfsUnconfiguredError
    Just "" -> pure $ Left $ IpfsUnconfiguredError 
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
