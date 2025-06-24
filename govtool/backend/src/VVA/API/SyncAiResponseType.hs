{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}

module VVA.API.SyncAiResponseType where

import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Data.Text (Text)
import qualified Data.Text (stripPrefix)
import           Data.Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key
import           Data.OpenApi (ToSchema)
import           Control.Applicative ((<|>))
import           Data.Scientific

-- Helper: safe lookup for Maybe Value
(.::?) :: Maybe Value -> Text -> Parser (Maybe Value)
(Just (Object o)) .::? k = o .:? Key.fromText k
_                 .::? _ = pure Nothing

newtype TextOrValue = TextOrValue { unTextOrValue :: Text }
  deriving (Show, Eq, Generic)
deriving instance ToSchema TextOrValue

instance FromJSON TextOrValue where
  parseJSON (String t) = pure (TextOrValue t)
  parseJSON (Object o) = do
    v <- o .: "@value"
    pure (TextOrValue v)
  parseJSON v = Aeson.typeMismatch "TextOrValue" v

instance ToJSON TextOrValue where
  toJSON (TextOrValue t) = String t

data SearchAiResponse = SearchAiResponse
  { elements      :: Maybe [DRepData]
  , meta       :: Meta
  , pagination :: Pagination
  } deriving (Show, Generic, ToSchema)

instance FromJSON SearchAiResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fixData }
    where fixData "elements" = "data"
          fixData other   = other
instance ToJSON SearchAiResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = fixData }
    where fixData "elements" = "data"
          fixData other   = other

data DRepData = DRepData
  { deposit                :: Scientific
  , drepId                 :: Text
  , givenName              :: Maybe Text
  , identityRefs           :: [Reference]
  , imageHash              :: Maybe TextOrValue
  , imageUrl               :: Maybe TextOrValue
  , isScriptBased          :: Bool
  -- , latestRegistrationDate :: UTCTime
  , latestRegistrationDate :: Maybe Text
  , latestTxHash           :: Maybe Text
  , linkReferences         :: [Reference]
  , metadataError          :: Maybe Text
  , metadataHash           :: Maybe Text
  , motivations            :: Maybe Text
  , objectives             :: Maybe Text
  , paymentAddress         :: Maybe TextOrValue
  , qualifications         :: Maybe TextOrValue
  -- , status                 :: DRepStatus
  , status                 :: Maybe Text
  -- , type                   :: DRepType
  , type_                  :: Maybe Text
  , url                    :: Maybe Text
  , view                   :: Text
  , votingPower            ::  Maybe Integer -- We might need to multiply this by 1e6 to get the actual voting power + Int
  -- THOSE ARE WEIRD FIELDS, NOT USED FOR NOW
  -- , drepHash :: Text
  -- , dataHash               :: Maybe Text
  } deriving (Show, Generic, ToSchema, ToJSON)

instance FromJSON DRepData where
  parseJSON = withObject "DRepData" $ \v -> do
    -- dRep data:
    motivations    <- v .:  "motivations"
    givenName      <- v .:  "name"
    objectives     <- v .:  "objectives"
    status         <- v .:  "status"
    url            <- v .:? "url"

    -- dRep data manipulation:
    mMeta     <- v .:? "metadata"
    mJsonMeta <- mMeta .::? "json_metadata"
    mBody     <- mJsonMeta .::? "body"

    drepId <- do
      m <- v .:? "metadata"
      m1 <- m .::? "hex"
      mt <- traverse parseJSON m1
      pure (fromMaybe "" mt)

    imageUrl <- do
      m <- v .:? "metadata"
      m1 <- m .::? "json_metadata"
      m2 <- m1 .::? "body"
      m3 <- m2 .::? "image"
      m4 <- m3 .::? "contentUrl"
      traverse parseJSON m4

    imageHash <- do
      m <- v .:? "metadata"
      m1 <- m .::? "json_metadata"
      m2 <- m1 .::? "body"
      m3 <- m2 .::? "imageHash"
      traverse parseJSON m3

    paymentAddress <- do
      m <- v .:? "metadata"
      m1 <- m .::? "json_metadata"
      m2 <- m1 .::? "body"
      m3 <- m2 .::? "paymentAddress"
      traverse parseJSON m3

    qualifications <- do
      m <- v .:? "metadata"
      m1 <- m .::? "json_metadata"
      m2 <- m1 .::? "body"
      m3 <- m2 .::? "qualifications"
      traverse parseJSON m3

    references <-  do
      m <- v .:? "metadata"
      m1 <- m .::? "json_metadata"
      m2 <- m1 .::? "body"
      m3 <- m2 .::? "references"
      traverse parseJSON m3

    -- Split references by @type
    let (linkRefs, identityRefs) = case references of
            Nothing -> ([], [])
            Just refs ->
                ( filter (\ref -> refType ref == Just "Link") refs
                , filter (\ref -> refType ref == Just "Identity") refs
                )

    -- Placeholders for fields not yet implemented
    deposit <- pure 123456789
    latestRegistrationDate <- pure (Just "2023-10-01T00:00:00Z")
    latestTxHash <- pure Nothing
    isScriptBased <- pure False
    metadataError <- pure Nothing
    metadataHash <- pure Nothing
    type_ <- pure (Just "DRep")
    view <- pure "https://example.com/view"
    votingPower <- pure (Just 100)

    pure $ DRepData
      deposit drepId givenName identityRefs imageHash imageUrl isScriptBased latestRegistrationDate
      latestTxHash linkRefs metadataError metadataHash motivations objectives paymentAddress
      qualifications status type_ url view votingPower

data Reference = Reference
  { refType :: Maybe Text -- "@type"
  , label   :: Maybe TextOrValue
  , uri     :: Maybe TextOrValue
  } deriving (Show, Generic, ToSchema)
instance FromJSON Reference where
  parseJSON = withObject "Reference" $ \v -> Reference
    <$> v .: "@type"
    <*> v .: "label"
    <*> v .: "uri"
instance ToJSON Reference where
  toJSON (Reference t l u) = object
    [ "@type" .= t
    , "label" .= l
    , "uri"   .= u
    ]

data Meta = Meta
  { explanation :: Text
  , resultCount :: Int
  } deriving (Show, Generic, ToSchema)
instance FromJSON Meta
instance ToJSON Meta

data Pagination = Pagination
  { page  :: Int
  , pages :: Int
  , limit :: Int
  , total :: Int
  } deriving (Show, Generic, ToSchema)
instance FromJSON Pagination
instance ToJSON Pagination
