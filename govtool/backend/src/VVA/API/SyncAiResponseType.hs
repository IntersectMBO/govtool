{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}

module VVA.API.SyncAiResponseType where

import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text (stripPrefix)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key
import Data.OpenApi (ToSchema)
import Control.Applicative ((<|>))

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
  { data_      :: Maybe [DRepData]
  , meta       :: Meta
  , pagination :: Pagination
  } deriving (Show, Generic, ToSchema)

instance FromJSON SearchAiResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fixData }
    where fixData "data_" = "data"
          fixData other   = other
instance ToJSON SearchAiResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = fixData }
    where fixData "data_" = "data"
          fixData other   = other

data DRepData = DRepData
  { drepId             :: Text
  -- Missing:
  -- , dRepHash               :: Text
  -- , view                   :: Text
  -- , isScriptBased          :: Bool
  , url                :: Maybe Text
  -- Missing:
  -- , dataHash               :: Maybe Text
  -- , deposit                :: Integer
  -- , votingPower        :: Double -- We might need to multiply this by 1e6 to get the actual voting power + Int
  , status             :: Text
  -- Missing:
  -- , type                   :: DRepType
  -- , latestTxHash           :: Maybe Text
  -- , latestRegistrationDate :: UTCTime
  -- , metadataError          :: Maybe Text
  , paymentAddress     :: Maybe TextOrValue
  , givenName          :: Maybe Text
  , objectives         :: Maybe Text
  , motivations        :: Maybe Text
  , qualifications     :: Maybe TextOrValue
  , imageUrl           :: Maybe TextOrValue
  , imageHash          :: Maybe TextOrValue
  , linkReferences     :: [Reference]
  , identityReferences :: [Reference]
  } deriving (Show, Generic, ToSchema, ToJSON)

instance FromJSON DRepData where
  parseJSON = withObject "DRepData" $ \v -> do
    -- dRep data:
    motivations    <- v .:  "motivations"
    givenName      <- v .:  "name"
    objectives     <- v .:  "objectives"
    status         <- v .:  "status"
    url            <- v .:? "url"
    -- votingPower    <- (v .: "votingPower" <|> (v .: "metrics" >>= (.: "votingPower")))

    --
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

    pure $ DRepData
      drepId motivations givenName objectives status url -- votingPower
      imageUrl imageHash paymentAddress qualifications linkRefs identityRefs

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
