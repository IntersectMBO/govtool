{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}


module VVA.Common.Types where

import           Control.Exception          (throw)
import           Control.Lens               ((.~), (?~))
import           Control.Monad              (guard)
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.KeyMap          as Aeson (toList)
import           Data.Aeson.TH              (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Cache                 as Cache
import           Data.Char                  (isHexDigit)
import           Data.Function              ((&))
import           Data.Has                   (Has, getter, modifier)
import           Data.Hashable              (Hashable)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.OpenApi               hiding (Info)
import           Data.Pool                  (Pool)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Swagger.Internal      (SwaggerType (SwaggerString))
import           Data.Text                  hiding (map)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy.Encoding    as Text
import           Data.Time

import           Database.PostgreSQL.Simple (Connection)

import           GHC.Exts                   (toList)
import           GHC.Generics

import           Servant.API                (FromHttpApiData, parseQueryParam, parseUrlPiece)

import           Text.Read                  (readMaybe)

import           VVA.API.Utils
import           VVA.API.Utils


newtype HexText
  = HexText { unHexText :: Text }
  deriving newtype (Eq, Show)

instance FromJSON HexText where
  parseJSON (Aeson.String t) = do
    if Text.length t `mod` 2 == 1 || Text.any (not . isHexDigit) t
      then mzero
      else pure $ HexText t

instance ToJSON HexText where
  toJSON (HexText t) = Aeson.String t

-- To use it in routes, we need to be able to parse it from Text:
instance FromHttpApiData HexText where
  parseUrlPiece txt
    | Text.all isHexDigit txt && even (Text.length txt) = Right (HexText txt)
    | otherwise            = Left "Not a valid hex value"


instance ToParamSchema HexText where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString
    & format ?~ "hex"

instance ToSchema HexText where
  declareNamedSchema _ = do
    textSchema <- declareNamedSchema (Proxy :: Proxy Text)
    return $ textSchema
      & name ?~ "HexText"
      & schema . type_ ?~ OpenApiString
      & schema . format ?~ "hex"
      & schema . example ?~ toJSON (HexText "a1b2c3")

newtype DRepHash
  = DRepHash Text
  deriving (Generic, Show)

instance FromJSON DRepHash where
  parseJSON (Aeson.String s) = pure $ DRepHash s
  parseJSON x                = fail ("expected DRepHash to be a string but got: " <> Char8.unpack (encode x))

instance ToJSON DRepHash where
  toJSON (DRepHash raw) = toJSON raw

exampleDrepHash :: Text
exampleDrepHash = "b4e4184bfedf920fec53cdc327de4da661ae427784c0ccca9e3c2f50"

instance ToSchema DRepHash where
    declareNamedSchema _ = pure $ NamedSchema (Just "DRepHash") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Hash of a DRep"
        & example
          ?~ toJSON exampleDrepHash

data DRepStatus = Active | Inactive | Retired deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- ToJSON instance for DRepStatus
instance ToJSON DRepStatus where
  toJSON Retired  = "Retired"
  toJSON Active   = "Active"
  toJSON Inactive = "Inactive"

-- FromJSON instance for DRepStatus
instance FromJSON DRepStatus where
  parseJSON = withText "DRepStatus" $ \case
    "Retired"  -> pure Retired
    "Active"   -> pure Active
    "Inactive" -> pure Inactive
    _          -> fail "Invalid DRepStatus"

-- ToSchema instance for DRepStatus
instance ToSchema DRepStatus where
    declareNamedSchema _ = pure $ NamedSchema (Just "DRepStatus") $ mempty
        & type_ ?~ OpenApiString
        & description ?~ "DRep Status"
        & enum_ ?~ map toJSON [Retired, Active, Inactive]

instance FromHttpApiData DRepStatus where
  parseQueryParam t = case readMaybe $ Text.unpack t of
    Just x  -> Right x
    Nothing -> Left ("incorrect DRep status " <> t)

instance ToParamSchema DRepStatus where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ map toJSON (enumFromTo minBound maxBound :: [DRepStatus])

data DRepType = NormalDRep | SoleVoter

instance Show DRepType where
  show NormalDRep = "DRep"
  show SoleVoter  = "SoleVoter"

-- ToJSON instance for DRepType
instance ToJSON DRepType where
  toJSON NormalDRep = "DRep"
  toJSON SoleVoter  = "SoleVoter"

-- FromJSON instance for DRepType
instance FromJSON DRepType where
  parseJSON = withText "DRepType" $ \case
    "DRep"      -> pure NormalDRep
    "SoleVoter" -> pure SoleVoter
    _           -> fail "Invalid DRepType"

-- ToSchema instance for DRepType
instance ToSchema DRepType where
    declareNamedSchema _ = pure $ NamedSchema (Just "DRepType") $ mempty
        & type_ ?~ OpenApiString
        & description ?~ "DRep Type"
        & enum_ ?~ map toJSON [NormalDRep, SoleVoter]


data DRepSortMode = Random | VotingPower | RegistrationDate | Status deriving (Bounded, Enum, Eq, Generic, Read, Show)

instance FromJSON DRepSortMode where
  parseJSON (Aeson.String dRepSortMode) = pure $ fromJust $ readMaybe (Text.unpack dRepSortMode)
  parseJSON _                           = fail ""

instance ToJSON DRepSortMode where
  toJSON x = Aeson.String $ Text.pack $ show x

instance ToSchema DRepSortMode where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <- genericDeclareNamedSchema (fromAesonOptions defaultOptions) proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "DRep Sort Mode"
          & example ?~ toJSON VotingPower

instance FromHttpApiData DRepSortMode where
  parseQueryParam t = case readMaybe $ Text.unpack t of
    Just x  -> Right x
    Nothing -> Left ("incorrect DRep sort mode: " <> t)

instance ToParamSchema DRepSortMode where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ map toJSON (enumFromTo minBound maxBound :: [DRepSortMode])


data DRep
  = DRep
      { dRepIsScriptBased          :: Bool
      , dRepDrepId                 :: DRepHash
      , dRepView                   :: Text
      , dRepUrl                    :: Maybe Text
      , dRepMetadataHash           :: Maybe Text
      , dRepDeposit                :: Integer
      , dRepVotingPower            :: Maybe Integer
      , dRepStatus                 :: DRepStatus
      , dRepType                   :: DRepType
      , dRepLatestTxHash           :: Maybe HexText
      , dRepLatestRegistrationDate :: UTCTime
      , dRepMetadataError          :: Maybe Text
      , dRepPaymentAddress         :: Maybe Text
      , dRepGivenName              :: Maybe Text
      , dRepObjectives             :: Maybe Text
      , dRepMotivations            :: Maybe Text
      , dRepQualifications         :: Maybe Text
      , dRepImageUrl               :: Maybe Text
      , dRepImageHash              :: Maybe HexText
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "dRep") ''DRep

instance ToSchema DRep where
    declareNamedSchema proxy = do
      NamedSchema name_ schema_ <-
        genericDeclareNamedSchema
        ( fromAesonOptions $ jsonOptions "dRep" )
        proxy
      return $
        NamedSchema name_ $
          schema_
            & description ?~ "DRep"
