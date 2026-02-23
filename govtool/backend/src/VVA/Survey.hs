{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module VVA.Survey where

import           Control.Monad.Reader

import           Data.Aeson               (Value, object, (.=))
import           Data.ByteString          (ByteString)
import           Data.FileEmbed           (embedFile)
import           Data.Has                 (Has)
import           Data.String              (fromString)
import           Data.Text                (Text, unpack)
import qualified Data.Text.Encoding       as Text

import qualified Database.PostgreSQL.Simple as SQL
import           VVA.Config
import           VVA.Pool                 (ConnectionPool, withPool)

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

getProposalSurveySql :: SQL.Query
getProposalSurveySql = sqlFrom $(embedFile "sql/get-proposal-survey.sql")

getProposalSurveyTallySql :: SQL.Query
getProposalSurveyTallySql = sqlFrom $(embedFile "sql/get-proposal-survey-tally.sql")

emptySurveyPayload :: Value
emptySurveyPayload =
  object
    [ "linked" .= False
    , "actionLifecycle" .= object ["startSlot" .= (0 :: Integer), "endSlot" .= (0 :: Integer)]
    , "surveyRef" .= (Nothing :: Maybe Value)
    , "computedSurveyHash" .= (Nothing :: Maybe Text)
    , "linkValidation" .= object ["valid" .= False, "errors" .= ["No survey link found for this proposal."]]
    , "surveyDetails" .= (Nothing :: Maybe Value)
    , "surveyDetailsValidation" .= object ["valid" .= False, "errors" .= ([] :: [Text])]
    ]

emptySurveyTallyPayload :: Text -> Value
emptySurveyTallyPayload weighting =
  object
    [ "surveyTxId" .= (Nothing :: Maybe Text)
    , "surveyHash" .= (Nothing :: Maybe Text)
    , "weightingMode" .= weighting
    , "totals" .= object
        [ "totalSeen" .= (0 :: Integer)
        , "valid" .= (0 :: Integer)
        , "invalid" .= (0 :: Integer)
        , "deduped" .= (0 :: Integer)
        , "uniqueResponders" .= (0 :: Integer)
        ]
    , "methodResults" .= ([] :: [Value])
    , "errors" .= ["No survey tally available for this proposal."]
    ]

getProposalSurvey ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  Text ->
  Integer ->
  m Value
getProposalSurvey txHash index = withPool $ \conn -> do
  result <- liftIO $
    SQL.query conn getProposalSurveySql (txHash, index) :: IO [SQL.Only Value]
  case result of
    [SQL.Only payload] -> pure payload
    _                  -> pure emptySurveyPayload

getProposalSurveyTally ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  Text ->
  Integer ->
  Text ->
  m Value
getProposalSurveyTally txHash index weighting = withPool $ \conn -> do
  result <- liftIO $
    SQL.query conn getProposalSurveyTallySql (txHash, index, weighting, weighting) :: IO [SQL.Only Value]
  case result of
    [SQL.Only payload] -> pure payload
    _                  -> pure (emptySurveyTallyPayload weighting)
