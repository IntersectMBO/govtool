{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VVA.DRep where

import           Control.Monad.Except       (MonadError)
import           Control.Monad.Reader
import           Crypto.Hash

import qualified Data.Aeson                 as Aeson
import           Data.Aeson                 (Value)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Char8      as C
import           Data.FileEmbed             (embedFile)
import           Data.Foldable              (Foldable (sum))
import           Data.Has                   (Has)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Scientific
import           Data.String                (fromString)
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text.Encoding         as Text
import           Data.Time
import           Data.Vector                (Vector)
import qualified Data.Vector                as V

import           Debug.Trace                (trace)


import           Numeric.Natural            (Natural)

import qualified Database.PostgreSQL.Simple as SQL
import           Database.PostgreSQL.Simple.Types (PGArray(..))
import           Database.PostgreSQL.Simple.ToField (ToField (..))

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import qualified VVA.Proposal               as Proposal
import           VVA.Types                  (AppError, DRepInfo (..), DRepRegistration (..), DRepStatus (..),
                                             DRepType (..), Proposal (..), Vote (..))
import qualified VVA.Common.Types           as CommonTypes
import qualified VVA.API.Types              as APITypes

sortModeToString :: CommonTypes.DRepSortMode -> Text
sortModeToString CommonTypes.Random           = "Random"
sortModeToString CommonTypes.VotingPower      = "VotingPower"
sortModeToString CommonTypes.RegistrationDate = "RegistrationDate"
sortModeToString CommonTypes.Status           = "Status"

convertDRepStatus :: VVA.Types.DRepStatus -> CommonTypes.DRepStatus
convertDRepStatus VVA.Types.Active   = CommonTypes.Active
convertDRepStatus VVA.Types.Inactive = CommonTypes.Inactive
convertDRepStatus VVA.Types.Retired  = CommonTypes.Retired

convertDRepStatusToDb :: CommonTypes.DRepStatus -> Text
convertDRepStatusToDb CommonTypes.Active   = "Active"
convertDRepStatusToDb CommonTypes.Inactive = "Inactive"
convertDRepStatusToDb CommonTypes.Retired  = "Retired"

drepRegistrationToDrep :: DRepRegistration -> CommonTypes.DRep
drepRegistrationToDrep DRepRegistration {..} =
  CommonTypes.DRep
    { dRepIsScriptBased          = dRepRegistrationIsScriptBased
    , dRepDrepId                 = CommonTypes.DRepHash dRepRegistrationDRepHash
    , dRepView                   = dRepRegistrationView
    , dRepUrl                    = dRepRegistrationUrl
    , dRepMetadataHash           = dRepRegistrationDataHash
    , dRepDeposit                = dRepRegistrationDeposit
    , dRepVotingPower            = dRepRegistrationVotingPower
    , dRepStatus                 = convertDRepStatus dRepRegistrationStatus
    , dRepType                   = case dRepRegistrationType of
                                      VVA.Types.DRep      -> CommonTypes.NormalDRep
                                      VVA.Types.SoleVoter -> CommonTypes.SoleVoter
    , dRepLatestTxHash           = fmap CommonTypes.HexText dRepRegistrationLatestTxHash
    , dRepLatestRegistrationDate = dRepRegistrationLatestRegistrationDate
    , dRepMetadataError          = dRepRegistrationMetadataError
    , dRepPaymentAddress         = dRepRegistrationPaymentAddress
    , dRepGivenName              = dRepRegistrationGivenName
    , dRepObjectives             = dRepRegistrationObjectives
    , dRepMotivations            = dRepRegistrationMotivations
    , dRepQualifications         = dRepRegistrationQualifications
    , dRepImageUrl               = dRepRegistrationImageUrl
    , dRepImageHash              = fmap CommonTypes.HexText dRepRegistrationImageHash
    }

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

listDRepsSql :: SQL.Query
listDRepsSql = sqlFrom $(embedFile "sql/list-dreps.sql")

listDReps ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  Maybe Text -> [CommonTypes.DRepStatus] -> Maybe CommonTypes.DRepSortMode -> Natural -> Natural -> m (Int, [CommonTypes.DRep])
listDReps mSearchQuery statuses order limit offset = withPool $ \conn -> do
  let searchParam = fromMaybe "" mSearchQuery
  let sortMode = fromMaybe CommonTypes.Random order  -- Default to Random
  let statusList :: [Text]
      statusList = if null statuses 
                  then map convertDRepStatusToDb [CommonTypes.Active, CommonTypes.Inactive, CommonTypes.Retired]
                  else map convertDRepStatusToDb statuses

  let emptyStatusCheck :: Text 
      emptyStatusCheck = if null statuses then "" else "not_empty"

  liftIO $ putStrLn "Running listDReps query"
  liftIO $ putStrLn $ "searchParam: " <> show searchParam
  liftIO $ putStrLn $ "statusList: " <> show statusList
  liftIO $ putStrLn $ "emptyStatusCheck: " <> show emptyStatusCheck
  liftIO $ putStrLn $ "sortMode: " <> show sortMode
  liftIO $ putStrLn $ "limit: " <> show limit
  liftIO $ putStrLn $ "offset: " <> show offset

  results <- liftIO $ SQL.query conn listDRepsSql
    ( searchParam -- COALESCE(?, '') (used for search conditions)
    , searchParam -- LENGTH(?) for hex validation
    , searchParam -- Hex validation regex check
    , searchParam -- drep_hash match
    , "%" <> searchParam <> "%" -- Used in `ILIKE ANY(ARRAY[...])`
    , emptyStatusCheck -- Status check flag
    , PGArray statusList -- status = ANY(?)
    , searchParam -- COALESCE(?, '') for SoleVoter filter
    , "%" <> searchParam <> "%" -- view ILIKE ?
    , sortModeToString sortMode -- Combined sorting case
    , sortModeToString sortMode -- Used for `Random` sort
    , (fromIntegral limit :: Integer) -- Limit ?
    , (fromIntegral offset :: Integer) -- Offset ?
    )

  liftIO $ putStrLn "listDReps query complete"

  case results of
    [(totalCountRaw :: Integer, Aeson.Array elements)] -> do
      let totalCount = fromIntegral totalCountRaw :: Int
      timeZone <- liftIO getCurrentTimeZone

      let drepsJSON = mapMaybe (\x -> case Aeson.fromJSON x of
                                  Aeson.Success drep -> Just drep
                                  Aeson.Error err -> trace ("JSON parsing error: " <> err <> " on input: " <> show x) Nothing) 
                          (V.toList elements)

      liftIO $ putStrLn $ "drepsJSON: " <> show drepsJSON

      let dreps = map drepRegistrationToDrep drepsJSON

      liftIO $ putStrLn $ "dreps: " <> show dreps

      return (totalCount, dreps)

    _ -> error "Unexpected result from database query in listDReps"

getVotingPowerSql :: SQL.Query
getVotingPowerSql = sqlFrom $(embedFile "sql/get-voting-power.sql")

getVotingPower ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m) =>
  Text ->
  m Integer
getVotingPower drepId = withPool $ \conn -> do
  result <- liftIO (SQL.query @_ @(SQL.Only Scientific) conn getVotingPowerSql $ SQL.Only drepId)
  case result of
    [SQL.Only votingPower] -> return $ floor votingPower
    []                     -> return 0

getVotesSql :: SQL.Query
getVotesSql = sqlFrom $(embedFile "sql/get-votes.sql")

getVotes ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
  Text ->
  [Text] ->
  m ([Vote], [Proposal])
getVotes drepId selectedProposals = withPool $ \conn -> do
  results <- liftIO $ SQL.query conn getVotesSql (SQL.Only drepId)
  
  if null results
    then return ([], [])
    else do
      let proposalsToSelect = if null selectedProposals
                              then [ govActionId | (_, govActionId, _, _, _, _, _, _, _) <- results]
                              else selectedProposals
      
      allProposals <- mapM (Proposal.getProposals . Just . (:[])) proposalsToSelect
      
      let proposals = concat allProposals

      let proposalMap = M.fromList $ map (\x -> (proposalId x, x)) proposals

      timeZone <- liftIO getCurrentTimeZone

      let votes = 
            [ Vote proposalId' drepId' vote' url' docHash' epochNo' (localTimeToUTC timeZone date') voteTxHash'
            | (proposalId', govActionId', drepId', vote', url', docHash', epochNo', date', voteTxHash') <- results
            , govActionId' `elem` proposalsToSelect
            ]

      return (votes, proposals)

getDRepInfoSql :: SQL.Query
getDRepInfoSql = sqlFrom $(embedFile "sql/get-drep-info.sql")

getDRepInfo
  :: ( Has ConnectionPool r
     , Has VVAConfig r
     , MonadReader r m
     , MonadIO m
     , MonadFail m
     , MonadError AppError m
     )
  => Text
  -> m DRepInfo
getDRepInfo drepId = withPool $ \conn -> do
  result <- liftIO $ SQL.query conn getDRepInfoSql (SQL.Only drepId)
  case result of
    [ ( isScriptBased
      , isRegisteredAsDRep
      , wasRegisteredAsDRep
      , isRegisteredAsSoleVoter
      , wasRegisteredAsSoleVoter
      , deposit
      , url
      , dataHash
      , votingPower
      , drepRegisterTx
      , drepRetireTx
      , soleVoterRegisterTx
      , soleVoterRetireTx
      , paymentAddress
      , givenName
      , objectives
      , motivations
      , qualifications
      , imageUrl
      , imageHash
      )] ->
      return $ DRepInfo
        { dRepInfoIsScriptBased = isScriptBased
        , dRepInfoIsRegisteredAsDRep = fromMaybe False isRegisteredAsDRep
        , dRepInfoWasRegisteredAsDRep = fromMaybe False wasRegisteredAsDRep
        , dRepInfoIsRegisteredAsSoleVoter = fromMaybe False isRegisteredAsSoleVoter
        , dRepInfoWasRegisteredAsSoleVoter = fromMaybe False wasRegisteredAsSoleVoter
        , dRepInfoDeposit = deposit
        , dRepInfoUrl = url
        , dRepInfoDataHash = dataHash
        , dRepInfoVotingPower = votingPower
        , dRepInfoDRepRegisterTx = drepRegisterTx
        , dRepInfoDRepRetireTx = drepRetireTx
        , dRepInfoSoleVoterRegisterTx = soleVoterRegisterTx
        , dRepInfoSoleVoterRetireTx = soleVoterRetireTx
        , dRepInfoPaymentAddress = paymentAddress
        , dRepInfoGivenName = givenName
        , dRepInfoObjectives = objectives
        , dRepInfoMotivations = motivations
        , dRepInfoQualifications = qualifications
        , dRepInfoImageUrl = imageUrl
        , dRepInfoImageHash = imageHash
        }
    [] -> return $ DRepInfo False False False False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    _ -> error "Unexpected result from database query in getDRepInfo"
