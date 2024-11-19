{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Control.Exception                      (Exception, SomeException, fromException, throw)
import           Control.Lens.Operators                 ((.~))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader

import           Data.Aeson                             hiding (Error)
import qualified Data.ByteString                        as BS
import           Data.ByteString.Char8                  (unpack)
import qualified Data.Cache                             as Cache
import           Data.Function                          ((&))
import           Data.Has                               (getter)
import           Data.Monoid                            (mempty)
import           Data.OpenApi                           (OpenApi, Server (Server), _openApiServers,
                                                         _serverDescription, _serverUrl, _serverVariables,
                                                         servers)
import           Data.Pool                              (createPool)
import           Data.Proxy
import           Data.String                            (fromString)
import           Data.String.Conversions                (cs)
import qualified Data.Text                              as Text
import           Data.Text.Encoding                     (encodeUtf8)
import qualified Data.Text.IO                           as Text
import qualified Data.Text.Lazy                         as LazyText
import qualified Data.Text.Lazy.Encoding                as LazyText

import           Database.PostgreSQL.Simple             (close, connectPostgreSQL)

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors

import           Options.Applicative                    (execParser)

import           Servant
import           Servant.API.ContentTypes
import           Servant.OpenApi                        (toOpenApi)
import qualified Servant.Server                         as Servant
import           Servant.Swagger.UI                     (SwaggerSchemaUI, swaggerSchemaUIServer)

import           System.Clock                           (TimeSpec (TimeSpec))
import           System.IO                              (stderr)
import           System.Log.Raven                       (initRaven, register, silentFallback)
import           System.Log.Raven.Transport.HttpConduit (sendRecord)
import           System.Log.Raven.Types                 (SentryLevel (Error), SentryRecord (..))
import           System.TimeManager                     (TimeoutThread (..))

import           VVA.API
import           VVA.API.Types
import           VVA.CommandLine
import           VVA.Config
import           VVA.Types                              (AppEnv (..),
                                                         AppError (CriticalError, InternalError, NotFoundError, ValidationError),
                                                         CacheEnv (..))

proxyAPI :: Proxy (VVAApi :<|> SwaggerAPI)
proxyAPI = Proxy

main :: IO ()
main = do
  commandLineConfig <- execParser cmdParser
  vvaConfig <- loadVVAConfig (clcConfigPath commandLineConfig)
  case clcCommand commandLineConfig of
    StartApp   -> startApp vvaConfig
    ShowConfig -> Text.putStrLn $ vvaConfigToText vvaConfig

startApp :: VVAConfig -> IO ()
startApp vvaConfig = do
  let vvaPort = serverPort vvaConfig
      vvaHost = fromString (Text.unpack (serverHost vvaConfig))
      settings =
        setPort vvaPort
          $ setHost vvaHost
          $ setTimeout 120 -- 120 seconds timeout
          $ setBeforeMainLoop
            ( Text.hPutStrLn stderr $
                Text.pack
                  ( "listening on "
                      ++ show vvaHost
                      ++ ":"
                      ++ show vvaPort
                  )
            )
          $ setOnException (exceptionHandler vvaConfig) defaultSettings
  cacheEnv <- do
    let newCache = Cache.newCache (Just $ TimeSpec (fromIntegral (cacheDurationSeconds vvaConfig)) 0)
    proposalListCache <- newCache
    getProposalCache <- newCache
    currentEpochCache <- newCache
    adaHolderVotingPowerCache <- newCache
    adaHolderGetCurrentDelegationCache <- newCache
    dRepGetVotesCache <- newCache
    dRepInfoCache <- newCache
    dRepVotingPowerCache <- newCache
    dRepListCache <- newCache
    networkMetricsCache <- newCache
    return $ CacheEnv
      { proposalListCache
      , getProposalCache
      , currentEpochCache
      , adaHolderVotingPowerCache
      , adaHolderGetCurrentDelegationCache
      , dRepGetVotesCache
      , dRepInfoCache
      , dRepVotingPowerCache
      , dRepListCache
      , networkMetricsCache
      }
  connectionPool <- createPool (connectPostgreSQL (encodeUtf8 (dbSyncConnectionString $ getter vvaConfig))) close 10 10 120
  let appEnv = AppEnv {vvaConfig=vvaConfig, vvaCache=cacheEnv, vvaConnectionPool=connectionPool }
  server' <- mkVVAServer appEnv
  runSettings settings server'

exceptionHandler :: VVAConfig -> Maybe Request -> SomeException -> IO ()
exceptionHandler vvaConfig mRequest exception = do
  print mRequest
  print exception
  let isNotTimeoutThread x = case fromException x of
        Just TimeoutThread -> False
        _                  -> True
      isNotConnectionClosedByPeer x = case fromException x of
        Just ConnectionClosedByPeer -> False
        _                           -> True
  guard . isNotTimeoutThread $ exception
  guard . isNotConnectionClosedByPeer $ exception
  let env = sentryEnv vvaConfig
  sentryService <-
    initRaven
      (sentryDSN vvaConfig)
      id
      sendRecord
      silentFallback
  register
    sentryService
    "vva.be"
    Error
    (formatMessage mRequest exception)
    (recordUpdate env mRequest exception)



formatMessage :: Maybe Request -> SomeException -> String
formatMessage Nothing exception = "Exception before request could be parsed: " ++ show exception
formatMessage (Just request) exception = "Exception " ++ show exception ++ " while handling request " ++ show request

recordUpdate :: String -> Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate env Nothing exception record = record { srEnvironment = Just env }
recordUpdate env (Just request) exception record =
  record
    { srCulprit = Just $ unpack $ rawPathInfo request,
      srServerName = unpack <$> requestHeaderHost request,
      srEnvironment = Just env
    }

shouldDisplayException :: SomeException -> Bool
shouldDisplayException se
  | Just (_ :: InvalidRequest) <- fromException se = True
  | otherwise = defaultShouldDisplayException se

vvaCors :: Middleware
vvaCors = cors (const $ Just vvaCorsResourcePolicy)

vvaCorsResourcePolicy :: CorsResourcePolicy
vvaCorsResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing,
      corsMethods =
        [ "GET",
          "HEAD",
          "POST",
          "OPTIONS"
        ],
      corsRequestHeaders = ["Authorization", "Content-Type"],
      corsExposedHeaders = Nothing,
      corsMaxAge = Nothing,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = False
    }

customFormatter :: ErrorFormatter
customFormatter tr req err =
  let -- aeson Value which will be sent to the client
      value = object ["combinator" .= show tr, "error" .= err]
      -- Accept header of the request
      accH = getAcceptHeader req
   in -- handleAcceptH is Servant's function that checks whether the client can accept a
      -- certain message type.
      -- In this case we call it with "Proxy '[JSON]" argument, meaning that we want to return a JSON.
      case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
        -- If client can't handle JSON, we just return the body the old way
        Nothing -> err400 {errBody = cs err}
        -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
        Just (ctypeH, body) ->
          err400
            { errBody = body,
              errHeaders = [("Content-Type", cs ctypeH)]
            }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req =
  err404 {errBody = cs $ "Not found path: " <> rawPathInfo req}

customFormatters :: ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = customFormatter,
      notFoundErrorFormatter = notFoundFormatter
    }

mkVVAServer ::
  AppEnv ->
  IO Application
mkVVAServer appEnv = do
  return $
    vvaCors
      ( serveWithContext
          proxyAPI
          (customFormatters :. EmptyContext)
          (liftServer appEnv :<|> swagger)
      )

newtype TextException
  = TextException Text.Text

instance Show TextException where
  show (TextException e) = show e

instance Exception TextException

liftServer ::
  AppEnv ->
  ServerT VVAApi Handler
liftServer appEnv =
  hoistServer
    (Proxy @VVAApi)
    (\x -> runReaderT (runExceptT x) appEnv >>= handleErrors)
    server
  where
    handleErrors :: Either AppError a -> Handler a
    handleErrors (Right x) = pure x
    handleErrors (Left (ValidationError msg)) = throwError $ err400 { errBody = BS.fromStrict $ encodeUtf8 msg }
    handleErrors (Left (NotFoundError msg)) = throwError $ err404 { errBody = BS.fromStrict $ encodeUtf8 msg }
    handleErrors (Left (CriticalError msg)) = throwError $ err500 { errBody = BS.fromStrict $ encodeUtf8 msg }
    handleErrors (Left (InternalError msg)) = throwError $ err500 { errBody = BS.fromStrict $ encodeUtf8 msg }
-- * Swagger

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: Servant.Server SwaggerAPI
swagger =
  hoistServer
    (Proxy @SwaggerAPI)
    id
    ( swaggerSchemaUIServer ((toOpenApi (Proxy :: Proxy VVAApi)) {_openApiServers = servers})
    )
  where
    servers =
      [ Server
          { _serverUrl = "/api",
            _serverDescription = Nothing,
            _serverVariables = mempty
          },
        Server
          { _serverUrl = "/",
            _serverDescription = Nothing,
            _serverVariables = mempty
          }
      ]
