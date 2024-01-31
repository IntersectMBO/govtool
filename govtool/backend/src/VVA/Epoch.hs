{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module VVA.Epoch where


import Data.ByteString (ByteString)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader
import Data.FileEmbed (embedFile)
import Data.Text (Text, unpack)
import Data.String (fromString)
import qualified Data.Text.Encoding as Text
import qualified Database.PostgreSQL.Simple as SQL
import VVA.Config
import Data.Aeson (Value)
import Data.Has (Has)
import VVA.Pool (ConnectionPool, withPool)

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

getCurrentEpochParamsSql :: SQL.Query
getCurrentEpochParamsSql = sqlFrom $(embedFile "sql/get-current-epoch-params.sql")

getCurrentEpochParams ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  m (Maybe Value)
getCurrentEpochParams = withPool $ \conn -> do
  result <- liftIO $ SQL.query_ conn getCurrentEpochParamsSql
  case result of
    [] -> return Nothing
    (SQL.Only params:_) -> return $ Just params
