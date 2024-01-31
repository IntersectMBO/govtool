module VVA.API.Utils where

import Data.Aeson (Options (..), defaultOptions)
import Data.Char
import Foreign (pooledMalloc)

-- | Apply function to first element in the list.
applyFirst :: (a -> a) -> [a] -> [a]
applyFirst _ [] = []
applyFirst f [x] = [f x]
applyFirst f (x : xs) = f x : xs

jsonOptions :: String -> Options
jsonOptions p =
  defaultOptions
    { fieldLabelModifier = applyFirst toLower . drop (length p),
      unwrapUnaryRecords = False
    }
