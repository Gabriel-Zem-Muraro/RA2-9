module Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)

data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read, Eq)

type Inventario = Map String Item

data AcaoLog
  = Add
  | Remove
  | Update
  | QueryFail
  deriving (Show, Read, Eq)

data StatusLog
-- terminar

data LogEntry = LogEntry
-- terminar tbm
