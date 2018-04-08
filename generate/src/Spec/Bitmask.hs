module Spec.Bitmask where

import           Data.Word (Word32)

data Bitmask = Bitmask { bmName         :: String
                       , bmComment      :: Maybe String
                       , bmValues       :: [BitmaskValue]
                       , bmBitPositions :: [BitmaskBitPosition]
                       }
  deriving (Show)

data BitmaskValue = BitmaskValue { bmvName    :: String
                                 , bmvValue   :: Word32
                                 , bmvComment :: Maybe String
                                 }
  deriving (Show)

data BitmaskBitPosition = BitmaskBitPosition { bmbpName    :: String
                                             , bmbpBitPos  :: !Int
                                             , bmbpComment :: Maybe String
                                             }
  deriving (Show)
