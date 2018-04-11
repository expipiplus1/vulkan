module Spec.Bitmask where

import           Data.Text
import           Data.Word (Word32)

data Bitmask = Bitmask { bmName         :: Text
                       , bmComment      :: Maybe Text
                       , bmValues       :: [BitmaskValue]
                       , bmBitPositions :: [BitmaskBitPosition]
                       }
  deriving (Show)

data BitmaskValue = BitmaskValue { bmvName    :: Text
                                 , bmvValue   :: Word32
                                 , bmvComment :: Maybe Text
                                 }
  deriving (Show)

data BitmaskBitPosition = BitmaskBitPosition { bmbpName    :: Text
                                             , bmbpBitPos  :: !Int
                                             , bmbpComment :: Maybe Text
                                             }
  deriving (Show)
