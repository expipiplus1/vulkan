module Spec.Bitmask where

import           Data.Word (Word32)

data Bitmask = Bitmask { bmName         :: String
                       , bmHsName       :: String
                       , bmNamespace    :: Maybe String
                       , bmComment      :: Maybe String
                       , bmValues       :: [BitmaskValue]
                       , bmBitPositions :: [BitmaskBitPosition]
                       }
  deriving (Show)

data BitmaskValue = BitmaskValue { bmvName    :: String
                                 , bmvHsName  :: String
                                 , bmvValue   :: Word32
                                 , bmvComment :: Maybe String
                                 }
  deriving (Show)

data BitmaskBitPosition = BitmaskBitPosition { bmbpName    :: String
                                             , bmbpHsName  :: String
                                             , bmbpBitPos  :: !Int
                                             , bmbpComment :: Maybe String
                                             }
  deriving (Show)

