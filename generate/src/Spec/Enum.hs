module Spec.Enum where

import           Data.Int (Int32)

-- TODO: Parse the XML comments into here
data Enum = Enum { eName        :: String
                 , eComment     :: Maybe String
                 , eElements    :: [EnumElement]
                 , eUnusedStart :: Maybe String
                 }
  deriving (Show)

data EnumElement = EnumElement { eeName    :: String
                               , eeValue   :: !Int32
                               , eeComment :: Maybe String
                               }
  deriving (Show)
