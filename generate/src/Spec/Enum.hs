module Spec.Enum where

import Data.Int(Int32)

-- TODO: Parse the XML comments into here
data Enum = Enum { eName :: String
                 , eNamespace :: Maybe String
                 , eExpand :: Maybe String 
                 , eComment :: Maybe String
                 , eElements :: [EnumElement]
                 }
  deriving (Show)

data EnumElement = EnumElement { eeName :: String
                               , eeValue :: !Int32
                               , eeComment :: Maybe String
                               }
  deriving (Show)
