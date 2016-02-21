module Spec.Constant where

import Data.Word(Word32, Word64)

data Constant = Constant { cName :: String
                         , cValueString :: String
                         , cValue :: ConstantValue
                         , cComment :: Maybe String
                         }
  deriving (Show)

data ConstantValue = -- | An integral value with no specific size
                     IntegralValue Integer
                   | -- | A value ending in 'f'
                     FloatValue Float
                   | -- A value sized with 'U'
                     Word32Value Word32
                   | -- A value sized with 'ULL'
                     Word64Value Word64
  deriving (Show)
