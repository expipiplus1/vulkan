module Spec.Constant where

import           Data.Text
import           Data.Word (Word32, Word64)

data Constant = Constant { cName    :: Text
                         , cValue   :: Either ConstantAlias Text
                         , cComment :: Maybe Text
                         }
  deriving (Show)

newtype ConstantAlias
  = ConstantAlias { unConstantAlias :: Text }
  deriving (Show)

