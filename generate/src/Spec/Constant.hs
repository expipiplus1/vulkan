module Spec.Constant where

import           Data.Text

data Constant = Constant { cName    :: Text
                         , cValue   :: Either ConstantAlias Text
                         , cComment :: Maybe Text
                         }
  deriving (Show)

newtype ConstantAlias
  = ConstantAlias { unConstantAlias :: Text }
  deriving (Show)

