module Spec.Enum where

import           Data.Int  (Int32)
import           Data.Text

-- TODO: Parse the XML comments into here
data Enum = Enum { eName        :: Text
                 , eComment     :: Maybe Text
                 , eElements    :: [EnumElement]
                 , eUnusedStart :: Maybe Text
                 }
  deriving (Show)

data EnumElement = EnumElement { eeName    :: Text
                               , eeValue   :: !Int32
                               , eeComment :: Maybe Text
                               }
  deriving (Show)
