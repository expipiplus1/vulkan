module Vulkan.JSON.Constant (Constant (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Scientific      (Scientific)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data Constant = Constant
  { name           :: Text
  , type'          :: Text
  , value          :: Scientific
  , valueStr       :: Text
  , videoStdHeader :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Constant)
