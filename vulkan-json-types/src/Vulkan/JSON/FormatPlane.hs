module Vulkan.JSON.FormatPlane (FormatPlane (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data FormatPlane = FormatPlane
  { index         :: Int
  , widthDivisor  :: Int
  , heightDivisor :: Int
  , compatible    :: Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''FormatPlane)
