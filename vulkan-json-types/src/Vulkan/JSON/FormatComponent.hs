module Vulkan.JSON.FormatComponent (FormatComponent (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data FormatComponent = FormatComponent
  { type'         :: Text
  , bits          :: Text
  , numericFormat :: Text
  , planeIndex    :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''FormatComponent)
