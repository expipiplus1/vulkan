module Vulkan.JSON.Format (Format (..)) where

import           Data.Aeson.TH                  (deriveJSON)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)

import           Vulkan.JSON.FormatComponent    (FormatComponent)
import           Vulkan.JSON.FormatPlane        (FormatPlane)
import           Vulkan.JSON.Internal           (vkAesonOptions)

data Format = Format
  { name             :: Text
  , className        :: Text
  , blockSize        :: Int
  , texelsPerBlock   :: Int
  , blockExtent      :: [Text]
  , packed           :: Maybe Int
  , chroma           :: Maybe Text
  , compressed       :: Maybe Text
  , components       :: [FormatComponent]
  , planes           :: [FormatPlane]
  , spirvImageFormat :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Format)
