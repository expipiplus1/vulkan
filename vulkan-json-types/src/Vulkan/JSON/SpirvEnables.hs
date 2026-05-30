module Vulkan.JSON.SpirvEnables (SpirvEnables (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data SpirvEnables = SpirvEnables
  { version   :: Maybe Text
  , extension :: Maybe Text
  , struct    :: Maybe Text
  , feature   :: Maybe Text
  , requires  :: Maybe Text
  , property  :: Maybe Text
  , member    :: Maybe Text
  , value     :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''SpirvEnables)
