module Vulkan.JSON.Spirv (Spirv (..)) where

import           Data.Aeson.TH            (deriveJSON)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)

import           Vulkan.JSON.Internal     (vkAesonOptions)
import           Vulkan.JSON.SpirvEnables (SpirvEnables)

data Spirv = Spirv
  { name       :: Text
  , extension  :: Bool
  , capability :: Bool
  , enable     :: [SpirvEnables]
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Spirv)
