{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_scalar_block_layout
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  , 
#endif
  pattern EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
  , pattern EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout
  ( pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
  , pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceScalarBlockLayoutFeaturesEXT"
data PhysicalDeviceScalarBlockLayoutFeaturesEXT = PhysicalDeviceScalarBlockLayoutFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceScalarBlockLayoutFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceScalarBlockLayoutFeaturesEXT" "scalarBlockLayout"
  scalarBlockLayout :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceScalarBlockLayoutFeaturesEXT where
  zero = PhysicalDeviceScalarBlockLayoutFeaturesEXT Nothing
                                                    False

#endif

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME"
pattern EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME = VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION"
pattern EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION :: Integral a => a
pattern EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
