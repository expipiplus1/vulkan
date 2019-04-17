{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_scalar_block_layout
  ( withCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT
  , fromCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT
  , PhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  , pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
  , pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout
  ( VkPhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout
  ( pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
  , pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceScalarBlockLayoutFeaturesEXT"
data PhysicalDeviceScalarBlockLayoutFeaturesEXT = PhysicalDeviceScalarBlockLayoutFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceScalarBlockLayoutFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceScalarBlockLayoutFeaturesEXT" "scalarBlockLayout"
  vkScalarBlockLayout :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT :: PhysicalDeviceScalarBlockLayoutFeaturesEXT -> (VkPhysicalDeviceScalarBlockLayoutFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceScalarBlockLayoutFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceScalarBlockLayoutFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT pPNext (boolToBool32 (vkScalarBlockLayout (from :: PhysicalDeviceScalarBlockLayoutFeaturesEXT)))))
fromCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT -> IO PhysicalDeviceScalarBlockLayoutFeaturesEXT
fromCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT c = PhysicalDeviceScalarBlockLayoutFeaturesEXT <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT)))
                                                                                                     <*> pure (bool32ToBool (vkScalarBlockLayout (c :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT)))
instance Zero PhysicalDeviceScalarBlockLayoutFeaturesEXT where
  zero = PhysicalDeviceScalarBlockLayoutFeaturesEXT Nothing
                                                    False
