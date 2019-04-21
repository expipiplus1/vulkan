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



-- | VkPhysicalDeviceScalarBlockLayoutFeaturesEXT - Structure indicating
-- support for scalar block layouts
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout.VkPhysicalDeviceScalarBlockLayoutFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout.VkPhysicalDeviceScalarBlockLayoutFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout.VkPhysicalDeviceScalarBlockLayoutFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable this
-- feature.
--
-- Unresolved directive in VkPhysicalDeviceScalarBlockLayoutFeaturesEXT.txt
-- -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceScalarBlockLayoutFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceScalarBlockLayoutFeaturesEXT = PhysicalDeviceScalarBlockLayoutFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceScalarBlockLayoutFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceScalarBlockLayoutFeaturesEXT" "scalarBlockLayout"
  scalarBlockLayout :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceScalarBlockLayoutFeaturesEXT' and
-- marshal a 'PhysicalDeviceScalarBlockLayoutFeaturesEXT' into it. The 'VkPhysicalDeviceScalarBlockLayoutFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT :: PhysicalDeviceScalarBlockLayoutFeaturesEXT -> (VkPhysicalDeviceScalarBlockLayoutFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceScalarBlockLayoutFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceScalarBlockLayoutFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT pPNext (boolToBool32 (scalarBlockLayout (marshalled :: PhysicalDeviceScalarBlockLayoutFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceScalarBlockLayoutFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceScalarBlockLayoutFeaturesEXT'.
fromCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT -> IO PhysicalDeviceScalarBlockLayoutFeaturesEXT
fromCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT c = PhysicalDeviceScalarBlockLayoutFeaturesEXT <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT)))
                                                                                                     <*> pure (bool32ToBool (vkScalarBlockLayout (c :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT)))

instance Zero PhysicalDeviceScalarBlockLayoutFeaturesEXT where
  zero = PhysicalDeviceScalarBlockLayoutFeaturesEXT Nothing
                                                    False

