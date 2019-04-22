{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout
  ( VkPhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  , pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
  , pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- | VkPhysicalDeviceScalarBlockLayoutFeaturesEXT - Structure indicating
-- support for scalar block layouts
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceScalarBlockLayoutFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'VkPhysicalDeviceScalarBlockLayoutFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'VkPhysicalDeviceScalarBlockLayoutFeaturesEXT' /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to
-- enable this feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceScalarBlockLayoutFeaturesEXT = VkPhysicalDeviceScalarBlockLayoutFeaturesEXT
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT'
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceScalarBlockLayoutFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @scalarBlockLayout@ indicates that the implementation supports the
  -- layout of resource blocks in shaders using
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#interfaces-scalar-block-layout scalar alignment>.
  vkScalarBlockLayout :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceScalarBlockLayoutFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceScalarBlockLayoutFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkScalarBlockLayout (poked :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT))

instance Zero VkPhysicalDeviceScalarBlockLayoutFeaturesEXT where
  zero = VkPhysicalDeviceScalarBlockLayoutFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
                                                      zero
                                                      zero

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME"
pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME = "VK_EXT_scalar_block_layout"

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION"
pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT = VkStructureType 1000221000
