{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage
  ( VkImageStencilUsageCreateInfoEXT(..)
  , pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
  , pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
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
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageUsageFlags
  )


-- | VkImageStencilUsageCreateInfoEXT - Specify separate usage flags for the
-- stencil aspect of a depth-stencil image
--
-- = Description
--
-- This structure specifies image usages which only apply to the stencil
-- aspect of a depth\/stencil format image. When this structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo', the stencil aspect
-- of the image /must/ only be used as specified by @stencilUsage@. When
-- this structure is not included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo', the stencil aspect
-- of an image /must/ only be used as specified
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@usage@. Use of
-- other aspects of an image are unaffected by this structure.
--
-- This structure /can/ also be included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
-- to query additional capabilities specific to image creation parameter
-- combinations including a separate set of usage flags for the stencil
-- aspect of the image using
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'.
-- When this structure is not present in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
-- then the implicit value of @stencilUsage@ matches that of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'::@usage@.
--
-- == Valid Usage
--
-- -   If @stencilUsage@ includes
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     then bits other than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     /must/ not be set
--
-- Unresolved directive in VkImageStencilUsageCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkImageStencilUsageCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkImageStencilUsageCreateInfoEXT = VkImageStencilUsageCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @stencilUsage@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
  -- describing the intended usage of the stencil aspect of the image.
  vkStencilUsage :: VkImageUsageFlags
  }
  deriving (Eq, Show)

instance Storable VkImageStencilUsageCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageStencilUsageCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageStencilUsageCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageStencilUsageCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkStencilUsage (poked :: VkImageStencilUsageCreateInfoEXT))

instance Zero VkImageStencilUsageCreateInfoEXT where
  zero = VkImageStencilUsageCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
                                          zero
                                          zero

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME"
pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME = "VK_EXT_separate_stencil_usage"

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION"
pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT = VkStructureType 1000246000
