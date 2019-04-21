{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_separate_stencil_usage
  ( withCStructImageStencilUsageCreateInfoEXT
  , fromCStructImageStencilUsageCreateInfoEXT
  , ImageStencilUsageCreateInfoEXT(..)
  , pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
  , pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage
  ( VkImageStencilUsageCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageUsageFlags
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage
  ( pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
  , pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
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
data ImageStencilUsageCreateInfoEXT = ImageStencilUsageCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ImageStencilUsageCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageStencilUsageCreateInfoEXT" "stencilUsage"
  stencilUsage :: ImageUsageFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageStencilUsageCreateInfoEXT' and
-- marshal a 'ImageStencilUsageCreateInfoEXT' into it. The 'VkImageStencilUsageCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageStencilUsageCreateInfoEXT :: ImageStencilUsageCreateInfoEXT -> (VkImageStencilUsageCreateInfoEXT -> IO a) -> IO a
withCStructImageStencilUsageCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImageStencilUsageCreateInfoEXT)) (\pPNext -> cont (VkImageStencilUsageCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT pPNext (stencilUsage (marshalled :: ImageStencilUsageCreateInfoEXT))))

-- | A function to read a 'VkImageStencilUsageCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'ImageStencilUsageCreateInfoEXT'.
fromCStructImageStencilUsageCreateInfoEXT :: VkImageStencilUsageCreateInfoEXT -> IO ImageStencilUsageCreateInfoEXT
fromCStructImageStencilUsageCreateInfoEXT c = ImageStencilUsageCreateInfoEXT <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageStencilUsageCreateInfoEXT)))
                                                                             <*> pure (vkStencilUsage (c :: VkImageStencilUsageCreateInfoEXT))

instance Zero ImageStencilUsageCreateInfoEXT where
  zero = ImageStencilUsageCreateInfoEXT Nothing
                                        zero

