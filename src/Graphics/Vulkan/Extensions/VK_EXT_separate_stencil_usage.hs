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


-- No documentation found for TopLevel "ImageStencilUsageCreateInfoEXT"
data ImageStencilUsageCreateInfoEXT = ImageStencilUsageCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageStencilUsageCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageStencilUsageCreateInfoEXT" "stencilUsage"
  vkStencilUsage :: ImageUsageFlags
  }
  deriving (Show, Eq)
withCStructImageStencilUsageCreateInfoEXT :: ImageStencilUsageCreateInfoEXT -> (VkImageStencilUsageCreateInfoEXT -> IO a) -> IO a
withCStructImageStencilUsageCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImageStencilUsageCreateInfoEXT)) (\pPNext -> cont (VkImageStencilUsageCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT pPNext (vkStencilUsage (from :: ImageStencilUsageCreateInfoEXT))))
fromCStructImageStencilUsageCreateInfoEXT :: VkImageStencilUsageCreateInfoEXT -> IO ImageStencilUsageCreateInfoEXT
fromCStructImageStencilUsageCreateInfoEXT c = ImageStencilUsageCreateInfoEXT <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageStencilUsageCreateInfoEXT)))
                                                                             <*> pure (vkStencilUsage (c :: VkImageStencilUsageCreateInfoEXT))
instance Zero ImageStencilUsageCreateInfoEXT where
  zero = ImageStencilUsageCreateInfoEXT Nothing
                                        zero
