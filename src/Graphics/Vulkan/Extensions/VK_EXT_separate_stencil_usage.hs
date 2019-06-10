{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_separate_stencil_usage
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ImageStencilUsageCreateInfoEXT(..)
  , 
#endif
  pattern EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
  , pattern EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage
  ( pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
  , pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageUsageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageStencilUsageCreateInfoEXT"
data ImageStencilUsageCreateInfoEXT = ImageStencilUsageCreateInfoEXT
  { -- No documentation found for Nested "ImageStencilUsageCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageStencilUsageCreateInfoEXT" "stencilUsage"
  stencilUsage :: ImageUsageFlags
  }
  deriving (Show, Eq)

instance Zero ImageStencilUsageCreateInfoEXT where
  zero = ImageStencilUsageCreateInfoEXT Nothing
                                        zero

#endif

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME"
pattern EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME = VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION"
pattern EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION :: Integral a => a
pattern EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
