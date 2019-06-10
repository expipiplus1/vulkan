{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_image_format_list
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ImageFormatListCreateInfoKHR(..)
  , 
#endif
  pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  , pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list
  ( pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  , pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageFormatListCreateInfoKHR"
data ImageFormatListCreateInfoKHR = ImageFormatListCreateInfoKHR
  { -- No documentation found for Nested "ImageFormatListCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageFormatListCreateInfoKHR" "pViewFormats"
  viewFormats :: Vector Format
  }
  deriving (Show, Eq)

instance Zero ImageFormatListCreateInfoKHR where
  zero = ImageFormatListCreateInfoKHR Nothing
                                      mempty

#endif

-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME"
pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION"
pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION :: Integral a => a
pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
