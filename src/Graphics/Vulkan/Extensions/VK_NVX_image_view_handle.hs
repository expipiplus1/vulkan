{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NVX_image_view_handle
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ImageViewHandleInfoNVX(..)
  , 
#endif
  getImageViewHandleNVX
  , pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
  , pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( with
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( vkGetImageViewHandleNVX
  , pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
  , pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorType
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.ImageView
  ( ImageView
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Sampler
  ( Sampler
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageViewHandleInfoNVX"
data ImageViewHandleInfoNVX = ImageViewHandleInfoNVX
  { -- No documentation found for Nested "ImageViewHandleInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewHandleInfoNVX" "imageView"
  imageView :: ImageView
  , -- No documentation found for Nested "ImageViewHandleInfoNVX" "descriptorType"
  descriptorType :: DescriptorType
  , -- No documentation found for Nested "ImageViewHandleInfoNVX" "sampler"
  sampler :: Sampler
  }
  deriving (Show, Eq)

instance Zero ImageViewHandleInfoNVX where
  zero = ImageViewHandleInfoNVX Nothing
                                zero
                                zero
                                zero

#endif


-- No documentation found for TopLevel "vkGetImageViewHandleNVX"
getImageViewHandleNVX :: Device ->  ImageViewHandleInfoNVX ->  IO (Word32)
getImageViewHandleNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME"
pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME = VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION"
pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION :: Integral a => a
pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
