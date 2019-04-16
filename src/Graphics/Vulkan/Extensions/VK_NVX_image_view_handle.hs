{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NVX_image_view_handle
  ( withCStructImageViewHandleInfoNVX
  , fromCStructImageViewHandleInfoNVX
  , ImageViewHandleInfoNVX(..)
  , getImageViewHandleNVX
  , pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
  , pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getImageViewHandleNVX
  )


import Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorType
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageView
  )
import Graphics.Vulkan.Core10.Sampler
  ( Sampler
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
  , pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
  )


-- No documentation found for TopLevel "ImageViewHandleInfoNVX"
data ImageViewHandleInfoNVX = ImageViewHandleInfoNVX
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageViewHandleInfoNVX" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewHandleInfoNVX" "imageView"
  vkImageView :: ImageView
  , -- No documentation found for Nested "ImageViewHandleInfoNVX" "descriptorType"
  vkDescriptorType :: DescriptorType
  , -- No documentation found for Nested "ImageViewHandleInfoNVX" "sampler"
  vkSampler :: Sampler
  }
  deriving (Show, Eq)
withCStructImageViewHandleInfoNVX :: ImageViewHandleInfoNVX -> (VkImageViewHandleInfoNVX -> IO a) -> IO a
withCStructImageViewHandleInfoNVX from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImageViewHandleInfoNVX)) (\pPNext -> cont (VkImageViewHandleInfoNVX VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX pPNext (vkImageView (from :: ImageViewHandleInfoNVX)) (vkDescriptorType (from :: ImageViewHandleInfoNVX)) (vkSampler (from :: ImageViewHandleInfoNVX))))
fromCStructImageViewHandleInfoNVX :: VkImageViewHandleInfoNVX -> IO ImageViewHandleInfoNVX
fromCStructImageViewHandleInfoNVX c = ImageViewHandleInfoNVX <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageViewHandleInfoNVX)))
                                                             <*> pure (vkImageView (c :: VkImageViewHandleInfoNVX))
                                                             <*> pure (vkDescriptorType (c :: VkImageViewHandleInfoNVX))
                                                             <*> pure (vkSampler (c :: VkImageViewHandleInfoNVX))

-- | Wrapper for vkGetImageViewHandleNVX
getImageViewHandleNVX :: Device ->  ImageViewHandleInfoNVX ->  IO (Word32)
getImageViewHandleNVX = \(Device device commandTable) -> \info -> (\a -> withCStructImageViewHandleInfoNVX a . flip with) info (\pInfo -> Graphics.Vulkan.C.Dynamic.getImageViewHandleNVX commandTable device pInfo >>= (\r -> pure r))
