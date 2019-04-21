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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX(..)
  , vkGetImageViewHandleNVX
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



-- | VkImageViewHandleInfoNVX - Structure specifying the image view for
-- handle queries
--
-- == Valid Usage
--
-- -   @descriptorType@ /must/ be
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--
-- -   @sampler@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' if @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--
-- -   If descriptorType is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     the image that @imageView@ was created from /must/ have been created
--     with the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT'
--     usage bit set
--
-- -   If descriptorType is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     the image that @imageView@ was created from /must/ have been created
--     with the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_STORAGE_BIT'
--     usage bit set
--
-- Unresolved directive in VkImageViewHandleInfoNVX.txt -
-- include::{generated}\/validity\/structs\/VkImageViewHandleInfoNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data ImageViewHandleInfoNVX = ImageViewHandleInfoNVX
  { -- Univalued member elided
  -- No documentation found for Nested "ImageViewHandleInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewHandleInfoNVX" "imageView"
  imageView :: ImageView
  , -- No documentation found for Nested "ImageViewHandleInfoNVX" "descriptorType"
  descriptorType :: DescriptorType
  , -- No documentation found for Nested "ImageViewHandleInfoNVX" "sampler"
  sampler :: Sampler
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageViewHandleInfoNVX' and
-- marshal a 'ImageViewHandleInfoNVX' into it. The 'VkImageViewHandleInfoNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageViewHandleInfoNVX :: ImageViewHandleInfoNVX -> (VkImageViewHandleInfoNVX -> IO a) -> IO a
withCStructImageViewHandleInfoNVX marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImageViewHandleInfoNVX)) (\pPNext -> cont (VkImageViewHandleInfoNVX VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX pPNext (imageView (marshalled :: ImageViewHandleInfoNVX)) (descriptorType (marshalled :: ImageViewHandleInfoNVX)) (sampler (marshalled :: ImageViewHandleInfoNVX))))

-- | A function to read a 'VkImageViewHandleInfoNVX' and all additional
-- structures in the pointer chain into a 'ImageViewHandleInfoNVX'.
fromCStructImageViewHandleInfoNVX :: VkImageViewHandleInfoNVX -> IO ImageViewHandleInfoNVX
fromCStructImageViewHandleInfoNVX c = ImageViewHandleInfoNVX <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageViewHandleInfoNVX)))
                                                             <*> pure (vkImageView (c :: VkImageViewHandleInfoNVX))
                                                             <*> pure (vkDescriptorType (c :: VkImageViewHandleInfoNVX))
                                                             <*> pure (vkSampler (c :: VkImageViewHandleInfoNVX))

instance Zero ImageViewHandleInfoNVX where
  zero = ImageViewHandleInfoNVX Nothing
                                zero
                                zero
                                zero



-- | vkGetImageViewHandleNVX - Get the handle for an image view for a
-- specific descriptor type
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image view.
--
-- -   @pInfo@ describes the image view to query and type of handle.
--
-- = Description
--
-- Unresolved directive in vkGetImageViewHandleNVX.txt -
-- include::{generated}\/validity\/protos\/vkGetImageViewHandleNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
getImageViewHandleNVX :: Device ->  ImageViewHandleInfoNVX ->  IO (Word32)
getImageViewHandleNVX = \(Device device' commandTable) -> \info' -> (\marshalled -> withCStructImageViewHandleInfoNVX marshalled . flip with) info' (\pInfo' -> vkGetImageViewHandleNVX commandTable device' pInfo' >>= (\ret -> pure ret))
