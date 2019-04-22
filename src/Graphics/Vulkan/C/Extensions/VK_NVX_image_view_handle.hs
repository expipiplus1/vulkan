{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX(..)
  , FN_vkGetImageViewHandleNVX
  , PFN_vkGetImageViewHandleNVX
  , vkGetImageViewHandleNVX
  , pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
  , pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
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
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageView
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkSampler
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @imageView@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' handle
--
-- -   @descriptorType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorType' value
--
-- -   If @sampler@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @sampler@
--     /must/ be a valid 'Graphics.Vulkan.C.Core10.Sampler.VkSampler'
--     handle
--
-- -   Both of @imageView@, and @sampler@ that are valid handles /must/
--     have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorType',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSampler',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetImageViewHandleNVX'
data VkImageViewHandleInfoNVX = VkImageViewHandleInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @imageView@ is the image view to query.
  vkImageView :: VkImageView
  , -- | @descriptorType@ is the type of descriptor for which to query a handle.
  vkDescriptorType :: VkDescriptorType
  , -- | @sampler@ is the sampler to combine with the image view when generating
  -- the handle.
  vkSampler :: VkSampler
  }
  deriving (Eq, Show)

instance Storable VkImageViewHandleInfoNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImageViewHandleInfoNVX <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageViewHandleInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageViewHandleInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkImageView (poked :: VkImageViewHandleInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkDescriptorType (poked :: VkImageViewHandleInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkSampler (poked :: VkImageViewHandleInfoNVX))

instance Zero VkImageViewHandleInfoNVX where
  zero = VkImageViewHandleInfoNVX VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
                                  zero
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkImageViewHandleInfoNVX'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageViewHandleNVX" vkGetImageViewHandleNVX :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32
#else
vkGetImageViewHandleNVX :: DeviceCmds -> ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32
vkGetImageViewHandleNVX deviceCmds = mkVkGetImageViewHandleNVX (pVkGetImageViewHandleNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewHandleNVX
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32)
#endif

type FN_vkGetImageViewHandleNVX = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32
type PFN_vkGetImageViewHandleNVX = FunPtr FN_vkGetImageViewHandleNVX

-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME"
pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME = "VK_NVX_image_view_handle"

-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION"
pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION :: Integral a => a
pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX"
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX = VkStructureType 1000030000
