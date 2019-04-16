{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetImageViewHandleNVX
#endif
  , FN_vkGetImageViewHandleNVX
  , PFN_vkGetImageViewHandleNVX
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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkImageViewHandleInfoNVX"
data VkImageViewHandleInfoNVX = VkImageViewHandleInfoNVX
  { -- No documentation found for Nested "VkImageViewHandleInfoNVX" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageViewHandleInfoNVX" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageViewHandleInfoNVX" "imageView"
  vkImageView :: VkImageView
  , -- No documentation found for Nested "VkImageViewHandleInfoNVX" "descriptorType"
  vkDescriptorType :: VkDescriptorType
  , -- No documentation found for Nested "VkImageViewHandleInfoNVX" "sampler"
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetImageViewHandleNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageViewHandleNVX" vkGetImageViewHandleNVX :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32

#endif
type FN_vkGetImageViewHandleNVX = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32
type PFN_vkGetImageViewHandleNVX = FunPtr FN_vkGetImageViewHandleNVX
-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME"
pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME = "VK_NVX_image_view_handle"
-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION"
pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION :: Integral a => a
pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX"
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX = VkStructureType 1000030000
