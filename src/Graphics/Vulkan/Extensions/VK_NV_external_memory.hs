{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory
  ( pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
  , pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION
  , pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  , VkExternalMemoryImageCreateInfoNV(..)
  , VkExportMemoryAllocateInfoNV(..)
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagsNV
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV = VkStructureType 1000056000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV = VkStructureType 1000056001
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_SPEC_VERSION"
pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION :: Integral a => a
pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"
-- | VkExternalMemoryImageCreateInfoNV - Specify that an image may be backed
-- by external memory
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV@
--
-- -   @handleTypes@ /must/ be a valid combination of
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'
--     values
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkExternalMemoryImageCreateInfoNV = VkExternalMemoryImageCreateInfoNV
  { -- No documentation found for Nested "VkExternalMemoryImageCreateInfoNV" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalMemoryImageCreateInfoNV" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExternalMemoryImageCreateInfoNV" "vkHandleTypes"
  vkHandleTypes :: VkExternalMemoryHandleTypeFlagsNV
  }
  deriving (Eq, Show)

instance Storable VkExternalMemoryImageCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryImageCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryImageCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalMemoryImageCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryImageCreateInfoNV))
-- | VkExportMemoryAllocateInfoNV - Specify memory handle types that may be
-- exported
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV@
--
-- -   @handleTypes@ /must/ be a valid combination of
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'
--     values
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkExportMemoryAllocateInfoNV = VkExportMemoryAllocateInfoNV
  { -- No documentation found for Nested "VkExportMemoryAllocateInfoNV" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportMemoryAllocateInfoNV" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportMemoryAllocateInfoNV" "vkHandleTypes"
  vkHandleTypes :: VkExternalMemoryHandleTypeFlagsNV
  }
  deriving (Eq, Show)

instance Storable VkExportMemoryAllocateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportMemoryAllocateInfoNV <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportMemoryAllocateInfoNV))
