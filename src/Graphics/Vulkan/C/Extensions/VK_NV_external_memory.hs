{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_external_memory
  ( VkExportMemoryAllocateInfoNV(..)
  , VkExternalMemoryImageCreateInfoNV(..)
  , pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
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
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagsNV
  )


-- | VkExportMemoryAllocateInfoNV - Specify memory handle types that may be
-- exported
--
-- = Description
--
-- Unresolved directive in VkExportMemoryAllocateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkExportMemoryAllocateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkExportMemoryAllocateInfoNV = VkExportMemoryAllocateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @handleTypes@ is a bitmask of
  -- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'
  -- specifying one or more memory handle types that /may/ be exported.
  -- Multiple handle types /may/ be requested for the same allocation as long
  -- as they are compatible, as reported by
  -- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV'.
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

instance Zero VkExportMemoryAllocateInfoNV where
  zero = VkExportMemoryAllocateInfoNV VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
                                      zero
                                      zero

-- | VkExternalMemoryImageCreateInfoNV - Specify that an image may be backed
-- by external memory
--
-- = Description
--
-- Unresolved directive in VkExternalMemoryImageCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkExternalMemoryImageCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkExternalMemoryImageCreateInfoNV = VkExternalMemoryImageCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @handleTypes@ is a bitmask of
  -- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'
  -- specifying one or more external memory handle types.
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

instance Zero VkExternalMemoryImageCreateInfoNV where
  zero = VkExternalMemoryImageCreateInfoNV VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
                                           zero
                                           zero

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_SPEC_VERSION"
pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION :: Integral a => a
pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV = VkStructureType 1000056001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV = VkStructureType 1000056000
