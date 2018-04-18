{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
  ( pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  , vkGetMemoryHostPointerPropertiesEXT
  , VkImportMemoryHostPointerInfoEXT(..)
  , VkMemoryHostPointerPropertiesEXT(..)
  , VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkDevice
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT = VkStructureType 1000178000
-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT = VkStructureType 1000178001
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT = VkStructureType 1000178002
-- | Nothing
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT = VkExternalMemoryHandleTypeFlagBits 0x00000080
-- | Nothing
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT = VkExternalMemoryHandleTypeFlagBits 0x00000100
pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION :: Integral a => a
pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1
pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME = "VK_EXT_external_memory_host"
-- | 
foreign import ccall "vkGetMemoryHostPointerPropertiesEXT" vkGetMemoryHostPointerPropertiesEXT :: ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr VkMemoryHostPointerPropertiesEXT) -> IO VkResult
-- | TODO: Struct comments
data VkImportMemoryHostPointerInfoEXT = VkImportMemoryHostPointerInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  , vkHostPointer :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkImportMemoryHostPointerInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImportMemoryHostPointerInfoEXT <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportMemoryHostPointerInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkImportMemoryHostPointerInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryHostPointerInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkHostPointer (poked :: VkImportMemoryHostPointerInfoEXT))
-- | TODO: Struct comments
data VkMemoryHostPointerPropertiesEXT = VkMemoryHostPointerPropertiesEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryHostPointerPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryHostPointerPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryHostPointerPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMemoryHostPointerPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryHostPointerPropertiesEXT))
-- | TODO: Struct comments
data VkPhysicalDeviceExternalMemoryHostPropertiesEXT = VkPhysicalDeviceExternalMemoryHostPropertiesEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMinImportedHostPointerAlignment :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalMemoryHostPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMinImportedHostPointerAlignment (poked :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
