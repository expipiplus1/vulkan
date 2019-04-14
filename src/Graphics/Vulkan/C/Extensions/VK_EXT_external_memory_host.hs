{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( VkImportMemoryHostPointerInfoEXT(..)
  , VkMemoryHostPointerPropertiesEXT(..)
  , VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetMemoryHostPointerPropertiesEXT
#endif
  , FN_vkGetMemoryHostPointerPropertiesEXT
  , PFN_vkGetMemoryHostPointerPropertiesEXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  , VkExternalMemoryHandleTypeFlagsKHR
  , VkExternalMemoryHandleTypeFlagBitsKHR
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
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBitsKHR
  , VkExternalMemoryHandleTypeFlagsKHR
  )


-- No documentation found for TopLevel "VkImportMemoryHostPointerInfoEXT"
data VkImportMemoryHostPointerInfoEXT = VkImportMemoryHostPointerInfoEXT
  { -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "handleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "pHostPointer"
  vkPHostPointer :: Ptr ()
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportMemoryHostPointerInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryHostPointerInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPHostPointer (poked :: VkImportMemoryHostPointerInfoEXT))
-- No documentation found for TopLevel "VkMemoryHostPointerPropertiesEXT"
data VkMemoryHostPointerPropertiesEXT = VkMemoryHostPointerPropertiesEXT
  { -- No documentation found for Nested "VkMemoryHostPointerPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryHostPointerPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryHostPointerPropertiesEXT" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryHostPointerPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryHostPointerPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryHostPointerPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryHostPointerPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryHostPointerPropertiesEXT))
-- No documentation found for TopLevel "VkPhysicalDeviceExternalMemoryHostPropertiesEXT"
data VkPhysicalDeviceExternalMemoryHostPropertiesEXT = VkPhysicalDeviceExternalMemoryHostPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceExternalMemoryHostPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceExternalMemoryHostPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceExternalMemoryHostPropertiesEXT" "minImportedHostPointerAlignment"
  vkMinImportedHostPointerAlignment :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalMemoryHostPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMinImportedHostPointerAlignment (poked :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetMemoryHostPointerPropertiesEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryHostPointerPropertiesEXT" vkGetMemoryHostPointerPropertiesEXT :: ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr VkMemoryHostPointerPropertiesEXT) -> IO VkResult

#endif
type FN_vkGetMemoryHostPointerPropertiesEXT = ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr VkMemoryHostPointerPropertiesEXT) -> IO VkResult
type PFN_vkGetMemoryHostPointerPropertiesEXT = FunPtr FN_vkGetMemoryHostPointerPropertiesEXT
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT = VkExternalMemoryHandleTypeFlagBits 0x00000080
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT = VkExternalMemoryHandleTypeFlagBits 0x00000100
-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME"
pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME = "VK_EXT_external_memory_host"
-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION"
pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION :: Integral a => a
pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT"
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT = VkStructureType 1000178000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT = VkStructureType 1000178001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT = VkStructureType 1000178002
