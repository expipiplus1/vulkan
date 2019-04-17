{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( VkImportMemoryFdInfoKHR(..)
  , VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetMemoryFdKHR
#endif
  , FN_vkGetMemoryFdKHR
  , PFN_vkGetMemoryFdKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetMemoryFdPropertiesKHR
#endif
  , FN_vkGetMemoryFdPropertiesKHR
  , PFN_vkGetMemoryFdPropertiesKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CInt(..)
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
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkImportMemoryFdInfoKHR"
data VkImportMemoryFdInfoKHR = VkImportMemoryFdInfoKHR
  { -- No documentation found for Nested "VkImportMemoryFdInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportMemoryFdInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportMemoryFdInfoKHR" "handleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportMemoryFdInfoKHR" "fd"
  vkFd :: CInt
  }
  deriving (Eq, Show)

instance Storable VkImportMemoryFdInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImportMemoryFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportMemoryFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportMemoryFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryFdInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkFd (poked :: VkImportMemoryFdInfoKHR))

instance Zero VkImportMemoryFdInfoKHR where
  zero = VkImportMemoryFdInfoKHR zero
                                 zero
                                 zero
                                 zero
-- No documentation found for TopLevel "VkMemoryFdPropertiesKHR"
data VkMemoryFdPropertiesKHR = VkMemoryFdPropertiesKHR
  { -- No documentation found for Nested "VkMemoryFdPropertiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryFdPropertiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryFdPropertiesKHR" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryFdPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryFdPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryFdPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryFdPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryFdPropertiesKHR))

instance Zero VkMemoryFdPropertiesKHR where
  zero = VkMemoryFdPropertiesKHR zero
                                 zero
                                 zero
-- No documentation found for TopLevel "VkMemoryGetFdInfoKHR"
data VkMemoryGetFdInfoKHR = VkMemoryGetFdInfoKHR
  { -- No documentation found for Nested "VkMemoryGetFdInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryGetFdInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryGetFdInfoKHR" "memory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkMemoryGetFdInfoKHR" "handleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkMemoryGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryGetFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryGetFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryGetFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMemoryGetFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkMemoryGetFdInfoKHR))

instance Zero VkMemoryGetFdInfoKHR where
  zero = VkMemoryGetFdInfoKHR zero
                              zero
                              zero
                              zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetMemoryFdKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryFdKHR" vkGetMemoryFdKHR :: ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult

#endif
type FN_vkGetMemoryFdKHR = ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
type PFN_vkGetMemoryFdKHR = FunPtr FN_vkGetMemoryFdKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetMemoryFdPropertiesKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryFdPropertiesKHR" vkGetMemoryFdPropertiesKHR :: ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult

#endif
type FN_vkGetMemoryFdPropertiesKHR = ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult
type PFN_vkGetMemoryFdPropertiesKHR = FunPtr FN_vkGetMemoryFdPropertiesKHR
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME = "VK_KHR_external_memory_fd"
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR = VkStructureType 1000074000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR = VkStructureType 1000074001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR = VkStructureType 1000074002
