{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  , VkExportMemoryWin32HandleInfoKHR(..)
  , VkImportMemoryWin32HandleInfoKHR(..)
  , VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetMemoryWin32HandleKHR
#endif
  , FN_vkGetMemoryWin32HandleKHR
  , PFN_vkGetMemoryWin32HandleKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetMemoryWin32HandlePropertiesKHR
#endif
  , FN_vkGetMemoryWin32HandlePropertiesKHR
  , PFN_vkGetMemoryWin32HandlePropertiesKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CWchar
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
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "LPCWSTR"
type LPCWSTR = Ptr CWchar
  
-- No documentation found for TopLevel "VkExportMemoryWin32HandleInfoKHR"
data VkExportMemoryWin32HandleInfoKHR = VkExportMemoryWin32HandleInfoKHR
  { -- No documentation found for Nested "VkExportMemoryWin32HandleInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoKHR" "pAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoKHR" "dwAccess"
  vkDwAccess :: DWORD
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkExportMemoryWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkExportMemoryWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
                                              <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkPAttributes (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkExportMemoryWin32HandleInfoKHR))

instance Zero VkExportMemoryWin32HandleInfoKHR where
  zero = VkExportMemoryWin32HandleInfoKHR zero
                                          zero
                                          zero
                                          zero
                                          zero
-- No documentation found for TopLevel "VkImportMemoryWin32HandleInfoKHR"
data VkImportMemoryWin32HandleInfoKHR = VkImportMemoryWin32HandleInfoKHR
  { -- No documentation found for Nested "VkImportMemoryWin32HandleInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoKHR" "handleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoKHR" "handle"
  vkHandle :: HANDLE
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkImportMemoryWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImportMemoryWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
                                              <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandle (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkImportMemoryWin32HandleInfoKHR))

instance Zero VkImportMemoryWin32HandleInfoKHR where
  zero = VkImportMemoryWin32HandleInfoKHR zero
                                          zero
                                          zero
                                          zero
                                          zero
-- No documentation found for TopLevel "VkMemoryGetWin32HandleInfoKHR"
data VkMemoryGetWin32HandleInfoKHR = VkMemoryGetWin32HandleInfoKHR
  { -- No documentation found for Nested "VkMemoryGetWin32HandleInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryGetWin32HandleInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryGetWin32HandleInfoKHR" "memory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkMemoryGetWin32HandleInfoKHR" "handleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkMemoryGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryGetWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMemoryGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkMemoryGetWin32HandleInfoKHR))

instance Zero VkMemoryGetWin32HandleInfoKHR where
  zero = VkMemoryGetWin32HandleInfoKHR zero
                                       zero
                                       zero
                                       zero
-- No documentation found for TopLevel "VkMemoryWin32HandlePropertiesKHR"
data VkMemoryWin32HandlePropertiesKHR = VkMemoryWin32HandlePropertiesKHR
  { -- No documentation found for Nested "VkMemoryWin32HandlePropertiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryWin32HandlePropertiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryWin32HandlePropertiesKHR" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryWin32HandlePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryWin32HandlePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryWin32HandlePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryWin32HandlePropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryWin32HandlePropertiesKHR))

instance Zero VkMemoryWin32HandlePropertiesKHR where
  zero = VkMemoryWin32HandlePropertiesKHR zero
                                          zero
                                          zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetMemoryWin32HandleKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryWin32HandleKHR" vkGetMemoryWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult

#endif
type FN_vkGetMemoryWin32HandleKHR = ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetMemoryWin32HandleKHR = FunPtr FN_vkGetMemoryWin32HandleKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetMemoryWin32HandlePropertiesKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryWin32HandlePropertiesKHR" vkGetMemoryWin32HandlePropertiesKHR :: ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult

#endif
type FN_vkGetMemoryWin32HandlePropertiesKHR = ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult
type PFN_vkGetMemoryWin32HandlePropertiesKHR = FunPtr FN_vkGetMemoryWin32HandlePropertiesKHR
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_KHR_external_memory_win32"
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR = VkStructureType 1000073001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR = VkStructureType 1000073000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000073003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR = VkStructureType 1000073002
