{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  , VkExportMemoryWin32HandleInfoNV(..)
  , VkImportMemoryWin32HandleInfoNV(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetMemoryWin32HandleNV
#endif
  , FN_vkGetMemoryWin32HandleNV
  , PFN_vkGetMemoryWin32HandleNV
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
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
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagsNV
  )

#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBitsNV(..)
  )
#endif
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "DWORD"
type DWORD = Word32
  
-- No documentation found for TopLevel "HANDLE"
type HANDLE = Ptr ()
  
-- | Opaque data
data SECURITY_ATTRIBUTES
-- No documentation found for TopLevel "VkExportMemoryWin32HandleInfoNV"
data VkExportMemoryWin32HandleInfoNV = VkExportMemoryWin32HandleInfoNV
  { -- No documentation found for Nested "VkExportMemoryWin32HandleInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoNV" "pAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoNV" "dwAccess"
  vkDwAccess :: DWORD
  }
  deriving (Eq, Show)

instance Storable VkExportMemoryWin32HandleInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExportMemoryWin32HandleInfoNV <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 16) (vkPAttributes (poked :: VkExportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportMemoryWin32HandleInfoNV))
-- No documentation found for TopLevel "VkImportMemoryWin32HandleInfoNV"
data VkImportMemoryWin32HandleInfoNV = VkImportMemoryWin32HandleInfoNV
  { -- No documentation found for Nested "VkImportMemoryWin32HandleInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoNV" "handleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagsNV
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoNV" "handle"
  vkHandle :: HANDLE
  }
  deriving (Eq, Show)

instance Storable VkImportMemoryWin32HandleInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImportMemoryWin32HandleInfoNV <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 24) (vkHandle (poked :: VkImportMemoryWin32HandleInfoNV))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetMemoryWin32HandleNV"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryWin32HandleNV" vkGetMemoryWin32HandleNV :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult

#endif
type FN_vkGetMemoryWin32HandleNV = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetMemoryWin32HandleNV = FunPtr FN_vkGetMemoryWin32HandleNV
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_NV_external_memory_win32"
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV = VkStructureType 1000057001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV"
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV = VkStructureType 1000057000
