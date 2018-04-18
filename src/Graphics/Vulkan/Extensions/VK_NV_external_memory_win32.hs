{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( HANDLE
  , SECURITY_ATTRIBUTES
  , DWORD
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , vkGetMemoryWin32HandleNV
  , VkImportMemoryWin32HandleInfoNV(..)
  , VkExportMemoryWin32HandleInfoNV(..)
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
  ( VkDevice
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBitsNV(..)
  , VkExternalMemoryHandleTypeFlagsNV
  )


type HANDLE = Ptr ()
data SECURITY_ATTRIBUTES
type DWORD = Word32
-- | Nothing
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV = VkStructureType 1000057000
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV = VkStructureType 1000057001
pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1
pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_NV_external_memory_win32"
-- | 
foreign import ccall "vkGetMemoryWin32HandleNV" vkGetMemoryWin32HandleNV :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
-- | TODO: Struct comments
data VkImportMemoryWin32HandleInfoNV = VkImportMemoryWin32HandleInfoNV
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkHandleType :: VkExternalMemoryHandleTypeFlagsNV
  , vkHandle :: HANDLE
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkImportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 24) (vkHandle (poked :: VkImportMemoryWin32HandleInfoNV))
-- | TODO: Struct comments
data VkExportMemoryWin32HandleInfoNV = VkExportMemoryWin32HandleInfoNV
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkAttributes :: Ptr SECURITY_ATTRIBUTES
  , vkDwAccess :: DWORD
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkExportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 16) (vkAttributes (poked :: VkExportMemoryWin32HandleInfoNV))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportMemoryWin32HandleInfoNV))
