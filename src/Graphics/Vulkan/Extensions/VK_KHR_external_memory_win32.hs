{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , vkGetMemoryWin32HandleKHR
  , vkGetMemoryWin32HandlePropertiesKHR
  , VkImportMemoryWin32HandleInfoKHR(..)
  , VkExportMemoryWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
  , VkMemoryGetWin32HandleInfoKHR(..)
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
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , SECURITY_ATTRIBUTES
  , HANDLE
  )


type LPCWSTR = Ptr CWchar
-- | Nothing
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR = VkStructureType 1000073000
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR = VkStructureType 1000073001
-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR = VkStructureType 1000073002
-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000073003
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_KHR_external_memory_win32"
-- | 
foreign import ccall "vkGetMemoryWin32HandleKHR" vkGetMemoryWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
-- | 
foreign import ccall "vkGetMemoryWin32HandlePropertiesKHR" vkGetMemoryWin32HandlePropertiesKHR :: ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult
-- | TODO: Struct comments
data VkImportMemoryWin32HandleInfoKHR = VkImportMemoryWin32HandleInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  , vkHandle :: HANDLE
  , vkName :: LPCWSTR
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandle (poked :: VkImportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkImportMemoryWin32HandleInfoKHR))
-- | TODO: Struct comments
data VkExportMemoryWin32HandleInfoKHR = VkExportMemoryWin32HandleInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkAttributes :: Ptr SECURITY_ATTRIBUTES
  , vkDwAccess :: DWORD
  , vkName :: LPCWSTR
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkAttributes (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportMemoryWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkExportMemoryWin32HandleInfoKHR))
-- | TODO: Struct comments
data VkMemoryWin32HandlePropertiesKHR = VkMemoryWin32HandlePropertiesKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryWin32HandlePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryWin32HandlePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryWin32HandlePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMemoryWin32HandlePropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryWin32HandlePropertiesKHR))
-- | TODO: Struct comments
data VkMemoryGetWin32HandleInfoKHR = VkMemoryGetWin32HandleInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMemory :: VkDeviceMemory
  , vkHandleType :: VkExternalMemoryHandleTypeFlagBits
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMemoryGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMemoryGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkMemoryGetWin32HandleInfoKHR))
