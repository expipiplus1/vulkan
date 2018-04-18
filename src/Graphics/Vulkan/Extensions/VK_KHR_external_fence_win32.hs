{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , vkGetFenceWin32HandleKHR
  , vkImportFenceWin32HandleKHR
  , VkImportFenceWin32HandleInfoKHR(..)
  , VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
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
import Graphics.Vulkan.Core10.Queue
  ( VkFence
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , SECURITY_ATTRIBUTES
  , HANDLE
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114000
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114001
-- | Nothing
pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114002
pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1
pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = "VK_KHR_external_fence_win32"
-- | 
foreign import ccall "vkGetFenceWin32HandleKHR" vkGetFenceWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
-- | 
foreign import ccall "vkImportFenceWin32HandleKHR" vkImportFenceWin32HandleKHR :: ("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult
-- | TODO: Struct comments
data VkImportFenceWin32HandleInfoKHR = VkImportFenceWin32HandleInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFence :: VkFence
  , vkFlags :: VkFenceImportFlags
  , vkHandleType :: VkExternalFenceHandleTypeFlagBits
  , vkHandle :: HANDLE
  , vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkImportFenceWin32HandleInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkImportFenceWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 28)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkHandle (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkName (poked :: VkImportFenceWin32HandleInfoKHR))
-- | TODO: Struct comments
data VkExportFenceWin32HandleInfoKHR = VkExportFenceWin32HandleInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , vkDwAccess :: DWORD
  , vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkExportFenceWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkExportFenceWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkPAttributes (poked :: VkExportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkExportFenceWin32HandleInfoKHR))
-- | TODO: Struct comments
data VkFenceGetWin32HandleInfoKHR = VkFenceGetWin32HandleInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFence :: VkFence
  , vkHandleType :: VkExternalFenceHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkFenceGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkFenceGetWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkFenceGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkFenceGetWin32HandleInfoKHR))
