{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  , vkGetSemaphoreWin32HandleKHR
  , vkImportSemaphoreWin32HandleKHR
  , VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkExportSemaphoreWin32HandleInfoKHR(..)
  , VkD3D12FenceSubmitInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  , Word32
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
  ( VkSemaphore
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkSemaphoreImportFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlagBits(..)
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
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000078000
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000078001
-- | Nothing
pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR = VkStructureType 1000078002
-- | Nothing
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000078003
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = 1
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME = "VK_KHR_external_semaphore_win32"
-- | 
foreign import ccall "vkGetSemaphoreWin32HandleKHR" vkGetSemaphoreWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
-- | 
foreign import ccall "vkImportSemaphoreWin32HandleKHR" vkImportSemaphoreWin32HandleKHR :: ("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult
-- | TODO: Struct comments
data VkImportSemaphoreWin32HandleInfoKHR = VkImportSemaphoreWin32HandleInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkSemaphore :: VkSemaphore
  , vkFlags :: VkSemaphoreImportFlags
  , vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  , vkHandle :: HANDLE
  , vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkImportSemaphoreWin32HandleInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkImportSemaphoreWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
                                                 <*> peek (ptr `plusPtr` 32)
                                                 <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkHandle (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkName (poked :: VkImportSemaphoreWin32HandleInfoKHR))
-- | TODO: Struct comments
data VkExportSemaphoreWin32HandleInfoKHR = VkExportSemaphoreWin32HandleInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , vkDwAccess :: DWORD
  , vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkExportSemaphoreWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkExportSemaphoreWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkPAttributes (poked :: VkExportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkExportSemaphoreWin32HandleInfoKHR))
-- | TODO: Struct comments
data VkD3D12FenceSubmitInfoKHR = VkD3D12FenceSubmitInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkWaitSemaphoreValuesCount :: Word32
  , vkPWaitSemaphoreValues :: Ptr Word64
  , vkSignalSemaphoreValuesCount :: Word32
  , vkPSignalSemaphoreValues :: Ptr Word64
  }
  deriving (Eq, Show)

instance Storable VkD3D12FenceSubmitInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkD3D12FenceSubmitInfoKHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
                                       <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreValuesCount (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphoreValues (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSignalSemaphoreValuesCount (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkPSignalSemaphoreValues (poked :: VkD3D12FenceSubmitInfoKHR))
-- | TODO: Struct comments
data VkSemaphoreGetWin32HandleInfoKHR = VkSemaphoreGetWin32HandleInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkSemaphore :: VkSemaphore
  , vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkSemaphoreGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkSemaphoreGetWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSemaphoreGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSemaphoreGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkSemaphoreGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkSemaphoreGetWin32HandleInfoKHR))
