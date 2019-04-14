{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( VkD3D12FenceSubmitInfoKHR(..)
  , VkExportSemaphoreWin32HandleInfoKHR(..)
  , VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetSemaphoreWin32HandleKHR
#endif
  , FN_vkGetSemaphoreWin32HandleKHR
  , PFN_vkGetSemaphoreWin32HandleKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkImportSemaphoreWin32HandleKHR
#endif
  , FN_vkImportSemaphoreWin32HandleKHR
  , PFN_vkImportSemaphoreWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
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
import Graphics.Vulkan.C.Core10.Queue
  ( VkSemaphore
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkSemaphoreImportFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkD3D12FenceSubmitInfoKHR"
data VkD3D12FenceSubmitInfoKHR = VkD3D12FenceSubmitInfoKHR
  { -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "waitSemaphoreValuesCount"
  vkWaitSemaphoreValuesCount :: Word32
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "pWaitSemaphoreValues"
  vkPWaitSemaphoreValues :: Ptr Word64
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "signalSemaphoreValuesCount"
  vkSignalSemaphoreValuesCount :: Word32
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "pSignalSemaphoreValues"
  vkPSignalSemaphoreValues :: Ptr Word64
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
-- No documentation found for TopLevel "VkExportSemaphoreWin32HandleInfoKHR"
data VkExportSemaphoreWin32HandleInfoKHR = VkExportSemaphoreWin32HandleInfoKHR
  { -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "pAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "dwAccess"
  vkDwAccess :: DWORD
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
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
-- No documentation found for TopLevel "VkImportSemaphoreWin32HandleInfoKHR"
data VkImportSemaphoreWin32HandleInfoKHR = VkImportSemaphoreWin32HandleInfoKHR
  { -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "semaphore"
  vkSemaphore :: VkSemaphore
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "flags"
  vkFlags :: VkSemaphoreImportFlags
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "handleType"
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "handle"
  vkHandle :: HANDLE
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
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
-- No documentation found for TopLevel "VkSemaphoreGetWin32HandleInfoKHR"
data VkSemaphoreGetWin32HandleInfoKHR = VkSemaphoreGetWin32HandleInfoKHR
  { -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "semaphore"
  vkSemaphore :: VkSemaphore
  , -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "handleType"
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetSemaphoreWin32HandleKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetSemaphoreWin32HandleKHR" vkGetSemaphoreWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult

#endif
type FN_vkGetSemaphoreWin32HandleKHR = ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetSemaphoreWin32HandleKHR = FunPtr FN_vkGetSemaphoreWin32HandleKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkImportSemaphoreWin32HandleKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkImportSemaphoreWin32HandleKHR" vkImportSemaphoreWin32HandleKHR :: ("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult

#endif
type FN_vkImportSemaphoreWin32HandleKHR = ("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult
type PFN_vkImportSemaphoreWin32HandleKHR = FunPtr FN_vkImportSemaphoreWin32HandleKHR
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME = "VK_KHR_external_semaphore_win32"
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR"
pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR = VkStructureType 1000078002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000078001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000078000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000078003
