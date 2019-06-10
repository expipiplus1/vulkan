{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  , FN_vkGetFenceWin32HandleKHR
  , PFN_vkGetFenceWin32HandleKHR
  , vkGetFenceWin32HandleKHR
  , FN_vkImportFenceWin32HandleKHR
  , PFN_vkImportFenceWin32HandleKHR
  , vkImportFenceWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  ) where

import Data.String
  ( IsString
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
import Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
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


-- No documentation found for TopLevel "VkExportFenceWin32HandleInfoKHR"
data VkExportFenceWin32HandleInfoKHR = VkExportFenceWin32HandleInfoKHR
  { -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "pAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "dwAccess"
  vkDwAccess :: DWORD
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
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

instance Zero VkExportFenceWin32HandleInfoKHR where
  zero = VkExportFenceWin32HandleInfoKHR VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
                                         zero
                                         zero
                                         zero
                                         zero

-- No documentation found for TopLevel "VkFenceGetWin32HandleInfoKHR"
data VkFenceGetWin32HandleInfoKHR = VkFenceGetWin32HandleInfoKHR
  { -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "fence"
  vkFence :: VkFence
  , -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "handleType"
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
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

instance Zero VkFenceGetWin32HandleInfoKHR where
  zero = VkFenceGetWin32HandleInfoKHR VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
                                      zero
                                      zero
                                      zero

-- No documentation found for TopLevel "VkImportFenceWin32HandleInfoKHR"
data VkImportFenceWin32HandleInfoKHR = VkImportFenceWin32HandleInfoKHR
  { -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "fence"
  vkFence :: VkFence
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "flags"
  vkFlags :: VkFenceImportFlags
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "handleType"
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "handle"
  vkHandle :: HANDLE
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
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

instance Zero VkImportFenceWin32HandleInfoKHR where
  zero = VkImportFenceWin32HandleInfoKHR VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
                                         zero
                                         zero
                                         zero
                                         zero
                                         zero
                                         zero

-- No documentation found for TopLevel "vkGetFenceWin32HandleKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetFenceWin32HandleKHR" vkGetFenceWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
#else
vkGetFenceWin32HandleKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
vkGetFenceWin32HandleKHR deviceCmds = mkVkGetFenceWin32HandleKHR (pVkGetFenceWin32HandleKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceWin32HandleKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
#endif

type FN_vkGetFenceWin32HandleKHR = ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetFenceWin32HandleKHR = FunPtr FN_vkGetFenceWin32HandleKHR

-- No documentation found for TopLevel "vkImportFenceWin32HandleKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkImportFenceWin32HandleKHR" vkImportFenceWin32HandleKHR :: ("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult
#else
vkImportFenceWin32HandleKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult
vkImportFenceWin32HandleKHR deviceCmds = mkVkImportFenceWin32HandleKHR (pVkImportFenceWin32HandleKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportFenceWin32HandleKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult)
#endif

type FN_vkImportFenceWin32HandleKHR = ("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult
type PFN_vkImportFenceWin32HandleKHR = FunPtr FN_vkImportFenceWin32HandleKHR

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = "VK_KHR_external_fence_win32"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114000
