{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  D3D12FenceSubmitInfoKHR(..)
  , 
  ExportSemaphoreWin32HandleInfoKHR(..)
  , ImportSemaphoreWin32HandleInfoKHR(..)
  , SemaphoreGetWin32HandleInfoKHR(..)
#endif
  , getSemaphoreWin32HandleKHR
  , importSemaphoreWin32HandleKHR
  , pattern KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  , pattern KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  , Word64
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( with
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( Ptr
  )
#endif
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( vkGetSemaphoreWin32HandleKHR
  , vkImportSemaphoreWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( HANDLE
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , SECURITY_ATTRIBUTES
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Queue
  ( Semaphore
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( SemaphoreImportFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreHandleTypeFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkD3D12FenceSubmitInfoKHR"
data D3D12FenceSubmitInfoKHR = D3D12FenceSubmitInfoKHR
  { -- No documentation found for Nested "D3D12FenceSubmitInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "D3D12FenceSubmitInfoKHR" "pWaitSemaphoreValues"
  waitSemaphoreValues :: Either Word32 (Vector Word64)
  , -- No documentation found for Nested "D3D12FenceSubmitInfoKHR" "pSignalSemaphoreValues"
  signalSemaphoreValues :: Either Word32 (Vector Word64)
  }
  deriving (Show, Eq)

instance Zero D3D12FenceSubmitInfoKHR where
  zero = D3D12FenceSubmitInfoKHR Nothing
                                 (Left 0)
                                 (Left 0)

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExportSemaphoreWin32HandleInfoKHR"
data ExportSemaphoreWin32HandleInfoKHR = ExportSemaphoreWin32HandleInfoKHR
  { -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "pAttributes"
  attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "dwAccess"
  dwAccess :: DWORD
  , -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

instance Zero ExportSemaphoreWin32HandleInfoKHR where
  zero = ExportSemaphoreWin32HandleInfoKHR Nothing
                                           zero
                                           zero
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImportSemaphoreWin32HandleInfoKHR"
data ImportSemaphoreWin32HandleInfoKHR = ImportSemaphoreWin32HandleInfoKHR
  { -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "semaphore"
  semaphore :: Semaphore
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "flags"
  flags :: SemaphoreImportFlags
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "handle"
  handle :: HANDLE
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

instance Zero ImportSemaphoreWin32HandleInfoKHR where
  zero = ImportSemaphoreWin32HandleInfoKHR Nothing
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSemaphoreGetWin32HandleInfoKHR"
data SemaphoreGetWin32HandleInfoKHR = SemaphoreGetWin32HandleInfoKHR
  { -- No documentation found for Nested "SemaphoreGetWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SemaphoreGetWin32HandleInfoKHR" "semaphore"
  semaphore :: Semaphore
  , -- No documentation found for Nested "SemaphoreGetWin32HandleInfoKHR" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero SemaphoreGetWin32HandleInfoKHR where
  zero = SemaphoreGetWin32HandleInfoKHR Nothing
                                        zero
                                        zero

#endif


-- No documentation found for TopLevel "vkGetSemaphoreWin32HandleKHR"
getSemaphoreWin32HandleKHR :: Device ->  SemaphoreGetWin32HandleInfoKHR ->  IO (HANDLE)
getSemaphoreWin32HandleKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkImportSemaphoreWin32HandleKHR"
importSemaphoreWin32HandleKHR :: Device ->  ImportSemaphoreWin32HandleInfoKHR ->  IO ()
importSemaphoreWin32HandleKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME = VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
