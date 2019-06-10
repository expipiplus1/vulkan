{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ExportFenceWin32HandleInfoKHR(..)
  , 
  FenceGetWin32HandleInfoKHR(..)
  , ImportFenceWin32HandleInfoKHR(..)
#endif
  , getFenceWin32HandleKHR
  , importFenceWin32HandleKHR
  , pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  ) where

import Data.String
  ( IsString
  )
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
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( vkGetFenceWin32HandleKHR
  , vkImportFenceWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
#endif
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
  ( Fence
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( FenceImportFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceHandleTypeFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExportFenceWin32HandleInfoKHR"
data ExportFenceWin32HandleInfoKHR = ExportFenceWin32HandleInfoKHR
  { -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "pAttributes"
  attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "dwAccess"
  dwAccess :: DWORD
  , -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

instance Zero ExportFenceWin32HandleInfoKHR where
  zero = ExportFenceWin32HandleInfoKHR Nothing
                                       zero
                                       zero
                                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkFenceGetWin32HandleInfoKHR"
data FenceGetWin32HandleInfoKHR = FenceGetWin32HandleInfoKHR
  { -- No documentation found for Nested "FenceGetWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FenceGetWin32HandleInfoKHR" "fence"
  fence :: Fence
  , -- No documentation found for Nested "FenceGetWin32HandleInfoKHR" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero FenceGetWin32HandleInfoKHR where
  zero = FenceGetWin32HandleInfoKHR Nothing
                                    zero
                                    zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImportFenceWin32HandleInfoKHR"
data ImportFenceWin32HandleInfoKHR = ImportFenceWin32HandleInfoKHR
  { -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "fence"
  fence :: Fence
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "flags"
  flags :: FenceImportFlags
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "handle"
  handle :: HANDLE
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

instance Zero ImportFenceWin32HandleInfoKHR where
  zero = ImportFenceWin32HandleInfoKHR Nothing
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero

#endif


-- No documentation found for TopLevel "vkGetFenceWin32HandleKHR"
getFenceWin32HandleKHR :: Device ->  FenceGetWin32HandleInfoKHR ->  IO (HANDLE)
getFenceWin32HandleKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkImportFenceWin32HandleKHR"
importFenceWin32HandleKHR :: Device ->  ImportFenceWin32HandleInfoKHR ->  IO ()
importFenceWin32HandleKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
