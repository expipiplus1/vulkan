{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ExportMemoryWin32HandleInfoNV(..)
  , 
  ImportMemoryWin32HandleInfoNV(..)
#endif
  , getMemoryWin32HandleNV
  , pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Alloc
  ( alloca
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
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( HANDLE
  , vkGetMemoryWin32HandleNV
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagsNV
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExportMemoryWin32HandleInfoNV"
data ExportMemoryWin32HandleInfoNV = ExportMemoryWin32HandleInfoNV
  { -- No documentation found for Nested "ExportMemoryWin32HandleInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoNV" "pAttributes"
  attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoNV" "dwAccess"
  dwAccess :: DWORD
  }
  deriving (Show, Eq)

instance Zero ExportMemoryWin32HandleInfoNV where
  zero = ExportMemoryWin32HandleInfoNV Nothing
                                       zero
                                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImportMemoryWin32HandleInfoNV"
data ImportMemoryWin32HandleInfoNV = ImportMemoryWin32HandleInfoNV
  { -- No documentation found for Nested "ImportMemoryWin32HandleInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoNV" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagsNV
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoNV" "handle"
  handle :: HANDLE
  }
  deriving (Show, Eq)

instance Zero ImportMemoryWin32HandleInfoNV where
  zero = ImportMemoryWin32HandleInfoNV Nothing
                                       zero
                                       zero

#endif


-- No documentation found for TopLevel "vkGetMemoryWin32HandleNV"
getMemoryWin32HandleNV :: Device ->  DeviceMemory ->  ExternalMemoryHandleTypeFlagsNV ->  IO (HANDLE)
getMemoryWin32HandleNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
