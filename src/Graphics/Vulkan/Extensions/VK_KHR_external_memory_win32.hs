{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ExportMemoryWin32HandleInfoKHR(..)
  , 
  ImportMemoryWin32HandleInfoKHR(..)
  , MemoryGetWin32HandleInfoKHR(..)
  , MemoryWin32HandlePropertiesKHR(..)
#endif
  , getMemoryWin32HandleKHR
#if defined(VK_USE_PLATFORM_GGP)
  , getMemoryWin32HandlePropertiesKHR
#endif
  , pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
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
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( vkGetMemoryWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( vkGetMemoryWin32HandlePropertiesKHR
  )
#endif

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
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExportMemoryWin32HandleInfoKHR"
data ExportMemoryWin32HandleInfoKHR = ExportMemoryWin32HandleInfoKHR
  { -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "pAttributes"
  attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "dwAccess"
  dwAccess :: DWORD
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

instance Zero ExportMemoryWin32HandleInfoKHR where
  zero = ExportMemoryWin32HandleInfoKHR Nothing
                                        zero
                                        zero
                                        zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImportMemoryWin32HandleInfoKHR"
data ImportMemoryWin32HandleInfoKHR = ImportMemoryWin32HandleInfoKHR
  { -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "handle"
  handle :: HANDLE
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

instance Zero ImportMemoryWin32HandleInfoKHR where
  zero = ImportMemoryWin32HandleInfoKHR Nothing
                                        zero
                                        zero
                                        zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryGetWin32HandleInfoKHR"
data MemoryGetWin32HandleInfoKHR = MemoryGetWin32HandleInfoKHR
  { -- No documentation found for Nested "MemoryGetWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryGetWin32HandleInfoKHR" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "MemoryGetWin32HandleInfoKHR" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero MemoryGetWin32HandleInfoKHR where
  zero = MemoryGetWin32HandleInfoKHR Nothing
                                     zero
                                     zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryWin32HandlePropertiesKHR"
data MemoryWin32HandlePropertiesKHR = MemoryWin32HandlePropertiesKHR
  { -- No documentation found for Nested "MemoryWin32HandlePropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryWin32HandlePropertiesKHR" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

instance Zero MemoryWin32HandlePropertiesKHR where
  zero = MemoryWin32HandlePropertiesKHR Nothing
                                        zero

#endif


-- No documentation found for TopLevel "vkGetMemoryWin32HandleKHR"
getMemoryWin32HandleKHR :: Device ->  MemoryGetWin32HandleInfoKHR ->  IO (HANDLE)
getMemoryWin32HandleKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetMemoryWin32HandlePropertiesKHR"
getMemoryWin32HandlePropertiesKHR :: Device ->  ExternalMemoryHandleTypeFlagBits ->  HANDLE ->  IO (MemoryWin32HandlePropertiesKHR)
getMemoryWin32HandlePropertiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
