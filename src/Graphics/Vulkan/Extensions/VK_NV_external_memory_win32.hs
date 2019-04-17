{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( withCStructExportMemoryWin32HandleInfoNV
  , fromCStructExportMemoryWin32HandleInfoNV
  , ExportMemoryWin32HandleInfoNV(..)
  , withCStructImportMemoryWin32HandleInfoNV
  , fromCStructImportMemoryWin32HandleInfoNV
  , ImportMemoryWin32HandleInfoNV(..)
  , getMemoryWin32HandleNV
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getMemoryWin32HandleNV
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( VkExportMemoryWin32HandleInfoNV(..)
  , VkImportMemoryWin32HandleInfoNV(..)
  , DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagsNV
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  )


-- No documentation found for TopLevel "ExportMemoryWin32HandleInfoNV"
data ExportMemoryWin32HandleInfoNV = ExportMemoryWin32HandleInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "ExportMemoryWin32HandleInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoNV" "pAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoNV" "dwAccess"
  vkDwAccess :: DWORD
  }
  deriving (Show, Eq)
withCStructExportMemoryWin32HandleInfoNV :: ExportMemoryWin32HandleInfoNV -> (VkExportMemoryWin32HandleInfoNV -> IO a) -> IO a
withCStructExportMemoryWin32HandleInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExportMemoryWin32HandleInfoNV)) (\pPNext -> cont (VkExportMemoryWin32HandleInfoNV VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV pPNext (vkPAttributes (from :: ExportMemoryWin32HandleInfoNV)) (vkDwAccess (from :: ExportMemoryWin32HandleInfoNV))))
fromCStructExportMemoryWin32HandleInfoNV :: VkExportMemoryWin32HandleInfoNV -> IO ExportMemoryWin32HandleInfoNV
fromCStructExportMemoryWin32HandleInfoNV c = ExportMemoryWin32HandleInfoNV <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportMemoryWin32HandleInfoNV)))
                                                                           <*> pure (vkPAttributes (c :: VkExportMemoryWin32HandleInfoNV))
                                                                           <*> pure (vkDwAccess (c :: VkExportMemoryWin32HandleInfoNV))
instance Zero ExportMemoryWin32HandleInfoNV where
  zero = ExportMemoryWin32HandleInfoNV Nothing
                                       zero
                                       zero
-- No documentation found for TopLevel "ImportMemoryWin32HandleInfoNV"
data ImportMemoryWin32HandleInfoNV = ImportMemoryWin32HandleInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "ImportMemoryWin32HandleInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoNV" "handleType"
  vkHandleType :: ExternalMemoryHandleTypeFlagsNV
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoNV" "handle"
  vkHandle :: HANDLE
  }
  deriving (Show, Eq)
withCStructImportMemoryWin32HandleInfoNV :: ImportMemoryWin32HandleInfoNV -> (VkImportMemoryWin32HandleInfoNV -> IO a) -> IO a
withCStructImportMemoryWin32HandleInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImportMemoryWin32HandleInfoNV)) (\pPNext -> cont (VkImportMemoryWin32HandleInfoNV VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV pPNext (vkHandleType (from :: ImportMemoryWin32HandleInfoNV)) (vkHandle (from :: ImportMemoryWin32HandleInfoNV))))
fromCStructImportMemoryWin32HandleInfoNV :: VkImportMemoryWin32HandleInfoNV -> IO ImportMemoryWin32HandleInfoNV
fromCStructImportMemoryWin32HandleInfoNV c = ImportMemoryWin32HandleInfoNV <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportMemoryWin32HandleInfoNV)))
                                                                           <*> pure (vkHandleType (c :: VkImportMemoryWin32HandleInfoNV))
                                                                           <*> pure (vkHandle (c :: VkImportMemoryWin32HandleInfoNV))
instance Zero ImportMemoryWin32HandleInfoNV where
  zero = ImportMemoryWin32HandleInfoNV Nothing
                                       zero
                                       zero

-- | Wrapper for 'vkGetMemoryWin32HandleNV'
getMemoryWin32HandleNV :: Device ->  DeviceMemory ->  ExternalMemoryHandleTypeFlagsNV ->  IO (HANDLE)
getMemoryWin32HandleNV = \(Device device commandTable) -> \memory -> \handleType -> alloca (\pHandle -> Graphics.Vulkan.C.Dynamic.getMemoryWin32HandleNV commandTable device memory handleType pHandle >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pHandle)))
