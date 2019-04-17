{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( withCStructExportMemoryWin32HandleInfoKHR
  , fromCStructExportMemoryWin32HandleInfoKHR
  , ExportMemoryWin32HandleInfoKHR(..)
  , withCStructImportMemoryWin32HandleInfoKHR
  , fromCStructImportMemoryWin32HandleInfoKHR
  , ImportMemoryWin32HandleInfoKHR(..)
  , withCStructMemoryGetWin32HandleInfoKHR
  , fromCStructMemoryGetWin32HandleInfoKHR
  , MemoryGetWin32HandleInfoKHR(..)
  , withCStructMemoryWin32HandlePropertiesKHR
  , fromCStructMemoryWin32HandlePropertiesKHR
  , MemoryWin32HandlePropertiesKHR(..)
  , getMemoryWin32HandleKHR
  , getMemoryWin32HandlePropertiesKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getMemoryWin32HandleKHR
  , getMemoryWin32HandlePropertiesKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( VkExportMemoryWin32HandleInfoKHR(..)
  , VkImportMemoryWin32HandleInfoKHR(..)
  , VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
  , LPCWSTR
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagBits
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
  )


-- No documentation found for TopLevel "ExportMemoryWin32HandleInfoKHR"
data ExportMemoryWin32HandleInfoKHR = ExportMemoryWin32HandleInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "pAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "dwAccess"
  vkDwAccess :: DWORD
  , -- No documentation found for Nested "ExportMemoryWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
  }
  deriving (Show, Eq)
withCStructExportMemoryWin32HandleInfoKHR :: ExportMemoryWin32HandleInfoKHR -> (VkExportMemoryWin32HandleInfoKHR -> IO a) -> IO a
withCStructExportMemoryWin32HandleInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExportMemoryWin32HandleInfoKHR)) (\pPNext -> cont (VkExportMemoryWin32HandleInfoKHR VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR pPNext (vkPAttributes (from :: ExportMemoryWin32HandleInfoKHR)) (vkDwAccess (from :: ExportMemoryWin32HandleInfoKHR)) (vkName (from :: ExportMemoryWin32HandleInfoKHR))))
fromCStructExportMemoryWin32HandleInfoKHR :: VkExportMemoryWin32HandleInfoKHR -> IO ExportMemoryWin32HandleInfoKHR
fromCStructExportMemoryWin32HandleInfoKHR c = ExportMemoryWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportMemoryWin32HandleInfoKHR)))
                                                                             <*> pure (vkPAttributes (c :: VkExportMemoryWin32HandleInfoKHR))
                                                                             <*> pure (vkDwAccess (c :: VkExportMemoryWin32HandleInfoKHR))
                                                                             <*> pure (vkName (c :: VkExportMemoryWin32HandleInfoKHR))
instance Zero ExportMemoryWin32HandleInfoKHR where
  zero = ExportMemoryWin32HandleInfoKHR Nothing
                                        zero
                                        zero
                                        zero
-- No documentation found for TopLevel "ImportMemoryWin32HandleInfoKHR"
data ImportMemoryWin32HandleInfoKHR = ImportMemoryWin32HandleInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "handleType"
  vkHandleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "handle"
  vkHandle :: HANDLE
  , -- No documentation found for Nested "ImportMemoryWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
  }
  deriving (Show, Eq)
withCStructImportMemoryWin32HandleInfoKHR :: ImportMemoryWin32HandleInfoKHR -> (VkImportMemoryWin32HandleInfoKHR -> IO a) -> IO a
withCStructImportMemoryWin32HandleInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImportMemoryWin32HandleInfoKHR)) (\pPNext -> cont (VkImportMemoryWin32HandleInfoKHR VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR pPNext (vkHandleType (from :: ImportMemoryWin32HandleInfoKHR)) (vkHandle (from :: ImportMemoryWin32HandleInfoKHR)) (vkName (from :: ImportMemoryWin32HandleInfoKHR))))
fromCStructImportMemoryWin32HandleInfoKHR :: VkImportMemoryWin32HandleInfoKHR -> IO ImportMemoryWin32HandleInfoKHR
fromCStructImportMemoryWin32HandleInfoKHR c = ImportMemoryWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportMemoryWin32HandleInfoKHR)))
                                                                             <*> pure (vkHandleType (c :: VkImportMemoryWin32HandleInfoKHR))
                                                                             <*> pure (vkHandle (c :: VkImportMemoryWin32HandleInfoKHR))
                                                                             <*> pure (vkName (c :: VkImportMemoryWin32HandleInfoKHR))
instance Zero ImportMemoryWin32HandleInfoKHR where
  zero = ImportMemoryWin32HandleInfoKHR Nothing
                                        zero
                                        zero
                                        zero
-- No documentation found for TopLevel "MemoryGetWin32HandleInfoKHR"
data MemoryGetWin32HandleInfoKHR = MemoryGetWin32HandleInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryGetWin32HandleInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryGetWin32HandleInfoKHR" "memory"
  vkMemory :: DeviceMemory
  , -- No documentation found for Nested "MemoryGetWin32HandleInfoKHR" "handleType"
  vkHandleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructMemoryGetWin32HandleInfoKHR :: MemoryGetWin32HandleInfoKHR -> (VkMemoryGetWin32HandleInfoKHR -> IO a) -> IO a
withCStructMemoryGetWin32HandleInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryGetWin32HandleInfoKHR)) (\pPNext -> cont (VkMemoryGetWin32HandleInfoKHR VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR pPNext (vkMemory (from :: MemoryGetWin32HandleInfoKHR)) (vkHandleType (from :: MemoryGetWin32HandleInfoKHR))))
fromCStructMemoryGetWin32HandleInfoKHR :: VkMemoryGetWin32HandleInfoKHR -> IO MemoryGetWin32HandleInfoKHR
fromCStructMemoryGetWin32HandleInfoKHR c = MemoryGetWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryGetWin32HandleInfoKHR)))
                                                                       <*> pure (vkMemory (c :: VkMemoryGetWin32HandleInfoKHR))
                                                                       <*> pure (vkHandleType (c :: VkMemoryGetWin32HandleInfoKHR))
instance Zero MemoryGetWin32HandleInfoKHR where
  zero = MemoryGetWin32HandleInfoKHR Nothing
                                     zero
                                     zero
-- No documentation found for TopLevel "MemoryWin32HandlePropertiesKHR"
data MemoryWin32HandlePropertiesKHR = MemoryWin32HandlePropertiesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryWin32HandlePropertiesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryWin32HandlePropertiesKHR" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Show, Eq)
withCStructMemoryWin32HandlePropertiesKHR :: MemoryWin32HandlePropertiesKHR -> (VkMemoryWin32HandlePropertiesKHR -> IO a) -> IO a
withCStructMemoryWin32HandlePropertiesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryWin32HandlePropertiesKHR)) (\pPNext -> cont (VkMemoryWin32HandlePropertiesKHR VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR pPNext (vkMemoryTypeBits (from :: MemoryWin32HandlePropertiesKHR))))
fromCStructMemoryWin32HandlePropertiesKHR :: VkMemoryWin32HandlePropertiesKHR -> IO MemoryWin32HandlePropertiesKHR
fromCStructMemoryWin32HandlePropertiesKHR c = MemoryWin32HandlePropertiesKHR <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryWin32HandlePropertiesKHR)))
                                                                             <*> pure (vkMemoryTypeBits (c :: VkMemoryWin32HandlePropertiesKHR))
instance Zero MemoryWin32HandlePropertiesKHR where
  zero = MemoryWin32HandlePropertiesKHR Nothing
                                        zero

-- | Wrapper for 'vkGetMemoryWin32HandleKHR'
getMemoryWin32HandleKHR :: Device ->  MemoryGetWin32HandleInfoKHR ->  IO (HANDLE)
getMemoryWin32HandleKHR = \(Device device commandTable) -> \getWin32HandleInfo -> alloca (\pHandle -> (\a -> withCStructMemoryGetWin32HandleInfoKHR a . flip with) getWin32HandleInfo (\pGetWin32HandleInfo -> Graphics.Vulkan.C.Dynamic.getMemoryWin32HandleKHR commandTable device pGetWin32HandleInfo pHandle >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pHandle))))

-- | Wrapper for 'vkGetMemoryWin32HandlePropertiesKHR'
getMemoryWin32HandlePropertiesKHR :: Device ->  ExternalMemoryHandleTypeFlagBits ->  HANDLE ->  IO (MemoryWin32HandlePropertiesKHR)
getMemoryWin32HandlePropertiesKHR = \(Device device commandTable) -> \handleType -> \handle -> alloca (\pMemoryWin32HandleProperties -> Graphics.Vulkan.C.Dynamic.getMemoryWin32HandlePropertiesKHR commandTable device handleType handle pMemoryWin32HandleProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructMemoryWin32HandlePropertiesKHR <=< peek) pMemoryWin32HandleProperties)))
