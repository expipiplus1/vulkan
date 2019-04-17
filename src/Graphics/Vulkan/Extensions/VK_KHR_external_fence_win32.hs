{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( withCStructExportFenceWin32HandleInfoKHR
  , fromCStructExportFenceWin32HandleInfoKHR
  , ExportFenceWin32HandleInfoKHR(..)
  , withCStructFenceGetWin32HandleInfoKHR
  , fromCStructFenceGetWin32HandleInfoKHR
  , FenceGetWin32HandleInfoKHR(..)
  , withCStructImportFenceWin32HandleInfoKHR
  , fromCStructImportFenceWin32HandleInfoKHR
  , ImportFenceWin32HandleInfoKHR(..)
  , getFenceWin32HandleKHR
  , importFenceWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
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
  ( getFenceWin32HandleKHR
  , importFenceWin32HandleKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( Fence
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( FenceImportFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceHandleTypeFlagBits
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  )


-- No documentation found for TopLevel "ExportFenceWin32HandleInfoKHR"
data ExportFenceWin32HandleInfoKHR = ExportFenceWin32HandleInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "pAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "dwAccess"
  vkDwAccess :: DWORD
  , -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
  }
  deriving (Show, Eq)
withCStructExportFenceWin32HandleInfoKHR :: ExportFenceWin32HandleInfoKHR -> (VkExportFenceWin32HandleInfoKHR -> IO a) -> IO a
withCStructExportFenceWin32HandleInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExportFenceWin32HandleInfoKHR)) (\pPNext -> cont (VkExportFenceWin32HandleInfoKHR VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR pPNext (vkPAttributes (from :: ExportFenceWin32HandleInfoKHR)) (vkDwAccess (from :: ExportFenceWin32HandleInfoKHR)) (vkName (from :: ExportFenceWin32HandleInfoKHR))))
fromCStructExportFenceWin32HandleInfoKHR :: VkExportFenceWin32HandleInfoKHR -> IO ExportFenceWin32HandleInfoKHR
fromCStructExportFenceWin32HandleInfoKHR c = ExportFenceWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportFenceWin32HandleInfoKHR)))
                                                                           <*> pure (vkPAttributes (c :: VkExportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkDwAccess (c :: VkExportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkName (c :: VkExportFenceWin32HandleInfoKHR))
instance Zero ExportFenceWin32HandleInfoKHR where
  zero = ExportFenceWin32HandleInfoKHR Nothing
                                       zero
                                       zero
                                       zero
-- No documentation found for TopLevel "FenceGetWin32HandleInfoKHR"
data FenceGetWin32HandleInfoKHR = FenceGetWin32HandleInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "FenceGetWin32HandleInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FenceGetWin32HandleInfoKHR" "fence"
  vkFence :: Fence
  , -- No documentation found for Nested "FenceGetWin32HandleInfoKHR" "handleType"
  vkHandleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructFenceGetWin32HandleInfoKHR :: FenceGetWin32HandleInfoKHR -> (VkFenceGetWin32HandleInfoKHR -> IO a) -> IO a
withCStructFenceGetWin32HandleInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: FenceGetWin32HandleInfoKHR)) (\pPNext -> cont (VkFenceGetWin32HandleInfoKHR VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR pPNext (vkFence (from :: FenceGetWin32HandleInfoKHR)) (vkHandleType (from :: FenceGetWin32HandleInfoKHR))))
fromCStructFenceGetWin32HandleInfoKHR :: VkFenceGetWin32HandleInfoKHR -> IO FenceGetWin32HandleInfoKHR
fromCStructFenceGetWin32HandleInfoKHR c = FenceGetWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFenceGetWin32HandleInfoKHR)))
                                                                     <*> pure (vkFence (c :: VkFenceGetWin32HandleInfoKHR))
                                                                     <*> pure (vkHandleType (c :: VkFenceGetWin32HandleInfoKHR))
instance Zero FenceGetWin32HandleInfoKHR where
  zero = FenceGetWin32HandleInfoKHR Nothing
                                    zero
                                    zero
-- No documentation found for TopLevel "ImportFenceWin32HandleInfoKHR"
data ImportFenceWin32HandleInfoKHR = ImportFenceWin32HandleInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "fence"
  vkFence :: Fence
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "flags"
  vkFlags :: FenceImportFlags
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "handleType"
  vkHandleType :: ExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "handle"
  vkHandle :: HANDLE
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
  }
  deriving (Show, Eq)
withCStructImportFenceWin32HandleInfoKHR :: ImportFenceWin32HandleInfoKHR -> (VkImportFenceWin32HandleInfoKHR -> IO a) -> IO a
withCStructImportFenceWin32HandleInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImportFenceWin32HandleInfoKHR)) (\pPNext -> cont (VkImportFenceWin32HandleInfoKHR VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR pPNext (vkFence (from :: ImportFenceWin32HandleInfoKHR)) (vkFlags (from :: ImportFenceWin32HandleInfoKHR)) (vkHandleType (from :: ImportFenceWin32HandleInfoKHR)) (vkHandle (from :: ImportFenceWin32HandleInfoKHR)) (vkName (from :: ImportFenceWin32HandleInfoKHR))))
fromCStructImportFenceWin32HandleInfoKHR :: VkImportFenceWin32HandleInfoKHR -> IO ImportFenceWin32HandleInfoKHR
fromCStructImportFenceWin32HandleInfoKHR c = ImportFenceWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportFenceWin32HandleInfoKHR)))
                                                                           <*> pure (vkFence (c :: VkImportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkFlags (c :: VkImportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkHandleType (c :: VkImportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkHandle (c :: VkImportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkName (c :: VkImportFenceWin32HandleInfoKHR))
instance Zero ImportFenceWin32HandleInfoKHR where
  zero = ImportFenceWin32HandleInfoKHR Nothing
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero

-- | Wrapper for 'vkGetFenceWin32HandleKHR'
getFenceWin32HandleKHR :: Device ->  FenceGetWin32HandleInfoKHR ->  IO (HANDLE)
getFenceWin32HandleKHR = \(Device device commandTable) -> \getWin32HandleInfo -> alloca (\pHandle -> (\a -> withCStructFenceGetWin32HandleInfoKHR a . flip with) getWin32HandleInfo (\pGetWin32HandleInfo -> Graphics.Vulkan.C.Dynamic.getFenceWin32HandleKHR commandTable device pGetWin32HandleInfo pHandle >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pHandle))))

-- | Wrapper for 'vkImportFenceWin32HandleKHR'
importFenceWin32HandleKHR :: Device ->  ImportFenceWin32HandleInfoKHR ->  IO ()
importFenceWin32HandleKHR = \(Device device commandTable) -> \importFenceWin32HandleInfo -> (\a -> withCStructImportFenceWin32HandleInfoKHR a . flip with) importFenceWin32HandleInfo (\pImportFenceWin32HandleInfo -> Graphics.Vulkan.C.Dynamic.importFenceWin32HandleKHR commandTable device pImportFenceWin32HandleInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))
