{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( withCStructD3D12FenceSubmitInfoKHR
  , fromCStructD3D12FenceSubmitInfoKHR
  , D3D12FenceSubmitInfoKHR(..)
  , withCStructExportSemaphoreWin32HandleInfoKHR
  , fromCStructExportSemaphoreWin32HandleInfoKHR
  , ExportSemaphoreWin32HandleInfoKHR(..)
  , withCStructImportSemaphoreWin32HandleInfoKHR
  , fromCStructImportSemaphoreWin32HandleInfoKHR
  , ImportSemaphoreWin32HandleInfoKHR(..)
  , withCStructSemaphoreGetWin32HandleInfoKHR
  , fromCStructSemaphoreGetWin32HandleInfoKHR
  , SemaphoreGetWin32HandleInfoKHR(..)
  , getSemaphoreWin32HandleKHR
  , importSemaphoreWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Function
  ( (&)
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word64
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
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getSemaphoreWin32HandleKHR
  , importSemaphoreWin32HandleKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( VkD3D12FenceSubmitInfoKHR(..)
  , VkExportSemaphoreWin32HandleInfoKHR(..)
  , VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
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
  ( Semaphore
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( SemaphoreImportFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreHandleTypeFlagBits
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
  )


-- No documentation found for TopLevel "D3D12FenceSubmitInfoKHR"
data D3D12FenceSubmitInfoKHR = D3D12FenceSubmitInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "D3D12FenceSubmitInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "D3D12FenceSubmitInfoKHR" "pWaitSemaphoreValues"
  vkPWaitSemaphoreValues :: Maybe (Vector Word64)
  -- Optional length valued member elided
  , -- No documentation found for Nested "D3D12FenceSubmitInfoKHR" "pSignalSemaphoreValues"
  vkPSignalSemaphoreValues :: Maybe (Vector Word64)
  }
  deriving (Show, Eq)
withCStructD3D12FenceSubmitInfoKHR :: D3D12FenceSubmitInfoKHR -> (VkD3D12FenceSubmitInfoKHR -> IO a) -> IO a
withCStructD3D12FenceSubmitInfoKHR from cont = maybeWith (withVec (&)) (vkPSignalSemaphoreValues (from :: D3D12FenceSubmitInfoKHR)) (\pSignalSemaphoreValues -> maybeWith (withVec (&)) (vkPWaitSemaphoreValues (from :: D3D12FenceSubmitInfoKHR)) (\pWaitSemaphoreValues -> maybeWith withSomeVkStruct (vkPNext (from :: D3D12FenceSubmitInfoKHR)) (\pPNext -> cont (VkD3D12FenceSubmitInfoKHR VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR pPNext (maybe 0 (fromIntegral . Data.Vector.length) (vkPWaitSemaphoreValues (from :: D3D12FenceSubmitInfoKHR))) pWaitSemaphoreValues (maybe 0 (fromIntegral . Data.Vector.length) (vkPSignalSemaphoreValues (from :: D3D12FenceSubmitInfoKHR))) pSignalSemaphoreValues))))
fromCStructD3D12FenceSubmitInfoKHR :: VkD3D12FenceSubmitInfoKHR -> IO D3D12FenceSubmitInfoKHR
fromCStructD3D12FenceSubmitInfoKHR c = D3D12FenceSubmitInfoKHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkD3D12FenceSubmitInfoKHR)))
                                                               -- Optional length valued member elided
                                                               <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkWaitSemaphoreValuesCount (c :: VkD3D12FenceSubmitInfoKHR))) (peekElemOff p)) (vkPWaitSemaphoreValues (c :: VkD3D12FenceSubmitInfoKHR))
                                                               -- Optional length valued member elided
                                                               <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkSignalSemaphoreValuesCount (c :: VkD3D12FenceSubmitInfoKHR))) (peekElemOff p)) (vkPSignalSemaphoreValues (c :: VkD3D12FenceSubmitInfoKHR))
instance Zero D3D12FenceSubmitInfoKHR where
  zero = D3D12FenceSubmitInfoKHR Nothing
                                 Nothing
                                 Nothing
-- No documentation found for TopLevel "ExportSemaphoreWin32HandleInfoKHR"
data ExportSemaphoreWin32HandleInfoKHR = ExportSemaphoreWin32HandleInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "pAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "dwAccess"
  vkDwAccess :: DWORD
  , -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
  }
  deriving (Show, Eq)
withCStructExportSemaphoreWin32HandleInfoKHR :: ExportSemaphoreWin32HandleInfoKHR -> (VkExportSemaphoreWin32HandleInfoKHR -> IO a) -> IO a
withCStructExportSemaphoreWin32HandleInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExportSemaphoreWin32HandleInfoKHR)) (\pPNext -> cont (VkExportSemaphoreWin32HandleInfoKHR VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR pPNext (vkPAttributes (from :: ExportSemaphoreWin32HandleInfoKHR)) (vkDwAccess (from :: ExportSemaphoreWin32HandleInfoKHR)) (vkName (from :: ExportSemaphoreWin32HandleInfoKHR))))
fromCStructExportSemaphoreWin32HandleInfoKHR :: VkExportSemaphoreWin32HandleInfoKHR -> IO ExportSemaphoreWin32HandleInfoKHR
fromCStructExportSemaphoreWin32HandleInfoKHR c = ExportSemaphoreWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportSemaphoreWin32HandleInfoKHR)))
                                                                                   <*> pure (vkPAttributes (c :: VkExportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkDwAccess (c :: VkExportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkName (c :: VkExportSemaphoreWin32HandleInfoKHR))
instance Zero ExportSemaphoreWin32HandleInfoKHR where
  zero = ExportSemaphoreWin32HandleInfoKHR Nothing
                                           zero
                                           zero
                                           zero
-- No documentation found for TopLevel "ImportSemaphoreWin32HandleInfoKHR"
data ImportSemaphoreWin32HandleInfoKHR = ImportSemaphoreWin32HandleInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "semaphore"
  vkSemaphore :: Semaphore
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "flags"
  vkFlags :: SemaphoreImportFlags
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "handleType"
  vkHandleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "handle"
  vkHandle :: HANDLE
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "name"
  vkName :: LPCWSTR
  }
  deriving (Show, Eq)
withCStructImportSemaphoreWin32HandleInfoKHR :: ImportSemaphoreWin32HandleInfoKHR -> (VkImportSemaphoreWin32HandleInfoKHR -> IO a) -> IO a
withCStructImportSemaphoreWin32HandleInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImportSemaphoreWin32HandleInfoKHR)) (\pPNext -> cont (VkImportSemaphoreWin32HandleInfoKHR VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR pPNext (vkSemaphore (from :: ImportSemaphoreWin32HandleInfoKHR)) (vkFlags (from :: ImportSemaphoreWin32HandleInfoKHR)) (vkHandleType (from :: ImportSemaphoreWin32HandleInfoKHR)) (vkHandle (from :: ImportSemaphoreWin32HandleInfoKHR)) (vkName (from :: ImportSemaphoreWin32HandleInfoKHR))))
fromCStructImportSemaphoreWin32HandleInfoKHR :: VkImportSemaphoreWin32HandleInfoKHR -> IO ImportSemaphoreWin32HandleInfoKHR
fromCStructImportSemaphoreWin32HandleInfoKHR c = ImportSemaphoreWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportSemaphoreWin32HandleInfoKHR)))
                                                                                   <*> pure (vkSemaphore (c :: VkImportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkFlags (c :: VkImportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkHandleType (c :: VkImportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkHandle (c :: VkImportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkName (c :: VkImportSemaphoreWin32HandleInfoKHR))
instance Zero ImportSemaphoreWin32HandleInfoKHR where
  zero = ImportSemaphoreWin32HandleInfoKHR Nothing
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero
-- No documentation found for TopLevel "SemaphoreGetWin32HandleInfoKHR"
data SemaphoreGetWin32HandleInfoKHR = SemaphoreGetWin32HandleInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SemaphoreGetWin32HandleInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SemaphoreGetWin32HandleInfoKHR" "semaphore"
  vkSemaphore :: Semaphore
  , -- No documentation found for Nested "SemaphoreGetWin32HandleInfoKHR" "handleType"
  vkHandleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructSemaphoreGetWin32HandleInfoKHR :: SemaphoreGetWin32HandleInfoKHR -> (VkSemaphoreGetWin32HandleInfoKHR -> IO a) -> IO a
withCStructSemaphoreGetWin32HandleInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: SemaphoreGetWin32HandleInfoKHR)) (\pPNext -> cont (VkSemaphoreGetWin32HandleInfoKHR VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR pPNext (vkSemaphore (from :: SemaphoreGetWin32HandleInfoKHR)) (vkHandleType (from :: SemaphoreGetWin32HandleInfoKHR))))
fromCStructSemaphoreGetWin32HandleInfoKHR :: VkSemaphoreGetWin32HandleInfoKHR -> IO SemaphoreGetWin32HandleInfoKHR
fromCStructSemaphoreGetWin32HandleInfoKHR c = SemaphoreGetWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSemaphoreGetWin32HandleInfoKHR)))
                                                                             <*> pure (vkSemaphore (c :: VkSemaphoreGetWin32HandleInfoKHR))
                                                                             <*> pure (vkHandleType (c :: VkSemaphoreGetWin32HandleInfoKHR))
instance Zero SemaphoreGetWin32HandleInfoKHR where
  zero = SemaphoreGetWin32HandleInfoKHR Nothing
                                        zero
                                        zero

-- | Wrapper for 'vkGetSemaphoreWin32HandleKHR'
getSemaphoreWin32HandleKHR :: Device ->  SemaphoreGetWin32HandleInfoKHR ->  IO (HANDLE)
getSemaphoreWin32HandleKHR = \(Device device commandTable) -> \getWin32HandleInfo -> alloca (\pHandle -> (\a -> withCStructSemaphoreGetWin32HandleInfoKHR a . flip with) getWin32HandleInfo (\pGetWin32HandleInfo -> Graphics.Vulkan.C.Dynamic.getSemaphoreWin32HandleKHR commandTable device pGetWin32HandleInfo pHandle >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pHandle))))

-- | Wrapper for 'vkImportSemaphoreWin32HandleKHR'
importSemaphoreWin32HandleKHR :: Device ->  ImportSemaphoreWin32HandleInfoKHR ->  IO ()
importSemaphoreWin32HandleKHR = \(Device device commandTable) -> \importSemaphoreWin32HandleInfo -> (\a -> withCStructImportSemaphoreWin32HandleInfoKHR a . flip with) importSemaphoreWin32HandleInfo (\pImportSemaphoreWin32HandleInfo -> Graphics.Vulkan.C.Dynamic.importSemaphoreWin32HandleKHR commandTable device pImportSemaphoreWin32HandleInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))
