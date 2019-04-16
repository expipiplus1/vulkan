{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  ( withCStructImportSemaphoreFdInfoKHR
  , fromCStructImportSemaphoreFdInfoKHR
  , ImportSemaphoreFdInfoKHR(..)
  , withCStructSemaphoreGetFdInfoKHR
  , fromCStructSemaphoreGetFdInfoKHR
  , SemaphoreGetFdInfoKHR(..)
  , getSemaphoreFdKHR
  , importSemaphoreFdKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.C.Types
  ( CInt(..)
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
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getSemaphoreFdKHR
  , importSemaphoreFdKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
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
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
  )


-- No documentation found for TopLevel "ImportSemaphoreFdInfoKHR"
data ImportSemaphoreFdInfoKHR = ImportSemaphoreFdInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "semaphore"
  vkSemaphore :: Semaphore
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "flags"
  vkFlags :: SemaphoreImportFlags
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "handleType"
  vkHandleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "fd"
  vkFd :: CInt
  }
  deriving (Show, Eq)
withCStructImportSemaphoreFdInfoKHR :: ImportSemaphoreFdInfoKHR -> (VkImportSemaphoreFdInfoKHR -> IO a) -> IO a
withCStructImportSemaphoreFdInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImportSemaphoreFdInfoKHR)) (\pPNext -> cont (VkImportSemaphoreFdInfoKHR VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR pPNext (vkSemaphore (from :: ImportSemaphoreFdInfoKHR)) (vkFlags (from :: ImportSemaphoreFdInfoKHR)) (vkHandleType (from :: ImportSemaphoreFdInfoKHR)) (vkFd (from :: ImportSemaphoreFdInfoKHR))))
fromCStructImportSemaphoreFdInfoKHR :: VkImportSemaphoreFdInfoKHR -> IO ImportSemaphoreFdInfoKHR
fromCStructImportSemaphoreFdInfoKHR c = ImportSemaphoreFdInfoKHR <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportSemaphoreFdInfoKHR)))
                                                                 <*> pure (vkSemaphore (c :: VkImportSemaphoreFdInfoKHR))
                                                                 <*> pure (vkFlags (c :: VkImportSemaphoreFdInfoKHR))
                                                                 <*> pure (vkHandleType (c :: VkImportSemaphoreFdInfoKHR))
                                                                 <*> pure (vkFd (c :: VkImportSemaphoreFdInfoKHR))
-- No documentation found for TopLevel "SemaphoreGetFdInfoKHR"
data SemaphoreGetFdInfoKHR = SemaphoreGetFdInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SemaphoreGetFdInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SemaphoreGetFdInfoKHR" "semaphore"
  vkSemaphore :: Semaphore
  , -- No documentation found for Nested "SemaphoreGetFdInfoKHR" "handleType"
  vkHandleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructSemaphoreGetFdInfoKHR :: SemaphoreGetFdInfoKHR -> (VkSemaphoreGetFdInfoKHR -> IO a) -> IO a
withCStructSemaphoreGetFdInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: SemaphoreGetFdInfoKHR)) (\pPNext -> cont (VkSemaphoreGetFdInfoKHR VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR pPNext (vkSemaphore (from :: SemaphoreGetFdInfoKHR)) (vkHandleType (from :: SemaphoreGetFdInfoKHR))))
fromCStructSemaphoreGetFdInfoKHR :: VkSemaphoreGetFdInfoKHR -> IO SemaphoreGetFdInfoKHR
fromCStructSemaphoreGetFdInfoKHR c = SemaphoreGetFdInfoKHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSemaphoreGetFdInfoKHR)))
                                                           <*> pure (vkSemaphore (c :: VkSemaphoreGetFdInfoKHR))
                                                           <*> pure (vkHandleType (c :: VkSemaphoreGetFdInfoKHR))

-- | Wrapper for 'vkGetSemaphoreFdKHR'
getSemaphoreFdKHR :: Device ->  SemaphoreGetFdInfoKHR ->  IO (CInt)
getSemaphoreFdKHR = \(Device device commandTable) -> \getFdInfo -> alloca (\pFd -> (\a -> withCStructSemaphoreGetFdInfoKHR a . flip with) getFdInfo (\pGetFdInfo -> Graphics.Vulkan.C.Dynamic.getSemaphoreFdKHR commandTable device pGetFdInfo pFd >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pFd))))

-- | Wrapper for 'vkImportSemaphoreFdKHR'
importSemaphoreFdKHR :: Device ->  ImportSemaphoreFdInfoKHR ->  IO ()
importSemaphoreFdKHR = \(Device device commandTable) -> \importSemaphoreFdInfo -> (\a -> withCStructImportSemaphoreFdInfoKHR a . flip with) importSemaphoreFdInfo (\pImportSemaphoreFdInfo -> Graphics.Vulkan.C.Dynamic.importSemaphoreFdKHR commandTable device pImportSemaphoreFdInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))
