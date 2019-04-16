{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( withCStructFenceGetFdInfoKHR
  , fromCStructFenceGetFdInfoKHR
  , FenceGetFdInfoKHR(..)
  , withCStructImportFenceFdInfoKHR
  , fromCStructImportFenceFdInfoKHR
  , ImportFenceFdInfoKHR(..)
  , getFenceFdKHR
  , importFenceFdKHR
  , pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
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
  ( getFenceFdKHR
  , importFenceFdKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
  )


-- No documentation found for TopLevel "FenceGetFdInfoKHR"
data FenceGetFdInfoKHR = FenceGetFdInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "FenceGetFdInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FenceGetFdInfoKHR" "fence"
  vkFence :: Fence
  , -- No documentation found for Nested "FenceGetFdInfoKHR" "handleType"
  vkHandleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructFenceGetFdInfoKHR :: FenceGetFdInfoKHR -> (VkFenceGetFdInfoKHR -> IO a) -> IO a
withCStructFenceGetFdInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: FenceGetFdInfoKHR)) (\pPNext -> cont (VkFenceGetFdInfoKHR VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR pPNext (vkFence (from :: FenceGetFdInfoKHR)) (vkHandleType (from :: FenceGetFdInfoKHR))))
fromCStructFenceGetFdInfoKHR :: VkFenceGetFdInfoKHR -> IO FenceGetFdInfoKHR
fromCStructFenceGetFdInfoKHR c = FenceGetFdInfoKHR <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFenceGetFdInfoKHR)))
                                                   <*> pure (vkFence (c :: VkFenceGetFdInfoKHR))
                                                   <*> pure (vkHandleType (c :: VkFenceGetFdInfoKHR))
-- No documentation found for TopLevel "ImportFenceFdInfoKHR"
data ImportFenceFdInfoKHR = ImportFenceFdInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ImportFenceFdInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "fence"
  vkFence :: Fence
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "flags"
  vkFlags :: FenceImportFlags
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "handleType"
  vkHandleType :: ExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "fd"
  vkFd :: CInt
  }
  deriving (Show, Eq)
withCStructImportFenceFdInfoKHR :: ImportFenceFdInfoKHR -> (VkImportFenceFdInfoKHR -> IO a) -> IO a
withCStructImportFenceFdInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImportFenceFdInfoKHR)) (\pPNext -> cont (VkImportFenceFdInfoKHR VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR pPNext (vkFence (from :: ImportFenceFdInfoKHR)) (vkFlags (from :: ImportFenceFdInfoKHR)) (vkHandleType (from :: ImportFenceFdInfoKHR)) (vkFd (from :: ImportFenceFdInfoKHR))))
fromCStructImportFenceFdInfoKHR :: VkImportFenceFdInfoKHR -> IO ImportFenceFdInfoKHR
fromCStructImportFenceFdInfoKHR c = ImportFenceFdInfoKHR <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportFenceFdInfoKHR)))
                                                         <*> pure (vkFence (c :: VkImportFenceFdInfoKHR))
                                                         <*> pure (vkFlags (c :: VkImportFenceFdInfoKHR))
                                                         <*> pure (vkHandleType (c :: VkImportFenceFdInfoKHR))
                                                         <*> pure (vkFd (c :: VkImportFenceFdInfoKHR))

-- | Wrapper for vkGetFenceFdKHR
getFenceFdKHR :: Device ->  FenceGetFdInfoKHR ->  IO (CInt)
getFenceFdKHR = \(Device device commandTable) -> \getFdInfo -> alloca (\pFd -> (\a -> withCStructFenceGetFdInfoKHR a . flip with) getFdInfo (\pGetFdInfo -> Graphics.Vulkan.C.Dynamic.getFenceFdKHR commandTable device pGetFdInfo pFd >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pFd))))

-- | Wrapper for vkImportFenceFdKHR
importFenceFdKHR :: Device ->  ImportFenceFdInfoKHR ->  IO ()
importFenceFdKHR = \(Device device commandTable) -> \importFenceFdInfo -> (\a -> withCStructImportFenceFdInfoKHR a . flip with) importFenceFdInfo (\pImportFenceFdInfo -> Graphics.Vulkan.C.Dynamic.importFenceFdKHR commandTable device pImportFenceFdInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))
