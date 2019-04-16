{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( withCStructImportMemoryFdInfoKHR
  , fromCStructImportMemoryFdInfoKHR
  , ImportMemoryFdInfoKHR(..)
  , withCStructMemoryFdPropertiesKHR
  , fromCStructMemoryFdPropertiesKHR
  , MemoryFdPropertiesKHR(..)
  , withCStructMemoryGetFdInfoKHR
  , fromCStructMemoryGetFdInfoKHR
  , MemoryGetFdInfoKHR(..)
  , getMemoryFdKHR
  , getMemoryFdPropertiesKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
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
  ( getMemoryFdKHR
  , getMemoryFdPropertiesKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( VkImportMemoryFdInfoKHR(..)
  , VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
  )


-- No documentation found for TopLevel "ImportMemoryFdInfoKHR"
data ImportMemoryFdInfoKHR = ImportMemoryFdInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ImportMemoryFdInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryFdInfoKHR" "handleType"
  vkHandleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "ImportMemoryFdInfoKHR" "fd"
  vkFd :: CInt
  }
  deriving (Show, Eq)
withCStructImportMemoryFdInfoKHR :: ImportMemoryFdInfoKHR -> (VkImportMemoryFdInfoKHR -> IO a) -> IO a
withCStructImportMemoryFdInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImportMemoryFdInfoKHR)) (\pPNext -> cont (VkImportMemoryFdInfoKHR VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR pPNext (vkHandleType (from :: ImportMemoryFdInfoKHR)) (vkFd (from :: ImportMemoryFdInfoKHR))))
fromCStructImportMemoryFdInfoKHR :: VkImportMemoryFdInfoKHR -> IO ImportMemoryFdInfoKHR
fromCStructImportMemoryFdInfoKHR c = ImportMemoryFdInfoKHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportMemoryFdInfoKHR)))
                                                           <*> pure (vkHandleType (c :: VkImportMemoryFdInfoKHR))
                                                           <*> pure (vkFd (c :: VkImportMemoryFdInfoKHR))
-- No documentation found for TopLevel "MemoryFdPropertiesKHR"
data MemoryFdPropertiesKHR = MemoryFdPropertiesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryFdPropertiesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryFdPropertiesKHR" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Show, Eq)
withCStructMemoryFdPropertiesKHR :: MemoryFdPropertiesKHR -> (VkMemoryFdPropertiesKHR -> IO a) -> IO a
withCStructMemoryFdPropertiesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryFdPropertiesKHR)) (\pPNext -> cont (VkMemoryFdPropertiesKHR VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR pPNext (vkMemoryTypeBits (from :: MemoryFdPropertiesKHR))))
fromCStructMemoryFdPropertiesKHR :: VkMemoryFdPropertiesKHR -> IO MemoryFdPropertiesKHR
fromCStructMemoryFdPropertiesKHR c = MemoryFdPropertiesKHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryFdPropertiesKHR)))
                                                           <*> pure (vkMemoryTypeBits (c :: VkMemoryFdPropertiesKHR))
-- No documentation found for TopLevel "MemoryGetFdInfoKHR"
data MemoryGetFdInfoKHR = MemoryGetFdInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryGetFdInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryGetFdInfoKHR" "memory"
  vkMemory :: DeviceMemory
  , -- No documentation found for Nested "MemoryGetFdInfoKHR" "handleType"
  vkHandleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructMemoryGetFdInfoKHR :: MemoryGetFdInfoKHR -> (VkMemoryGetFdInfoKHR -> IO a) -> IO a
withCStructMemoryGetFdInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryGetFdInfoKHR)) (\pPNext -> cont (VkMemoryGetFdInfoKHR VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR pPNext (vkMemory (from :: MemoryGetFdInfoKHR)) (vkHandleType (from :: MemoryGetFdInfoKHR))))
fromCStructMemoryGetFdInfoKHR :: VkMemoryGetFdInfoKHR -> IO MemoryGetFdInfoKHR
fromCStructMemoryGetFdInfoKHR c = MemoryGetFdInfoKHR <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryGetFdInfoKHR)))
                                                     <*> pure (vkMemory (c :: VkMemoryGetFdInfoKHR))
                                                     <*> pure (vkHandleType (c :: VkMemoryGetFdInfoKHR))

-- | Wrapper for vkGetMemoryFdKHR
getMemoryFdKHR :: Device ->  MemoryGetFdInfoKHR ->  IO (CInt)
getMemoryFdKHR = \(Device device commandTable) -> \getFdInfo -> alloca (\pFd -> (\a -> withCStructMemoryGetFdInfoKHR a . flip with) getFdInfo (\pGetFdInfo -> Graphics.Vulkan.C.Dynamic.getMemoryFdKHR commandTable device pGetFdInfo pFd >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pFd))))

-- | Wrapper for vkGetMemoryFdPropertiesKHR
getMemoryFdPropertiesKHR :: Device ->  ExternalMemoryHandleTypeFlagBits ->  CInt ->  IO (MemoryFdPropertiesKHR)
getMemoryFdPropertiesKHR = \(Device device commandTable) -> \handleType -> \fd -> alloca (\pMemoryFdProperties -> Graphics.Vulkan.C.Dynamic.getMemoryFdPropertiesKHR commandTable device handleType fd pMemoryFdProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructMemoryFdPropertiesKHR <=< peek) pMemoryFdProperties)))
