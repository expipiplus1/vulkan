{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
  ( withCStructImportMemoryHostPointerInfoEXT
  , fromCStructImportMemoryHostPointerInfoEXT
  , ImportMemoryHostPointerInfoEXT(..)
  , withCStructMemoryHostPointerPropertiesEXT
  , fromCStructMemoryHostPointerPropertiesEXT
  , MemoryHostPointerPropertiesEXT(..)
  , withCStructPhysicalDeviceExternalMemoryHostPropertiesEXT
  , fromCStructPhysicalDeviceExternalMemoryHostPropertiesEXT
  , PhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  , getMemoryHostPointerPropertiesEXT
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
  , ExternalMemoryHandleTypeFlagsKHR
  , ExternalMemoryHandleTypeFlagBitsKHR
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
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getMemoryHostPointerPropertiesEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( VkImportMemoryHostPointerInfoEXT(..)
  , VkMemoryHostPointerPropertiesEXT(..)
  , VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
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
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagBitsKHR
  , ExternalMemoryHandleTypeFlagsKHR
  )


-- No documentation found for TopLevel "ImportMemoryHostPointerInfoEXT"
data ImportMemoryHostPointerInfoEXT = ImportMemoryHostPointerInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ImportMemoryHostPointerInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryHostPointerInfoEXT" "handleType"
  vkHandleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "ImportMemoryHostPointerInfoEXT" "pHostPointer"
  vkPHostPointer :: Ptr ()
  }
  deriving (Show, Eq)
withCStructImportMemoryHostPointerInfoEXT :: ImportMemoryHostPointerInfoEXT -> (VkImportMemoryHostPointerInfoEXT -> IO a) -> IO a
withCStructImportMemoryHostPointerInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImportMemoryHostPointerInfoEXT)) (\pPNext -> cont (VkImportMemoryHostPointerInfoEXT VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT pPNext (vkHandleType (from :: ImportMemoryHostPointerInfoEXT)) (vkPHostPointer (from :: ImportMemoryHostPointerInfoEXT))))
fromCStructImportMemoryHostPointerInfoEXT :: VkImportMemoryHostPointerInfoEXT -> IO ImportMemoryHostPointerInfoEXT
fromCStructImportMemoryHostPointerInfoEXT c = ImportMemoryHostPointerInfoEXT <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportMemoryHostPointerInfoEXT)))
                                                                             <*> pure (vkHandleType (c :: VkImportMemoryHostPointerInfoEXT))
                                                                             <*> pure (vkPHostPointer (c :: VkImportMemoryHostPointerInfoEXT))
-- No documentation found for TopLevel "MemoryHostPointerPropertiesEXT"
data MemoryHostPointerPropertiesEXT = MemoryHostPointerPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryHostPointerPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryHostPointerPropertiesEXT" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Show, Eq)
withCStructMemoryHostPointerPropertiesEXT :: MemoryHostPointerPropertiesEXT -> (VkMemoryHostPointerPropertiesEXT -> IO a) -> IO a
withCStructMemoryHostPointerPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryHostPointerPropertiesEXT)) (\pPNext -> cont (VkMemoryHostPointerPropertiesEXT VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT pPNext (vkMemoryTypeBits (from :: MemoryHostPointerPropertiesEXT))))
fromCStructMemoryHostPointerPropertiesEXT :: VkMemoryHostPointerPropertiesEXT -> IO MemoryHostPointerPropertiesEXT
fromCStructMemoryHostPointerPropertiesEXT c = MemoryHostPointerPropertiesEXT <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryHostPointerPropertiesEXT)))
                                                                             <*> pure (vkMemoryTypeBits (c :: VkMemoryHostPointerPropertiesEXT))
-- No documentation found for TopLevel "PhysicalDeviceExternalMemoryHostPropertiesEXT"
data PhysicalDeviceExternalMemoryHostPropertiesEXT = PhysicalDeviceExternalMemoryHostPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceExternalMemoryHostPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalMemoryHostPropertiesEXT" "minImportedHostPointerAlignment"
  vkMinImportedHostPointerAlignment :: DeviceSize
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceExternalMemoryHostPropertiesEXT :: PhysicalDeviceExternalMemoryHostPropertiesEXT -> (VkPhysicalDeviceExternalMemoryHostPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceExternalMemoryHostPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceExternalMemoryHostPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceExternalMemoryHostPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT pPNext (vkMinImportedHostPointerAlignment (from :: PhysicalDeviceExternalMemoryHostPropertiesEXT))))
fromCStructPhysicalDeviceExternalMemoryHostPropertiesEXT :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT -> IO PhysicalDeviceExternalMemoryHostPropertiesEXT
fromCStructPhysicalDeviceExternalMemoryHostPropertiesEXT c = PhysicalDeviceExternalMemoryHostPropertiesEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT)))
                                                                                                           <*> pure (vkMinImportedHostPointerAlignment (c :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))

-- | Wrapper for 'vkGetMemoryHostPointerPropertiesEXT'
getMemoryHostPointerPropertiesEXT :: Device ->  ExternalMemoryHandleTypeFlagBits ->  Ptr () ->  IO ( MemoryHostPointerPropertiesEXT )
getMemoryHostPointerPropertiesEXT = \(Device device commandTable) -> \handleType -> \pHostPointer -> alloca (\pMemoryHostPointerProperties -> Graphics.Vulkan.C.Dynamic.getMemoryHostPointerPropertiesEXT commandTable device handleType pHostPointer pMemoryHostPointerProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructMemoryHostPointerPropertiesEXT <=< peek) pMemoryHostPointerProperties)))
