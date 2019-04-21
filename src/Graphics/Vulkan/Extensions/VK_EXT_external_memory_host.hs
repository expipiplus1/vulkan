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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( VkImportMemoryHostPointerInfoEXT(..)
  , VkMemoryHostPointerPropertiesEXT(..)
  , VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  , vkGetMemoryHostPointerPropertiesEXT
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



-- | VkImportMemoryHostPointerInfoEXT - import memory from a host pointer
--
-- = Description
--
-- Importing memory from a host pointer shares ownership of the memory
-- between the host and the Vulkan implementation. The application /can/
-- continue to access the memory through the host pointer but it is the
-- application’s responsibility to synchronize device and non-device access
-- to the underlying memory as defined in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-device-hostaccess Host Access to Device Memory Objects>.
--
-- Applications /can/ import the same underlying memory into multiple
-- instances of Vulkan and multiple times into a given Vulkan instance.
-- However, implementations /may/ fail to import the same underlying memory
-- multiple times into a given physical device due to platform constraints.
--
-- Importing memory from a particular host pointer /may/ not be possible
-- due to additional platform-specific restrictions beyond the scope of
-- this specification in which case the implementation /must/ fail the
-- memory import operation with the error code
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory.VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
-- The application /must/ ensure that the imported memory range remains
-- valid and accessible for the lifetime of the imported memory object.
--
-- == Valid Usage
--
-- -   If @handleType@ is not @0@, it /must/ be supported for import, as
--     reported in
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities.VkExternalMemoryPropertiesKHR'
--
-- -   If @handleType@ is not @0@, it /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'
--
-- -   @pHostPointer@ /must/ be a pointer aligned to an integer multiple of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkPhysicalDeviceExternalMemoryHostPropertiesEXT'::@minImportedHostPointerAlignment@
--
-- -   If @handleType@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT',
--     @pHostPointer@ /must/ be a pointer to @allocationSize@ number of
--     bytes of host memory, where @allocationSize@ is the member of the
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' structure
--     this structure is chained to
--
-- -   If @handleType@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT',
--     @pHostPointer@ /must/ be a pointer to @allocationSize@ number of
--     bytes of host mapped foreign memory, where @allocationSize@ is the
--     member of the 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'
--     structure this structure is chained to
--
-- Unresolved directive in VkImportMemoryHostPointerInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkImportMemoryHostPointerInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data ImportMemoryHostPointerInfoEXT = ImportMemoryHostPointerInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ImportMemoryHostPointerInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryHostPointerInfoEXT" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "ImportMemoryHostPointerInfoEXT" "pHostPointer"
  hostPointer :: Ptr ()
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImportMemoryHostPointerInfoEXT' and
-- marshal a 'ImportMemoryHostPointerInfoEXT' into it. The 'VkImportMemoryHostPointerInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImportMemoryHostPointerInfoEXT :: ImportMemoryHostPointerInfoEXT -> (VkImportMemoryHostPointerInfoEXT -> IO a) -> IO a
withCStructImportMemoryHostPointerInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImportMemoryHostPointerInfoEXT)) (\pPNext -> cont (VkImportMemoryHostPointerInfoEXT VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT pPNext (handleType (marshalled :: ImportMemoryHostPointerInfoEXT)) (hostPointer (marshalled :: ImportMemoryHostPointerInfoEXT))))

-- | A function to read a 'VkImportMemoryHostPointerInfoEXT' and all additional
-- structures in the pointer chain into a 'ImportMemoryHostPointerInfoEXT'.
fromCStructImportMemoryHostPointerInfoEXT :: VkImportMemoryHostPointerInfoEXT -> IO ImportMemoryHostPointerInfoEXT
fromCStructImportMemoryHostPointerInfoEXT c = ImportMemoryHostPointerInfoEXT <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportMemoryHostPointerInfoEXT)))
                                                                             <*> pure (vkHandleType (c :: VkImportMemoryHostPointerInfoEXT))
                                                                             <*> pure (vkPHostPointer (c :: VkImportMemoryHostPointerInfoEXT))

instance Zero ImportMemoryHostPointerInfoEXT where
  zero = ImportMemoryHostPointerInfoEXT Nothing
                                        zero
                                        zero



-- | VkMemoryHostPointerPropertiesEXT - Properties of external memory host
-- pointer
--
-- = Description
--
-- The value returned by @memoryTypeBits@ /must/ only include bits that
-- identify memory types which are host visible.
--
-- Unresolved directive in VkMemoryHostPointerPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkMemoryHostPointerPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data MemoryHostPointerPropertiesEXT = MemoryHostPointerPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryHostPointerPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryHostPointerPropertiesEXT" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryHostPointerPropertiesEXT' and
-- marshal a 'MemoryHostPointerPropertiesEXT' into it. The 'VkMemoryHostPointerPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryHostPointerPropertiesEXT :: MemoryHostPointerPropertiesEXT -> (VkMemoryHostPointerPropertiesEXT -> IO a) -> IO a
withCStructMemoryHostPointerPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryHostPointerPropertiesEXT)) (\pPNext -> cont (VkMemoryHostPointerPropertiesEXT VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT pPNext (memoryTypeBits (marshalled :: MemoryHostPointerPropertiesEXT))))

-- | A function to read a 'VkMemoryHostPointerPropertiesEXT' and all additional
-- structures in the pointer chain into a 'MemoryHostPointerPropertiesEXT'.
fromCStructMemoryHostPointerPropertiesEXT :: VkMemoryHostPointerPropertiesEXT -> IO MemoryHostPointerPropertiesEXT
fromCStructMemoryHostPointerPropertiesEXT c = MemoryHostPointerPropertiesEXT <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryHostPointerPropertiesEXT)))
                                                                             <*> pure (vkMemoryTypeBits (c :: VkMemoryHostPointerPropertiesEXT))

instance Zero MemoryHostPointerPropertiesEXT where
  zero = MemoryHostPointerPropertiesEXT Nothing
                                        zero



-- | VkPhysicalDeviceExternalMemoryHostPropertiesEXT - Structure describing
-- external memory host pointer limits that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkPhysicalDeviceExternalMemoryHostPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkPhysicalDeviceExternalMemoryHostPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in
-- VkPhysicalDeviceExternalMemoryHostPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceExternalMemoryHostPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceExternalMemoryHostPropertiesEXT = PhysicalDeviceExternalMemoryHostPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceExternalMemoryHostPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalMemoryHostPropertiesEXT" "minImportedHostPointerAlignment"
  minImportedHostPointerAlignment :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceExternalMemoryHostPropertiesEXT' and
-- marshal a 'PhysicalDeviceExternalMemoryHostPropertiesEXT' into it. The 'VkPhysicalDeviceExternalMemoryHostPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceExternalMemoryHostPropertiesEXT :: PhysicalDeviceExternalMemoryHostPropertiesEXT -> (VkPhysicalDeviceExternalMemoryHostPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceExternalMemoryHostPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceExternalMemoryHostPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceExternalMemoryHostPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT pPNext (minImportedHostPointerAlignment (marshalled :: PhysicalDeviceExternalMemoryHostPropertiesEXT))))

-- | A function to read a 'VkPhysicalDeviceExternalMemoryHostPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceExternalMemoryHostPropertiesEXT'.
fromCStructPhysicalDeviceExternalMemoryHostPropertiesEXT :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT -> IO PhysicalDeviceExternalMemoryHostPropertiesEXT
fromCStructPhysicalDeviceExternalMemoryHostPropertiesEXT c = PhysicalDeviceExternalMemoryHostPropertiesEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT)))
                                                                                                           <*> pure (vkMinImportedHostPointerAlignment (c :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))

instance Zero PhysicalDeviceExternalMemoryHostPropertiesEXT where
  zero = PhysicalDeviceExternalMemoryHostPropertiesEXT Nothing
                                                       zero



-- | vkGetMemoryHostPointerPropertiesEXT - Get properties of external memory
-- host pointer
--
-- = Parameters
--
-- -   @device@ is the logical device that will be importing
--     @pHostPointer@.
--
-- -   @handleType@ is the type of the handle @pHostPointer@.
--
-- -   @pHostPointer@ is the host pointer to import from.
--
-- -   @pMemoryHostPointerProperties@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkMemoryHostPointerPropertiesEXT'
--     structure in which the host pointer properties are returned.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'
--
-- -   @pHostPointer@ /must/ be a pointer aligned to an integer multiple of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkPhysicalDeviceExternalMemoryHostPropertiesEXT'::@minImportedHostPointerAlignment@
--
-- -   If @handleType@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT',
--     @pHostPointer@ /must/ be a pointer to host memory
--
-- -   If @handleType@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT',
--     @pHostPointer@ /must/ be a pointer to host mapped foreign memory
--
-- Unresolved directive in vkGetMemoryHostPointerPropertiesEXT.txt -
-- include::{generated}\/validity\/protos\/vkGetMemoryHostPointerPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
getMemoryHostPointerPropertiesEXT :: Device ->  ExternalMemoryHandleTypeFlagBits ->  Ptr () ->  IO (MemoryHostPointerPropertiesEXT)
getMemoryHostPointerPropertiesEXT = \(Device device' commandTable) -> \handleType' -> \pHostPointer' -> alloca (\pMemoryHostPointerProperties' -> vkGetMemoryHostPointerPropertiesEXT commandTable device' handleType' pHostPointer' pMemoryHostPointerProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructMemoryHostPointerPropertiesEXT <=< peek) pMemoryHostPointerProperties')))
