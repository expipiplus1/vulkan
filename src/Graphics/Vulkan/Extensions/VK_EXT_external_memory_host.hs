{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
  ( pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  , vkGetMemoryHostPointerPropertiesEXT
  , VkImportMemoryHostPointerInfoEXT(..)
  , VkMemoryHostPointerPropertiesEXT(..)
  , VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkDevice
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT"
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT = VkStructureType 1000178000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT = VkStructureType 1000178001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT = VkStructureType 1000178002
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT = VkExternalMemoryHandleTypeFlagBits 0x00000080
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT = VkExternalMemoryHandleTypeFlagBits 0x00000100
-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION"
pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION :: Integral a => a
pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME"
pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME = "VK_EXT_external_memory_host"
-- | vkGetMemoryHostPointerPropertiesEXT - Get properties of external memory
-- host pointer
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that will be importing
--     @pHostPointer@.
--
-- -   @handleType@ is the type of the handle @pHostPointer@.
--
-- -   @pHostPointer@ is the host pointer to import from.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT@ or
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT@
--
-- -   @pHostPointer@ /must/ be a pointer aligned to an integer multiple of
--     @VkPhysicalDeviceExternalMemoryHostPropertiesEXT@::@minImportedHostPointerAlignment@
--
-- -   If @handleType@ is
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT@,
--     @pHostPointer@ /must/ be a pointer to host memory
--
-- -   If @handleType@ is
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT@,
--     @pHostPointer@ /must/ be a pointer to host mapped foreign memory
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- -   @pMemoryHostPointerProperties@ /must/ be a valid pointer to a
--     @VkMemoryHostPointerPropertiesEXT@ structure
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_INVALID_EXTERNAL_HANDLE@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'VkMemoryHostPointerPropertiesEXT'
foreign import ccall "vkGetMemoryHostPointerPropertiesEXT" vkGetMemoryHostPointerPropertiesEXT :: ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr VkMemoryHostPointerPropertiesEXT) -> IO VkResult
-- | VkImportMemoryHostPointerInfoEXT - import memory from a host pointer
--
-- = Description
-- #_description#
--
-- Importing memory from a host pointer shares ownership of the memory
-- between the host and the Vulkan implementation. The application /can/
-- continue to access the memory through the host pointer but it is the
-- application’s responsibility to synchronize device and non-device access
-- to the underlying memory as defined in
-- <{html_spec_relative}#memory-device-hostaccess Host Access to Device Memory Objects>.
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
-- @VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR@.
--
-- The application /must/ ensure that the imported memory range remains
-- valid and accessible for the lifetime of the imported memory object.
--
-- == Valid Usage
--
-- -   If @handleType@ is not @0@, it /must/ be supported for import, as
--     reported in
--     'Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities.VkExternalMemoryPropertiesKHR'
--
-- -   If @handleType@ is not @0@, it /must/ be
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT@ or
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT@
--
-- -   @pHostPointer@ /must/ be a pointer aligned to an integer multiple of
--     @VkPhysicalDeviceExternalMemoryHostPropertiesEXT@::@minImportedHostPointerAlignment@
--
-- -   If @handleType@ is
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT@,
--     @pHostPointer@ /must/ be a pointer to @allocationSize@ number of
--     bytes of host memory, where @allocationSize@ is the member of the
--     @VkMemoryAllocateInfo@ structure this structure is chained to
--
-- -   If @handleType@ is
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT@,
--     @pHostPointer@ /must/ be a pointer to @allocationSize@ number of
--     bytes of host mapped foreign memory, where @allocationSize@ is the
--     member of the @VkMemoryAllocateInfo@ structure this structure is
--     chained to
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT@
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkImportMemoryHostPointerInfoEXT = VkImportMemoryHostPointerInfoEXT
  { -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "vkHandleType"
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "vkPHostPointer"
  vkPHostPointer :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkImportMemoryHostPointerInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImportMemoryHostPointerInfoEXT <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportMemoryHostPointerInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportMemoryHostPointerInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryHostPointerInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPHostPointer (poked :: VkImportMemoryHostPointerInfoEXT))
-- No documentation found for TopLevel "VkMemoryHostPointerPropertiesEXT"
data VkMemoryHostPointerPropertiesEXT = VkMemoryHostPointerPropertiesEXT
  { -- No documentation found for Nested "VkMemoryHostPointerPropertiesEXT" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryHostPointerPropertiesEXT" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryHostPointerPropertiesEXT" "vkMemoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryHostPointerPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryHostPointerPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryHostPointerPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryHostPointerPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryHostPointerPropertiesEXT))
-- | VkPhysicalDeviceExternalMemoryHostPropertiesEXT - Structure describing
-- external memory host pointer limits that can be supported by an
-- implementation
--
-- = Members
-- #_members#
--
-- The members of the @VkPhysicalDeviceExternalMemoryHostPropertiesEXT@
-- structure describe the following implementation-dependent limits:
--
-- = Description
-- #_description#
--
-- -   @minImportedHostPointerAlignment@ is the minimum /required/
--     alignment, in bytes, for the base address and size of host pointers
--     that /can/ be imported to a Vulkan memory object.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT@
--
-- If the @VkPhysicalDeviceExternalMemoryHostPropertiesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2KHR',
-- it is filled with the implementation-dependent limits.
--
-- = See Also
-- #_see_also#
--
-- @VkDeviceSize@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceExternalMemoryHostPropertiesEXT = VkPhysicalDeviceExternalMemoryHostPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceExternalMemoryHostPropertiesEXT" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceExternalMemoryHostPropertiesEXT" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceExternalMemoryHostPropertiesEXT" "vkMinImportedHostPointerAlignment"
  vkMinImportedHostPointerAlignment :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalMemoryHostPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMinImportedHostPointerAlignment (poked :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
