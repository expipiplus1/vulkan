{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkDeviceGroupDeviceCreateInfo(..)
  , VkPhysicalDeviceGroupProperties(..)
  , FN_vkEnumeratePhysicalDeviceGroups
  , PFN_vkEnumeratePhysicalDeviceGroups
  , vkEnumeratePhysicalDeviceGroups
  , VK_MAX_DEVICE_GROUP_SIZE
  , pattern VK_MAX_DEVICE_GROUP_SIZE
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  ) where

import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkMemoryHeapFlagBits(..)
  , VkInstance
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkDeviceGroupDeviceCreateInfo - Create a logical device from multiple
-- physical devices
--
-- = Description
--
-- The elements of the @pPhysicalDevices@ array are an ordered list of the
-- physical devices that the logical device represents. These /must/ be a
-- subset of a single device group, and need not be in the same order as
-- they were enumerated. The order of the physical devices in the
-- @pPhysicalDevices@ array determines the /device index/ of each physical
-- device, with element i being assigned a device index of i. Certain
-- commands and structures refer to one or more physical devices by using
-- device indices or /device masks/ formed using device indices.
--
-- A logical device created without using 'VkDeviceGroupDeviceCreateInfo',
-- or with @physicalDeviceCount@ equal to zero, is equivalent to a
-- @physicalDeviceCount@ of one and @pPhysicalDevices@ pointing to the
-- @physicalDevice@ parameter to
-- 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice'. In particular, the
-- device index of that physical device is zero.
--
-- == Valid Usage
--
-- -   Each element of @pPhysicalDevices@ /must/ be unique
--
-- -   All elements of @pPhysicalDevices@ /must/ be in the same device
--     group as enumerated by 'vkEnumeratePhysicalDeviceGroups'
--
-- -   If @physicalDeviceCount@ is not @0@, the @physicalDevice@ parameter
--     of 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice' /must/ be an
--     element of @pPhysicalDevices@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO'
--
-- -   If @physicalDeviceCount@ is not @0@, @pPhysicalDevices@ /must/ be a
--     valid pointer to an array of @physicalDeviceCount@ valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handles
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkDeviceGroupDeviceCreateInfo = VkDeviceGroupDeviceCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @physicalDeviceCount@ is the number of elements in the
  -- @pPhysicalDevices@ array.
  vkPhysicalDeviceCount :: Word32
  , -- | @pPhysicalDevices@ is an array of physical device handles belonging to
  -- the same device group.
  vkPPhysicalDevices :: Ptr VkPhysicalDevice
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupDeviceCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDeviceGroupDeviceCreateInfo <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupDeviceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupDeviceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDeviceCount (poked :: VkDeviceGroupDeviceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPPhysicalDevices (poked :: VkDeviceGroupDeviceCreateInfo))

instance Zero VkDeviceGroupDeviceCreateInfo where
  zero = VkDeviceGroupDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
                                       zero
                                       zero
                                       zero

-- | VkPhysicalDeviceGroupProperties - Structure specifying physical device
-- group properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkEnumeratePhysicalDeviceGroups',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group_creation.vkEnumeratePhysicalDeviceGroupsKHR'
data VkPhysicalDeviceGroupProperties = VkPhysicalDeviceGroupProperties
  { -- | @sType@ /must/ be 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @physicalDeviceCount@ is the number of physical devices in the group.
  vkPhysicalDeviceCount :: Word32
  , -- | @physicalDevices@ is an array of physical device handles representing
  -- all physical devices in the group. The first @physicalDeviceCount@
  -- elements of the array will be valid.
  vkPhysicalDevices :: Vector VK_MAX_DEVICE_GROUP_SIZE VkPhysicalDevice
  , -- | @subsetAllocation@ specifies whether logical devices created from the
  -- group support allocating device memory on a subset of devices, via the
  -- @deviceMask@ member of the
  -- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo'.
  -- If this is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then all device
  -- memory allocations are made across all physical devices in the group. If
  -- @physicalDeviceCount@ is @1@, then @subsetAllocation@ /must/ be
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
  vkSubsetAllocation :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceGroupProperties where
  sizeOf ~_ = 288
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceGroupProperties <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 280)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceGroupProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceGroupProperties))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDeviceCount (poked :: VkPhysicalDeviceGroupProperties))
                *> poke (ptr `plusPtr` 24) (vkPhysicalDevices (poked :: VkPhysicalDeviceGroupProperties))
                *> poke (ptr `plusPtr` 280) (vkSubsetAllocation (poked :: VkPhysicalDeviceGroupProperties))

instance Zero VkPhysicalDeviceGroupProperties where
  zero = VkPhysicalDeviceGroupProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
                                         zero
                                         zero
                                         zero
                                         zero

-- | vkEnumeratePhysicalDeviceGroups - Enumerates groups of physical devices
-- that can be used to create a single logical device
--
-- = Parameters
--
-- -   @instance@ is a handle to a Vulkan instance previously created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance'.
--
-- -   @pPhysicalDeviceGroupCount@ is a pointer to an integer related to
--     the number of device groups available or queried, as described
--     below.
--
-- -   @pPhysicalDeviceGroupProperties@ is either @NULL@ or a pointer to an
--     array of 'VkPhysicalDeviceGroupProperties' structures.
--
-- = Description
--
-- If @pPhysicalDeviceGroupProperties@ is @NULL@, then the number of device
-- groups available is returned in @pPhysicalDeviceGroupCount@. Otherwise,
-- @pPhysicalDeviceGroupCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pPhysicalDeviceGroupProperties@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pPhysicalDeviceGroupProperties@. If
-- @pPhysicalDeviceGroupCount@ is less than the number of device groups
-- available, at most @pPhysicalDeviceGroupCount@ structures will be
-- written. If @pPhysicalDeviceGroupCount@ is smaller than the number of
-- device groups available, 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
-- will be returned instead of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS',
-- to indicate that not all the available device groups were returned.
--
-- Every physical device /must/ be in exactly one device group.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pPhysicalDeviceGroupCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pPhysicalDeviceGroupCount@ is not @0@,
--     and @pPhysicalDeviceGroupProperties@ is not @NULL@,
--     @pPhysicalDeviceGroupProperties@ /must/ be a valid pointer to an
--     array of @pPhysicalDeviceGroupCount@
--     'VkPhysicalDeviceGroupProperties' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'VkPhysicalDeviceGroupProperties'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumeratePhysicalDeviceGroups" vkEnumeratePhysicalDeviceGroups :: ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
#else
vkEnumeratePhysicalDeviceGroups :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
vkEnumeratePhysicalDeviceGroups deviceCmds = mkVkEnumeratePhysicalDeviceGroups (pVkEnumeratePhysicalDeviceGroups deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDeviceGroups
  :: FunPtr (("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult)
#endif

type FN_vkEnumeratePhysicalDeviceGroups = ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
type PFN_vkEnumeratePhysicalDeviceGroups = FunPtr FN_vkEnumeratePhysicalDeviceGroups

-- No documentation found for TopLevel "VK_MAX_DEVICE_GROUP_SIZE"
type VK_MAX_DEVICE_GROUP_SIZE = 32
-- No documentation found for Nested "Integral a => a" "VK_MAX_DEVICE_GROUP_SIZE"
pattern VK_MAX_DEVICE_GROUP_SIZE :: Integral a => a
pattern VK_MAX_DEVICE_GROUP_SIZE = 32

-- No documentation found for Nested "VkMemoryHeapFlagBits" "VK_MEMORY_HEAP_MULTI_INSTANCE_BIT"
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT :: VkMemoryHeapFlagBits
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT = VkMemoryHeapFlagBits 0x00000002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO = VkStructureType 1000070001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES = VkStructureType 1000070000
