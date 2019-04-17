{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressCreateInfoEXT(..)
  , VkBufferDeviceAddressInfoEXT(..)
  , VkDeviceAddress
  , VkPhysicalDeviceBufferAddressFeaturesEXT
  , pattern VkPhysicalDeviceBufferAddressFeaturesEXT
  , VkPhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetBufferDeviceAddressEXT
#endif
  , FN_vkGetBufferDeviceAddressEXT
  , PFN_vkGetBufferDeviceAddressEXT
  , pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  , pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits(..)
  , VkBufferUsageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkBufferDeviceAddressCreateInfoEXT - Request a specific address for a
-- buffer
--
-- = Description
--
-- If @deviceAddress@ is zero, no specific address is requested.
--
-- If @deviceAddress@ is not zero, @deviceAddress@ /must/ be an address
-- retrieved from an identically created buffer on the same implementation.
-- The buffer /must/ also be bound to an identically created
-- @VkDeviceMemory@ object.
--
-- If this structure is not present, it is as if @deviceAddress@ is zero.
--
-- Apps /should/ avoid creating buffers with app-provided addresses and
-- implementation-provided addresses in the same process, to reduce the
-- likelihood of @VK_ERROR_INVALID_DEVICE_ADDRESS_EXT@ errors.
--
-- __Note__
--
-- The expected usage for this is that a trace capture\/replay tool will
-- add the @VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT@ flag to
-- all buffers that use @VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT@,
-- and during capture will save the queried device addresses in the trace.
-- During replay, the buffers will be created specifying the original
-- address so any address values stored in the trace data will remain
-- valid.
--
-- Implementations are expected to separate such buffers in the GPU address
-- space so normal allocations will avoid using these addresses.
-- Apps\/tools should avoid mixing app-provided and implementation-provided
-- addresses for buffers created with
-- @VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT@, to avoid
-- address space allocation conflicts.
--
-- Unresolved directive in VkBufferDeviceAddressCreateInfoEXT.txt -
-- include::..\/validity\/structs\/VkBufferDeviceAddressCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkBufferDeviceAddressCreateInfoEXT = VkBufferDeviceAddressCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @deviceAddress@ is the device address requested for the buffer.
  vkDeviceAddress :: VkDeviceAddress
  }
  deriving (Eq, Show)

instance Storable VkBufferDeviceAddressCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferDeviceAddressCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferDeviceAddressCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferDeviceAddressCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDeviceAddress (poked :: VkBufferDeviceAddressCreateInfoEXT))

instance Zero VkBufferDeviceAddressCreateInfoEXT where
  zero = VkBufferDeviceAddressCreateInfoEXT zero
                                            zero
                                            zero
-- | VkBufferDeviceAddressInfoEXT - Structure specifying the buffer to query
-- an address for
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse and was not created with the
--     @VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT@ flag, then
--     it /must/ be bound completely and contiguously to a single
--     @VkDeviceMemory@ object
--
-- -   @buffer@ /must/ have been created with
--     @VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT@
--
-- Unresolved directive in VkBufferDeviceAddressInfoEXT.txt -
-- include::..\/validity\/structs\/VkBufferDeviceAddressInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkBufferDeviceAddressInfoEXT = VkBufferDeviceAddressInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @buffer@ specifies the buffer whose address is being queried.
  vkBuffer :: VkBuffer
  }
  deriving (Eq, Show)

instance Storable VkBufferDeviceAddressInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferDeviceAddressInfoEXT <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferDeviceAddressInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferDeviceAddressInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBufferDeviceAddressInfoEXT))

instance Zero VkBufferDeviceAddressInfoEXT where
  zero = VkBufferDeviceAddressInfoEXT zero
                                      zero
                                      zero
-- | VkDeviceAddress - Vulkan device address type
--
-- = See Also
--
-- No cross-references are available
type VkDeviceAddress = Word64
-- No documentation found for TopLevel "VkPhysicalDeviceBufferAddressFeaturesEXT"
type VkPhysicalDeviceBufferAddressFeaturesEXT = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT


-- No documentation found for TopLevel "VkPhysicalDeviceBufferAddressFeaturesEXT"
pattern VkPhysicalDeviceBufferAddressFeaturesEXT :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("bufferDeviceAddress" ::: VkBool32) -> ("bufferDeviceAddressCaptureReplay" ::: VkBool32) -> ("bufferDeviceAddressMultiDevice" ::: VkBool32) -> VkPhysicalDeviceBufferAddressFeaturesEXT
pattern VkPhysicalDeviceBufferAddressFeaturesEXT vkSType vkPNext vkBufferDeviceAddress vkBufferDeviceAddressCaptureReplay vkBufferDeviceAddressMultiDevice = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT vkSType vkPNext vkBufferDeviceAddress vkBufferDeviceAddressCaptureReplay vkBufferDeviceAddressMultiDevice
-- | VkPhysicalDeviceBufferDeviceAddressFeaturesEXT - Structure describing
-- buffer address features that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceBufferDeviceAddressFeaturesEXT@
-- structure describe the following features:
--
-- = Description
--
-- __Note__
--
-- @bufferDeviceAddressMultiDevice@ exists to allow certain legacy
-- platforms to be able to support @bufferDeviceAddress@ without needing to
-- support shared GPU virtual addresses for multi-device configurations.
--
-- See 'vkGetBufferDeviceAddressEXT' for more information.
--
-- If the @VkPhysicalDeviceBufferDeviceAddressFeaturesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- @VkPhysicalDeviceBufferDeviceAddressFeaturesEXT@ /can/ also be used in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in
-- VkPhysicalDeviceBufferDeviceAddressFeaturesEXT.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceBufferDeviceAddressFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceBufferDeviceAddressFeaturesEXT = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @bufferDeviceAddress@ indicates that the implementation supports
  -- accessing buffer memory in shaders as storage buffers via an address
  -- queried from 'vkGetBufferDeviceAddressEXT'.
  vkBufferDeviceAddress :: VkBool32
  , -- | @bufferDeviceAddressCaptureReplay@ indicates that the implementation
  -- supports saving and reusing buffer addresses, e.g. for trace capture and
  -- replay.
  vkBufferDeviceAddressCaptureReplay :: VkBool32
  , -- | @bufferDeviceAddressMultiDevice@ indicates that the implementation
  -- supports the @bufferDeviceAddress@ feature for logical devices created
  -- with multiple physical devices. If this feature is not supported, buffer
  -- addresses /must/ not be queried on a logical device created with more
  -- than one physical device.
  vkBufferDeviceAddressMultiDevice :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceBufferDeviceAddressFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 20)
                                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkBufferDeviceAddress (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkBufferDeviceAddressCaptureReplay (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
                *> poke (ptr `plusPtr` 24) (vkBufferDeviceAddressMultiDevice (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))

instance Zero VkPhysicalDeviceBufferDeviceAddressFeaturesEXT where
  zero = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetBufferDeviceAddressEXT - Query an address of a buffer
--
-- = Parameters
--
-- -   @device@ is the logical device that the buffer was created on.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'VkBufferDeviceAddressInfoEXT' structure specifying the buffer to
--     retrieve an address for.
--
-- = Description
--
-- The 64-bit return value is an address of the start of @pInfo@::@buffer@.
-- The address range starting at this value and whose size is the size of
-- the buffer /can/ be used in a shader to access the memory bound to that
-- buffer, using the @SPV_EXT_physical_storage_buffer@ extension and the
-- @PhysicalStorageBufferEXT@ storage class. For example, this value /can/
-- be stored in a uniform buffer, and the shader /can/ read the value from
-- the uniform buffer and use it to do a dependent read\/write to this
-- buffer. A value of zero is reserved as a “null” pointer and /must/ not
-- be returned as a valid buffer device address. All loads, stores, and
-- atomics in a shader through @PhysicalStorageBufferEXT@ pointers /must/
-- access addresses in the address range of some buffer.
--
-- If the buffer was created with a non-zero value of
-- 'VkBufferDeviceAddressCreateInfoEXT'::@deviceAddress@, the return value
-- will be the same address.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-bufferDeviceAddress bufferDeviceAddress>
--     feature /must/ be enabled
--
-- -   If @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- Unresolved directive in vkGetBufferDeviceAddressEXT.txt -
-- include::..\/validity\/protos\/vkGetBufferDeviceAddressEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetBufferDeviceAddressEXT" vkGetBufferDeviceAddressEXT :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferDeviceAddressInfoEXT) -> IO VkDeviceAddress

#endif
type FN_vkGetBufferDeviceAddressEXT = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferDeviceAddressInfoEXT) -> IO VkDeviceAddress
type PFN_vkGetBufferDeviceAddressEXT = FunPtr FN_vkGetBufferDeviceAddressEXT
-- | @VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT@ specifies that
-- the buffer’s address /can/ be saved and reused on a subsequent run (e.g.
-- for trace capture and replay), see 'VkBufferDeviceAddressCreateInfoEXT'
-- for more detail.
pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT = VkBufferCreateFlagBits 0x00000010
-- | @VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT@ specifies that the
-- buffer /can/ be used to retrieve a buffer device address via
-- 'vkGetBufferDeviceAddressEXT' and use that address to access the
-- buffer’s memory from a shader.
pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT = VkBufferUsageFlagBits 0x00020000
-- | @VK_ERROR_INVALID_DEVICE_ADDRESS_EXT@ A buffer creation failed because
-- the requested address is not available.
pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT :: VkResult
pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT = VkResult (-1000244000)
-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME"
pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = "VK_EXT_buffer_device_address"
-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION"
pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT = VkStructureType 1000244002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT"
pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT = VkStructureType 1000244001
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT = VkStructureType 1000244000
