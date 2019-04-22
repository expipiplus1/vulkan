{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address
  ( withCStructBufferDeviceAddressCreateInfoEXT
  , fromCStructBufferDeviceAddressCreateInfoEXT
  , BufferDeviceAddressCreateInfoEXT(..)
  , withCStructBufferDeviceAddressInfoEXT
  , fromCStructBufferDeviceAddressInfoEXT
  , BufferDeviceAddressInfoEXT(..)
  , DeviceAddress
  , PhysicalDeviceBufferAddressFeaturesEXT
  , withCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT
  , fromCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT
  , PhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  , getBufferDeviceAddressEXT
  , pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  , pattern EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  , pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern ERROR_INVALID_DEVICE_ADDRESS_EXT
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressCreateInfoEXT(..)
  , VkBufferDeviceAddressInfoEXT(..)
  , VkPhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  , VkDeviceAddress
  , vkGetBufferDeviceAddressEXT
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Buffer
  ( pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_INVALID_DEVICE_ADDRESS_EXT
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
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
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object.
--
-- If this structure is not present, it is as if @deviceAddress@ is zero.
--
-- Apps /should/ avoid creating buffers with app-provided addresses and
-- implementation-provided addresses in the same process, to reduce the
-- likelihood of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_ERROR_INVALID_DEVICE_ADDRESS_EXT'
-- errors.
--
-- __Note__
--
-- The expected usage for this is that a trace capture\/replay tool will
-- add the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT'
-- flag to all buffers that use
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT',
-- and during capture will save the queried device addresses in the trace.
-- During replay, the buffers will be created specifying the original
-- address so any address values stored in the trace data will remain
-- valid.
--
-- Implementations are expected to separate such buffers in the GPU address
-- space so normal allocations will avoid using these addresses.
-- Apps\/tools should avoid mixing app-provided and implementation-provided
-- addresses for buffers created with
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT',
-- to avoid address space allocation conflicts.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkDeviceAddress',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data BufferDeviceAddressCreateInfoEXT = BufferDeviceAddressCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "BufferDeviceAddressCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferDeviceAddressCreateInfoEXT" "deviceAddress"
  deviceAddress :: DeviceAddress
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBufferDeviceAddressCreateInfoEXT' and
-- marshal a 'BufferDeviceAddressCreateInfoEXT' into it. The 'VkBufferDeviceAddressCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBufferDeviceAddressCreateInfoEXT :: BufferDeviceAddressCreateInfoEXT -> (VkBufferDeviceAddressCreateInfoEXT -> IO a) -> IO a
withCStructBufferDeviceAddressCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BufferDeviceAddressCreateInfoEXT)) (\pPNext -> cont (VkBufferDeviceAddressCreateInfoEXT VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT pPNext (deviceAddress (marshalled :: BufferDeviceAddressCreateInfoEXT))))

-- | A function to read a 'VkBufferDeviceAddressCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'BufferDeviceAddressCreateInfoEXT'.
fromCStructBufferDeviceAddressCreateInfoEXT :: VkBufferDeviceAddressCreateInfoEXT -> IO BufferDeviceAddressCreateInfoEXT
fromCStructBufferDeviceAddressCreateInfoEXT c = BufferDeviceAddressCreateInfoEXT <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferDeviceAddressCreateInfoEXT)))
                                                                                 <*> pure (vkDeviceAddress (c :: VkBufferDeviceAddressCreateInfoEXT))

instance Zero BufferDeviceAddressCreateInfoEXT where
  zero = BufferDeviceAddressCreateInfoEXT Nothing
                                          zero



-- | VkBufferDeviceAddressInfoEXT - Structure specifying the buffer to query
-- an address for
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse and was not created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT'
--     flag, then it /must/ be bound completely and contiguously to a
--     single 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @buffer@ /must/ have been created with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.vkGetBufferDeviceAddressEXT'
data BufferDeviceAddressInfoEXT = BufferDeviceAddressInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "BufferDeviceAddressInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferDeviceAddressInfoEXT" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBufferDeviceAddressInfoEXT' and
-- marshal a 'BufferDeviceAddressInfoEXT' into it. The 'VkBufferDeviceAddressInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBufferDeviceAddressInfoEXT :: BufferDeviceAddressInfoEXT -> (VkBufferDeviceAddressInfoEXT -> IO a) -> IO a
withCStructBufferDeviceAddressInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BufferDeviceAddressInfoEXT)) (\pPNext -> cont (VkBufferDeviceAddressInfoEXT VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT pPNext (buffer (marshalled :: BufferDeviceAddressInfoEXT))))

-- | A function to read a 'VkBufferDeviceAddressInfoEXT' and all additional
-- structures in the pointer chain into a 'BufferDeviceAddressInfoEXT'.
fromCStructBufferDeviceAddressInfoEXT :: VkBufferDeviceAddressInfoEXT -> IO BufferDeviceAddressInfoEXT
fromCStructBufferDeviceAddressInfoEXT c = BufferDeviceAddressInfoEXT <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferDeviceAddressInfoEXT)))
                                                                     <*> pure (vkBuffer (c :: VkBufferDeviceAddressInfoEXT))

instance Zero BufferDeviceAddressInfoEXT where
  zero = BufferDeviceAddressInfoEXT Nothing
                                    zero


-- | VkDeviceAddress - Vulkan device address type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkBufferDeviceAddressCreateInfoEXT'
type DeviceAddress = VkDeviceAddress
  

type PhysicalDeviceBufferAddressFeaturesEXT = PhysicalDeviceBufferDeviceAddressFeaturesEXT
-- TODO: Pattern constructor alias)


-- | VkPhysicalDeviceBufferDeviceAddressFeaturesEXT - Structure describing
-- buffer address features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkPhysicalDeviceBufferDeviceAddressFeaturesEXT'
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
-- See
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.vkGetBufferDeviceAddressEXT'
-- for more information.
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkPhysicalDeviceBufferDeviceAddressFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkPhysicalDeviceBufferDeviceAddressFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceBufferDeviceAddressFeaturesEXT = PhysicalDeviceBufferDeviceAddressFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddress"
  bufferDeviceAddress :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressCaptureReplay"
  bufferDeviceAddressCaptureReplay :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressMultiDevice"
  bufferDeviceAddressMultiDevice :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceBufferDeviceAddressFeaturesEXT' and
-- marshal a 'PhysicalDeviceBufferDeviceAddressFeaturesEXT' into it. The 'VkPhysicalDeviceBufferDeviceAddressFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT :: PhysicalDeviceBufferDeviceAddressFeaturesEXT -> (VkPhysicalDeviceBufferDeviceAddressFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceBufferDeviceAddressFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceBufferDeviceAddressFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT pPNext (boolToBool32 (bufferDeviceAddress (marshalled :: PhysicalDeviceBufferDeviceAddressFeaturesEXT))) (boolToBool32 (bufferDeviceAddressCaptureReplay (marshalled :: PhysicalDeviceBufferDeviceAddressFeaturesEXT))) (boolToBool32 (bufferDeviceAddressMultiDevice (marshalled :: PhysicalDeviceBufferDeviceAddressFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceBufferDeviceAddressFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceBufferDeviceAddressFeaturesEXT'.
fromCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT -> IO PhysicalDeviceBufferDeviceAddressFeaturesEXT
fromCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT c = PhysicalDeviceBufferDeviceAddressFeaturesEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkBufferDeviceAddress (c :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkBufferDeviceAddressCaptureReplay (c :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkBufferDeviceAddressMultiDevice (c :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT)))

instance Zero PhysicalDeviceBufferDeviceAddressFeaturesEXT where
  zero = PhysicalDeviceBufferDeviceAddressFeaturesEXT Nothing
                                                      False
                                                      False
                                                      False



-- | vkGetBufferDeviceAddressEXT - Query an address of a buffer
--
-- = Parameters
--
-- -   @device@ is the logical device that the buffer was created on.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkBufferDeviceAddressInfoEXT'
--     structure specifying the buffer to retrieve an address for.
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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkBufferDeviceAddressCreateInfoEXT'::@deviceAddress@,
-- the return value will be the same address.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-bufferDeviceAddress bufferDeviceAddress>
--     feature /must/ be enabled
--
-- -   If @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkBufferDeviceAddressInfoEXT'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkBufferDeviceAddressInfoEXT',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
getBufferDeviceAddressEXT :: Device ->  BufferDeviceAddressInfoEXT ->  IO (VkDeviceAddress)
getBufferDeviceAddressEXT = \(Device device' commandTable) -> \info' -> (\marshalled -> withCStructBufferDeviceAddressInfoEXT marshalled . flip with) info' (\pInfo' -> vkGetBufferDeviceAddressEXT commandTable device' pInfo' >>= (\ret -> pure ret))

-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME"
pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION"
pattern EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION :: Integral a => a
pattern EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
