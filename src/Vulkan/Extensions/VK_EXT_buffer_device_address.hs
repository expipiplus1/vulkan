{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_buffer_device_address  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT
                                                       , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
                                                       , pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
                                                       , pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
                                                       , pattern ERROR_INVALID_DEVICE_ADDRESS_EXT
                                                       , getBufferDeviceAddressEXT
                                                       , PhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
                                                       , BufferDeviceAddressCreateInfoEXT(..)
                                                       , PhysicalDeviceBufferAddressFeaturesEXT
                                                       , BufferDeviceAddressInfoEXT
                                                       , EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
                                                       , pattern EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
                                                       , EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                                                       , pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                                                       ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (getBufferDeviceAddress)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo)
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlagBits(BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT))
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlagBits(BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT))
import Vulkan.Core10.Enums.Result (Result(ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT"
pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT = STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO


-- No documentation found for TopLevel "VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT"
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT = BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT


-- No documentation found for TopLevel "VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT"
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT = BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT


-- No documentation found for TopLevel "VK_ERROR_INVALID_DEVICE_ADDRESS_EXT"
pattern ERROR_INVALID_DEVICE_ADDRESS_EXT = ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS


-- No documentation found for TopLevel "vkGetBufferDeviceAddressEXT"
getBufferDeviceAddressEXT = getBufferDeviceAddress


-- | VkPhysicalDeviceBufferDeviceAddressFeaturesEXT - Structure describing
-- buffer address features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceBufferDeviceAddressFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- -   #features-bufferDeviceAddressEXT# @bufferDeviceAddress@ indicates
--     that the implementation supports accessing buffer memory in shaders
--     as storage buffers via an address queried from
--     'getBufferDeviceAddressEXT'.
--
-- -   #features-bufferDeviceAddressCaptureReplayEXT#
--     @bufferDeviceAddressCaptureReplay@ indicates that the implementation
--     supports saving and reusing buffer addresses, e.g. for trace capture
--     and replay.
--
-- -   #features-bufferDeviceAddressMultiDeviceEXT#
--     @bufferDeviceAddressMultiDevice@ indicates that the implementation
--     supports the @bufferDeviceAddress@ feature for logical devices
--     created with multiple physical devices. If this feature is not
--     supported, buffer addresses /must/ not be queried on a logical
--     device created with more than one physical device.
--
-- If the 'PhysicalDeviceBufferDeviceAddressFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceBufferDeviceAddressFeaturesEXT' /can/ also be included in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- Note
--
-- The 'PhysicalDeviceBufferDeviceAddressFeaturesEXT' structure has the
-- same members as the
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures'
-- structure, but the functionality indicated by the members is expressed
-- differently. The features indicated by the
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures'
-- structure requires additional flags to be passed at memory allocation
-- time, and the capture and replay mechanism is built around opaque
-- capture addresses for buffer and memory objects.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceBufferDeviceAddressFeaturesEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceBufferDeviceAddressFeaturesEXT = PhysicalDeviceBufferDeviceAddressFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddress"
    bufferDeviceAddress :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressCaptureReplay"
    bufferDeviceAddressCaptureReplay :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressMultiDevice"
    bufferDeviceAddressMultiDevice :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceBufferDeviceAddressFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceBufferDeviceAddressFeaturesEXT

instance ToCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceBufferDeviceAddressFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (bufferDeviceAddress))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (bufferDeviceAddressCaptureReplay))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (bufferDeviceAddressMultiDevice))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT where
  peekCStruct p = do
    bufferDeviceAddress <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    bufferDeviceAddressCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    bufferDeviceAddressMultiDevice <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceBufferDeviceAddressFeaturesEXT
             (bool32ToBool bufferDeviceAddress) (bool32ToBool bufferDeviceAddressCaptureReplay) (bool32ToBool bufferDeviceAddressMultiDevice)

instance Storable PhysicalDeviceBufferDeviceAddressFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceBufferDeviceAddressFeaturesEXT where
  zero = PhysicalDeviceBufferDeviceAddressFeaturesEXT
           zero
           zero
           zero


-- | VkBufferDeviceAddressCreateInfoEXT - Request a specific address for a
-- buffer
--
-- = Description
--
-- If @deviceAddress@ is zero, no specific address is requested.
--
-- If @deviceAddress@ is not zero, then it /must/ be an address retrieved
-- from an identically created buffer on the same implementation. The
-- buffer /must/ also be bound to an identically created
-- 'Vulkan.Core10.Handles.DeviceMemory' object.
--
-- If this structure is not present, it is as if @deviceAddress@ is zero.
--
-- Apps /should/ avoid creating buffers with app-provided addresses and
-- implementation-provided addresses in the same process, to reduce the
-- likelihood of 'ERROR_INVALID_DEVICE_ADDRESS_EXT' errors.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferDeviceAddressCreateInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferDeviceAddressCreateInfoEXT = BufferDeviceAddressCreateInfoEXT
  { -- | @deviceAddress@ is the device address requested for the buffer.
    deviceAddress :: DeviceAddress }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferDeviceAddressCreateInfoEXT)
#endif
deriving instance Show BufferDeviceAddressCreateInfoEXT

instance ToCStruct BufferDeviceAddressCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferDeviceAddressCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (deviceAddress)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (zero)
    f

instance FromCStruct BufferDeviceAddressCreateInfoEXT where
  peekCStruct p = do
    deviceAddress <- peek @DeviceAddress ((p `plusPtr` 16 :: Ptr DeviceAddress))
    pure $ BufferDeviceAddressCreateInfoEXT
             deviceAddress

instance Storable BufferDeviceAddressCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferDeviceAddressCreateInfoEXT where
  zero = BufferDeviceAddressCreateInfoEXT
           zero


-- No documentation found for TopLevel "VkPhysicalDeviceBufferAddressFeaturesEXT"
type PhysicalDeviceBufferAddressFeaturesEXT = PhysicalDeviceBufferDeviceAddressFeaturesEXT


-- No documentation found for TopLevel "VkBufferDeviceAddressInfoEXT"
type BufferDeviceAddressInfoEXT = BufferDeviceAddressInfo


type EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION"
pattern EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 2


type EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = "VK_EXT_buffer_device_address"

-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME"
pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = "VK_EXT_buffer_device_address"

