{-# language CPP #-}
-- | = Name
--
-- VK_EXT_zero_initialize_device_memory - device extension
--
-- = VK_EXT_zero_initialize_device_memory
--
-- [__Name String__]
--     @VK_EXT_zero_initialize_device_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     621
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_zero_initialize_device_memory] @zmike%0A*Here describe the issue or question you have about the VK_EXT_zero_initialize_device_memory extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_zero_initialize_device_memory.adoc VK_EXT_zero_initialize_device_memory>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-04-09
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with Vulkan 1.1.
--
--     -   Interacts with @VK_KHR_get_physical_device_properties2@.
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, VALVE
--
--     -   Mike Blumenkrantz, VALVE
--
--     -   Tobias Hector, AMD
--
--     -   Faith Ekstrand, Collabora
--
--     -   Ricardo Garcia, Igalia
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Spencer Fricke, LunarG
--
-- == Description
--
-- By default, Vulkan provides no guarantees that device memory allocated
-- through vkAllocateMemory is cleared to zero. This means that
-- applications wanting resources to be zero-initialized must execute a
-- command such as vkCmdFillBuffer or vkCmdClearColorImage on the device to
-- ensure a deterministic result. This can be wasteful if the underlying
-- platform either:
--
-- -   Already performs that zero clear anyway, due to e.g. security
--     concerns.
--
-- -   Can be performed more efficiently in implementation, by e.g.
--     clearing pages to zero in the background after device memory is
--     freed.
--
-- This extension also has uses in API layering and porting efforts, where
-- zero memory behavior may be more strict than Vulkan. Different OS
-- platforms also have wildly different behaviors here, which leads to
-- implementations needing to apply workarounds to paper over these issues
-- in the wild. If an extension exists to make allocation behavior
-- explicit, we hopefully achieve a more robust ecosystem for Vulkan.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ZERO_INITIALIZE_DEVICE_MEMORY_EXTENSION_NAME'
--
-- -   'EXT_ZERO_INITIALIZE_DEVICE_MEMORY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ZERO_INITIALIZED_EXT'
--
-- -   Extending
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlagBits':
--
--     -   'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_ZERO_INITIALIZE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_DEVICE_MEMORY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2025-03-10 (Mike Blumenkrantz)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_zero_initialize_device_memory Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_zero_initialize_device_memory  ( PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT(..)
                                                               , EXT_ZERO_INITIALIZE_DEVICE_MEMORY_SPEC_VERSION
                                                               , pattern EXT_ZERO_INITIALIZE_DEVICE_MEMORY_SPEC_VERSION
                                                               , EXT_ZERO_INITIALIZE_DEVICE_MEMORY_EXTENSION_NAME
                                                               , pattern EXT_ZERO_INITIALIZE_DEVICE_MEMORY_EXTENSION_NAME
                                                               ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_DEVICE_MEMORY_FEATURES_EXT))
-- | VkPhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT - Structure
-- describing whether the implementation supports cleared allocation
-- functionality
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_zero_initialize_device_memory VK_EXT_zero_initialize_device_memory>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT = PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT
  { -- | #features-zeroInitializeDeviceMemory# @zeroInitializeDeviceMemory@
    -- indicates that the implementation supports zeroing memory allocations
    -- using a user-specified flag.
    zeroInitializeDeviceMemory :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT

instance ToCStruct PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_DEVICE_MEMORY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zeroInitializeDeviceMemory))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_DEVICE_MEMORY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT where
  peekCStruct p = do
    zeroInitializeDeviceMemory <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT
             (bool32ToBool zeroInitializeDeviceMemory)

instance Storable PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT where
  zero = PhysicalDeviceZeroInitializeDeviceMemoryFeaturesEXT
           zero


type EXT_ZERO_INITIALIZE_DEVICE_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_ZERO_INITIALIZE_DEVICE_MEMORY_SPEC_VERSION"
pattern EXT_ZERO_INITIALIZE_DEVICE_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_ZERO_INITIALIZE_DEVICE_MEMORY_SPEC_VERSION = 1


type EXT_ZERO_INITIALIZE_DEVICE_MEMORY_EXTENSION_NAME = "VK_EXT_zero_initialize_device_memory"

-- No documentation found for TopLevel "VK_EXT_ZERO_INITIALIZE_DEVICE_MEMORY_EXTENSION_NAME"
pattern EXT_ZERO_INITIALIZE_DEVICE_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_ZERO_INITIALIZE_DEVICE_MEMORY_EXTENSION_NAME = "VK_EXT_zero_initialize_device_memory"

