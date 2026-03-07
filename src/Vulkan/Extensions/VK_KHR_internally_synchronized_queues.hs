{-# language CPP #-}
-- | = Name
--
-- VK_KHR_internally_synchronized_queues - device extension
--
-- = VK_KHR_internally_synchronized_queues
--
-- [__Name String__]
--     @VK_KHR_internally_synchronized_queues@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     505
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_internally_synchronized_queues] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_internally_synchronized_queues extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_internally_synchronized_queues.adoc VK_KHR_internally_synchronized_queues>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-02-04
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Daniel Rakos, RasterGrid
--
--     -   Jeff Bolz, Nvidia
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_internally_synchronized_queues VK_KHR_internally_synchronized_queues>
-- allows queues to opt into being internally synchronized via the
-- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_INTERNALLY_SYNCHRONIZED_BIT_KHR'
-- flag.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_INTERNALLY_SYNCHRONIZED_QUEUES_EXTENSION_NAME'
--
-- -   'KHR_INTERNALLY_SYNCHRONIZED_QUEUES_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_INTERNALLY_SYNCHRONIZED_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INTERNALLY_SYNCHRONIZED_QUEUES_FEATURES_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 0, 2025-01-07 (Daniel Rakos)
--
--     -   Initial proposal
--
-- -   Revision 1, 2025-02-04 (Shahbaz Youssefi)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_internally_synchronized_queues Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_internally_synchronized_queues  ( PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR(..)
                                                                , KHR_INTERNALLY_SYNCHRONIZED_QUEUES_SPEC_VERSION
                                                                , pattern KHR_INTERNALLY_SYNCHRONIZED_QUEUES_SPEC_VERSION
                                                                , KHR_INTERNALLY_SYNCHRONIZED_QUEUES_EXTENSION_NAME
                                                                , pattern KHR_INTERNALLY_SYNCHRONIZED_QUEUES_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INTERNALLY_SYNCHRONIZED_QUEUES_FEATURES_KHR))
-- | VkPhysicalDeviceInternallySynchronizedQueuesFeaturesKHR - Structure
-- indicating support for queues that are internally synchronized
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR', it /must/ add
-- an instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_internally_synchronized_queues VK_KHR_internally_synchronized_queues>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR = PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR
  { -- | #features-internallySynchronizedQueues# @internallySynchronizedQueues@
    -- indicates that
    -- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_INTERNALLY_SYNCHRONIZED_BIT_KHR'
    -- can be used to make queues internally synchronized.
    internallySynchronizedQueues :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR

instance ToCStruct PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INTERNALLY_SYNCHRONIZED_QUEUES_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (internallySynchronizedQueues))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INTERNALLY_SYNCHRONIZED_QUEUES_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR where
  peekCStruct p = do
    internallySynchronizedQueues <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR
             (bool32ToBool internallySynchronizedQueues)

instance Storable PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR where
  zero = PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR
           zero


type KHR_INTERNALLY_SYNCHRONIZED_QUEUES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_INTERNALLY_SYNCHRONIZED_QUEUES_SPEC_VERSION"
pattern KHR_INTERNALLY_SYNCHRONIZED_QUEUES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_INTERNALLY_SYNCHRONIZED_QUEUES_SPEC_VERSION = 1


type KHR_INTERNALLY_SYNCHRONIZED_QUEUES_EXTENSION_NAME = "VK_KHR_internally_synchronized_queues"

-- No documentation found for TopLevel "VK_KHR_INTERNALLY_SYNCHRONIZED_QUEUES_EXTENSION_NAME"
pattern KHR_INTERNALLY_SYNCHRONIZED_QUEUES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_INTERNALLY_SYNCHRONIZED_QUEUES_EXTENSION_NAME = "VK_KHR_internally_synchronized_queues"

