{-# language CPP #-}
-- | = Name
--
-- VK_NV_command_buffer_inheritance - device extension
--
-- = VK_NV_command_buffer_inheritance
--
-- [__Name String__]
--     @VK_NV_command_buffer_inheritance@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     560
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Lujin Wang
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_command_buffer_inheritance] @lujinwangnv%0A*Here describe the issue or question you have about the VK_NV_command_buffer_inheritance extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-02-15
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- This extension allows applications to take advantage of the graphics and
-- compute state that remains valid in the queue between executions of
-- submitted command buffers. This works across both primary and secondary
-- command buffers.
--
-- The state inherited includes the previously bound pipeline state,
-- previously bound shader objects, previously bound vertex and index
-- buffers, previously bound descriptor sets and push constants, and all
-- previously set dynamic state.
--
-- This extension relaxes the requirement that all that state needs to be
-- bound and set after begin command buffer and before the next draw or
-- dispatch.
--
-- By not having to set state that has been inherited applications can save
-- both CPU and GPU cycles by not having to set state redundantly, and also
-- have improved flexibility when reusing secondary command buffers.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCommandBufferInheritanceFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_COMMAND_BUFFER_INHERITANCE_EXTENSION_NAME'
--
-- -   'NV_COMMAND_BUFFER_INHERITANCE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMMAND_BUFFER_INHERITANCE_FEATURES_NV'
--
-- == Issues
--
-- 1) How can the validation layer know when state is valid at draw or
-- dispatch time if it is inherited at execution time?
--
-- __RESOLVED__: Validation of invalid state at draw and dispatch time
-- cannot be done while recording those commands. Instead the validation
-- layer will need to keep track of any unset state when draw and dispatch
-- commands are recorded, but not report an error at that time. It should
-- also keep track of what state is valid at the end of each recorded
-- command buffer. When secondary command buffer execution is recorded the
-- validation layer can update its unset state tracking for that command
-- buffer, and also for draw and dispatch commands recorded after execution
-- of the secondary as they will inherit state from the executed secondary.
-- This can be done recursively so every recorded primary command buffer
-- has a final tally of any unset state used at draw and dispatch time.
-- Finally when the primary is submitted to the queue the validation layer
-- will know the previous primaries submitted to the queue and will know if
-- there is any unset state used and can report the error then.
--
-- == Version History
--
-- -   Revision 1, 2024-02-15 (Lujin Wang)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_command_buffer_inheritance Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_command_buffer_inheritance  ( PhysicalDeviceCommandBufferInheritanceFeaturesNV(..)
                                                           , NV_COMMAND_BUFFER_INHERITANCE_SPEC_VERSION
                                                           , pattern NV_COMMAND_BUFFER_INHERITANCE_SPEC_VERSION
                                                           , NV_COMMAND_BUFFER_INHERITANCE_EXTENSION_NAME
                                                           , pattern NV_COMMAND_BUFFER_INHERITANCE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COMMAND_BUFFER_INHERITANCE_FEATURES_NV))
-- | VkPhysicalDeviceCommandBufferInheritanceFeaturesNV - Structure
-- describing whether the command buffer inheritance feature is supported
-- by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceCommandBufferInheritanceFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceCommandBufferInheritanceFeaturesNV' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_command_buffer_inheritance VK_NV_command_buffer_inheritance>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCommandBufferInheritanceFeaturesNV = PhysicalDeviceCommandBufferInheritanceFeaturesNV
  { -- | #features-commandBufferInheritance# @commandBufferInheritance@ indicates
    -- that command buffers executed in a queue inherit graphics and compute
    -- state from the previously executed command buffer in that queue.
    commandBufferInheritance :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCommandBufferInheritanceFeaturesNV)
#endif
deriving instance Show PhysicalDeviceCommandBufferInheritanceFeaturesNV

instance ToCStruct PhysicalDeviceCommandBufferInheritanceFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCommandBufferInheritanceFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMMAND_BUFFER_INHERITANCE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (commandBufferInheritance))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMMAND_BUFFER_INHERITANCE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCommandBufferInheritanceFeaturesNV where
  peekCStruct p = do
    commandBufferInheritance <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCommandBufferInheritanceFeaturesNV
             (bool32ToBool commandBufferInheritance)

instance Storable PhysicalDeviceCommandBufferInheritanceFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCommandBufferInheritanceFeaturesNV where
  zero = PhysicalDeviceCommandBufferInheritanceFeaturesNV
           zero


type NV_COMMAND_BUFFER_INHERITANCE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COMMAND_BUFFER_INHERITANCE_SPEC_VERSION"
pattern NV_COMMAND_BUFFER_INHERITANCE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COMMAND_BUFFER_INHERITANCE_SPEC_VERSION = 1


type NV_COMMAND_BUFFER_INHERITANCE_EXTENSION_NAME = "VK_NV_command_buffer_inheritance"

-- No documentation found for TopLevel "VK_NV_COMMAND_BUFFER_INHERITANCE_EXTENSION_NAME"
pattern NV_COMMAND_BUFFER_INHERITANCE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COMMAND_BUFFER_INHERITANCE_EXTENSION_NAME = "VK_NV_command_buffer_inheritance"

