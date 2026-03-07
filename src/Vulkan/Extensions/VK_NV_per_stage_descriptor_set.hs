{-# language CPP #-}
-- | = Name
--
-- VK_NV_per_stage_descriptor_set - device extension
--
-- = VK_NV_per_stage_descriptor_set
--
-- [__Name String__]
--     @VK_NV_per_stage_descriptor_set@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     517
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_per_stage_descriptor_set] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_NV_per_stage_descriptor_set extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-10-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- This extension introduces a new descriptor set layout creation flag that
-- allows bindings in a descriptor set to be scoped to each shader stage.
-- This means that shaders bound at the same time /may/ use completely
-- different descriptor set layouts without any restrictions on
-- compatibility, and that the descriptor limits that would otherwise apply
-- to the union of all stages together instead apply to each stage
-- individually. It also means that descriptors shared by multiple stages
-- /must/ be bound to each stage or set of stages that use a unique
-- descriptor set layout using their specific per stage descriptor set
-- layout(s).
--
-- This extension also allows each of the new descriptor binding functions
-- from VK_KHR_maintenance6 to have their
-- 'Vulkan.Core10.Handles.PipelineLayout' member be optionally set to
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', in which case the pipeline
-- layout information is taken from a
-- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure in the
-- @pNext@ chain. This enables descriptors to be directly bound using
-- descriptor set layouts without applications needing to create and manage
-- 'Vulkan.Core10.Handles.PipelineLayout' objects at command recording
-- time.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePerStageDescriptorSetFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_PER_STAGE_DESCRIPTOR_SET_EXTENSION_NAME'
--
-- -   'NV_PER_STAGE_DESCRIPTOR_SET_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PER_STAGE_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PER_STAGE_DESCRIPTOR_SET_FEATURES_NV'
--
-- == Issues
--
-- None
--
-- == Version History
--
-- -   Revision 1, 2023-10-16 (Piers Daniell)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_per_stage_descriptor_set Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_per_stage_descriptor_set  ( PhysicalDevicePerStageDescriptorSetFeaturesNV(..)
                                                         , NV_PER_STAGE_DESCRIPTOR_SET_SPEC_VERSION
                                                         , pattern NV_PER_STAGE_DESCRIPTOR_SET_SPEC_VERSION
                                                         , NV_PER_STAGE_DESCRIPTOR_SET_EXTENSION_NAME
                                                         , pattern NV_PER_STAGE_DESCRIPTOR_SET_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PER_STAGE_DESCRIPTOR_SET_FEATURES_NV))
-- | VkPhysicalDevicePerStageDescriptorSetFeaturesNV - Structure describing
-- feature to allow descriptor set layout bindings to be per-stage
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePerStageDescriptorSetFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePerStageDescriptorSetFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_per_stage_descriptor_set VK_NV_per_stage_descriptor_set>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePerStageDescriptorSetFeaturesNV = PhysicalDevicePerStageDescriptorSetFeaturesNV
  { -- | #features-perStageDescriptorSet# @perStageDescriptorSet@ indicates that
    -- the implementation allows the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PER_STAGE_BIT_NV'
    -- descriptor set layout creation flag to be used so the bindings are
    -- specified per-stage rather than across all stages.
    perStageDescriptorSet :: Bool
  , -- | #features-dynamicPipelineLayout# @dynamicPipelineLayout@ indicates the
    -- implementation allows the @layout@ member of
    -- 'Vulkan.Extensions.VK_KHR_maintenance6.BindDescriptorSetsInfoKHR',
    -- 'Vulkan.Extensions.VK_KHR_maintenance6.PushConstantsInfoKHR',
    -- 'Vulkan.Extensions.VK_KHR_maintenance6.PushDescriptorSetInfoKHR',
    -- 'Vulkan.Extensions.VK_KHR_maintenance6.PushDescriptorSetWithTemplateInfoKHR',
    -- 'Vulkan.Extensions.VK_KHR_maintenance6.SetDescriptorBufferOffsetsInfoEXT'
    -- and
    -- 'Vulkan.Extensions.VK_KHR_maintenance6.BindDescriptorBufferEmbeddedSamplersInfoEXT'
    -- to be 'Vulkan.Core10.APIConstants.NULL_HANDLE' and
    -- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' /can/ be chained
    -- off those structures\' @pNext@ instead.
    dynamicPipelineLayout :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePerStageDescriptorSetFeaturesNV)
#endif
deriving instance Show PhysicalDevicePerStageDescriptorSetFeaturesNV

instance ToCStruct PhysicalDevicePerStageDescriptorSetFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePerStageDescriptorSetFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PER_STAGE_DESCRIPTOR_SET_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (perStageDescriptorSet))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (dynamicPipelineLayout))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PER_STAGE_DESCRIPTOR_SET_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePerStageDescriptorSetFeaturesNV where
  peekCStruct p = do
    perStageDescriptorSet <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    dynamicPipelineLayout <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDevicePerStageDescriptorSetFeaturesNV
             (bool32ToBool perStageDescriptorSet)
             (bool32ToBool dynamicPipelineLayout)

instance Storable PhysicalDevicePerStageDescriptorSetFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePerStageDescriptorSetFeaturesNV where
  zero = PhysicalDevicePerStageDescriptorSetFeaturesNV
           zero
           zero


type NV_PER_STAGE_DESCRIPTOR_SET_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_PER_STAGE_DESCRIPTOR_SET_SPEC_VERSION"
pattern NV_PER_STAGE_DESCRIPTOR_SET_SPEC_VERSION :: forall a . Integral a => a
pattern NV_PER_STAGE_DESCRIPTOR_SET_SPEC_VERSION = 1


type NV_PER_STAGE_DESCRIPTOR_SET_EXTENSION_NAME = "VK_NV_per_stage_descriptor_set"

-- No documentation found for TopLevel "VK_NV_PER_STAGE_DESCRIPTOR_SET_EXTENSION_NAME"
pattern NV_PER_STAGE_DESCRIPTOR_SET_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_PER_STAGE_DESCRIPTOR_SET_EXTENSION_NAME = "VK_NV_per_stage_descriptor_set"

