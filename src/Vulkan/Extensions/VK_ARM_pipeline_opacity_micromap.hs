{-# language CPP #-}
-- | = Name
--
-- VK_ARM_pipeline_opacity_micromap - device extension
--
-- = VK_ARM_pipeline_opacity_micromap
--
-- [__Name String__]
--     @VK_ARM_pipeline_opacity_micromap@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     597
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>
--
-- [__Contact__]
--
--     -   Mathieu Robart
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_pipeline_opacity_micromap] @mathieurobart-arm%0A*Here describe the issue or question you have about the VK_ARM_pipeline_opacity_micromap extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ARM_pipeline_opacity_micromap.adoc VK_ARM_pipeline_opacity_micromap>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-07
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mathieu Robart, Arm
--
--     -   Marius Bjorge, Arm
--
--     -   Yaozhong Zhang, Arm
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- The Opacity Micromap extension @VK_EXT_opacity_micromap@ supports the
-- new pipeline creation flag
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT',
-- indicating that the ray tracing pipeline may be used with acceleration
-- structures referencing micromaps. This allows for possible
-- optimizations, knowing beforehand that opacity micromaps may be used
-- with the pipeline.
--
-- An equivalent flag does not exist for pipelines supporting Ray Query
-- with opacity micromaps, such as graphics and compute. Consequently, it
-- is currently not possible to optimize such pipelines for no-opacity,
-- e.g. when opacity micromaps are supported by an application but not used
-- by the pipeline. This may lead to performance degradation.
--
-- This extension adds a new flag,
-- 'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DISALLOW_OPACITY_MICROMAP_BIT_ARM',
-- indicating that a pipeline will NOT be used with an acceleration
-- structure referencing an opacity micromap, therefore allowing possible
-- pipeline optimizations.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineOpacityMicromapFeaturesARM'
--
-- == New Enum Constants
--
-- -   'ARM_PIPELINE_OPACITY_MICROMAP_EXTENSION_NAME'
--
-- -   'ARM_PIPELINE_OPACITY_MICROMAP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DISALLOW_OPACITY_MICROMAP_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_OPACITY_MICROMAP_FEATURES_ARM'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-01-07 (Mathieu Robart)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_pipeline_opacity_micromap Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_pipeline_opacity_micromap  ( PhysicalDevicePipelineOpacityMicromapFeaturesARM(..)
                                                           , ARM_PIPELINE_OPACITY_MICROMAP_SPEC_VERSION
                                                           , pattern ARM_PIPELINE_OPACITY_MICROMAP_SPEC_VERSION
                                                           , ARM_PIPELINE_OPACITY_MICROMAP_EXTENSION_NAME
                                                           , pattern ARM_PIPELINE_OPACITY_MICROMAP_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_OPACITY_MICROMAP_FEATURES_ARM))
-- | VkPhysicalDevicePipelineOpacityMicromapFeaturesARM - Structure
-- describing features supported by VK_ARM_pipeline_opacity_micromap
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDevicePipelineOpacityMicromapFeaturesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDevicePipelineOpacityMicromapFeaturesARM', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_pipeline_opacity_micromap VK_ARM_pipeline_opacity_micromap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineOpacityMicromapFeaturesARM = PhysicalDevicePipelineOpacityMicromapFeaturesARM
  { -- | #features-pipelineOpacityMicromap# @pipelineOpacityMicromap@ indicates
    -- if a pipeline /can/ declare if it can be used with an acceleration
    -- structure referencing an opacity micromap, or not.
    pipelineOpacityMicromap :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineOpacityMicromapFeaturesARM)
#endif
deriving instance Show PhysicalDevicePipelineOpacityMicromapFeaturesARM

instance ToCStruct PhysicalDevicePipelineOpacityMicromapFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineOpacityMicromapFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_OPACITY_MICROMAP_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineOpacityMicromap))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_OPACITY_MICROMAP_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineOpacityMicromapFeaturesARM where
  peekCStruct p = do
    pipelineOpacityMicromap <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineOpacityMicromapFeaturesARM
             (bool32ToBool pipelineOpacityMicromap)

instance Storable PhysicalDevicePipelineOpacityMicromapFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineOpacityMicromapFeaturesARM where
  zero = PhysicalDevicePipelineOpacityMicromapFeaturesARM
           zero


type ARM_PIPELINE_OPACITY_MICROMAP_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_PIPELINE_OPACITY_MICROMAP_SPEC_VERSION"
pattern ARM_PIPELINE_OPACITY_MICROMAP_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_PIPELINE_OPACITY_MICROMAP_SPEC_VERSION = 1


type ARM_PIPELINE_OPACITY_MICROMAP_EXTENSION_NAME = "VK_ARM_pipeline_opacity_micromap"

-- No documentation found for TopLevel "VK_ARM_PIPELINE_OPACITY_MICROMAP_EXTENSION_NAME"
pattern ARM_PIPELINE_OPACITY_MICROMAP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_PIPELINE_OPACITY_MICROMAP_EXTENSION_NAME = "VK_ARM_pipeline_opacity_micromap"

