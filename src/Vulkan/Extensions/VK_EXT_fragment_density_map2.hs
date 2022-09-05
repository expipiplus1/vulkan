{-# language CPP #-}
-- | = Name
--
-- VK_EXT_fragment_density_map2 - device extension
--
-- == VK_EXT_fragment_density_map2
--
-- [__Name String__]
--     @VK_EXT_fragment_density_map2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     333
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_EXT_fragment_density_map@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_fragment_density_map2] @mnetsch%0A<<Here describe the issue or question you have about the VK_EXT_fragment_density_map2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-06-16
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with Vulkan 1.1
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Jonathan Tinkham, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Jan-Harald Fredriksen, ARM
--
-- == Description
--
-- This extension adds additional features and properties to
-- @VK_EXT_fragment_density_map@ in order to reduce fragment density map
-- host latency as well as improved queries for subsampled sampler
-- implementation-dependent behavior.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentDensityMap2FeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMap2PropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME'
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-06-16 (Matthew Netsch)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceFragmentDensityMap2FeaturesEXT',
-- 'PhysicalDeviceFragmentDensityMap2PropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_fragment_density_map2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_fragment_density_map2  ( PhysicalDeviceFragmentDensityMap2FeaturesEXT(..)
                                                       , PhysicalDeviceFragmentDensityMap2PropertiesEXT(..)
                                                       , EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION
                                                       , pattern EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION
                                                       , EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME
                                                       , pattern EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT))
-- | VkPhysicalDeviceFragmentDensityMap2FeaturesEXT - Structure describing
-- additional fragment density map features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentDensityMap2FeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceFragmentDensityMap2FeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map2 VK_EXT_fragment_density_map2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMap2FeaturesEXT = PhysicalDeviceFragmentDensityMap2FeaturesEXT
  { -- | #features-fragmentDensityMapDeferred# @fragmentDensityMapDeferred@
    -- specifies whether the implementation supports deferred reads of fragment
    -- density map image views. If this feature is not enabled,
    -- 'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT'
    -- /must/ not be included in
    -- 'Vulkan.Core10.ImageView.ImageViewCreateInfo'::@flags@.
    fragmentDensityMapDeferred :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMap2FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMap2FeaturesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMap2FeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMap2FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMapDeferred))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentDensityMap2FeaturesEXT where
  peekCStruct p = do
    fragmentDensityMapDeferred <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentDensityMap2FeaturesEXT
             (bool32ToBool fragmentDensityMapDeferred)

instance Storable PhysicalDeviceFragmentDensityMap2FeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMap2FeaturesEXT where
  zero = PhysicalDeviceFragmentDensityMap2FeaturesEXT
           zero


-- | VkPhysicalDeviceFragmentDensityMap2PropertiesEXT - Structure describing
-- additional fragment density map properties that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentDensityMap2PropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map2 VK_EXT_fragment_density_map2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMap2PropertiesEXT = PhysicalDeviceFragmentDensityMap2PropertiesEXT
  { -- | #limits-subsampledLoads# @subsampledLoads@ specifies if performing image
    -- data read with load operations on subsampled attachments will be
    -- resampled to the fragment density of the render pass
    subsampledLoads :: Bool
  , -- | #limits-subsampledCoarseReconstructionEarlyAccess#
    -- @subsampledCoarseReconstructionEarlyAccess@ specifies if performing
    -- image data read with samplers created with @flags@ containing
    -- 'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT'
    -- in fragment shader will trigger additional reads during
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_VERTEX_SHADER_BIT'
    subsampledCoarseReconstructionEarlyAccess :: Bool
  , -- | #limits-maxSubsampledArrayLayers# @maxSubsampledArrayLayers@ is the
    -- maximum number of 'Vulkan.Core10.Handles.ImageView' array layers for
    -- usages supporting subsampled samplers
    maxSubsampledArrayLayers :: Word32
  , -- | #limits-maxDescriptorSetSubsampledSamplers#
    -- @maxDescriptorSetSubsampledSamplers@ is the maximum number of subsampled
    -- samplers that /can/ be included in a
    -- 'Vulkan.Core10.Handles.PipelineLayout'
    maxDescriptorSetSubsampledSamplers :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMap2PropertiesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMap2PropertiesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMap2PropertiesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMap2PropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (subsampledLoads))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (subsampledCoarseReconstructionEarlyAccess))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxSubsampledArrayLayers)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxDescriptorSetSubsampledSamplers)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceFragmentDensityMap2PropertiesEXT where
  peekCStruct p = do
    subsampledLoads <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    subsampledCoarseReconstructionEarlyAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    maxSubsampledArrayLayers <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxDescriptorSetSubsampledSamplers <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ PhysicalDeviceFragmentDensityMap2PropertiesEXT
             (bool32ToBool subsampledLoads) (bool32ToBool subsampledCoarseReconstructionEarlyAccess) maxSubsampledArrayLayers maxDescriptorSetSubsampledSamplers

instance Storable PhysicalDeviceFragmentDensityMap2PropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMap2PropertiesEXT where
  zero = PhysicalDeviceFragmentDensityMap2PropertiesEXT
           zero
           zero
           zero
           zero


type EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION"
pattern EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION = 1


type EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME = "VK_EXT_fragment_density_map2"

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME"
pattern EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME = "VK_EXT_fragment_density_map2"

