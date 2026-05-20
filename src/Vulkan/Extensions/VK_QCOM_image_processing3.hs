{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_image_processing3 - device extension
--
-- = VK_QCOM_image_processing3
--
-- [__Name String__]
--     @VK_QCOM_image_processing3@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     304
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_QCOM_image_processing
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_image_processing3.html SPV_QCOM_image_processing3>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_image_processing3] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_image_processing3 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_image_processing3.adoc VK_QCOM_image_processing3>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-05-08
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/qcom/GLSL_QCOM_image_processing3.txt GL_QCOM_image_processing3>
--
--     -   Interacts with @VK_QCOM_tile_shading@
--
--     -   Interacts with @VK_QCOM_image_processing@
--
--     -   Interacts with @VK_QCOM_image_processing2@
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Liang Li, Qualcomm Technologies, Inc.
--
--     -   Wooyoung Kim, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension introduces a new SPIR-V built-in function to support
-- predefined image gather operations used in popular image processing
-- algorithms such as super resolution upscaling and contrast-adaptive
-- sharpening.
--
-- The @OpImageGatherQCOM@ instruction supports the following modes:
--
-- -   @GatherH2QCOM@ - produces an image gather with an extra horizontal
--     offset
--
-- -   @GatherV2QCOM@ - produces an image gather with an extra vertical
--     offset. Combined with @OpImageGather@ and @GatherH2QCOM@, this is
--     useful for creating a 12-tap filter for upscaling.
--
-- -   @GatherDQCOM@ - produces an image gather by sampling the cardinal
--     offsets. Combined with a point sample of the center texel, this is
--     useful for creating a 5-tap sharpening filter (eg. CAS).
--
-- -   @Gather4x1QCOM@ - produces an image gather by sampling 4 texels in a
--     horizontal row. This is useful for kernels requiring vectorized
--     loads, and can help with cache locality for linear access.
--
-- Each of the image processing instructions operate on the same sampled
-- images that the @OpImage*Gather@ instructions support with the exception
-- of cube-maps, depth comparison, @ConstOffsets@, and sparse residency
-- check.
--
-- Implementations of this extension should support these operations
-- natively at the HW instruction level, offering potential performance
-- gains as well as ease of development.
--
-- This extension also adds some block matching improvements over
-- @VK_QCOM_image_processing@ and @VK_QCOM_image_processing2@ by exposing
-- more formats and wrap modes.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageProcessing3FeaturesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_IMAGE_PROCESSING_3_EXTENSION_NAME'
--
-- -   'QCOM_IMAGE_PROCESSING_3_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_3_FEATURES_QCOM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing VK_QCOM_image_processing>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_SXD_BIT_QCOM'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-ImageGatherLinearQCOM ImageGatherLinearQCOM>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-ImageGatherExtendedModesQCOM ImageGatherExtendedModesQCOM>
--
-- == Version History
--
-- -   Revision 1, 2026-05-08 (Matthew Netsch)
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_image_processing3 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_image_processing3  ( PhysicalDeviceImageProcessing3FeaturesQCOM(..)
                                                    , QCOM_IMAGE_PROCESSING_3_SPEC_VERSION
                                                    , pattern QCOM_IMAGE_PROCESSING_3_SPEC_VERSION
                                                    , QCOM_IMAGE_PROCESSING_3_EXTENSION_NAME
                                                    , pattern QCOM_IMAGE_PROCESSING_3_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_3_FEATURES_QCOM))
-- | VkPhysicalDeviceImageProcessing3FeaturesQCOM - Structure describing
-- image processing features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceImageProcessing3FeaturesQCOM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceImageProcessing3FeaturesQCOM', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing3 VK_QCOM_image_processing3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageProcessing3FeaturesQCOM = PhysicalDeviceImageProcessing3FeaturesQCOM
  { -- | #features-imageGatherLinear# @imageGatherLinear@ indicates that the
    -- implementation supports shader modules that declare the
    -- @ImageGatherLinearQCOM@ capability.
    imageGatherLinear :: Bool
  , -- | #features-imageGatherExtendedModes# @imageGatherExtendedModes@ indicates
    -- that the implementation supports shader modules that declare the
    -- @ImageGatherExtendedModesQCOM@ capability.
    imageGatherExtendedModes :: Bool
  , -- | #features-blockMatchExtendedClampToEdge# @blockMatchExtendedClampToEdge@
    -- indicates that the implementation extends support for
    -- 'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
    -- to both input images of all
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#textures-blockmatch block matching operations>.
    blockMatchExtendedClampToEdge :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageProcessing3FeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceImageProcessing3FeaturesQCOM

instance ToCStruct PhysicalDeviceImageProcessing3FeaturesQCOM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageProcessing3FeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_3_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (imageGatherLinear))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (imageGatherExtendedModes))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (blockMatchExtendedClampToEdge))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_3_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageProcessing3FeaturesQCOM where
  peekCStruct p = do
    imageGatherLinear <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    imageGatherExtendedModes <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    blockMatchExtendedClampToEdge <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceImageProcessing3FeaturesQCOM
             (bool32ToBool imageGatherLinear)
             (bool32ToBool imageGatherExtendedModes)
             (bool32ToBool blockMatchExtendedClampToEdge)

instance Storable PhysicalDeviceImageProcessing3FeaturesQCOM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageProcessing3FeaturesQCOM where
  zero = PhysicalDeviceImageProcessing3FeaturesQCOM
           zero
           zero
           zero


type QCOM_IMAGE_PROCESSING_3_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_IMAGE_PROCESSING_3_SPEC_VERSION"
pattern QCOM_IMAGE_PROCESSING_3_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_IMAGE_PROCESSING_3_SPEC_VERSION = 1


type QCOM_IMAGE_PROCESSING_3_EXTENSION_NAME = "VK_QCOM_image_processing3"

-- No documentation found for TopLevel "VK_QCOM_IMAGE_PROCESSING_3_EXTENSION_NAME"
pattern QCOM_IMAGE_PROCESSING_3_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_IMAGE_PROCESSING_3_EXTENSION_NAME = "VK_QCOM_image_processing3"

