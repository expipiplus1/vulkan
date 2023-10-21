{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_image_processing - device extension
--
-- == VK_QCOM_image_processing
--
-- [__Name String__]
--     @VK_QCOM_image_processing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     441
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_image_processing] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_image_processing extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_image_processing.adoc VK_QCOM_image_processing>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-08
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_image_processing.html SPV_QCOM_image_processing>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/qcom/GLSL_QCOM_image_processing.txt GL_QCOM_image_processing>
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Ruihao Zhang, Qualcomm Technologies, Inc.
--
-- == Description
--
-- GPUs are commonly used to process images for various applications from
-- 3D graphics to UI and from composition to compute applications. Simple
-- scaling and filtering can be done with bilinear filtering, which comes
-- for free during texture sampling. However, as screen sizes get larger
-- and more use cases rely on GPU such as camera and video post-processing
-- needs, there is increasing demand for GPU to support higher order
-- filtering and other advanced image processing.
--
-- This extension introduces a new set of SPIR-V built-in functions for
-- image processing. It exposes the following new imaging operations
--
-- -   The @OpImageSampleWeightedQCOM@ instruction takes 3 operands:
--     /sampled image/, /weight image/, and texture coordinates. The
--     instruction computes a weighted average of an MxN region of texels
--     in the /sampled image/, using a set of MxN weights in the /weight
--     image/.
--
-- -   The @OpImageBoxFilterQCOM@ instruction takes 3 operands: /sampled
--     image/, /box size/, and texture coordinates. Note that /box size/
--     specifies a floating point width and height in texels. The
--     instruction computes a weighted average of all texels in the
--     /sampled image/ that are covered (either partially or fully) by a
--     box with the specified size and centered at the specified texture
--     coordinates.
--
-- -   The @OpImageBlockMatchSADQCOM@ and @OpImageBlockMatchSSDQCOM@
--     instructions each takes 5 operands: /target image/, /target
--     coordinates/, /reference image/, /reference coordinates/, and /block
--     size/. Each instruction computes an error metric, that describes
--     whether a block of texels in the /target image/ matches a
--     corresponding block of texels in the /reference image/. The error
--     metric is computed per-component. @OpImageBlockMatchSADQCOM@
--     computes \"Sum Of Absolute Difference\" and
--     @OpImageBlockMatchSSDQCOM@ computes \"Sum of Squared Difference\".
--
-- Each of the image processing instructions operate only on 2D images. The
-- instructions do not-support sampling of mipmap, multi-plane,
-- multi-layer, multi-sampled, or depth\/stencil images. The instructions
-- can be used in any shader stage.
--
-- Implementations of this this extension should support these operations
-- natively at the HW instruction level, offering potential performance
-- gains as well as ease of development.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'ImageViewSampleWeightCreateInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageProcessingFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceImageProcessingPropertiesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_IMAGE_PROCESSING_EXTENSION_NAME'
--
-- -   'QCOM_IMAGE_PROCESSING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM'
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLE_BLOCK_MATCH_BIT_QCOM'
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLE_WEIGHT_BIT_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_SAMPLE_WEIGHT_CREATE_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_PROPERTIES_QCOM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2022-07-08 (Jeff Leger)
--
-- == See Also
--
-- 'ImageViewSampleWeightCreateInfoQCOM',
-- 'PhysicalDeviceImageProcessingFeaturesQCOM',
-- 'PhysicalDeviceImageProcessingPropertiesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_image_processing Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_image_processing  ( ImageViewSampleWeightCreateInfoQCOM(..)
                                                   , PhysicalDeviceImageProcessingFeaturesQCOM(..)
                                                   , PhysicalDeviceImageProcessingPropertiesQCOM(..)
                                                   , QCOM_IMAGE_PROCESSING_SPEC_VERSION
                                                   , pattern QCOM_IMAGE_PROCESSING_SPEC_VERSION
                                                   , QCOM_IMAGE_PROCESSING_EXTENSION_NAME
                                                   , pattern QCOM_IMAGE_PROCESSING_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Offset2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_SAMPLE_WEIGHT_CREATE_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_PROPERTIES_QCOM))
-- | VkImageViewSampleWeightCreateInfoQCOM - Structure describing weight
-- sampling parameters for image view
--
-- = Description
--
-- The @filterCenter@ specifies the origin or center of the filter kernel,
-- as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-weightimage-filteroperation Weight Sampling Operation>.
-- The @numPhases@ describes the number of sub-pixel filter phases as
-- described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-weightimage-filterphases Weight Sampling Phases>.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing VK_QCOM_image_processing>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.FundamentalTypes.Offset2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageViewSampleWeightCreateInfoQCOM = ImageViewSampleWeightCreateInfoQCOM
  { -- | @filterCenter@ is a 'Vulkan.Core10.FundamentalTypes.Offset2D' describing
    -- the location of the weight filter origin.
    filterCenter :: Offset2D
  , -- | @filterSize@ is a 'Vulkan.Core10.FundamentalTypes.Extent2D' specifying
    -- weight filter dimensions.
    filterSize :: Extent2D
  , -- | @numPhases@ is number of sub-pixel filter phases.
    --
    -- #VUID-VkImageViewSampleWeightCreateInfoQCOM-numPhases-06962# @numPhases@
    -- /must/ be a power of two squared value (i.e., 1, 4, 16, 64, 256, etc.)
    --
    -- #VUID-VkImageViewSampleWeightCreateInfoQCOM-numPhases-06963# @numPhases@
    -- /must/ be less than or equal to
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-weightfilter-phases ::maxWeightFilterPhases>
    numPhases :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewSampleWeightCreateInfoQCOM)
#endif
deriving instance Show ImageViewSampleWeightCreateInfoQCOM

instance ToCStruct ImageViewSampleWeightCreateInfoQCOM where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewSampleWeightCreateInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_SAMPLE_WEIGHT_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Offset2D)) (filterCenter)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (filterSize)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (numPhases)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_SAMPLE_WEIGHT_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageViewSampleWeightCreateInfoQCOM where
  peekCStruct p = do
    filterCenter <- peekCStruct @Offset2D ((p `plusPtr` 16 :: Ptr Offset2D))
    filterSize <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    numPhases <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ ImageViewSampleWeightCreateInfoQCOM
             filterCenter filterSize numPhases

instance Storable ImageViewSampleWeightCreateInfoQCOM where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageViewSampleWeightCreateInfoQCOM where
  zero = ImageViewSampleWeightCreateInfoQCOM
           zero
           zero
           zero


-- | VkPhysicalDeviceImageProcessingFeaturesQCOM - Structure describing image
-- processing features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceImageProcessingFeaturesQCOM' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImageProcessingFeaturesQCOM' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing VK_QCOM_image_processing>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageProcessingFeaturesQCOM = PhysicalDeviceImageProcessingFeaturesQCOM
  { -- | #features-textureSampleWeighted# @textureSampleWeighted@ indicates that
    -- the implementation supports shader modules that declare the
    -- @TextureSampleWeightedQCOM@ capability.
    textureSampleWeighted :: Bool
  , -- | #features-textureBoxFilter# @textureBoxFilter@ indicates that the
    -- implementation supports shader modules that declare the
    -- @TextureBoxFilterQCOM@ capability.
    textureBoxFilter :: Bool
  , -- | #features-textureBlockMatch# @textureBlockMatch@ indicates that the
    -- implementation supports shader modules that declare the
    -- @TextureBlockMatchQCOM@ capability.
    textureBlockMatch :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageProcessingFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceImageProcessingFeaturesQCOM

instance ToCStruct PhysicalDeviceImageProcessingFeaturesQCOM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageProcessingFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (textureSampleWeighted))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (textureBoxFilter))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (textureBlockMatch))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageProcessingFeaturesQCOM where
  peekCStruct p = do
    textureSampleWeighted <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    textureBoxFilter <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    textureBlockMatch <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceImageProcessingFeaturesQCOM
             (bool32ToBool textureSampleWeighted)
             (bool32ToBool textureBoxFilter)
             (bool32ToBool textureBlockMatch)

instance Storable PhysicalDeviceImageProcessingFeaturesQCOM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageProcessingFeaturesQCOM where
  zero = PhysicalDeviceImageProcessingFeaturesQCOM
           zero
           zero
           zero


-- | VkPhysicalDeviceImageProcessingPropertiesQCOM - Structure containing
-- image processing properties
--
-- = Description
--
-- If the 'PhysicalDeviceImageProcessingPropertiesQCOM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These are properties of the image processing information of a physical
-- device.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing VK_QCOM_image_processing>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageProcessingPropertiesQCOM = PhysicalDeviceImageProcessingPropertiesQCOM
  { -- | #limits-weightfilter-phases# @maxWeightFilterPhases@ is the maximum
    -- value that /can/ be specified for
    -- 'ImageViewSampleWeightCreateInfoQCOM'::@numPhases@ in
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-weightimage-filterphases weight image sampling>
    -- operations.
    maxWeightFilterPhases :: Word32
  , -- | #limits-weightfilter-maxdimension# @maxWeightFilterDimension@ is a
    -- 'Vulkan.Core10.FundamentalTypes.Extent2D' describing the largest
    -- dimensions (@width@ and @height@) that /can/ be specified for
    -- 'ImageViewSampleWeightCreateInfoQCOM'::@filterSize@.
    maxWeightFilterDimension :: Extent2D
  , -- | #limits-blockmatch-maxblocksize# @maxBlockMatchRegion@ is a
    -- 'Vulkan.Core10.FundamentalTypes.Extent2D' describing the largest
    -- dimensions (@width@ and @height@) that /can/ be specified for
    -- @blockSize@ in
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-blockmatch block matching>
    -- operations.
    maxBlockMatchRegion :: Extent2D
  , -- | #limits-boxfilter-maxblocksize# @maxBoxFilterBlockSize@ is a
    -- 'Vulkan.Core10.FundamentalTypes.Extent2D' describing the maximum
    -- dimensions (@width@ and @height@) that /can/ be specified for
    -- @blocksize@ in
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-boxfilter box filter sampling>
    -- operations.
    maxBoxFilterBlockSize :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageProcessingPropertiesQCOM)
#endif
deriving instance Show PhysicalDeviceImageProcessingPropertiesQCOM

instance ToCStruct PhysicalDeviceImageProcessingPropertiesQCOM where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageProcessingPropertiesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxWeightFilterPhases)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (maxWeightFilterDimension)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (maxBlockMatchRegion)
    poke ((p `plusPtr` 36 :: Ptr Extent2D)) (maxBoxFilterBlockSize)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PhysicalDeviceImageProcessingPropertiesQCOM where
  peekCStruct p = do
    maxWeightFilterPhases <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxWeightFilterDimension <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    maxBlockMatchRegion <- peekCStruct @Extent2D ((p `plusPtr` 28 :: Ptr Extent2D))
    maxBoxFilterBlockSize <- peekCStruct @Extent2D ((p `plusPtr` 36 :: Ptr Extent2D))
    pure $ PhysicalDeviceImageProcessingPropertiesQCOM
             maxWeightFilterPhases
             maxWeightFilterDimension
             maxBlockMatchRegion
             maxBoxFilterBlockSize

instance Storable PhysicalDeviceImageProcessingPropertiesQCOM where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageProcessingPropertiesQCOM where
  zero = PhysicalDeviceImageProcessingPropertiesQCOM
           zero
           zero
           zero
           zero


type QCOM_IMAGE_PROCESSING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_IMAGE_PROCESSING_SPEC_VERSION"
pattern QCOM_IMAGE_PROCESSING_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_IMAGE_PROCESSING_SPEC_VERSION = 1


type QCOM_IMAGE_PROCESSING_EXTENSION_NAME = "VK_QCOM_image_processing"

-- No documentation found for TopLevel "VK_QCOM_IMAGE_PROCESSING_EXTENSION_NAME"
pattern QCOM_IMAGE_PROCESSING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_IMAGE_PROCESSING_EXTENSION_NAME = "VK_QCOM_image_processing"

