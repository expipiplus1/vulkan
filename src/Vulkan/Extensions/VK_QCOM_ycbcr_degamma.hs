{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_ycbcr_degamma - device extension
--
-- == VK_QCOM_ycbcr_degamma
--
-- [__Name String__]
--     @VK_QCOM_ycbcr_degamma@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     521
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_ycbcr_degamma] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_ycbcr_degamma extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-07-31
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--     None
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jonathan Wicks, Qualcomm
--
-- == Description
--
-- This extension allows implementations to expose support for “sRGB EOTF”
-- also known as “sRGB degamma”, used in combination with images using
-- 8-bit Y′CBCR formats. In addition, the degamma can be selectively
-- applied to the Y (luma) or CrCb (chroma).
--
-- @VK_KHR_sampler_ycbcr_conversion@ adds support for Y′CBCR conversion,
-- but allows texture sampling in a non-linear space which can cause
-- artifacts. This extension allows implementations to expose sRGB degamma
-- for Y′CBCR formats, which is performed during texture filtering,
-- allowing texture filtering to operate in a linear space.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceYcbcrDegammaFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo':
--
--     -   'SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_YCBCR_DEGAMMA_EXTENSION_NAME'
--
-- -   'QCOM_YCBCR_DEGAMMA_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_DEGAMMA_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_YCBCR_DEGAMMA_CREATE_INFO_QCOM'
--
-- == Issues
--
-- 1) Which Y′CBCR formats support the degamma feature?
--
-- __RESOLVED__: For implementations that support the extension, each
-- format that contains 8-bit R, G, and B components and supports either
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT'
-- or
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT'
-- must support degamma.
--
-- Since non-compressed Vulkan sRGB formats are already limited to 8-bit
-- components, and since Adreno supports degamma for all 8bit Y′CBCR
-- formats, this extension does not introduce a new VK_FORMAT_FEATURE* bit
-- for the degamma feature.
--
-- 2) On which Y′CBCR components is the degamma applied?
--
-- __RESOLVED__: While degamma is expected to be applied to only the Y
-- (luma) component, the extension provides the ability to selectively
-- enable degamma for both the Y (luma) and\/or CbCr (chroma) components.
--
-- 3) Should degamma be enabled for the sampler object or for the image
-- view object?
--
-- __RESOLVED__: Both. This extension extends
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
-- and the specification already requires that both sampler and view
-- objects must be created with an /identical/
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
-- in their pNext chains.
--
-- 4) Why apply the “sRGB” transfer function directly to Y′CBCR data when
-- it would be more correct to use the “ITU transfer function”, and do so
-- only after the values have been converted into non-linear R’G’B\'?
--
-- __RESOLVED__: Y′CBCR is frequently stored according to standards (e.g.
-- BT.601 and BT.709) that specify that the conversion between linear and
-- non-linear should use the ITU Transfer function. The ITU transfer
-- function is mathematically different from the sRGB transfer function and
-- while sRGB and ITU define similar curves, the difference is significant.
-- Performing the “sRGB degamma” prior to range expansion can introduce
-- artifacts if the content uses
-- 'Vulkan.Core11.Enums.SamplerYcbcrRange.SAMPLER_YCBCR_RANGE_ITU_NARROW'
-- encoding. Nevertheless, using sRGB can make sense for certain use-cases
-- where camera YCbCr images are known to be encoded with sRGB (or a pure
-- gamma 2.2) transfer function and are known to use full-range encoding.
--
-- For those use-cases, this extension leverages the GPU ability to enable
-- sRGB degamma at little cost, and can improve quality because texture
-- filtering is able to occur in linear space.
--
-- == Version History
--
-- -   Revision 1, 2023-07-31 (Jeff Leger)
--
-- == See Also
--
-- 'PhysicalDeviceYcbcrDegammaFeaturesQCOM',
-- 'SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_ycbcr_degamma Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_ycbcr_degamma  ( PhysicalDeviceYcbcrDegammaFeaturesQCOM(..)
                                                , SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM(..)
                                                , QCOM_YCBCR_DEGAMMA_SPEC_VERSION
                                                , pattern QCOM_YCBCR_DEGAMMA_SPEC_VERSION
                                                , QCOM_YCBCR_DEGAMMA_EXTENSION_NAME
                                                , pattern QCOM_YCBCR_DEGAMMA_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_DEGAMMA_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_YCBCR_DEGAMMA_CREATE_INFO_QCOM))
-- | VkPhysicalDeviceYcbcrDegammaFeaturesQCOM - Structure describing Y′CBCR
-- degamma features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceYcbcrDegammaFeaturesQCOM' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceYcbcrDegammaFeaturesQCOM' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_ycbcr_degamma VK_QCOM_ycbcr_degamma>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceYcbcrDegammaFeaturesQCOM = PhysicalDeviceYcbcrDegammaFeaturesQCOM
  { -- | #features-ycbcr-degamma# @ycbcrDegamma@ indicates whether the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-ycbcr-degamma Y′CBCR degamma>.
    ycbcrDegamma :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceYcbcrDegammaFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceYcbcrDegammaFeaturesQCOM

instance ToCStruct PhysicalDeviceYcbcrDegammaFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceYcbcrDegammaFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_DEGAMMA_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (ycbcrDegamma))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_DEGAMMA_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceYcbcrDegammaFeaturesQCOM where
  peekCStruct p = do
    ycbcrDegamma <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceYcbcrDegammaFeaturesQCOM
             (bool32ToBool ycbcrDegamma)

instance Storable PhysicalDeviceYcbcrDegammaFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceYcbcrDegammaFeaturesQCOM where
  zero = PhysicalDeviceYcbcrDegammaFeaturesQCOM
           zero


-- | VkSamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM - Structure
-- specifying Y′CBCR degamma parameters
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_ycbcr_degamma VK_QCOM_ycbcr_degamma>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM = SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM
  { -- | @enableYDegamma@ indicates
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-ycbcr-degamma sRGB to linear>
    -- conversion is enabled for the G component.
    enableYDegamma :: Bool
  , -- | @enableCbCrDegamma@ indicates
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-ycbcr-degamma sRGB to linear>
    -- conversion is enabled for the R and B components.
    enableCbCrDegamma :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM)
#endif
deriving instance Show SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM

instance ToCStruct SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_YCBCR_DEGAMMA_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (enableYDegamma))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (enableCbCrDegamma))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_YCBCR_DEGAMMA_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM where
  peekCStruct p = do
    enableYDegamma <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    enableCbCrDegamma <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM
             (bool32ToBool enableYDegamma) (bool32ToBool enableCbCrDegamma)

instance Storable SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM where
  zero = SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM
           zero
           zero


type QCOM_YCBCR_DEGAMMA_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_YCBCR_DEGAMMA_SPEC_VERSION"
pattern QCOM_YCBCR_DEGAMMA_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_YCBCR_DEGAMMA_SPEC_VERSION = 1


type QCOM_YCBCR_DEGAMMA_EXTENSION_NAME = "VK_QCOM_ycbcr_degamma"

-- No documentation found for TopLevel "VK_QCOM_YCBCR_DEGAMMA_EXTENSION_NAME"
pattern QCOM_YCBCR_DEGAMMA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_YCBCR_DEGAMMA_EXTENSION_NAME = "VK_QCOM_ycbcr_degamma"

