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
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
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
module Vulkan.Extensions.VK_QCOM_ycbcr_degamma  ( PhysicalDeviceYcbcrDegammaFeaturesQCOM
                                                , SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceYcbcrDegammaFeaturesQCOM

instance ToCStruct PhysicalDeviceYcbcrDegammaFeaturesQCOM
instance Show PhysicalDeviceYcbcrDegammaFeaturesQCOM

instance FromCStruct PhysicalDeviceYcbcrDegammaFeaturesQCOM


data SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM

instance ToCStruct SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM
instance Show SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM

instance FromCStruct SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM

