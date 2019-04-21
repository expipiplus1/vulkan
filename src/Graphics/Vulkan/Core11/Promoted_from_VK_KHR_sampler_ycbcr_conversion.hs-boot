{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( ChromaLocation
  , ChromaLocationKHR
  , SamplerYcbcrConversion
  , SamplerYcbcrModelConversion
  , SamplerYcbcrModelConversionKHR
  , SamplerYcbcrRange
  , SamplerYcbcrRangeKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkChromaLocation
  , VkSamplerYcbcrModelConversion
  , VkSamplerYcbcrRange
  , VkSamplerYcbcrConversion
  )


-- | VkChromaLocation - Position of downsampled chroma samples
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
type ChromaLocation = VkChromaLocation

-- No documentation found for TopLevel "ChromaLocationKHR"
type ChromaLocationKHR = ChromaLocation

-- | VkSamplerYcbcrConversion - Opaque handle to a device-specific sampler
-- Y’CBCR conversion description
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversion'
type SamplerYcbcrConversion = VkSamplerYcbcrConversion

-- | VkSamplerYcbcrModelConversion - Color model component of a color space
--
-- = Description
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY'
--     specifies that the input values to the conversion are unmodified.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY'
--     specifies no model conversion but the inputs are range expanded as
--     for Y’CBCR.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709'
--     specifies the color model conversion from Y’CBCR to R’G’B\' defined
--     in BT.709 and described in the “BT.709 Y’CBCR conversion” section of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601'
--     specifies the color model conversion from Y’CBCR to R’G’B\' defined
--     in BT.601 and described in the “BT.601 Y’CBCR conversion” section of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020'
--     specifies the color model conversion from Y’CBCR to R’G’B\' defined
--     in BT.2020 and described in the “BT.2020 Y’CBCR conversion” section
--     of the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- In the @VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_*@ color models, for the
-- input to the sampler Y’CBCR range expansion and model conversion:
--
-- -   the Y (Y\' luma) channel corresponds to the G channel of an RGB
--     image.
--
-- -   the CB (CB or “U” blue color difference) channel corresponds to the
--     B channel of an RGB image.
--
-- -   the CR (CR or “V” red color difference) channel corresponds to the R
--     channel of an RGB image.
--
-- -   the alpha channel, if present, is not modified by color model
--     conversion.
--
-- These rules reflect the mapping of channels after the channel swizzle
-- operation (controlled by
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'::@components@).
--
-- __Note__
--
-- For example, an “YUVA” 32-bit format comprising four 8-bit channels can
-- be implemented as
-- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_UNORM' with a
-- component mapping:
--
-- -   @components.a@ =
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--
-- -   @components.r@ =
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_B'
--
-- -   @components.g@ =
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_R'
--
-- -   @components.b@ =
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_G'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
type SamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion

-- No documentation found for TopLevel "SamplerYcbcrModelConversionKHR"
type SamplerYcbcrModelConversionKHR = SamplerYcbcrModelConversion

-- | VkSamplerYcbcrRange - Range of encoded values in a color space
--
-- = Description
--
-- The formulae for these conversions is described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion-rangeexpand Sampler Y’CBCR Range Expansion>
-- section of the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures Image Operations>
-- chapter.
--
-- No range modification takes place if @ycbcrModel@ is
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY';
-- the @ycbcrRange@ field of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
-- is ignored in this case.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
type SamplerYcbcrRange = VkSamplerYcbcrRange

-- No documentation found for TopLevel "SamplerYcbcrRangeKHR"
type SamplerYcbcrRangeKHR = SamplerYcbcrRange
