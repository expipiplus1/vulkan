{-# language CPP #-}
-- No documentation found for Chapter "SamplerYcbcrModelConversion"
module Vulkan.Core11.Enums.SamplerYcbcrModelConversion  (SamplerYcbcrModelConversion( SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
                                                                                    , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
                                                                                    , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
                                                                                    , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
                                                                                    , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
                                                                                    , ..
                                                                                    )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkSamplerYcbcrModelConversion - Color model component of a color space
--
-- = Description
--
-- -   'SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY' specifies that the
--     input values to the conversion are unmodified.
--
-- -   'SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY' specifies no model
--     conversion but the inputs are range expanded as for Y′CBCR.
--
-- -   'SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709' specifies the color model
--     conversion from Y′CBCR to R′G′B′ defined in BT.709 and described in
--     the “BT.709 Y′CBCR conversion” section of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- -   'SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601' specifies the color model
--     conversion from Y′CBCR to R′G′B′ defined in BT.601 and described in
--     the “BT.601 Y′CBCR conversion” section of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- -   'SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020' specifies the color
--     model conversion from Y′CBCR to R′G′B′ defined in BT.2020 and
--     described in the “BT.2020 Y′CBCR conversion” section of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- In the @VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_*@ color models, for the
-- input to the sampler Y′CBCR range expansion and model conversion:
--
-- -   the Y (Y′ luma) component corresponds to the G component of an RGB
--     image.
--
-- -   the CB (CB or “U” blue color difference) component corresponds to
--     the B component of an RGB image.
--
-- -   the CR (CR or “V” red color difference) component corresponds to the
--     R component of an RGB image.
--
-- -   the alpha component, if present, is not modified by color model
--     conversion.
--
-- These rules reflect the mapping of components after the component
-- swizzle operation (controlled by
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'::@components@).
--
-- Note
--
-- For example, an “YUVA” 32-bit format comprising four 8-bit components
-- can be implemented as 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM'
-- with a component mapping:
--
-- -   @components.a@ =
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY'
--
-- -   @components.r@ =
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_B'
--
-- -   @components.g@ =
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_R'
--
-- -   @components.b@ =
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_G'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatProperties2ANDROID',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID',
-- 'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionPropertiesFUCHSIA',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
newtype SamplerYcbcrModelConversion = SamplerYcbcrModelConversion Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY = SamplerYcbcrModelConversion 0

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY = SamplerYcbcrModelConversion 1

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 = SamplerYcbcrModelConversion 2

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 = SamplerYcbcrModelConversion 3

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 = SamplerYcbcrModelConversion 4

{-# COMPLETE
  SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
  , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
  , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
  , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
  , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 ::
    SamplerYcbcrModelConversion
  #-}

conNameSamplerYcbcrModelConversion :: String
conNameSamplerYcbcrModelConversion = "SamplerYcbcrModelConversion"

enumPrefixSamplerYcbcrModelConversion :: String
enumPrefixSamplerYcbcrModelConversion = "SAMPLER_YCBCR_MODEL_CONVERSION_"

showTableSamplerYcbcrModelConversion :: [(SamplerYcbcrModelConversion, String)]
showTableSamplerYcbcrModelConversion =
  [
    ( SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
    , "RGB_IDENTITY"
    )
  ,
    ( SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
    , "YCBCR_IDENTITY"
    )
  ,
    ( SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
    , "YCBCR_709"
    )
  ,
    ( SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
    , "YCBCR_601"
    )
  ,
    ( SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
    , "YCBCR_2020"
    )
  ]

instance Show SamplerYcbcrModelConversion where
  showsPrec =
    enumShowsPrec
      enumPrefixSamplerYcbcrModelConversion
      showTableSamplerYcbcrModelConversion
      conNameSamplerYcbcrModelConversion
      (\(SamplerYcbcrModelConversion x) -> x)
      (showsPrec 11)

instance Read SamplerYcbcrModelConversion where
  readPrec =
    enumReadPrec
      enumPrefixSamplerYcbcrModelConversion
      showTableSamplerYcbcrModelConversion
      conNameSamplerYcbcrModelConversion
      SamplerYcbcrModelConversion
