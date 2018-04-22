{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkSamplerYcbcrModelConversion(..)
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
  , VkSamplerYcbcrRange(..)
  , pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL
  , pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW
  , VkChromaLocation(..)
  , pattern VK_CHROMA_LOCATION_COSITED_EVEN
  , pattern VK_CHROMA_LOCATION_MIDPOINT
  , pattern VK_FORMAT_G8B8G8R8_422_UNORM
  , pattern VK_FORMAT_B8G8R8G8_422_UNORM
  , pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM
  , pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM
  , pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM
  , pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM
  , pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM
  , pattern VK_FORMAT_R10X6_UNORM_PACK16
  , pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16
  , pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16
  , pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16
  , pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16
  , pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16
  , pattern VK_FORMAT_R12X4_UNORM_PACK16
  , pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16
  , pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16
  , pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16
  , pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16
  , pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16
  , pattern VK_FORMAT_G16B16G16R16_422_UNORM
  , pattern VK_FORMAT_B16G16R16G16_422_UNORM
  , pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM
  , pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM
  , pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM
  , pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM
  , pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
  , pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION
  , pattern VK_IMAGE_CREATE_DISJOINT_BIT
  , pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern VK_FORMAT_FEATURE_DISJOINT_BIT
  , pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_0_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_1_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_2_BIT
  , VkSamplerYcbcrConversion
  , vkCreateSamplerYcbcrConversion
  , vkDestroySamplerYcbcrConversion
  , VkSamplerYcbcrConversionInfo(..)
  , VkSamplerYcbcrConversionCreateInfo(..)
  , VkBindImagePlaneMemoryInfo(..)
  , VkImagePlaneMemoryRequirementsInfo(..)
  , VkPhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , VkSamplerYcbcrConversionImageFormatProperties(..)
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.ImageView
  ( VkComponentMapping(..)
  )
import Graphics.Vulkan.Core10.Sampler
  ( VkFilter(..)
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlagBits(..)
  )


-- ** VkSamplerYcbcrModelConversion

-- | VkSamplerYcbcrModelConversion - Color model component of a color space
--
-- = Description
--
-- -   @VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY@ specifies that the
--     input values to the conversion are unmodified.
--
-- -   @VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY@ specifies no
--     model conversion but the inputs are range expanded as for Y’CBCR.
--
-- -   @VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709@ specifies the color
--     model conversion from Y’CBCR to R’G’B\' defined in BT.709 and
--     described in the “BT.709 Y’CBCR conversion” section of the [Khronos
--     Data Format
--     Specification](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#data-format).
--
-- -   @VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601@ specifies the color
--     model conversion from Y’CBCR to R’G’B\' defined in BT.601 and
--     described in the “BT.601 Y’CBCR conversion” section of the [Khronos
--     Data Format
--     Specification](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#data-format).
--
-- -   @VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020@ specifies the color
--     model conversion from Y’CBCR to R’G’B\' defined in BT.2020 and
--     described in the “BT.2020 Y’CBCR conversion” section of the [Khronos
--     Data Format
--     Specification](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#data-format).
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
-- 'VkSamplerYcbcrConversionCreateInfo'::@components@).
--
-- __Note__
--
-- For example, an “YUVA” 32-bit format comprising four 8-bit channels can
-- be implemented as @VK_FORMAT_R8G8B8A8_UNORM@ with a component mapping:
--
-- -   @components.a@ = @VK_COMPONENT_SWIZZLE_IDENTITY@
--
-- -   @components.r@ = @VK_COMPONENT_SWIZZLE_B@
--
-- -   @components.g@ = @VK_COMPONENT_SWIZZLE_R@
--
-- -   @components.b@ = @VK_COMPONENT_SWIZZLE_G@
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'VkSamplerYcbcrConversionCreateInfo'
newtype VkSamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerYcbcrModelConversion where
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020"
  showsPrec p (VkSamplerYcbcrModelConversion x) = showParen (p >= 11) (showString "VkSamplerYcbcrModelConversion " . showsPrec 11 x)

instance Read VkSamplerYcbcrModelConversion where
  readPrec = parens ( choose [ ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY",   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY", pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709",      pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601",      pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020",     pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerYcbcrModelConversion")
                        v <- step readPrec
                        pure (VkSamplerYcbcrModelConversion v)
                        )
                    )

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY = VkSamplerYcbcrModelConversion 0

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY = VkSamplerYcbcrModelConversion 1

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 = VkSamplerYcbcrModelConversion 2

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 = VkSamplerYcbcrModelConversion 3

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 = VkSamplerYcbcrModelConversion 4
-- ** VkSamplerYcbcrRange

-- | VkSamplerYcbcrRange - Range of encoded values in a color space
--
-- = Description
--
-- -   @VK_SAMPLER_YCBCR_RANGE_ITU_FULL@ specifies that the full range of
--     the encoded values are valid and interpreted according to the ITU
--     “full range” quantization rules.
--
-- -   @VK_SAMPLER_YCBCR_RANGE_ITU_NARROW@ specifies that headroom and foot
--     room are reserved in the numerical range of encoded values, and the
--     remaining values are expanded according to the ITU “narrow range”
--     quantization rules.
--
-- The formulae for these conversions is described in the [Sampler Y’CBCR
-- Range
-- Expansion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion-rangeexpand)
-- section of the [Image
-- Operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures)
-- chapter.
--
-- No range modification takes place if @ycbcrModel@ is
-- @VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY@; the @ycbcrRange@ field
-- of @VkSamplerYcbcrConversionCreateInfo@ is ignored in this case.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'VkSamplerYcbcrConversionCreateInfo'
newtype VkSamplerYcbcrRange = VkSamplerYcbcrRange Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerYcbcrRange where
  showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_FULL = showString "VK_SAMPLER_YCBCR_RANGE_ITU_FULL"
  showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_NARROW = showString "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW"
  showsPrec p (VkSamplerYcbcrRange x) = showParen (p >= 11) (showString "VkSamplerYcbcrRange " . showsPrec 11 x)

instance Read VkSamplerYcbcrRange where
  readPrec = parens ( choose [ ("VK_SAMPLER_YCBCR_RANGE_ITU_FULL",   pure VK_SAMPLER_YCBCR_RANGE_ITU_FULL)
                             , ("VK_SAMPLER_YCBCR_RANGE_ITU_NARROW", pure VK_SAMPLER_YCBCR_RANGE_ITU_NARROW)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerYcbcrRange")
                        v <- step readPrec
                        pure (VkSamplerYcbcrRange v)
                        )
                    )

-- No documentation found for Nested "VkSamplerYcbcrRange" "VK_SAMPLER_YCBCR_RANGE_ITU_FULL"
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL :: VkSamplerYcbcrRange
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL = VkSamplerYcbcrRange 0

-- No documentation found for Nested "VkSamplerYcbcrRange" "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW"
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW :: VkSamplerYcbcrRange
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW = VkSamplerYcbcrRange 1
-- ** VkChromaLocation

-- | VkChromaLocation - Position of downsampled chroma samples
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'VkSamplerYcbcrConversionCreateInfo'
newtype VkChromaLocation = VkChromaLocation Int32
  deriving (Eq, Ord, Storable)

instance Show VkChromaLocation where
  showsPrec _ VK_CHROMA_LOCATION_COSITED_EVEN = showString "VK_CHROMA_LOCATION_COSITED_EVEN"
  showsPrec _ VK_CHROMA_LOCATION_MIDPOINT = showString "VK_CHROMA_LOCATION_MIDPOINT"
  showsPrec p (VkChromaLocation x) = showParen (p >= 11) (showString "VkChromaLocation " . showsPrec 11 x)

instance Read VkChromaLocation where
  readPrec = parens ( choose [ ("VK_CHROMA_LOCATION_COSITED_EVEN", pure VK_CHROMA_LOCATION_COSITED_EVEN)
                             , ("VK_CHROMA_LOCATION_MIDPOINT",     pure VK_CHROMA_LOCATION_MIDPOINT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkChromaLocation")
                        v <- step readPrec
                        pure (VkChromaLocation v)
                        )
                    )

-- | @VK_CHROMA_LOCATION_COSITED_EVEN@ specifies that downsampled chroma
-- samples are aligned with luma samples with even coordinates.
pattern VK_CHROMA_LOCATION_COSITED_EVEN :: VkChromaLocation
pattern VK_CHROMA_LOCATION_COSITED_EVEN = VkChromaLocation 0

-- | @VK_CHROMA_LOCATION_MIDPOINT@ specifies that downsampled chroma samples
-- are located half way between each even luma sample and the nearest
-- higher odd luma sample.
pattern VK_CHROMA_LOCATION_MIDPOINT :: VkChromaLocation
pattern VK_CHROMA_LOCATION_MIDPOINT = VkChromaLocation 1
-- | @VK_FORMAT_G8B8G8R8_422_UNORM@ specifies a four-component, 32-bit format
-- containing a pair of G components, an R component, and a B component,
-- collectively encoding a 2×1 rectangle of unsigned normalized RGB texel
-- data. One G value is present at each /i/ coordinate, with the B and R
-- values shared across both G values and thus recorded at half the
-- horizontal resolution of the image. This format has an 8-bit G component
-- for the even /i/ coordinate in byte 0, an 8-bit B component in byte 1,
-- an 8-bit G component for the odd /i/ coordinate in byte 2, and an 8-bit
-- R component in byte 3. Images in this format /must/ be defined with a
-- width that is a multiple of two. For the purposes of the constraints on
-- copy extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern VK_FORMAT_G8B8G8R8_422_UNORM :: VkFormat
pattern VK_FORMAT_G8B8G8R8_422_UNORM = VkFormat 1000156000
-- | @VK_FORMAT_B8G8R8G8_422_UNORM@ specifies a four-component, 32-bit format
-- containing a pair of G components, an R component, and a B component,
-- collectively encoding a 2×1 rectangle of unsigned normalized RGB texel
-- data. One G value is present at each /i/ coordinate, with the B and R
-- values shared across both G values and thus recorded at half the
-- horizontal resolution of the image. This format has an 8-bit B component
-- in byte 0, an 8-bit G component for the even /i/ coordinate in byte 1,
-- an 8-bit R component in byte 2, and an 8-bit G component for the odd /i/
-- coordinate in byte 3. Images in this format /must/ be defined with a
-- width that is a multiple of two. For the purposes of the constraints on
-- copy extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern VK_FORMAT_B8G8R8G8_422_UNORM :: VkFormat
pattern VK_FORMAT_B8G8R8G8_422_UNORM = VkFormat 1000156001
-- | @VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, an 8-bit
-- B component in plane 1, and an 8-bit R component in plane 2. The
-- horizontal and vertical dimensions of the R and B planes are halved
-- relative to the image dimensions, and each R and B component is shared
-- with the G components for which \(\lfloor i_G \times 0.5
-- \rfloor = i_B = i_R\) and \(\lfloor j_G \times 0.5 \rfloor = j_B
-- = j_R\). The location of each plane when this image is in linear layout
-- can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane. Images in this format
-- /must/ be defined with a width and height that is a multiple of two.
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM = VkFormat 1000156002
-- | @VK_FORMAT_G8_B8R8_2PLANE_420_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, and a
-- two-component, 16-bit BR plane 1 consisting of an 8-bit B component in
-- byte 0 and an 8-bit R component in byte 1. The horizontal and vertical
-- dimensions of the BR plane is halved relative to the image dimensions,
-- and each R and B value is shared with the G components for which
-- \(\lfloor i_G \times 0.5 \rfloor =
-- i_B = i_R\) and \(\lfloor j_G \times 0.5 \rfloor = j_B = j_R\). The
-- location of each plane when this image is in linear layout can be
-- determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane, and
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the BR plane. Images in this format
-- /must/ be defined with a width and height that is a multiple of two.
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM = VkFormat 1000156003
-- | @VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, an 8-bit
-- B component in plane 1, and an 8-bit R component in plane 2. The
-- horizontal dimension of the R and B plane is halved relative to the
-- image dimensions, and each R and B value is shared with the G components
-- for which \(\lfloor i_G \times 0.5 \rfloor = i_B = i_R\). The location
-- of each plane when this image is in linear layout can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane. Images in this format
-- /must/ be defined with a width that is a multiple of two.
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM = VkFormat 1000156004
-- | @VK_FORMAT_G8_B8R8_2PLANE_422_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, and a
-- two-component, 16-bit BR plane 1 consisting of an 8-bit B component in
-- byte 0 and an 8-bit R component in byte 1. The horizontal dimensions of
-- the BR plane is halved relative to the image dimensions, and each R and
-- B value is shared with the G components for which
-- \(\lfloor i_G \times 0.5 \rfloor = i_B = i_R\). The location of each
-- plane when this image is in linear layout can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane, and
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the BR plane. Images in this format
-- /must/ be defined with a width that is a multiple of two.
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM = VkFormat 1000156005
-- | @VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, an 8-bit
-- B component in plane 1, and an 8-bit R component in plane 2. Each plane
-- has the same dimensions and each R, G and B component contributes to a
-- single texel. The location of each plane when this image is in linear
-- layout can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane.
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM = VkFormat 1000156006
-- | @VK_FORMAT_R10X6_UNORM_PACK16@ specifies a one-component, 16-bit
-- unsigned normalized format that has a single 10-bit R component in the
-- top 10 bits of a 16-bit word, with the bottom 6 bits set to 0.
pattern VK_FORMAT_R10X6_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R10X6_UNORM_PACK16 = VkFormat 1000156007
-- | @VK_FORMAT_R10X6G10X6_UNORM_2PACK16@ specifies a two-component, 32-bit
-- unsigned normalized format that has a 10-bit R component in the top 10
-- bits of the word in bytes 0..1, and a 10-bit G component in the top 10
-- bits of the word in bytes 2..3, with the bottom 6 bits of each word set
-- to 0.
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 = VkFormat 1000156008
-- | @VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16@ specifies a
-- four-component, 64-bit unsigned normalized format that has a 10-bit R
-- component in the top 10 bits of the word in bytes 0..1, a 10-bit G
-- component in the top 10 bits of the word in bytes 2..3, a 10-bit B
-- component in the top 10 bits of the word in bytes 4..5, and a 10-bit A
-- component in the top 10 bits of the word in bytes 6..7, with the bottom
-- 6 bits of each word set to 0.
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 = VkFormat 1000156009
-- | @VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16@ specifies a
-- four-component, 64-bit format containing a pair of G components, an R
-- component, and a B component, collectively encoding a 2×1 rectangle of
-- unsigned normalized RGB texel data. One G value is present at each /i/
-- coordinate, with the B and R values shared across both G values and thus
-- recorded at half the horizontal resolution of the image. This format has
-- a 10-bit G component for the even /i/ coordinate in the top 10 bits of
-- the word in bytes 0..1, a 10-bit B component in the top 10 bits of the
-- word in bytes 2..3, a 10-bit G component for the odd /i/ coordinate in
-- the top 10 bits of the word in bytes 4..5, and a 10-bit R component in
-- the top 10 bits of the word in bytes 6..7, with the bottom 6 bits of
-- each word set to 0. Images in this format /must/ be defined with a width
-- that is a multiple of two. For the purposes of the constraints on copy
-- extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 = VkFormat 1000156010
-- | @VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16@ specifies a
-- four-component, 64-bit format containing a pair of G components, an R
-- component, and a B component, collectively encoding a 2×1 rectangle of
-- unsigned normalized RGB texel data. One G value is present at each /i/
-- coordinate, with the B and R values shared across both G values and thus
-- recorded at half the horizontal resolution of the image. This format has
-- a 10-bit B component in the top 10 bits of the word in bytes 0..1, a
-- 10-bit G component for the even /i/ coordinate in the top 10 bits of the
-- word in bytes 2..3, a 10-bit R component in the top 10 bits of the word
-- in bytes 4..5, and a 10-bit G component for the odd /i/ coordinate in
-- the top 10 bits of the word in bytes 6..7, with the bottom 6 bits of
-- each word set to 0. Images in this format /must/ be defined with a width
-- that is a multiple of two. For the purposes of the constraints on copy
-- extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 = VkFormat 1000156011
-- | @VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 10-bit G component
-- in the top 10 bits of each 16-bit word of plane 0, a 10-bit B component
-- in the top 10 bits of each 16-bit word of plane 1, and a 10-bit R
-- component in the top 10 bits of each 16-bit word of plane 2, with the
-- bottom 6 bits of each word set to 0. The horizontal and vertical
-- dimensions of the R and B planes are halved relative to the image
-- dimensions, and each R and B component is shared with the G components
-- for which \(\lfloor i_G \times 0.5
-- \rfloor = i_B = i_R\) and \(\lfloor j_G \times 0.5 \rfloor = j_B
-- = j_R\). The location of each plane when this image is in linear layout
-- can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane. Images in this format
-- /must/ be defined with a width and height that is a multiple of two.
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156012
-- | @VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 10-bit G component
-- in the top 10 bits of each 16-bit word of plane 0, and a two-component,
-- 32-bit BR plane 1 consisting of a 10-bit B component in the top 10 bits
-- of the word in bytes 0..1, and a 10-bit R component in the top 10 bits
-- of the word in bytes 2..3, the bottom 6 bits of each word set to 0. The
-- horizontal and vertical dimensions of the BR plane is halved relative to
-- the image dimensions, and each R and B value is shared with the G
-- components for which \(\lfloor i_G \times 0.5 \rfloor =
-- i_B = i_R\) and \(\lfloor j_G \times 0.5 \rfloor = j_B = j_R\). The
-- location of each plane when this image is in linear layout can be
-- determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane, and
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the BR plane. Images in this format
-- /must/ be defined with a width and height that is a multiple of two.
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156013
-- | @VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 10-bit G component
-- in the top 10 bits of each 16-bit word of plane 0, a 10-bit B component
-- in the top 10 bits of each 16-bit word of plane 1, and a 10-bit R
-- component in the top 10 bits of each 16-bit word of plane 2, with the
-- bottom 6 bits of each word set to 0. The horizontal dimension of the R
-- and B plane is halved relative to the image dimensions, and each R and B
-- value is shared with the G components for which
-- \(\lfloor i_G \times 0.5 \rfloor = i_B = i_R\). The location of each
-- plane when this image is in linear layout can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane. Images in this format
-- /must/ be defined with a width that is a multiple of two.
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156014
-- | @VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 10-bit G component
-- in the top 10 bits of each 16-bit word of plane 0, and a two-component,
-- 32-bit BR plane 1 consisting of a 10-bit B component in the top 10 bits
-- of the word in bytes 0..1, and a 10-bit R component in the top 10 bits
-- of the word in bytes 2..3, the bottom 6 bits of each word set to 0. The
-- horizontal dimensions of the BR plane is halved relative to the image
-- dimensions, and each R and B value is shared with the G components for
-- which \(\lfloor i_G \times 0.5 \rfloor = i_B = i_R\). The location of
-- each plane when this image is in linear layout can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane, and
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the BR plane. Images in this format
-- /must/ be defined with a width that is a multiple of two.
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156015
-- | @VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 10-bit G component
-- in the top 10 bits of each 16-bit word of plane 0, a 10-bit B component
-- in the top 10 bits of each 16-bit word of plane 1, and a 10-bit R
-- component in the top 10 bits of each 16-bit word of plane 2, with the
-- bottom 6 bits of each word set to 0. Each plane has the same dimensions
-- and each R, G and B component contributes to a single texel. The
-- location of each plane when this image is in linear layout can be
-- determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane.
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156016
-- | @VK_FORMAT_R12X4_UNORM_PACK16@ specifies a one-component, 16-bit
-- unsigned normalized format that has a single 12-bit R component in the
-- top 12 bits of a 16-bit word, with the bottom 4 bits set to 0.
pattern VK_FORMAT_R12X4_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R12X4_UNORM_PACK16 = VkFormat 1000156017
-- | @VK_FORMAT_R12X4G12X4_UNORM_2PACK16@ specifies a two-component, 32-bit
-- unsigned normalized format that has a 12-bit R component in the top 12
-- bits of the word in bytes 0..1, and a 12-bit G component in the top 12
-- bits of the word in bytes 2..3, with the bottom 4 bits of each word set
-- to 0.
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 = VkFormat 1000156018
-- | @VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16@ specifies a
-- four-component, 64-bit unsigned normalized format that has a 12-bit R
-- component in the top 12 bits of the word in bytes 0..1, a 12-bit G
-- component in the top 12 bits of the word in bytes 2..3, a 12-bit B
-- component in the top 12 bits of the word in bytes 4..5, and a 12-bit A
-- component in the top 12 bits of the word in bytes 6..7, with the bottom
-- 4 bits of each word set to 0.
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 = VkFormat 1000156019
-- | @VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16@ specifies a
-- four-component, 64-bit format containing a pair of G components, an R
-- component, and a B component, collectively encoding a 2×1 rectangle of
-- unsigned normalized RGB texel data. One G value is present at each /i/
-- coordinate, with the B and R values shared across both G values and thus
-- recorded at half the horizontal resolution of the image. This format has
-- a 12-bit G component for the even /i/ coordinate in the top 12 bits of
-- the word in bytes 0..1, a 12-bit B component in the top 12 bits of the
-- word in bytes 2..3, a 12-bit G component for the odd /i/ coordinate in
-- the top 12 bits of the word in bytes 4..5, and a 12-bit R component in
-- the top 12 bits of the word in bytes 6..7, with the bottom 4 bits of
-- each word set to 0. Images in this format /must/ be defined with a width
-- that is a multiple of two. For the purposes of the constraints on copy
-- extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 = VkFormat 1000156020
-- | @VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16@ specifies a
-- four-component, 64-bit format containing a pair of G components, an R
-- component, and a B component, collectively encoding a 2×1 rectangle of
-- unsigned normalized RGB texel data. One G value is present at each /i/
-- coordinate, with the B and R values shared across both G values and thus
-- recorded at half the horizontal resolution of the image. This format has
-- a 12-bit B component in the top 12 bits of the word in bytes 0..1, a
-- 12-bit G component for the even /i/ coordinate in the top 12 bits of the
-- word in bytes 2..3, a 12-bit R component in the top 12 bits of the word
-- in bytes 4..5, and a 12-bit G component for the odd /i/ coordinate in
-- the top 12 bits of the word in bytes 6..7, with the bottom 4 bits of
-- each word set to 0. Images in this format /must/ be defined with a width
-- that is a multiple of two. For the purposes of the constraints on copy
-- extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 = VkFormat 1000156021
-- | @VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 12-bit G component
-- in the top 12 bits of each 16-bit word of plane 0, a 12-bit B component
-- in the top 12 bits of each 16-bit word of plane 1, and a 12-bit R
-- component in the top 12 bits of each 16-bit word of plane 2, with the
-- bottom 4 bits of each word set to 0. The horizontal and vertical
-- dimensions of the R and B planes are halved relative to the image
-- dimensions, and each R and B component is shared with the G components
-- for which \(\lfloor i_G \times 0.5
-- \rfloor = i_B = i_R\) and \(\lfloor j_G \times 0.5 \rfloor = j_B
-- = j_R\). The location of each plane when this image is in linear layout
-- can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane. Images in this format
-- /must/ be defined with a width and height that is a multiple of two.
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156022
-- | @VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 12-bit G component
-- in the top 12 bits of each 16-bit word of plane 0, and a two-component,
-- 32-bit BR plane 1 consisting of a 12-bit B component in the top 12 bits
-- of the word in bytes 0..1, and a 12-bit R component in the top 12 bits
-- of the word in bytes 2..3, the bottom 4 bits of each word set to 0. The
-- horizontal and vertical dimensions of the BR plane is halved relative to
-- the image dimensions, and each R and B value is shared with the G
-- components for which \(\lfloor i_G \times 0.5 \rfloor =
-- i_B = i_R\) and \(\lfloor j_G \times 0.5 \rfloor = j_B = j_R\). The
-- location of each plane when this image is in linear layout can be
-- determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane, and
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the BR plane. Images in this format
-- /must/ be defined with a width and height that is a multiple of two.
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156023
-- | @VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 12-bit G component
-- in the top 12 bits of each 16-bit word of plane 0, a 12-bit B component
-- in the top 12 bits of each 16-bit word of plane 1, and a 12-bit R
-- component in the top 12 bits of each 16-bit word of plane 2, with the
-- bottom 4 bits of each word set to 0. The horizontal dimension of the R
-- and B plane is halved relative to the image dimensions, and each R and B
-- value is shared with the G components for which
-- \(\lfloor i_G \times 0.5 \rfloor = i_B = i_R\). The location of each
-- plane when this image is in linear layout can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane. Images in this format
-- /must/ be defined with a width that is a multiple of two.
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156024
-- | @VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 12-bit G component
-- in the top 12 bits of each 16-bit word of plane 0, and a two-component,
-- 32-bit BR plane 1 consisting of a 12-bit B component in the top 12 bits
-- of the word in bytes 0..1, and a 12-bit R component in the top 12 bits
-- of the word in bytes 2..3, the bottom 4 bits of each word set to 0. The
-- horizontal dimensions of the BR plane is halved relative to the image
-- dimensions, and each R and B value is shared with the G components for
-- which \(\lfloor i_G \times 0.5 \rfloor = i_B = i_R\). The location of
-- each plane when this image is in linear layout can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane, and
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the BR plane. Images in this format
-- /must/ be defined with a width that is a multiple of two.
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156025
-- | @VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16@ specifies a
-- unsigned normalized /multi-planar format/ that has a 12-bit G component
-- in the top 12 bits of each 16-bit word of plane 0, a 12-bit B component
-- in the top 12 bits of each 16-bit word of plane 1, and a 12-bit R
-- component in the top 12 bits of each 16-bit word of plane 2, with the
-- bottom 4 bits of each word set to 0. Each plane has the same dimensions
-- and each R, G and B component contributes to a single texel. The
-- location of each plane when this image is in linear layout can be
-- determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane.
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156026
-- | @VK_FORMAT_G16B16G16R16_422_UNORM@ specifies a four-component, 64-bit
-- format containing a pair of G components, an R component, and a B
-- component, collectively encoding a 2×1 rectangle of unsigned normalized
-- RGB texel data. One G value is present at each /i/ coordinate, with the
-- B and R values shared across both G values and thus recorded at half the
-- horizontal resolution of the image. This format has a 16-bit G component
-- for the even /i/ coordinate in the word in bytes 0..1, a 16-bit B
-- component in the word in bytes 2..3, a 16-bit G component for the odd
-- /i/ coordinate in the word in bytes 4..5, and a 16-bit R component in
-- the word in bytes 6..7. Images in this format /must/ be defined with a
-- width that is a multiple of two. For the purposes of the constraints on
-- copy extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern VK_FORMAT_G16B16G16R16_422_UNORM :: VkFormat
pattern VK_FORMAT_G16B16G16R16_422_UNORM = VkFormat 1000156027
-- | @VK_FORMAT_B16G16R16G16_422_UNORM@ specifies a four-component, 64-bit
-- format containing a pair of G components, an R component, and a B
-- component, collectively encoding a 2×1 rectangle of unsigned normalized
-- RGB texel data. One G value is present at each /i/ coordinate, with the
-- B and R values shared across both G values and thus recorded at half the
-- horizontal resolution of the image. This format has a 16-bit B component
-- in the word in bytes 0..1, a 16-bit G component for the even /i/
-- coordinate in the word in bytes 2..3, a 16-bit R component in the word
-- in bytes 4..5, and a 16-bit G component for the odd /i/ coordinate in
-- the word in bytes 6..7. Images in this format /must/ be defined with a
-- width that is a multiple of two. For the purposes of the constraints on
-- copy extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern VK_FORMAT_B16G16R16G16_422_UNORM :: VkFormat
pattern VK_FORMAT_B16G16R16G16_422_UNORM = VkFormat 1000156028
-- | @VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, a 16-bit B component in each 16-bit word of plane 1, and a
-- 16-bit R component in each 16-bit word of plane 2. The horizontal and
-- vertical dimensions of the R and B planes are halved relative to the
-- image dimensions, and each R and B component is shared with the G
-- components for which \(\lfloor i_G \times 0.5
-- \rfloor = i_B = i_R\) and \(\lfloor j_G \times 0.5 \rfloor = j_B
-- = j_R\). The location of each plane when this image is in linear layout
-- can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane. Images in this format
-- /must/ be defined with a width and height that is a multiple of two.
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM = VkFormat 1000156029
-- | @VK_FORMAT_G16_B16R16_2PLANE_420_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, and a two-component, 32-bit BR plane 1 consisting of a
-- 16-bit B component in the word in bytes 0..1, and a 16-bit R component
-- in the word in bytes 2..3. The horizontal and vertical dimensions of the
-- BR plane is halved relative to the image dimensions, and each R and B
-- value is shared with the G components for which
-- \(\lfloor i_G \times 0.5 \rfloor =
-- i_B = i_R\) and \(\lfloor j_G \times 0.5 \rfloor = j_B = j_R\). The
-- location of each plane when this image is in linear layout can be
-- determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane, and
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the BR plane. Images in this format
-- /must/ be defined with a width and height that is a multiple of two.
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM = VkFormat 1000156030
-- | @VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, a 16-bit B component in each 16-bit word of plane 1, and a
-- 16-bit R component in each 16-bit word of plane 2. The horizontal
-- dimension of the R and B plane is halved relative to the image
-- dimensions, and each R and B value is shared with the G components for
-- which \(\lfloor i_G \times 0.5 \rfloor = i_B = i_R\). The location of
-- each plane when this image is in linear layout can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane. Images in this format
-- /must/ be defined with a width that is a multiple of two.
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM = VkFormat 1000156031
-- | @VK_FORMAT_G16_B16R16_2PLANE_422_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, and a two-component, 32-bit BR plane 1 consisting of a
-- 16-bit B component in the word in bytes 0..1, and a 16-bit R component
-- in the word in bytes 2..3. The horizontal dimensions of the BR plane is
-- halved relative to the image dimensions, and each R and B value is
-- shared with the G components for which
-- \(\lfloor i_G \times 0.5 \rfloor = i_B = i_R\). The location of each
-- plane when this image is in linear layout can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane, and
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the BR plane. Images in this format
-- /must/ be defined with a width that is a multiple of two.
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM = VkFormat 1000156032
-- | @VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM@ specifies a unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, a 16-bit B component in each 16-bit word of plane 1, and a
-- 16-bit R component in each 16-bit word of plane 2. Each plane has the
-- same dimensions and each R, G and B component contributes to a single
-- texel. The location of each plane when this image is in linear layout
-- can be determined via
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout', using
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ for the G plane,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@ for the B plane, and
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@ for the R plane.
pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM = VkFormat 1000156033
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO = VkStructureType 1000156000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO"
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO = VkStructureType 1000156001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO = VkStructureType 1000156002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO"
pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO = VkStructureType 1000156003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES = VkStructureType 1000156004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES"
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES = VkStructureType 1000156005
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION"
pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION :: VkObjectType
pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION = VkObjectType 1000156000
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_DISJOINT_BIT"
pattern VK_IMAGE_CREATE_DISJOINT_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_DISJOINT_BIT = VkImageCreateFlagBits 0x00000200
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT"
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT = VkFormatFeatureFlagBits 0x00020000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT = VkFormatFeatureFlagBits 0x00040000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT = VkFormatFeatureFlagBits 0x00080000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT = VkFormatFeatureFlagBits 0x00100000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT = VkFormatFeatureFlagBits 0x00200000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_DISJOINT_BIT"
pattern VK_FORMAT_FEATURE_DISJOINT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_DISJOINT_BIT = VkFormatFeatureFlagBits 0x00400000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT"
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT = VkFormatFeatureFlagBits 0x00800000
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_0_BIT"
pattern VK_IMAGE_ASPECT_PLANE_0_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_0_BIT = VkImageAspectFlagBits 0x00000010
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_1_BIT"
pattern VK_IMAGE_ASPECT_PLANE_1_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_1_BIT = VkImageAspectFlagBits 0x00000020
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_2_BIT"
pattern VK_IMAGE_ASPECT_PLANE_2_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_2_BIT = VkImageAspectFlagBits 0x00000040
-- | Dummy data to tag the 'Ptr' with
data VkSamplerYcbcrConversion_T
-- | VkSamplerYcbcrConversion - NO SHORT DESCRIPTION PROVIDED
--
-- = See Also
--
-- 'VkSamplerYcbcrConversionInfo', 'vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR',
-- 'vkDestroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversionKHR'
type VkSamplerYcbcrConversion = Ptr VkSamplerYcbcrConversion_T
-- | vkCreateSamplerYcbcrConversion - Create a new Ycbcr conversion
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the sampler Y’CBCR
--     conversion.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkSamplerYcbcrConversionCreateInfo' specifying the requested
--     sampler Y’CBCR conversion.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pYcbcrConversion@ points to a 'VkSamplerYcbcrConversion' handle in
--     which the resulting sampler Y’CBCR conversion is returned.
--
-- = Description
--
-- The interpretation of the configured sampler Y’CBCR conversion is
-- described in more detail in [the description of sampler Y’CBCR
-- conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion)
-- in the [Image
-- Operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures)
-- chapter.
--
-- == Valid Usage
--
-- -   The [sampler Y’CBCR conversion
--     feature](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sampler-YCbCr-conversion)
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkSamplerYcbcrConversionCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pYcbcrConversion@ /must/ be a valid pointer to a
--     @VkSamplerYcbcrConversion@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkSamplerYcbcrConversion', 'VkSamplerYcbcrConversionCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSamplerYcbcrConversion" vkCreateSamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult
-- | vkDestroySamplerYcbcrConversion - Destroy a created Y’CbCr conversion
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the Y’CBCR conversion.
--
-- -   @ycbcrConversion@ is the conversion to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @ycbcrConversion@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @ycbcrConversion@
--     /must/ be a valid @VkSamplerYcbcrConversion@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @ycbcrConversion@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @ycbcrConversion@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkSamplerYcbcrConversion'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroySamplerYcbcrConversion" vkDestroySamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkSamplerYcbcrConversionInfo - Structure specifying Y’CbCr conversion to
-- a sampler or image view
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO@
--
-- -   @conversion@ /must/ be a valid @VkSamplerYcbcrConversion@ handle
--
-- = See Also
--
-- 'VkSamplerYcbcrConversion',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkSamplerYcbcrConversionInfo = VkSamplerYcbcrConversionInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @conversion@ is a 'VkSamplerYcbcrConversion' handle created with
  -- 'vkCreateSamplerYcbcrConversion'.
  vkConversion :: VkSamplerYcbcrConversion
  }
  deriving (Eq, Show)

instance Storable VkSamplerYcbcrConversionInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerYcbcrConversionInfo))
                *> poke (ptr `plusPtr` 16) (vkConversion (poked :: VkSamplerYcbcrConversionInfo))
-- | VkSamplerYcbcrConversionCreateInfo - Structure specifying the parameters
-- of the newly created conversion
--
-- = Description
--
-- __Note__
--
-- Setting @forceExplicitReconstruction@ to @VK_TRUE@ /may/ have a
-- performance penalty on implementations where explicit reconstruction is
-- not the default mode of operation.
--
-- If the @pNext@ chain has an instance of
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
-- with non-zero @externalFormat@ member, the sampler Y’CBCR conversion
-- object represents an /external format conversion/, and @format@ /must/
-- be @VK_FORMAT_UNDEFINED@. Such conversions /must/ only be used to sample
-- image views with a matching [external
-- format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats).
-- When creating an external format conversion, the value of @components@
-- is ignored.
--
-- == Valid Usage
--
-- -   If an external format conversion is being created, @format@ /must/
--     be @VK_FORMAT_UNDEFINED@, otherwise it /must/ not be
--     @VK_FORMAT_UNDEFINED@.
--
-- -   @format@ /must/ support
--     @VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT@ or
--     @VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT@
--
-- -   If the format does not support
--     @VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT@, @xChromaOffset@ and
--     @yChromaOffset@ /must/ not be @VK_CHROMA_LOCATION_COSITED_EVEN@
--
-- -   If the format does not support
--     @VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT@, @xChromaOffset@ and
--     @yChromaOffset@ /must/ not be @VK_CHROMA_LOCATION_MIDPOINT@
--
-- -   @format@ /must/ represent unsigned normalized values (i.e. the
--     format must be a @UNORM@ format)
--
-- -   If the format has a @_422@ or @_420@ suffix:
--
--     -   @components.g@ /must/ be @VK_COMPONENT_SWIZZLE_IDENTITY@
--
--     -   @components.a@ /must/ be @VK_COMPONENT_SWIZZLE_IDENTITY@,
--         @VK_COMPONENT_SWIZZLE_ONE@, or @VK_COMPONENT_SWIZZLE_ZERO@
--
--     -   @components.r@ /must/ be @VK_COMPONENT_SWIZZLE_IDENTITY@ or
--         @VK_COMPONENT_SWIZZLE_B@
--
--     -   @components.b@ /must/ be @VK_COMPONENT_SWIZZLE_IDENTITY@ or
--         @VK_COMPONENT_SWIZZLE_R@
--
--     -   If either @components.r@ or @components.b@ is
--         @VK_COMPONENT_SWIZZLE_IDENTITY@, both values /must/ be
--         @VK_COMPONENT_SWIZZLE_IDENTITY@
--
-- -   If @ycbcrModel@ is not
--     @VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY@, then
--     @components.r@, @components.g@, and @components.b@ /must/ correspond
--     to channels of the @format@; that is, @components.r@,
--     @components.g@, and @components.b@ /must/ not be
--     @VK_COMPONENT_SWIZZLE_ZERO@ or @VK_COMPONENT_SWIZZLE_ONE@, and
--     /must/ not correspond to a channel which contains zero or one as a
--     consequence of [conversion to
--     RGBA](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-conversion-to-rgba)
--
-- -   If the format does not support
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT@,
--     @forceExplicitReconstruction@ /must/ be FALSE
--
-- -   If the format does not support
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT@,
--     @chromaFilter@ /must/ be @VK_FILTER_NEAREST@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.Core10.Core.VkFormat'
--     value
--
-- -   @ycbcrModel@ /must/ be a valid 'VkSamplerYcbcrModelConversion' value
--
-- -   @ycbcrRange@ /must/ be a valid 'VkSamplerYcbcrRange' value
--
-- -   @components@ /must/ be a valid @VkComponentMapping@ structure
--
-- -   @xChromaOffset@ /must/ be a valid 'VkChromaLocation' value
--
-- -   @yChromaOffset@ /must/ be a valid 'VkChromaLocation' value
--
-- -   @chromaFilter@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Sampler.VkFilter' value
--
-- If @chromaFilter@ is @VK_FILTER_NEAREST@, chroma samples are
-- reconstructed to luma channel resolution using nearest-neighbour
-- sampling. Otherwise, chroma samples are reconstructed using
-- interpolation. More details can be found in [the description of sampler
-- Y’CBCR
-- conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion)
-- in the [Image
-- Operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures)
-- chapter.
--
-- = See Also
--
-- @VkBool32@, 'VkChromaLocation',
-- 'Graphics.Vulkan.Core10.ImageView.VkComponentMapping',
-- 'Graphics.Vulkan.Core10.Sampler.VkFilter',
-- 'Graphics.Vulkan.Core10.Core.VkFormat', 'VkSamplerYcbcrModelConversion',
-- 'VkSamplerYcbcrRange', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR'
data VkSamplerYcbcrConversionCreateInfo = VkSamplerYcbcrConversionCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @format@ is the format of the image from which color information will be
  -- retrieved.
  vkFormat :: VkFormat
  , -- | @ycbcrModel@ describes the color matrix for conversion between color
  -- models.
  vkYcbcrModel :: VkSamplerYcbcrModelConversion
  , -- | @ycbcrRange@ describes whether the encoded values have headroom and foot
  -- room, or whether the encoding uses the full numerical range.
  vkYcbcrRange :: VkSamplerYcbcrRange
  , -- | @components@ applies a /swizzle/ based on
  -- 'Graphics.Vulkan.Core10.ImageView.VkComponentSwizzle' enums prior to
  -- range expansion and color model conversion.
  vkComponents :: VkComponentMapping
  , -- | @xChromaOffset@ describes the [sample
  -- location](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-chroma-reconstruction)
  -- associated with downsampled chroma channels in the x dimension.
  -- @xChromaOffset@ has no effect for formats in which chroma channels are
  -- the same resolution as the luma channel.
  vkXChromaOffset :: VkChromaLocation
  , -- | @yChromaOffset@ describes the [sample
  -- location](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-chroma-reconstruction)
  -- associated with downsampled chroma channels in the y dimension.
  -- @yChromaOffset@ has no effect for formats in which the chroma channels
  -- are not downsampled vertically.
  vkYChromaOffset :: VkChromaLocation
  , -- | @chromaFilter@ is the filter for chroma reconstruction.
  vkChromaFilter :: VkFilter
  , -- | @forceExplicitReconstruction@ /can/ be used to ensure that
  -- reconstruction is done explicitly, if supported.
  vkForceExplicitReconstruction :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkSamplerYcbcrConversionCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionCreateInfo <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 28)
                                                <*> peek (ptr `plusPtr` 44)
                                                <*> peek (ptr `plusPtr` 48)
                                                <*> peek (ptr `plusPtr` 52)
                                                <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkYcbcrModel (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkYcbcrRange (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkComponents (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkXChromaOffset (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkYChromaOffset (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkChromaFilter (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkForceExplicitReconstruction (poked :: VkSamplerYcbcrConversionCreateInfo))
-- | VkBindImagePlaneMemoryInfo - Structure specifying how to bind an image
-- plane to memory
--
-- == Valid Usage
--
-- -   @planeAspect@ /must/ be a single valid plane aspect for the image
--     format (that is, @planeAspect@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@ or @VK_IMAGE_ASPECT_PLANE_1_BIT@ for
--     “@_2PLANE@” formats and @planeAspect@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@ for “@_3PLANE@” formats)
--
-- -   A single call to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'
--     /must/ bind all or none of the planes of an image (i.e. bindings to
--     all planes of an image /must/ be made in a single
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'
--     call), as separate bindings
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO@
--
-- -   @planeAspect@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkBindImagePlaneMemoryInfo = VkBindImagePlaneMemoryInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @planeAspect@ is the aspect of the disjoint image plane to bind.
  vkPlaneAspect :: VkImageAspectFlagBits
  }
  deriving (Eq, Show)

instance Storable VkBindImagePlaneMemoryInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBindImagePlaneMemoryInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImagePlaneMemoryInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImagePlaneMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkPlaneAspect (poked :: VkBindImagePlaneMemoryInfo))
-- | VkImagePlaneMemoryRequirementsInfo - Structure specifying image plane
-- for memory requirements
--
-- == Valid Usage
--
-- -   @planeAspect@ /must/ be an aspect that exists in the format; that
--     is, for a two-plane image @planeAspect@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@ or @VK_IMAGE_ASPECT_PLANE_1_BIT@, and
--     for a three-plane image @planeAspect@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@ or
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO@
--
-- -   @planeAspect@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkImagePlaneMemoryRequirementsInfo = VkImagePlaneMemoryRequirementsInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @planeAspect@ is the aspect corresponding to the image plane to query.
  vkPlaneAspect :: VkImageAspectFlagBits
  }
  deriving (Eq, Show)

instance Storable VkImagePlaneMemoryRequirementsInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImagePlaneMemoryRequirementsInfo <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImagePlaneMemoryRequirementsInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImagePlaneMemoryRequirementsInfo))
                *> poke (ptr `plusPtr` 16) (vkPlaneAspect (poked :: VkImagePlaneMemoryRequirementsInfo))
-- | VkPhysicalDeviceSamplerYcbcrConversionFeatures - Structure describing
-- Y’CbCr conversion features that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceSamplerYcbcrConversionFeatures@
-- structure describe the following feature:
--
-- = Description
--
-- -   @samplerYcbcrConversion@ specifies whether the implementation
--     supports [sampler Y’CBCR
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion).
--     If @samplerYcbcrConversion@ is @VK_FALSE@, sampler Y’CBCR conversion
--     is not supported, and samplers using sampler Y’CBCR conversion
--     /must/ not be used.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceSamplerYcbcrConversionFeatures = VkPhysicalDeviceSamplerYcbcrConversionFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceSamplerYcbcrConversionFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceSamplerYcbcrConversionFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceSamplerYcbcrConversionFeatures" "samplerYcbcrConversion"
  vkSamplerYcbcrConversion :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSamplerYcbcrConversionFeatures <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeatures))
                *> poke (ptr `plusPtr` 16) (vkSamplerYcbcrConversion (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeatures))
-- | VkSamplerYcbcrConversionImageFormatProperties - Structure specifying
-- combined image sampler descriptor count for multi-planar images
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkSamplerYcbcrConversionImageFormatProperties = VkSamplerYcbcrConversionImageFormatProperties
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @combinedImageSamplerDescriptorCount@ is the number of combined image
  -- sampler descriptors that the implementation uses to access the format.
  vkCombinedImageSamplerDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkSamplerYcbcrConversionImageFormatProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionImageFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerYcbcrConversionImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkCombinedImageSamplerDescriptorCount (poked :: VkSamplerYcbcrConversionImageFormatProperties))
