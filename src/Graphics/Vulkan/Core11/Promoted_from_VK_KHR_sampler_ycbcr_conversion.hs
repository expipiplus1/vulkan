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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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
  , VkResult(..)
  , VkObjectType(..)
  , VkStructureType(..)
  , VkFormat(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
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

-- No documentation found for Nested "VkChromaLocation" "VK_CHROMA_LOCATION_COSITED_EVEN"
pattern VK_CHROMA_LOCATION_COSITED_EVEN :: VkChromaLocation
pattern VK_CHROMA_LOCATION_COSITED_EVEN = VkChromaLocation 0

-- No documentation found for Nested "VkChromaLocation" "VK_CHROMA_LOCATION_MIDPOINT"
pattern VK_CHROMA_LOCATION_MIDPOINT :: VkChromaLocation
pattern VK_CHROMA_LOCATION_MIDPOINT = VkChromaLocation 1
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8B8G8R8_422_UNORM"
pattern VK_FORMAT_G8B8G8R8_422_UNORM :: VkFormat
pattern VK_FORMAT_G8B8G8R8_422_UNORM = VkFormat 1000156000
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8G8_422_UNORM"
pattern VK_FORMAT_B8G8R8G8_422_UNORM :: VkFormat
pattern VK_FORMAT_B8G8R8G8_422_UNORM = VkFormat 1000156001
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM"
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM = VkFormat 1000156002
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8R8_2PLANE_420_UNORM"
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM = VkFormat 1000156003
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM"
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM = VkFormat 1000156004
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8R8_2PLANE_422_UNORM"
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM = VkFormat 1000156005
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM"
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM = VkFormat 1000156006
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6_UNORM_PACK16"
pattern VK_FORMAT_R10X6_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R10X6_UNORM_PACK16 = VkFormat 1000156007
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6G10X6_UNORM_2PACK16"
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 = VkFormat 1000156008
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16"
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 = VkFormat 1000156009
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16"
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 = VkFormat 1000156010
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16"
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 = VkFormat 1000156011
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156012
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156013
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156014
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156015
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156016
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4_UNORM_PACK16"
pattern VK_FORMAT_R12X4_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R12X4_UNORM_PACK16 = VkFormat 1000156017
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4G12X4_UNORM_2PACK16"
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 = VkFormat 1000156018
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16"
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 = VkFormat 1000156019
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16"
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 = VkFormat 1000156020
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16"
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 = VkFormat 1000156021
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156022
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156023
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156024
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156025
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156026
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16B16G16R16_422_UNORM"
pattern VK_FORMAT_G16B16G16R16_422_UNORM :: VkFormat
pattern VK_FORMAT_G16B16G16R16_422_UNORM = VkFormat 1000156027
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B16G16R16G16_422_UNORM"
pattern VK_FORMAT_B16G16R16G16_422_UNORM :: VkFormat
pattern VK_FORMAT_B16G16R16G16_422_UNORM = VkFormat 1000156028
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM"
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM = VkFormat 1000156029
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16R16_2PLANE_420_UNORM"
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM = VkFormat 1000156030
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM"
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM = VkFormat 1000156031
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16R16_2PLANE_422_UNORM"
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM = VkFormat 1000156032
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM"
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
type VkSamplerYcbcrConversion = Ptr VkSamplerYcbcrConversion_T
-- | vkCreateSamplerYcbcrConversion - Create a new Ycbcr conversion
foreign import ccall "vkCreateSamplerYcbcrConversion" vkCreateSamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult
-- | vkDestroySamplerYcbcrConversion - Destroy a created Y’CbCr conversion
foreign import ccall "vkDestroySamplerYcbcrConversion" vkDestroySamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkSamplerYcbcrConversionInfo - Structure specifying Y’CbCr conversion to
-- a sampler or image view
data VkSamplerYcbcrConversionInfo = VkSamplerYcbcrConversionInfo
  { -- No documentation found for Nested "VkSamplerYcbcrConversionInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSamplerYcbcrConversionInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSamplerYcbcrConversionInfo" "vkConversion"
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
data VkSamplerYcbcrConversionCreateInfo = VkSamplerYcbcrConversionCreateInfo
  { -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkFormat"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkYcbcrModel"
  vkYcbcrModel :: VkSamplerYcbcrModelConversion
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkYcbcrRange"
  vkYcbcrRange :: VkSamplerYcbcrRange
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkComponents"
  vkComponents :: VkComponentMapping
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkXChromaOffset"
  vkXChromaOffset :: VkChromaLocation
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkYChromaOffset"
  vkYChromaOffset :: VkChromaLocation
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkChromaFilter"
  vkChromaFilter :: VkFilter
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "vkForceExplicitReconstruction"
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
data VkBindImagePlaneMemoryInfo = VkBindImagePlaneMemoryInfo
  { -- No documentation found for Nested "VkBindImagePlaneMemoryInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindImagePlaneMemoryInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindImagePlaneMemoryInfo" "vkPlaneAspect"
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
data VkImagePlaneMemoryRequirementsInfo = VkImagePlaneMemoryRequirementsInfo
  { -- No documentation found for Nested "VkImagePlaneMemoryRequirementsInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImagePlaneMemoryRequirementsInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImagePlaneMemoryRequirementsInfo" "vkPlaneAspect"
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
data VkPhysicalDeviceSamplerYcbcrConversionFeatures = VkPhysicalDeviceSamplerYcbcrConversionFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceSamplerYcbcrConversionFeatures" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceSamplerYcbcrConversionFeatures" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceSamplerYcbcrConversionFeatures" "vkSamplerYcbcrConversion"
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
data VkSamplerYcbcrConversionImageFormatProperties = VkSamplerYcbcrConversionImageFormatProperties
  { -- No documentation found for Nested "VkSamplerYcbcrConversionImageFormatProperties" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSamplerYcbcrConversionImageFormatProperties" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSamplerYcbcrConversionImageFormatProperties" "vkCombinedImageSamplerDescriptorCount"
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
