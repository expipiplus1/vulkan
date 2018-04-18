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

-- | 
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

-- | 
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY = VkSamplerYcbcrModelConversion 0

-- | just range expansion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY = VkSamplerYcbcrModelConversion 1

-- | aka HD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 = VkSamplerYcbcrModelConversion 2

-- | aka SD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 = VkSamplerYcbcrModelConversion 3

-- | aka UHD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 = VkSamplerYcbcrModelConversion 4
-- ** VkSamplerYcbcrRange

-- | 
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

-- | Luma 0..1 maps to 0..255, chroma -0.5..0.5 to 1..255 (clamped)
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL :: VkSamplerYcbcrRange
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL = VkSamplerYcbcrRange 0

-- | Luma 0..1 maps to 16..235, chroma -0.5..0.5 to 16..240
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW :: VkSamplerYcbcrRange
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW = VkSamplerYcbcrRange 1
-- ** VkChromaLocation

-- | 
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

-- | 
pattern VK_CHROMA_LOCATION_COSITED_EVEN :: VkChromaLocation
pattern VK_CHROMA_LOCATION_COSITED_EVEN = VkChromaLocation 0

-- | 
pattern VK_CHROMA_LOCATION_MIDPOINT :: VkChromaLocation
pattern VK_CHROMA_LOCATION_MIDPOINT = VkChromaLocation 1
-- | Nothing
pattern VK_FORMAT_G8B8G8R8_422_UNORM :: VkFormat
pattern VK_FORMAT_G8B8G8R8_422_UNORM = VkFormat 1000156000
-- | Nothing
pattern VK_FORMAT_B8G8R8G8_422_UNORM :: VkFormat
pattern VK_FORMAT_B8G8R8G8_422_UNORM = VkFormat 1000156001
-- | Nothing
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM = VkFormat 1000156002
-- | Nothing
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM = VkFormat 1000156003
-- | Nothing
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM = VkFormat 1000156004
-- | Nothing
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM = VkFormat 1000156005
-- | Nothing
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM = VkFormat 1000156006
-- | Nothing
pattern VK_FORMAT_R10X6_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R10X6_UNORM_PACK16 = VkFormat 1000156007
-- | Nothing
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 = VkFormat 1000156008
-- | Nothing
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 = VkFormat 1000156009
-- | Nothing
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 = VkFormat 1000156010
-- | Nothing
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 = VkFormat 1000156011
-- | Nothing
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156012
-- | Nothing
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156013
-- | Nothing
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156014
-- | Nothing
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156015
-- | Nothing
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156016
-- | Nothing
pattern VK_FORMAT_R12X4_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R12X4_UNORM_PACK16 = VkFormat 1000156017
-- | Nothing
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 = VkFormat 1000156018
-- | Nothing
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 = VkFormat 1000156019
-- | Nothing
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 = VkFormat 1000156020
-- | Nothing
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 = VkFormat 1000156021
-- | Nothing
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156022
-- | Nothing
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156023
-- | Nothing
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156024
-- | Nothing
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156025
-- | Nothing
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156026
-- | Nothing
pattern VK_FORMAT_G16B16G16R16_422_UNORM :: VkFormat
pattern VK_FORMAT_G16B16G16R16_422_UNORM = VkFormat 1000156027
-- | Nothing
pattern VK_FORMAT_B16G16R16G16_422_UNORM :: VkFormat
pattern VK_FORMAT_B16G16R16G16_422_UNORM = VkFormat 1000156028
-- | Nothing
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM = VkFormat 1000156029
-- | Nothing
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM = VkFormat 1000156030
-- | Nothing
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM = VkFormat 1000156031
-- | Nothing
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM = VkFormat 1000156032
-- | Nothing
pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM = VkFormat 1000156033
-- | Nothing
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO = VkStructureType 1000156000
-- | Nothing
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO = VkStructureType 1000156001
-- | Nothing
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO = VkStructureType 1000156002
-- | Nothing
pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO = VkStructureType 1000156003
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES = VkStructureType 1000156004
-- | Nothing
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES = VkStructureType 1000156005
-- | Nothing
pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION :: VkObjectType
pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION = VkObjectType 1000156000
-- | Nothing
pattern VK_IMAGE_CREATE_DISJOINT_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_DISJOINT_BIT = VkImageCreateFlagBits 0x00000200
-- | Just "Format can have midpoint rather than cosited chroma samples"
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT = VkFormatFeatureFlagBits 0x00020000
-- | Just "Format can be used with linear filtering whilst color conversion is enabled"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT = VkFormatFeatureFlagBits 0x00040000
-- | Just "Format can have different chroma, min and mag filters"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT = VkFormatFeatureFlagBits 0x00080000
-- | Nothing
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT = VkFormatFeatureFlagBits 0x00100000
-- | Nothing
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT = VkFormatFeatureFlagBits 0x00200000
-- | Just "Format supports disjoint planes"
pattern VK_FORMAT_FEATURE_DISJOINT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_DISJOINT_BIT = VkFormatFeatureFlagBits 0x00400000
-- | Just "Format can have cosited rather than midpoint chroma samples"
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT = VkFormatFeatureFlagBits 0x00800000
-- | Nothing
pattern VK_IMAGE_ASPECT_PLANE_0_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_0_BIT = VkImageAspectFlagBits 0x00000010
-- | Nothing
pattern VK_IMAGE_ASPECT_PLANE_1_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_1_BIT = VkImageAspectFlagBits 0x00000020
-- | Nothing
pattern VK_IMAGE_ASPECT_PLANE_2_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_2_BIT = VkImageAspectFlagBits 0x00000040
-- |
data VkSamplerYcbcrConversion_T
type VkSamplerYcbcrConversion = Ptr VkSamplerYcbcrConversion_T
-- | 
foreign import ccall "vkCreateSamplerYcbcrConversion" vkCreateSamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult
-- | 
foreign import ccall "vkDestroySamplerYcbcrConversion" vkDestroySamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | TODO: Struct comments
data VkSamplerYcbcrConversionInfo = VkSamplerYcbcrConversionInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkConversion :: VkSamplerYcbcrConversion
  }
  deriving (Eq, Show)

instance Storable VkSamplerYcbcrConversionInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkSamplerYcbcrConversionInfo))
                *> poke (ptr `plusPtr` 16) (vkConversion (poked :: VkSamplerYcbcrConversionInfo))
-- | TODO: Struct comments
data VkSamplerYcbcrConversionCreateInfo = VkSamplerYcbcrConversionCreateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFormat :: VkFormat
  , vkYcbcrModel :: VkSamplerYcbcrModelConversion
  , vkYcbcrRange :: VkSamplerYcbcrRange
  , vkComponents :: VkComponentMapping
  , vkXChromaOffset :: VkChromaLocation
  , vkYChromaOffset :: VkChromaLocation
  , vkChromaFilter :: VkFilter
  , vkForceExplicitReconstruction :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkYcbcrModel (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkYcbcrRange (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkComponents (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkXChromaOffset (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkYChromaOffset (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkChromaFilter (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkForceExplicitReconstruction (poked :: VkSamplerYcbcrConversionCreateInfo))
-- | TODO: Struct comments
data VkBindImagePlaneMemoryInfo = VkBindImagePlaneMemoryInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkPlaneAspect :: VkImageAspectFlagBits
  }
  deriving (Eq, Show)

instance Storable VkBindImagePlaneMemoryInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBindImagePlaneMemoryInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImagePlaneMemoryInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkBindImagePlaneMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkPlaneAspect (poked :: VkBindImagePlaneMemoryInfo))
-- | TODO: Struct comments
data VkImagePlaneMemoryRequirementsInfo = VkImagePlaneMemoryRequirementsInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkPlaneAspect :: VkImageAspectFlagBits
  }
  deriving (Eq, Show)

instance Storable VkImagePlaneMemoryRequirementsInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImagePlaneMemoryRequirementsInfo <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImagePlaneMemoryRequirementsInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkImagePlaneMemoryRequirementsInfo))
                *> poke (ptr `plusPtr` 16) (vkPlaneAspect (poked :: VkImagePlaneMemoryRequirementsInfo))
-- | TODO: Struct comments
data VkPhysicalDeviceSamplerYcbcrConversionFeatures = VkPhysicalDeviceSamplerYcbcrConversionFeatures
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSamplerYcbcrConversion :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSamplerYcbcrConversionFeatures <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeatures))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeatures))
                *> poke (ptr `plusPtr` 16) (vkSamplerYcbcrConversion (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeatures))
-- | TODO: Struct comments
data VkSamplerYcbcrConversionImageFormatProperties = VkSamplerYcbcrConversionImageFormatProperties
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkCombinedImageSamplerDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkSamplerYcbcrConversionImageFormatProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionImageFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkSamplerYcbcrConversionImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkCombinedImageSamplerDescriptorCount (poked :: VkSamplerYcbcrConversionImageFormatProperties))
