{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkBindImagePlaneMemoryInfo(..)
  , VkChromaLocation(..)
  , pattern VK_CHROMA_LOCATION_COSITED_EVEN
  , pattern VK_CHROMA_LOCATION_MIDPOINT
  , VkImagePlaneMemoryRequirementsInfo(..)
  , VkPhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , VkSamplerYcbcrConversion
  , VkSamplerYcbcrConversionCreateInfo(..)
  , VkSamplerYcbcrConversionImageFormatProperties(..)
  , VkSamplerYcbcrConversionInfo(..)
  , VkSamplerYcbcrModelConversion(..)
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
  , VkSamplerYcbcrRange(..)
  , pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL
  , pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkCreateSamplerYcbcrConversion
#endif
  , FN_vkCreateSamplerYcbcrConversion
  , PFN_vkCreateSamplerYcbcrConversion
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkDestroySamplerYcbcrConversion
#endif
  , FN_vkDestroySamplerYcbcrConversion
  , PFN_vkDestroySamplerYcbcrConversion
  , pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16
  , pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16
  , pattern VK_FORMAT_B16G16R16G16_422_UNORM
  , pattern VK_FORMAT_B8G8R8G8_422_UNORM
  , pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_DISJOINT_BIT
  , pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16
  , pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16
  , pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16
  , pattern VK_FORMAT_G16B16G16R16_422_UNORM
  , pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM
  , pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM
  , pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM
  , pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM
  , pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM
  , pattern VK_FORMAT_G8B8G8R8_422_UNORM
  , pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM
  , pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM
  , pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM
  , pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM
  , pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM
  , pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16
  , pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16
  , pattern VK_FORMAT_R10X6_UNORM_PACK16
  , pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16
  , pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16
  , pattern VK_FORMAT_R12X4_UNORM_PACK16
  , pattern VK_IMAGE_ASPECT_PLANE_0_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_1_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_2_BIT
  , pattern VK_IMAGE_CREATE_DISJOINT_BIT
  , pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping(..)
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkFilter(..)
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlagBits(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkBindImagePlaneMemoryInfo"
data VkBindImagePlaneMemoryInfo = VkBindImagePlaneMemoryInfo
  { -- No documentation found for Nested "VkBindImagePlaneMemoryInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindImagePlaneMemoryInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindImagePlaneMemoryInfo" "planeAspect"
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

instance Zero VkBindImagePlaneMemoryInfo where
  zero = VkBindImagePlaneMemoryInfo zero
                                    zero
                                    zero
-- ** VkChromaLocation

-- No documentation found for TopLevel "VkChromaLocation"
newtype VkChromaLocation = VkChromaLocation Int32
  deriving (Eq, Ord, Storable, Zero)

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
-- No documentation found for TopLevel "VkImagePlaneMemoryRequirementsInfo"
data VkImagePlaneMemoryRequirementsInfo = VkImagePlaneMemoryRequirementsInfo
  { -- No documentation found for Nested "VkImagePlaneMemoryRequirementsInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImagePlaneMemoryRequirementsInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImagePlaneMemoryRequirementsInfo" "planeAspect"
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

instance Zero VkImagePlaneMemoryRequirementsInfo where
  zero = VkImagePlaneMemoryRequirementsInfo zero
                                            zero
                                            zero
-- No documentation found for TopLevel "VkPhysicalDeviceSamplerYcbcrConversionFeatures"
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

instance Zero VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  zero = VkPhysicalDeviceSamplerYcbcrConversionFeatures zero
                                                        zero
                                                        zero
-- | Dummy data to tag the 'Ptr' with
data VkSamplerYcbcrConversion_T
-- No documentation found for TopLevel "VkSamplerYcbcrConversion"
type VkSamplerYcbcrConversion = Ptr VkSamplerYcbcrConversion_T
-- No documentation found for TopLevel "VkSamplerYcbcrConversionCreateInfo"
data VkSamplerYcbcrConversionCreateInfo = VkSamplerYcbcrConversionCreateInfo
  { -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "ycbcrModel"
  vkYcbcrModel :: VkSamplerYcbcrModelConversion
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "ycbcrRange"
  vkYcbcrRange :: VkSamplerYcbcrRange
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "components"
  vkComponents :: VkComponentMapping
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "xChromaOffset"
  vkXChromaOffset :: VkChromaLocation
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "yChromaOffset"
  vkYChromaOffset :: VkChromaLocation
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "chromaFilter"
  vkChromaFilter :: VkFilter
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "forceExplicitReconstruction"
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

instance Zero VkSamplerYcbcrConversionCreateInfo where
  zero = VkSamplerYcbcrConversionCreateInfo zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
-- No documentation found for TopLevel "VkSamplerYcbcrConversionImageFormatProperties"
data VkSamplerYcbcrConversionImageFormatProperties = VkSamplerYcbcrConversionImageFormatProperties
  { -- No documentation found for Nested "VkSamplerYcbcrConversionImageFormatProperties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSamplerYcbcrConversionImageFormatProperties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSamplerYcbcrConversionImageFormatProperties" "combinedImageSamplerDescriptorCount"
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

instance Zero VkSamplerYcbcrConversionImageFormatProperties where
  zero = VkSamplerYcbcrConversionImageFormatProperties zero
                                                       zero
                                                       zero
-- No documentation found for TopLevel "VkSamplerYcbcrConversionInfo"
data VkSamplerYcbcrConversionInfo = VkSamplerYcbcrConversionInfo
  { -- No documentation found for Nested "VkSamplerYcbcrConversionInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSamplerYcbcrConversionInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSamplerYcbcrConversionInfo" "conversion"
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

instance Zero VkSamplerYcbcrConversionInfo where
  zero = VkSamplerYcbcrConversionInfo zero
                                      zero
                                      zero
-- ** VkSamplerYcbcrModelConversion

-- No documentation found for TopLevel "VkSamplerYcbcrModelConversion"
newtype VkSamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for TopLevel "VkSamplerYcbcrRange"
newtype VkSamplerYcbcrRange = VkSamplerYcbcrRange Int32
  deriving (Eq, Ord, Storable, Zero)

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
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkCreateSamplerYcbcrConversion"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSamplerYcbcrConversion" vkCreateSamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult

#endif
type FN_vkCreateSamplerYcbcrConversion = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult
type PFN_vkCreateSamplerYcbcrConversion = FunPtr FN_vkCreateSamplerYcbcrConversion
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkDestroySamplerYcbcrConversion"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroySamplerYcbcrConversion" vkDestroySamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroySamplerYcbcrConversion = ("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySamplerYcbcrConversion = FunPtr FN_vkDestroySamplerYcbcrConversion
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16"
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 = VkFormat 1000156011
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16"
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 = VkFormat 1000156021
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B16G16R16G16_422_UNORM"
pattern VK_FORMAT_B16G16R16G16_422_UNORM :: VkFormat
pattern VK_FORMAT_B16G16R16G16_422_UNORM = VkFormat 1000156028
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8G8_422_UNORM"
pattern VK_FORMAT_B8G8R8G8_422_UNORM :: VkFormat
pattern VK_FORMAT_B8G8R8G8_422_UNORM = VkFormat 1000156001
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT"
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT = VkFormatFeatureFlagBits 0x00800000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_DISJOINT_BIT"
pattern VK_FORMAT_FEATURE_DISJOINT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_DISJOINT_BIT = VkFormatFeatureFlagBits 0x00400000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT"
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT = VkFormatFeatureFlagBits 0x00020000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT = VkFormatFeatureFlagBits 0x00100000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT = VkFormatFeatureFlagBits 0x00200000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT = VkFormatFeatureFlagBits 0x00040000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT = VkFormatFeatureFlagBits 0x00080000
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16"
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 = VkFormat 1000156010
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156013
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156015
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156012
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156014
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156016
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16"
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 = VkFormat 1000156020
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156023
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156025
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156022
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156024
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156026
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16B16G16R16_422_UNORM"
pattern VK_FORMAT_G16B16G16R16_422_UNORM :: VkFormat
pattern VK_FORMAT_G16B16G16R16_422_UNORM = VkFormat 1000156027
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16R16_2PLANE_420_UNORM"
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM = VkFormat 1000156030
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16R16_2PLANE_422_UNORM"
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM = VkFormat 1000156032
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM"
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM = VkFormat 1000156029
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM"
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM = VkFormat 1000156031
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM"
pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM = VkFormat 1000156033
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8B8G8R8_422_UNORM"
pattern VK_FORMAT_G8B8G8R8_422_UNORM :: VkFormat
pattern VK_FORMAT_G8B8G8R8_422_UNORM = VkFormat 1000156000
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8R8_2PLANE_420_UNORM"
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM = VkFormat 1000156003
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8R8_2PLANE_422_UNORM"
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM = VkFormat 1000156005
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM"
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM = VkFormat 1000156002
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM"
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM = VkFormat 1000156004
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM"
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM = VkFormat 1000156006
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16"
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 = VkFormat 1000156009
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6G10X6_UNORM_2PACK16"
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 = VkFormat 1000156008
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6_UNORM_PACK16"
pattern VK_FORMAT_R10X6_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R10X6_UNORM_PACK16 = VkFormat 1000156007
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16"
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 = VkFormat 1000156019
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4G12X4_UNORM_2PACK16"
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 = VkFormat 1000156018
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4_UNORM_PACK16"
pattern VK_FORMAT_R12X4_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R12X4_UNORM_PACK16 = VkFormat 1000156017
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_0_BIT"
pattern VK_IMAGE_ASPECT_PLANE_0_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_0_BIT = VkImageAspectFlagBits 0x00000010
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_1_BIT"
pattern VK_IMAGE_ASPECT_PLANE_1_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_1_BIT = VkImageAspectFlagBits 0x00000020
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_2_BIT"
pattern VK_IMAGE_ASPECT_PLANE_2_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_2_BIT = VkImageAspectFlagBits 0x00000040
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_DISJOINT_BIT"
pattern VK_IMAGE_CREATE_DISJOINT_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_DISJOINT_BIT = VkImageCreateFlagBits 0x00000200
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION"
pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION :: VkObjectType
pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION = VkObjectType 1000156000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO = VkStructureType 1000156002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO"
pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO = VkStructureType 1000156003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES = VkStructureType 1000156004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO = VkStructureType 1000156000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES"
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES = VkStructureType 1000156005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO"
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO = VkStructureType 1000156001
