{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  BindImagePlaneMemoryInfo(..)
  , 
#endif
  ChromaLocation
  , pattern CHROMA_LOCATION_COSITED_EVEN
  , pattern CHROMA_LOCATION_MIDPOINT
  , ChromaLocationKHR
#if defined(VK_USE_PLATFORM_GGP)
  , ImagePlaneMemoryRequirementsInfo(..)
  , PhysicalDeviceSamplerYcbcrConversionFeatures(..)
#endif
  , SamplerYcbcrConversion
#if defined(VK_USE_PLATFORM_GGP)
  , SamplerYcbcrConversionCreateInfo(..)
  , SamplerYcbcrConversionImageFormatProperties(..)
  , SamplerYcbcrConversionInfo(..)
#endif
  , SamplerYcbcrModelConversion
  , pattern SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
  , pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
  , pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
  , pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
  , pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
  , SamplerYcbcrModelConversionKHR
  , SamplerYcbcrRange
  , pattern SAMPLER_YCBCR_RANGE_ITU_FULL
  , pattern SAMPLER_YCBCR_RANGE_ITU_NARROW
  , SamplerYcbcrRangeKHR
  , createSamplerYcbcrConversion
  , destroySamplerYcbcrConversion
  , withSamplerYcbcrConversion
  , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
  , pattern STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
  , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
  , pattern OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION
  , pattern FORMAT_G8B8G8R8_422_UNORM
  , pattern FORMAT_B8G8R8G8_422_UNORM
  , pattern FORMAT_G8_B8_R8_3PLANE_420_UNORM
  , pattern FORMAT_G8_B8R8_2PLANE_420_UNORM
  , pattern FORMAT_G8_B8_R8_3PLANE_422_UNORM
  , pattern FORMAT_G8_B8R8_2PLANE_422_UNORM
  , pattern FORMAT_G8_B8_R8_3PLANE_444_UNORM
  , pattern FORMAT_R10X6_UNORM_PACK16
  , pattern FORMAT_R10X6G10X6_UNORM_2PACK16
  , pattern FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16
  , pattern FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16
  , pattern FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16
  , pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16
  , pattern FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16
  , pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16
  , pattern FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16
  , pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16
  , pattern FORMAT_R12X4_UNORM_PACK16
  , pattern FORMAT_R12X4G12X4_UNORM_2PACK16
  , pattern FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16
  , pattern FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16
  , pattern FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16
  , pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16
  , pattern FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16
  , pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16
  , pattern FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16
  , pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16
  , pattern FORMAT_G16B16G16R16_422_UNORM
  , pattern FORMAT_B16G16R16G16_422_UNORM
  , pattern FORMAT_G16_B16_R16_3PLANE_420_UNORM
  , pattern FORMAT_G16_B16R16_2PLANE_420_UNORM
  , pattern FORMAT_G16_B16_R16_3PLANE_422_UNORM
  , pattern FORMAT_G16_B16R16_2PLANE_422_UNORM
  , pattern FORMAT_G16_B16_R16_3PLANE_444_UNORM
  , pattern IMAGE_ASPECT_PLANE_0_BIT
  , pattern IMAGE_ASPECT_PLANE_1_BIT
  , pattern IMAGE_ASPECT_PLANE_2_BIT
  , pattern IMAGE_CREATE_DISJOINT_BIT
  , pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern FORMAT_FEATURE_DISJOINT_BIT
  , pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkChromaLocation(..)
  , VkSamplerYcbcrModelConversion(..)
  , VkSamplerYcbcrRange(..)
  , VkSamplerYcbcrConversion
  , vkCreateSamplerYcbcrConversion
  , vkDestroySamplerYcbcrConversion
  , pattern VK_CHROMA_LOCATION_COSITED_EVEN
  , pattern VK_CHROMA_LOCATION_MIDPOINT
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
  , pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL
  , pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.ImageView
  ( ComponentMapping(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Sampler
  ( Filter
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16
  , pattern FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16
  , pattern FORMAT_B16G16R16G16_422_UNORM
  , pattern FORMAT_B8G8R8G8_422_UNORM
  , pattern FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16
  , pattern FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16
  , pattern FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16
  , pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16
  , pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16
  , pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16
  , pattern FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16
  , pattern FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16
  , pattern FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16
  , pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16
  , pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16
  , pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16
  , pattern FORMAT_G16B16G16R16_422_UNORM
  , pattern FORMAT_G16_B16R16_2PLANE_420_UNORM
  , pattern FORMAT_G16_B16R16_2PLANE_422_UNORM
  , pattern FORMAT_G16_B16_R16_3PLANE_420_UNORM
  , pattern FORMAT_G16_B16_R16_3PLANE_422_UNORM
  , pattern FORMAT_G16_B16_R16_3PLANE_444_UNORM
  , pattern FORMAT_G8B8G8R8_422_UNORM
  , pattern FORMAT_G8_B8R8_2PLANE_420_UNORM
  , pattern FORMAT_G8_B8R8_2PLANE_422_UNORM
  , pattern FORMAT_G8_B8_R8_3PLANE_420_UNORM
  , pattern FORMAT_G8_B8_R8_3PLANE_422_UNORM
  , pattern FORMAT_G8_B8_R8_3PLANE_444_UNORM
  , pattern FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16
  , pattern FORMAT_R10X6G10X6_UNORM_2PACK16
  , pattern FORMAT_R10X6_UNORM_PACK16
  , pattern FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16
  , pattern FORMAT_R12X4G12X4_UNORM_2PACK16
  , pattern FORMAT_R12X4_UNORM_PACK16
  , pattern OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION
  , pattern STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
  , pattern STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
  , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
  , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  , pattern FORMAT_FEATURE_DISJOINT_BIT
  , pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern IMAGE_CREATE_DISJOINT_BIT
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( pattern IMAGE_ASPECT_PLANE_0_BIT
  , pattern IMAGE_ASPECT_PLANE_1_BIT
  , pattern IMAGE_ASPECT_PLANE_2_BIT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBindImagePlaneMemoryInfo"
data BindImagePlaneMemoryInfo = BindImagePlaneMemoryInfo
  { -- No documentation found for Nested "BindImagePlaneMemoryInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImagePlaneMemoryInfo" "planeAspect"
  planeAspect :: ImageAspectFlagBits
  }
  deriving (Show, Eq)

instance Zero BindImagePlaneMemoryInfo where
  zero = BindImagePlaneMemoryInfo Nothing
                                  zero

#endif

-- No documentation found for TopLevel "ChromaLocation"
type ChromaLocation = VkChromaLocation


{-# complete CHROMA_LOCATION_COSITED_EVEN, CHROMA_LOCATION_MIDPOINT :: ChromaLocation #-}


-- No documentation found for Nested "ChromaLocation" "CHROMA_LOCATION_COSITED_EVEN"
pattern CHROMA_LOCATION_COSITED_EVEN :: (a ~ ChromaLocation) => a
pattern CHROMA_LOCATION_COSITED_EVEN = VK_CHROMA_LOCATION_COSITED_EVEN


-- No documentation found for Nested "ChromaLocation" "CHROMA_LOCATION_MIDPOINT"
pattern CHROMA_LOCATION_MIDPOINT :: (a ~ ChromaLocation) => a
pattern CHROMA_LOCATION_MIDPOINT = VK_CHROMA_LOCATION_MIDPOINT

-- No documentation found for TopLevel "ChromaLocationKHR"
type ChromaLocationKHR = ChromaLocation


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImagePlaneMemoryRequirementsInfo"
data ImagePlaneMemoryRequirementsInfo = ImagePlaneMemoryRequirementsInfo
  { -- No documentation found for Nested "ImagePlaneMemoryRequirementsInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImagePlaneMemoryRequirementsInfo" "planeAspect"
  planeAspect :: ImageAspectFlagBits
  }
  deriving (Show, Eq)

instance Zero ImagePlaneMemoryRequirementsInfo where
  zero = ImagePlaneMemoryRequirementsInfo Nothing
                                          zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceSamplerYcbcrConversionFeatures"
data PhysicalDeviceSamplerYcbcrConversionFeatures = PhysicalDeviceSamplerYcbcrConversionFeatures
  { -- No documentation found for Nested "PhysicalDeviceSamplerYcbcrConversionFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSamplerYcbcrConversionFeatures" "samplerYcbcrConversion"
  samplerYcbcrConversion :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceSamplerYcbcrConversionFeatures where
  zero = PhysicalDeviceSamplerYcbcrConversionFeatures Nothing
                                                      False

#endif

-- No documentation found for TopLevel "SamplerYcbcrConversion"
type SamplerYcbcrConversion = VkSamplerYcbcrConversion


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSamplerYcbcrConversionCreateInfo"
data SamplerYcbcrConversionCreateInfo = SamplerYcbcrConversionCreateInfo
  { -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "format"
  format :: Format
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "ycbcrModel"
  ycbcrModel :: SamplerYcbcrModelConversion
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "ycbcrRange"
  ycbcrRange :: SamplerYcbcrRange
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "components"
  components :: ComponentMapping
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "xChromaOffset"
  xChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "yChromaOffset"
  yChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "chromaFilter"
  chromaFilter :: Filter
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "forceExplicitReconstruction"
  forceExplicitReconstruction :: Bool
  }
  deriving (Show, Eq)

instance Zero SamplerYcbcrConversionCreateInfo where
  zero = SamplerYcbcrConversionCreateInfo Nothing
                                          zero
                                          zero
                                          zero
                                          zero
                                          zero
                                          zero
                                          zero
                                          False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSamplerYcbcrConversionImageFormatProperties"
data SamplerYcbcrConversionImageFormatProperties = SamplerYcbcrConversionImageFormatProperties
  { -- No documentation found for Nested "SamplerYcbcrConversionImageFormatProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerYcbcrConversionImageFormatProperties" "combinedImageSamplerDescriptorCount"
  combinedImageSamplerDescriptorCount :: Word32
  }
  deriving (Show, Eq)

instance Zero SamplerYcbcrConversionImageFormatProperties where
  zero = SamplerYcbcrConversionImageFormatProperties Nothing
                                                     zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSamplerYcbcrConversionInfo"
data SamplerYcbcrConversionInfo = SamplerYcbcrConversionInfo
  { -- No documentation found for Nested "SamplerYcbcrConversionInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerYcbcrConversionInfo" "conversion"
  conversion :: SamplerYcbcrConversion
  }
  deriving (Show, Eq)

instance Zero SamplerYcbcrConversionInfo where
  zero = SamplerYcbcrConversionInfo Nothing
                                    zero

#endif

-- No documentation found for TopLevel "SamplerYcbcrModelConversion"
type SamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion


{-# complete SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY, SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY, SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709, SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601, SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 :: SamplerYcbcrModelConversion #-}


-- No documentation found for Nested "SamplerYcbcrModelConversion" "SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY :: (a ~ SamplerYcbcrModelConversion) => a
pattern SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY = VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY


-- No documentation found for Nested "SamplerYcbcrModelConversion" "SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY :: (a ~ SamplerYcbcrModelConversion) => a
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY = VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY


-- No documentation found for Nested "SamplerYcbcrModelConversion" "SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 :: (a ~ SamplerYcbcrModelConversion) => a
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 = VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709


-- No documentation found for Nested "SamplerYcbcrModelConversion" "SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 :: (a ~ SamplerYcbcrModelConversion) => a
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 = VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601


-- No documentation found for Nested "SamplerYcbcrModelConversion" "SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 :: (a ~ SamplerYcbcrModelConversion) => a
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 = VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020

-- No documentation found for TopLevel "SamplerYcbcrModelConversionKHR"
type SamplerYcbcrModelConversionKHR = SamplerYcbcrModelConversion

-- No documentation found for TopLevel "SamplerYcbcrRange"
type SamplerYcbcrRange = VkSamplerYcbcrRange


{-# complete SAMPLER_YCBCR_RANGE_ITU_FULL, SAMPLER_YCBCR_RANGE_ITU_NARROW :: SamplerYcbcrRange #-}


-- No documentation found for Nested "SamplerYcbcrRange" "SAMPLER_YCBCR_RANGE_ITU_FULL"
pattern SAMPLER_YCBCR_RANGE_ITU_FULL :: (a ~ SamplerYcbcrRange) => a
pattern SAMPLER_YCBCR_RANGE_ITU_FULL = VK_SAMPLER_YCBCR_RANGE_ITU_FULL


-- No documentation found for Nested "SamplerYcbcrRange" "SAMPLER_YCBCR_RANGE_ITU_NARROW"
pattern SAMPLER_YCBCR_RANGE_ITU_NARROW :: (a ~ SamplerYcbcrRange) => a
pattern SAMPLER_YCBCR_RANGE_ITU_NARROW = VK_SAMPLER_YCBCR_RANGE_ITU_NARROW

-- No documentation found for TopLevel "SamplerYcbcrRangeKHR"
type SamplerYcbcrRangeKHR = SamplerYcbcrRange


-- No documentation found for TopLevel "vkCreateSamplerYcbcrConversion"
createSamplerYcbcrConversion :: Device ->  SamplerYcbcrConversionCreateInfo ->  Maybe AllocationCallbacks ->  IO (SamplerYcbcrConversion)
createSamplerYcbcrConversion = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroySamplerYcbcrConversion"
destroySamplerYcbcrConversion :: Device ->  SamplerYcbcrConversion ->  Maybe AllocationCallbacks ->  IO ()
destroySamplerYcbcrConversion = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createSamplerYcbcrConversion' and 'destroySamplerYcbcrConversion' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withSamplerYcbcrConversion
  :: Device -> SamplerYcbcrConversionCreateInfo -> Maybe AllocationCallbacks -> (SamplerYcbcrConversion -> IO a) -> IO a
withSamplerYcbcrConversion device samplerYcbcrConversionCreateInfo allocationCallbacks = bracket
  (createSamplerYcbcrConversion device samplerYcbcrConversionCreateInfo allocationCallbacks)
  (\o -> destroySamplerYcbcrConversion device o allocationCallbacks)
