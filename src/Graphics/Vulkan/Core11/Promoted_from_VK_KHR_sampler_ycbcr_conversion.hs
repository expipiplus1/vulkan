{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( withCStructBindImagePlaneMemoryInfo
  , fromCStructBindImagePlaneMemoryInfo
  , BindImagePlaneMemoryInfo(..)
  , ChromaLocation
  , ChromaLocationKHR
  , withCStructImagePlaneMemoryRequirementsInfo
  , fromCStructImagePlaneMemoryRequirementsInfo
  , ImagePlaneMemoryRequirementsInfo(..)
  , withCStructPhysicalDeviceSamplerYcbcrConversionFeatures
  , fromCStructPhysicalDeviceSamplerYcbcrConversionFeatures
  , PhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , SamplerYcbcrConversion
  , withCStructSamplerYcbcrConversionCreateInfo
  , fromCStructSamplerYcbcrConversionCreateInfo
  , SamplerYcbcrConversionCreateInfo(..)
  , withCStructSamplerYcbcrConversionImageFormatProperties
  , fromCStructSamplerYcbcrConversionImageFormatProperties
  , SamplerYcbcrConversionImageFormatProperties(..)
  , withCStructSamplerYcbcrConversionInfo
  , fromCStructSamplerYcbcrConversionInfo
  , SamplerYcbcrConversionInfo(..)
  , SamplerYcbcrModelConversion
  , SamplerYcbcrModelConversionKHR
  , SamplerYcbcrRange
  , SamplerYcbcrRangeKHR
  , createSamplerYcbcrConversion
  , destroySamplerYcbcrConversion
  , withSamplerYcbcrConversion
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
  , pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION
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
  , pattern VK_IMAGE_ASPECT_PLANE_0_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_1_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_2_BIT
  , pattern VK_IMAGE_CREATE_DISJOINT_BIT
  , pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern VK_FORMAT_FEATURE_DISJOINT_BIT
  , pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createSamplerYcbcrConversion
  , destroySamplerYcbcrConversion
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkBindImagePlaneMemoryInfo(..)
  , VkChromaLocation(..)
  , VkImagePlaneMemoryRequirementsInfo(..)
  , VkPhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , VkSamplerYcbcrConversionCreateInfo(..)
  , VkSamplerYcbcrConversionImageFormatProperties(..)
  , VkSamplerYcbcrConversionInfo(..)
  , VkSamplerYcbcrModelConversion(..)
  , VkSamplerYcbcrRange(..)
  , VkSamplerYcbcrConversion
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.ImageView
  ( ComponentMapping(..)
  , fromCStructComponentMapping
  , withCStructComponentMapping
  )
import Graphics.Vulkan.Core10.Sampler
  ( Filter
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlagBits
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16
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
  )


-- No documentation found for TopLevel "BindImagePlaneMemoryInfo"
data BindImagePlaneMemoryInfo = BindImagePlaneMemoryInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "BindImagePlaneMemoryInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImagePlaneMemoryInfo" "planeAspect"
  vkPlaneAspect :: ImageAspectFlagBits
  }
  deriving (Show, Eq)
withCStructBindImagePlaneMemoryInfo :: BindImagePlaneMemoryInfo -> (VkBindImagePlaneMemoryInfo -> IO a) -> IO a
withCStructBindImagePlaneMemoryInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: BindImagePlaneMemoryInfo)) (\pPNext -> cont (VkBindImagePlaneMemoryInfo VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO pPNext (vkPlaneAspect (from :: BindImagePlaneMemoryInfo))))
fromCStructBindImagePlaneMemoryInfo :: VkBindImagePlaneMemoryInfo -> IO BindImagePlaneMemoryInfo
fromCStructBindImagePlaneMemoryInfo c = BindImagePlaneMemoryInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindImagePlaneMemoryInfo)))
                                                                 <*> pure (vkPlaneAspect (c :: VkBindImagePlaneMemoryInfo))
instance Zero BindImagePlaneMemoryInfo where
  zero = BindImagePlaneMemoryInfo Nothing
                                  zero
-- No documentation found for TopLevel "ChromaLocation"
type ChromaLocation = VkChromaLocation
-- No documentation found for TopLevel "ChromaLocationKHR"
type ChromaLocationKHR = ChromaLocation
-- No documentation found for TopLevel "ImagePlaneMemoryRequirementsInfo"
data ImagePlaneMemoryRequirementsInfo = ImagePlaneMemoryRequirementsInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ImagePlaneMemoryRequirementsInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImagePlaneMemoryRequirementsInfo" "planeAspect"
  vkPlaneAspect :: ImageAspectFlagBits
  }
  deriving (Show, Eq)
withCStructImagePlaneMemoryRequirementsInfo :: ImagePlaneMemoryRequirementsInfo -> (VkImagePlaneMemoryRequirementsInfo -> IO a) -> IO a
withCStructImagePlaneMemoryRequirementsInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImagePlaneMemoryRequirementsInfo)) (\pPNext -> cont (VkImagePlaneMemoryRequirementsInfo VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO pPNext (vkPlaneAspect (from :: ImagePlaneMemoryRequirementsInfo))))
fromCStructImagePlaneMemoryRequirementsInfo :: VkImagePlaneMemoryRequirementsInfo -> IO ImagePlaneMemoryRequirementsInfo
fromCStructImagePlaneMemoryRequirementsInfo c = ImagePlaneMemoryRequirementsInfo <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImagePlaneMemoryRequirementsInfo)))
                                                                                 <*> pure (vkPlaneAspect (c :: VkImagePlaneMemoryRequirementsInfo))
instance Zero ImagePlaneMemoryRequirementsInfo where
  zero = ImagePlaneMemoryRequirementsInfo Nothing
                                          zero
-- No documentation found for TopLevel "PhysicalDeviceSamplerYcbcrConversionFeatures"
data PhysicalDeviceSamplerYcbcrConversionFeatures = PhysicalDeviceSamplerYcbcrConversionFeatures
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceSamplerYcbcrConversionFeatures" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSamplerYcbcrConversionFeatures" "samplerYcbcrConversion"
  vkSamplerYcbcrConversion :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceSamplerYcbcrConversionFeatures :: PhysicalDeviceSamplerYcbcrConversionFeatures -> (VkPhysicalDeviceSamplerYcbcrConversionFeatures -> IO a) -> IO a
withCStructPhysicalDeviceSamplerYcbcrConversionFeatures from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceSamplerYcbcrConversionFeatures)) (\pPNext -> cont (VkPhysicalDeviceSamplerYcbcrConversionFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES pPNext (boolToBool32 (vkSamplerYcbcrConversion (from :: PhysicalDeviceSamplerYcbcrConversionFeatures)))))
fromCStructPhysicalDeviceSamplerYcbcrConversionFeatures :: VkPhysicalDeviceSamplerYcbcrConversionFeatures -> IO PhysicalDeviceSamplerYcbcrConversionFeatures
fromCStructPhysicalDeviceSamplerYcbcrConversionFeatures c = PhysicalDeviceSamplerYcbcrConversionFeatures <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSamplerYcbcrConversionFeatures)))
                                                                                                         <*> pure (bool32ToBool (vkSamplerYcbcrConversion (c :: VkPhysicalDeviceSamplerYcbcrConversionFeatures)))
instance Zero PhysicalDeviceSamplerYcbcrConversionFeatures where
  zero = PhysicalDeviceSamplerYcbcrConversionFeatures Nothing
                                                      False
-- No documentation found for TopLevel "SamplerYcbcrConversion"
type SamplerYcbcrConversion = VkSamplerYcbcrConversion
-- No documentation found for TopLevel "SamplerYcbcrConversionCreateInfo"
data SamplerYcbcrConversionCreateInfo = SamplerYcbcrConversionCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "ycbcrModel"
  vkYcbcrModel :: SamplerYcbcrModelConversion
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "ycbcrRange"
  vkYcbcrRange :: SamplerYcbcrRange
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "components"
  vkComponents :: ComponentMapping
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "xChromaOffset"
  vkXChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "yChromaOffset"
  vkYChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "chromaFilter"
  vkChromaFilter :: Filter
  , -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "forceExplicitReconstruction"
  vkForceExplicitReconstruction :: Bool
  }
  deriving (Show, Eq)
withCStructSamplerYcbcrConversionCreateInfo :: SamplerYcbcrConversionCreateInfo -> (VkSamplerYcbcrConversionCreateInfo -> IO a) -> IO a
withCStructSamplerYcbcrConversionCreateInfo from cont = withCStructComponentMapping (vkComponents (from :: SamplerYcbcrConversionCreateInfo)) (\components -> maybeWith withSomeVkStruct (vkPNext (from :: SamplerYcbcrConversionCreateInfo)) (\pPNext -> cont (VkSamplerYcbcrConversionCreateInfo VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO pPNext (vkFormat (from :: SamplerYcbcrConversionCreateInfo)) (vkYcbcrModel (from :: SamplerYcbcrConversionCreateInfo)) (vkYcbcrRange (from :: SamplerYcbcrConversionCreateInfo)) components (vkXChromaOffset (from :: SamplerYcbcrConversionCreateInfo)) (vkYChromaOffset (from :: SamplerYcbcrConversionCreateInfo)) (vkChromaFilter (from :: SamplerYcbcrConversionCreateInfo)) (boolToBool32 (vkForceExplicitReconstruction (from :: SamplerYcbcrConversionCreateInfo))))))
fromCStructSamplerYcbcrConversionCreateInfo :: VkSamplerYcbcrConversionCreateInfo -> IO SamplerYcbcrConversionCreateInfo
fromCStructSamplerYcbcrConversionCreateInfo c = SamplerYcbcrConversionCreateInfo <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSamplerYcbcrConversionCreateInfo)))
                                                                                 <*> pure (vkFormat (c :: VkSamplerYcbcrConversionCreateInfo))
                                                                                 <*> pure (vkYcbcrModel (c :: VkSamplerYcbcrConversionCreateInfo))
                                                                                 <*> pure (vkYcbcrRange (c :: VkSamplerYcbcrConversionCreateInfo))
                                                                                 <*> (fromCStructComponentMapping (vkComponents (c :: VkSamplerYcbcrConversionCreateInfo)))
                                                                                 <*> pure (vkXChromaOffset (c :: VkSamplerYcbcrConversionCreateInfo))
                                                                                 <*> pure (vkYChromaOffset (c :: VkSamplerYcbcrConversionCreateInfo))
                                                                                 <*> pure (vkChromaFilter (c :: VkSamplerYcbcrConversionCreateInfo))
                                                                                 <*> pure (bool32ToBool (vkForceExplicitReconstruction (c :: VkSamplerYcbcrConversionCreateInfo)))
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
-- No documentation found for TopLevel "SamplerYcbcrConversionImageFormatProperties"
data SamplerYcbcrConversionImageFormatProperties = SamplerYcbcrConversionImageFormatProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "SamplerYcbcrConversionImageFormatProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerYcbcrConversionImageFormatProperties" "combinedImageSamplerDescriptorCount"
  vkCombinedImageSamplerDescriptorCount :: Word32
  }
  deriving (Show, Eq)
withCStructSamplerYcbcrConversionImageFormatProperties :: SamplerYcbcrConversionImageFormatProperties -> (VkSamplerYcbcrConversionImageFormatProperties -> IO a) -> IO a
withCStructSamplerYcbcrConversionImageFormatProperties from cont = maybeWith withSomeVkStruct (vkPNext (from :: SamplerYcbcrConversionImageFormatProperties)) (\pPNext -> cont (VkSamplerYcbcrConversionImageFormatProperties VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES pPNext (vkCombinedImageSamplerDescriptorCount (from :: SamplerYcbcrConversionImageFormatProperties))))
fromCStructSamplerYcbcrConversionImageFormatProperties :: VkSamplerYcbcrConversionImageFormatProperties -> IO SamplerYcbcrConversionImageFormatProperties
fromCStructSamplerYcbcrConversionImageFormatProperties c = SamplerYcbcrConversionImageFormatProperties <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSamplerYcbcrConversionImageFormatProperties)))
                                                                                                       <*> pure (vkCombinedImageSamplerDescriptorCount (c :: VkSamplerYcbcrConversionImageFormatProperties))
instance Zero SamplerYcbcrConversionImageFormatProperties where
  zero = SamplerYcbcrConversionImageFormatProperties Nothing
                                                     zero
-- No documentation found for TopLevel "SamplerYcbcrConversionInfo"
data SamplerYcbcrConversionInfo = SamplerYcbcrConversionInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "SamplerYcbcrConversionInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerYcbcrConversionInfo" "conversion"
  vkConversion :: SamplerYcbcrConversion
  }
  deriving (Show, Eq)
withCStructSamplerYcbcrConversionInfo :: SamplerYcbcrConversionInfo -> (VkSamplerYcbcrConversionInfo -> IO a) -> IO a
withCStructSamplerYcbcrConversionInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: SamplerYcbcrConversionInfo)) (\pPNext -> cont (VkSamplerYcbcrConversionInfo VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO pPNext (vkConversion (from :: SamplerYcbcrConversionInfo))))
fromCStructSamplerYcbcrConversionInfo :: VkSamplerYcbcrConversionInfo -> IO SamplerYcbcrConversionInfo
fromCStructSamplerYcbcrConversionInfo c = SamplerYcbcrConversionInfo <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSamplerYcbcrConversionInfo)))
                                                                     <*> pure (vkConversion (c :: VkSamplerYcbcrConversionInfo))
instance Zero SamplerYcbcrConversionInfo where
  zero = SamplerYcbcrConversionInfo Nothing
                                    zero
-- No documentation found for TopLevel "SamplerYcbcrModelConversion"
type SamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion
-- No documentation found for TopLevel "SamplerYcbcrModelConversionKHR"
type SamplerYcbcrModelConversionKHR = SamplerYcbcrModelConversion
-- No documentation found for TopLevel "SamplerYcbcrRange"
type SamplerYcbcrRange = VkSamplerYcbcrRange
-- No documentation found for TopLevel "SamplerYcbcrRangeKHR"
type SamplerYcbcrRangeKHR = SamplerYcbcrRange

-- | Wrapper for 'vkCreateSamplerYcbcrConversion'
createSamplerYcbcrConversion :: Device ->  SamplerYcbcrConversionCreateInfo ->  Maybe AllocationCallbacks ->  IO (SamplerYcbcrConversion)
createSamplerYcbcrConversion = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pYcbcrConversion -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructSamplerYcbcrConversionCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createSamplerYcbcrConversion commandTable device pCreateInfo pAllocator pYcbcrConversion >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pYcbcrConversion)))))

-- | Wrapper for 'vkDestroySamplerYcbcrConversion'
destroySamplerYcbcrConversion :: Device ->  SamplerYcbcrConversion ->  Maybe AllocationCallbacks ->  IO ()
destroySamplerYcbcrConversion = \(Device device commandTable) -> \ycbcrConversion -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroySamplerYcbcrConversion commandTable device ycbcrConversion pAllocator *> (pure ()))
-- | Wrapper for 'createSamplerYcbcrConversion' and 'destroySamplerYcbcrConversion' using 'bracket'
withSamplerYcbcrConversion
  :: Device -> SamplerYcbcrConversionCreateInfo -> Maybe (AllocationCallbacks) -> (SamplerYcbcrConversion -> IO a) -> IO a
withSamplerYcbcrConversion device samplerYcbcrConversionCreateInfo allocationCallbacks = bracket
  (createSamplerYcbcrConversion device samplerYcbcrConversionCreateInfo allocationCallbacks)
  (\o -> destroySamplerYcbcrConversion device o allocationCallbacks)
