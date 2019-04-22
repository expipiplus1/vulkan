{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( withCStructBindImagePlaneMemoryInfo
  , fromCStructBindImagePlaneMemoryInfo
  , BindImagePlaneMemoryInfo(..)
  , ChromaLocation
  , pattern CHROMA_LOCATION_COSITED_EVEN
  , pattern CHROMA_LOCATION_MIDPOINT
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



-- | VkBindImagePlaneMemoryInfo - Structure specifying how to bind an image
-- plane to memory
--
-- == Valid Usage
--
-- -   If the image’s tiling is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL',
--     then @planeAspect@ /must/ be a single valid /format plane/ for the
--     image. (That is, @planeAspect@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_1_BIT'
--     for “@_2PLANE@” formats and @planeAspect@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_2_BIT'
--     for “@_3PLANE@” formats.)
--
-- -   A single call to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'
--     /must/ bind all or none of the planes of an image (i.e. bindings to
--     all planes of an image /must/ be made in a single
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'
--     call), as separate bindings
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO'
--
-- -   @planeAspect@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data BindImagePlaneMemoryInfo = BindImagePlaneMemoryInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BindImagePlaneMemoryInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImagePlaneMemoryInfo" "planeAspect"
  planeAspect :: ImageAspectFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindImagePlaneMemoryInfo' and
-- marshal a 'BindImagePlaneMemoryInfo' into it. The 'VkBindImagePlaneMemoryInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindImagePlaneMemoryInfo :: BindImagePlaneMemoryInfo -> (VkBindImagePlaneMemoryInfo -> IO a) -> IO a
withCStructBindImagePlaneMemoryInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BindImagePlaneMemoryInfo)) (\pPNext -> cont (VkBindImagePlaneMemoryInfo VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO pPNext (planeAspect (marshalled :: BindImagePlaneMemoryInfo))))

-- | A function to read a 'VkBindImagePlaneMemoryInfo' and all additional
-- structures in the pointer chain into a 'BindImagePlaneMemoryInfo'.
fromCStructBindImagePlaneMemoryInfo :: VkBindImagePlaneMemoryInfo -> IO BindImagePlaneMemoryInfo
fromCStructBindImagePlaneMemoryInfo c = BindImagePlaneMemoryInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindImagePlaneMemoryInfo)))
                                                                 <*> pure (vkPlaneAspect (c :: VkBindImagePlaneMemoryInfo))

instance Zero BindImagePlaneMemoryInfo where
  zero = BindImagePlaneMemoryInfo Nothing
                                  zero


-- | VkChromaLocation - Position of downsampled chroma samples
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
type ChromaLocation = VkChromaLocation


{-# complete CHROMA_LOCATION_COSITED_EVEN, CHROMA_LOCATION_MIDPOINT :: ChromaLocation #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_CHROMA_LOCATION_COSITED_EVEN'
-- specifies that downsampled chroma samples are aligned with luma samples
-- with even coordinates.
pattern CHROMA_LOCATION_COSITED_EVEN :: (a ~ ChromaLocation) => a
pattern CHROMA_LOCATION_COSITED_EVEN = VK_CHROMA_LOCATION_COSITED_EVEN


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_CHROMA_LOCATION_MIDPOINT'
-- specifies that downsampled chroma samples are located half way between
-- each even luma sample and the nearest higher odd luma sample.
pattern CHROMA_LOCATION_MIDPOINT :: (a ~ ChromaLocation) => a
pattern CHROMA_LOCATION_MIDPOINT = VK_CHROMA_LOCATION_MIDPOINT

-- No documentation found for TopLevel "ChromaLocationKHR"
type ChromaLocationKHR = ChromaLocation


-- | VkImagePlaneMemoryRequirementsInfo - Structure specifying image plane
-- for memory requirements
--
-- == Valid Usage
--
-- -   If the image’s tiling is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL',
--     then @planeAspect@ /must/ be a single valid /format plane/ for the
--     image. (That is, for a two-plane image @planeAspect@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_1_BIT',
--     and for a three-plane image @planeAspect@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_1_BIT'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_2_BIT').
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO'
--
-- -   @planeAspect@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ImagePlaneMemoryRequirementsInfo = ImagePlaneMemoryRequirementsInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ImagePlaneMemoryRequirementsInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImagePlaneMemoryRequirementsInfo" "planeAspect"
  planeAspect :: ImageAspectFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImagePlaneMemoryRequirementsInfo' and
-- marshal a 'ImagePlaneMemoryRequirementsInfo' into it. The 'VkImagePlaneMemoryRequirementsInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImagePlaneMemoryRequirementsInfo :: ImagePlaneMemoryRequirementsInfo -> (VkImagePlaneMemoryRequirementsInfo -> IO a) -> IO a
withCStructImagePlaneMemoryRequirementsInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImagePlaneMemoryRequirementsInfo)) (\pPNext -> cont (VkImagePlaneMemoryRequirementsInfo VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO pPNext (planeAspect (marshalled :: ImagePlaneMemoryRequirementsInfo))))

-- | A function to read a 'VkImagePlaneMemoryRequirementsInfo' and all additional
-- structures in the pointer chain into a 'ImagePlaneMemoryRequirementsInfo'.
fromCStructImagePlaneMemoryRequirementsInfo :: VkImagePlaneMemoryRequirementsInfo -> IO ImagePlaneMemoryRequirementsInfo
fromCStructImagePlaneMemoryRequirementsInfo c = ImagePlaneMemoryRequirementsInfo <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImagePlaneMemoryRequirementsInfo)))
                                                                                 <*> pure (vkPlaneAspect (c :: VkImagePlaneMemoryRequirementsInfo))

instance Zero ImagePlaneMemoryRequirementsInfo where
  zero = ImagePlaneMemoryRequirementsInfo Nothing
                                          zero



-- | VkPhysicalDeviceSamplerYcbcrConversionFeatures - Structure describing
-- Y’CbCr conversion features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkPhysicalDeviceSamplerYcbcrConversionFeatures'
-- structure describe the following feature:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceSamplerYcbcrConversionFeatures = PhysicalDeviceSamplerYcbcrConversionFeatures
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceSamplerYcbcrConversionFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSamplerYcbcrConversionFeatures" "samplerYcbcrConversion"
  samplerYcbcrConversion :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceSamplerYcbcrConversionFeatures' and
-- marshal a 'PhysicalDeviceSamplerYcbcrConversionFeatures' into it. The 'VkPhysicalDeviceSamplerYcbcrConversionFeatures' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceSamplerYcbcrConversionFeatures :: PhysicalDeviceSamplerYcbcrConversionFeatures -> (VkPhysicalDeviceSamplerYcbcrConversionFeatures -> IO a) -> IO a
withCStructPhysicalDeviceSamplerYcbcrConversionFeatures marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceSamplerYcbcrConversionFeatures)) (\pPNext -> cont (VkPhysicalDeviceSamplerYcbcrConversionFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES pPNext (boolToBool32 (samplerYcbcrConversion (marshalled :: PhysicalDeviceSamplerYcbcrConversionFeatures)))))

-- | A function to read a 'VkPhysicalDeviceSamplerYcbcrConversionFeatures' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceSamplerYcbcrConversionFeatures'.
fromCStructPhysicalDeviceSamplerYcbcrConversionFeatures :: VkPhysicalDeviceSamplerYcbcrConversionFeatures -> IO PhysicalDeviceSamplerYcbcrConversionFeatures
fromCStructPhysicalDeviceSamplerYcbcrConversionFeatures c = PhysicalDeviceSamplerYcbcrConversionFeatures <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSamplerYcbcrConversionFeatures)))
                                                                                                         <*> pure (bool32ToBool (vkSamplerYcbcrConversion (c :: VkPhysicalDeviceSamplerYcbcrConversionFeatures)))

instance Zero PhysicalDeviceSamplerYcbcrConversionFeatures where
  zero = PhysicalDeviceSamplerYcbcrConversionFeatures Nothing
                                                      False


-- | VkSamplerYcbcrConversion - Opaque handle to a device-specific sampler
-- Y’CBCR conversion description
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversionKHR'
type SamplerYcbcrConversion = VkSamplerYcbcrConversion


-- | VkSamplerYcbcrConversionCreateInfo - Structure specifying the parameters
-- of the newly created conversion
--
-- = Description
--
-- __Note__
--
-- Setting @forceExplicitReconstruction@ to
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' /may/ have a performance penalty
-- on implementations where explicit reconstruction is not the default mode
-- of operation.
--
-- Sampler Y’CBCR conversion objects do not support /external format
-- conversion/ without additional extensions defining /external formats/.
--
-- == Valid Usage
--
-- -   @format@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED'
--
-- -   @format@ /must/ support
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT'
--
-- -   If the format does not support
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT',
--     @xChromaOffset@ and @yChromaOffset@ /must/ not be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_CHROMA_LOCATION_COSITED_EVEN'
--
-- -   If the format does not support
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT',
--     @xChromaOffset@ and @yChromaOffset@ /must/ not be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_CHROMA_LOCATION_MIDPOINT'
--
-- -   @format@ /must/ represent unsigned normalized values (i.e. the
--     format must be a @UNORM@ format)
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.g@
--     /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.a@
--     /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ONE', or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ZERO'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.r@
--     /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--     or 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_B'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.b@
--     /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--     or 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_R'
--
-- -   If the format has a @_422@ or @_420@ suffix, and if either
--     @components.r@ or @components.b@ is
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY',
--     both values /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--
-- -   If @ycbcrModel@ is not
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY',
--     then @components.r@, @components.g@, and @components.b@ /must/
--     correspond to channels of the @format@; that is, @components.r@,
--     @components.g@, and @components.b@ /must/ not be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ZERO' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ONE', and
--     /must/ not correspond to a channel which contains zero or one as a
--     consequence of
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-conversion-to-rgba conversion to RGBA>
--
-- -   If the format does not support
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT',
--     @forceExplicitReconstruction@ /must/ be FALSE
--
-- -   If the format does not support
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT',
--     @chromaFilter@ /must/ be
--     'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_NEAREST'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Core.VkFormat'
--     value
--
-- -   @ycbcrModel@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrModelConversion'
--     value
--
-- -   @ycbcrRange@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrRange'
--     value
--
-- -   @components@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.ImageView.VkComponentMapping' structure
--
-- -   @xChromaOffset@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkChromaLocation'
--     value
--
-- -   @yChromaOffset@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkChromaLocation'
--     value
--
-- -   @chromaFilter@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Sampler.VkFilter' value
--
-- If @chromaFilter@ is
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_NEAREST', chroma samples are
-- reconstructed to luma channel resolution using nearest-neighbour
-- sampling. Otherwise, chroma samples are reconstructed using
-- interpolation. More details can be found in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion the description of sampler Y’CBCR conversion>
-- in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures Image Operations>
-- chapter.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkChromaLocation',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkComponentMapping',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkFilter',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrModelConversion',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrRange',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR'
data SamplerYcbcrConversionCreateInfo = SamplerYcbcrConversionCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "SamplerYcbcrConversionCreateInfo" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkSamplerYcbcrConversionCreateInfo' and
-- marshal a 'SamplerYcbcrConversionCreateInfo' into it. The 'VkSamplerYcbcrConversionCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSamplerYcbcrConversionCreateInfo :: SamplerYcbcrConversionCreateInfo -> (VkSamplerYcbcrConversionCreateInfo -> IO a) -> IO a
withCStructSamplerYcbcrConversionCreateInfo marshalled cont = withCStructComponentMapping (components (marshalled :: SamplerYcbcrConversionCreateInfo)) (\components'' -> maybeWith withSomeVkStruct (next (marshalled :: SamplerYcbcrConversionCreateInfo)) (\pPNext -> cont (VkSamplerYcbcrConversionCreateInfo VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO pPNext (format (marshalled :: SamplerYcbcrConversionCreateInfo)) (ycbcrModel (marshalled :: SamplerYcbcrConversionCreateInfo)) (ycbcrRange (marshalled :: SamplerYcbcrConversionCreateInfo)) components'' (xChromaOffset (marshalled :: SamplerYcbcrConversionCreateInfo)) (yChromaOffset (marshalled :: SamplerYcbcrConversionCreateInfo)) (chromaFilter (marshalled :: SamplerYcbcrConversionCreateInfo)) (boolToBool32 (forceExplicitReconstruction (marshalled :: SamplerYcbcrConversionCreateInfo))))))

-- | A function to read a 'VkSamplerYcbcrConversionCreateInfo' and all additional
-- structures in the pointer chain into a 'SamplerYcbcrConversionCreateInfo'.
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



-- | VkSamplerYcbcrConversionImageFormatProperties - Structure specifying
-- combined image sampler descriptor count for multi-planar images
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data SamplerYcbcrConversionImageFormatProperties = SamplerYcbcrConversionImageFormatProperties
  { -- Univalued member elided
  -- No documentation found for Nested "SamplerYcbcrConversionImageFormatProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerYcbcrConversionImageFormatProperties" "combinedImageSamplerDescriptorCount"
  combinedImageSamplerDescriptorCount :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSamplerYcbcrConversionImageFormatProperties' and
-- marshal a 'SamplerYcbcrConversionImageFormatProperties' into it. The 'VkSamplerYcbcrConversionImageFormatProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSamplerYcbcrConversionImageFormatProperties :: SamplerYcbcrConversionImageFormatProperties -> (VkSamplerYcbcrConversionImageFormatProperties -> IO a) -> IO a
withCStructSamplerYcbcrConversionImageFormatProperties marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SamplerYcbcrConversionImageFormatProperties)) (\pPNext -> cont (VkSamplerYcbcrConversionImageFormatProperties VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES pPNext (combinedImageSamplerDescriptorCount (marshalled :: SamplerYcbcrConversionImageFormatProperties))))

-- | A function to read a 'VkSamplerYcbcrConversionImageFormatProperties' and all additional
-- structures in the pointer chain into a 'SamplerYcbcrConversionImageFormatProperties'.
fromCStructSamplerYcbcrConversionImageFormatProperties :: VkSamplerYcbcrConversionImageFormatProperties -> IO SamplerYcbcrConversionImageFormatProperties
fromCStructSamplerYcbcrConversionImageFormatProperties c = SamplerYcbcrConversionImageFormatProperties <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSamplerYcbcrConversionImageFormatProperties)))
                                                                                                       <*> pure (vkCombinedImageSamplerDescriptorCount (c :: VkSamplerYcbcrConversionImageFormatProperties))

instance Zero SamplerYcbcrConversionImageFormatProperties where
  zero = SamplerYcbcrConversionImageFormatProperties Nothing
                                                     zero



-- | VkSamplerYcbcrConversionInfo - Structure specifying Y’CbCr conversion to
-- a sampler or image view
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data SamplerYcbcrConversionInfo = SamplerYcbcrConversionInfo
  { -- Univalued member elided
  -- No documentation found for Nested "SamplerYcbcrConversionInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerYcbcrConversionInfo" "conversion"
  conversion :: SamplerYcbcrConversion
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSamplerYcbcrConversionInfo' and
-- marshal a 'SamplerYcbcrConversionInfo' into it. The 'VkSamplerYcbcrConversionInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSamplerYcbcrConversionInfo :: SamplerYcbcrConversionInfo -> (VkSamplerYcbcrConversionInfo -> IO a) -> IO a
withCStructSamplerYcbcrConversionInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SamplerYcbcrConversionInfo)) (\pPNext -> cont (VkSamplerYcbcrConversionInfo VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO pPNext (conversion (marshalled :: SamplerYcbcrConversionInfo))))

-- | A function to read a 'VkSamplerYcbcrConversionInfo' and all additional
-- structures in the pointer chain into a 'SamplerYcbcrConversionInfo'.
fromCStructSamplerYcbcrConversionInfo :: VkSamplerYcbcrConversionInfo -> IO SamplerYcbcrConversionInfo
fromCStructSamplerYcbcrConversionInfo c = SamplerYcbcrConversionInfo <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSamplerYcbcrConversionInfo)))
                                                                     <*> pure (vkConversion (c :: VkSamplerYcbcrConversionInfo))

instance Zero SamplerYcbcrConversionInfo where
  zero = SamplerYcbcrConversionInfo Nothing
                                    zero


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
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
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
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
type SamplerYcbcrRange = VkSamplerYcbcrRange


{-# complete SAMPLER_YCBCR_RANGE_ITU_FULL, SAMPLER_YCBCR_RANGE_ITU_NARROW :: SamplerYcbcrRange #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_SAMPLER_YCBCR_RANGE_ITU_FULL'
-- specifies that the full range of the encoded values are valid and
-- interpreted according to the ITU “full range” quantization rules.
pattern SAMPLER_YCBCR_RANGE_ITU_FULL :: (a ~ SamplerYcbcrRange) => a
pattern SAMPLER_YCBCR_RANGE_ITU_FULL = VK_SAMPLER_YCBCR_RANGE_ITU_FULL


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_SAMPLER_YCBCR_RANGE_ITU_NARROW'
-- specifies that headroom and foot room are reserved in the numerical
-- range of encoded values, and the remaining values are expanded according
-- to the ITU “narrow range” quantization rules.
pattern SAMPLER_YCBCR_RANGE_ITU_NARROW :: (a ~ SamplerYcbcrRange) => a
pattern SAMPLER_YCBCR_RANGE_ITU_NARROW = VK_SAMPLER_YCBCR_RANGE_ITU_NARROW

-- No documentation found for TopLevel "SamplerYcbcrRangeKHR"
type SamplerYcbcrRangeKHR = SamplerYcbcrRange


-- | vkCreateSamplerYcbcrConversion - Create a new Ycbcr conversion
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the sampler Y’CBCR
--     conversion.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
--     specifying the requested sampler Y’CBCR conversion.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pYcbcrConversion@ points to a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion'
--     handle in which the resulting sampler Y’CBCR conversion is returned.
--
-- = Description
--
-- The interpretation of the configured sampler Y’CBCR conversion is
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion the description of sampler Y’CBCR conversion>
-- in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures Image Operations>
-- chapter.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sampler-YCbCr-conversion sampler Y’CBCR conversion feature>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
--     structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pYcbcrConversion@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion'
--     handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
createSamplerYcbcrConversion :: Device ->  SamplerYcbcrConversionCreateInfo ->  Maybe AllocationCallbacks ->  IO (SamplerYcbcrConversion)
createSamplerYcbcrConversion = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pYcbcrConversion' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructSamplerYcbcrConversionCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateSamplerYcbcrConversion commandTable device' pCreateInfo' pAllocator pYcbcrConversion' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pYcbcrConversion')))))


-- | vkDestroySamplerYcbcrConversion - Destroy a created Y’CbCr conversion
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the Y’CBCR conversion.
--
-- -   @ycbcrConversion@ is the conversion to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @ycbcrConversion@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @ycbcrConversion@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion'
destroySamplerYcbcrConversion :: Device ->  SamplerYcbcrConversion ->  Maybe AllocationCallbacks ->  IO ()
destroySamplerYcbcrConversion = \(Device device' commandTable) -> \ycbcrConversion' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroySamplerYcbcrConversion commandTable device' ycbcrConversion' pAllocator *> (pure ()))

-- | A safe wrapper for 'createSamplerYcbcrConversion' and 'destroySamplerYcbcrConversion' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withSamplerYcbcrConversion
  :: Device -> SamplerYcbcrConversionCreateInfo -> Maybe (AllocationCallbacks) -> (SamplerYcbcrConversion -> IO a) -> IO a
withSamplerYcbcrConversion device samplerYcbcrConversionCreateInfo allocationCallbacks = bracket
  (createSamplerYcbcrConversion device samplerYcbcrConversionCreateInfo allocationCallbacks)
  (\o -> destroySamplerYcbcrConversion device o allocationCallbacks)
