{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( withCStructAndroidHardwareBufferFormatPropertiesANDROID
  , fromCStructAndroidHardwareBufferFormatPropertiesANDROID
  , AndroidHardwareBufferFormatPropertiesANDROID(..)
  , withCStructAndroidHardwareBufferPropertiesANDROID
  , fromCStructAndroidHardwareBufferPropertiesANDROID
  , AndroidHardwareBufferPropertiesANDROID(..)
  , withCStructAndroidHardwareBufferUsageANDROID
  , fromCStructAndroidHardwareBufferUsageANDROID
  , AndroidHardwareBufferUsageANDROID(..)
  , withCStructExternalFormatANDROID
  , fromCStructExternalFormatANDROID
  , ExternalFormatANDROID(..)
  , withCStructImportAndroidHardwareBufferInfoANDROID
  , fromCStructImportAndroidHardwareBufferInfoANDROID
  , ImportAndroidHardwareBufferInfoANDROID(..)
  , withCStructMemoryGetAndroidHardwareBufferInfoANDROID
  , fromCStructMemoryGetAndroidHardwareBufferInfoANDROID
  , MemoryGetAndroidHardwareBufferInfoANDROID(..)
  , getAndroidHardwareBufferPropertiesANDROID
  , getMemoryAndroidHardwareBufferANDROID
  , pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  , pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
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
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( VkAndroidHardwareBufferFormatPropertiesANDROID(..)
  , VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkAndroidHardwareBufferUsageANDROID(..)
  , VkExternalFormatANDROID(..)
  , VkImportAndroidHardwareBufferInfoANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
  , AHardwareBuffer
  , vkGetAndroidHardwareBufferPropertiesANDROID
  , vkGetMemoryAndroidHardwareBufferANDROID
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
  , FormatFeatureFlags
  )
import Graphics.Vulkan.Core10.ImageView
  ( ComponentMapping(..)
  , fromCStructComponentMapping
  , withCStructComponentMapping
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( ChromaLocation
  , SamplerYcbcrModelConversion
  , SamplerYcbcrRange
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
  ( pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( pattern EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  )



-- | VkAndroidHardwareBufferFormatPropertiesANDROID - Structure describing
-- the image format properties of an Android hardware buffer
--
-- = Description
--
-- If the Android hardware buffer has one of the formats listed in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-external-android-hardware-buffer-formats Format Equivalence table>,
-- then @format@ /must/ have the equivalent Vulkan format listed in the
-- table. Otherwise, @format@ /may/ be
-- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED', indicating the
-- Android hardware buffer /can/ only be used with an external format.
--
-- The @formatFeatures@ member /must/ include
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
-- and at least one of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT'
-- or
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT',
-- and /should/ include
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
-- and
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT'.
--
-- __Note__
--
-- The @formatFeatures@ member only indicates the features available when
-- using an
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external-format image>
-- created from the Android hardware buffer. Images from Android hardware
-- buffers with a format other than
-- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED' are subject to the
-- format capabilities obtained from
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2',
-- and
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
-- with appropriate parameters. These sets of features are independent of
-- each other, e.g. the external format will support sampler Y’CBCR
-- conversion even if the non-external format does not, and writing to
-- non-external format images is possible but writing to external format
-- images is not.
--
-- Android hardware buffers with the same external format /must/ have the
-- same support for
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT',
-- and
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT'.
-- in @formatFeatures@. Other format features /may/ differ between Android
-- hardware buffers that have the same external format. This allows
-- applications to use the same
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion'
-- object (and samplers and pipelines created from them) for any Android
-- hardware buffers that have the same external format.
--
-- If @format@ is not 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED',
-- then the value of @samplerYcbcrConversionComponents@ /must/ be valid
-- when used as the @components@ member of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
-- with that format. If @format@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED', all members of
-- @samplerYcbcrConversionComponents@ /must/ be
-- 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'.
--
-- Implementations /may/ not always be able to determine the color model,
-- numerical range, or chroma offsets of the image contents, so the values
-- in
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID'
-- are only suggestions. Applications /should/ treat these values as
-- sensible defaults to use in the absence of more reliable information
-- obtained through some other means. If the underlying physical device is
-- also usable via OpenGL ES with the
-- <https://www.khronos.org/registry/OpenGL/extensions/OES/OES_EGL_image_external.txt GL_OES_EGL_image_external>
-- extension, the implementation /should/ suggest values that will produce
-- similar sampled values as would be obtained by sampling the same
-- external image via @samplerExternalOES@ in OpenGL ES using equivalent
-- sampler parameters.
--
-- __Note__
--
-- Since
-- <https://www.khronos.org/registry/OpenGL/extensions/OES/OES_EGL_image_external.txt GL_OES_EGL_image_external>
-- does not require the same sampling and conversion calculations as Vulkan
-- does, achieving identical results between APIs /may/ not be possible on
-- some implementations.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkChromaLocation',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkComponentMapping',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrModelConversion',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrRange',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data AndroidHardwareBufferFormatPropertiesANDROID = AndroidHardwareBufferFormatPropertiesANDROID
  { -- Univalued member elided
  -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "format"
  format :: Format
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "externalFormat"
  externalFormat :: Word64
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "formatFeatures"
  formatFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "samplerYcbcrConversionComponents"
  samplerYcbcrConversionComponents :: ComponentMapping
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrModel"
  suggestedYcbcrModel :: SamplerYcbcrModelConversion
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrRange"
  suggestedYcbcrRange :: SamplerYcbcrRange
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedXChromaOffset"
  suggestedXChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "AndroidHardwareBufferFormatPropertiesANDROID" "suggestedYChromaOffset"
  suggestedYChromaOffset :: ChromaLocation
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAndroidHardwareBufferFormatPropertiesANDROID' and
-- marshal a 'AndroidHardwareBufferFormatPropertiesANDROID' into it. The 'VkAndroidHardwareBufferFormatPropertiesANDROID' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAndroidHardwareBufferFormatPropertiesANDROID :: AndroidHardwareBufferFormatPropertiesANDROID -> (VkAndroidHardwareBufferFormatPropertiesANDROID -> IO a) -> IO a
withCStructAndroidHardwareBufferFormatPropertiesANDROID marshalled cont = withCStructComponentMapping (samplerYcbcrConversionComponents (marshalled :: AndroidHardwareBufferFormatPropertiesANDROID)) (\samplerYcbcrConversionComponents'' -> maybeWith withSomeVkStruct (next (marshalled :: AndroidHardwareBufferFormatPropertiesANDROID)) (\pPNext -> cont (VkAndroidHardwareBufferFormatPropertiesANDROID VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID pPNext (format (marshalled :: AndroidHardwareBufferFormatPropertiesANDROID)) (externalFormat (marshalled :: AndroidHardwareBufferFormatPropertiesANDROID)) (formatFeatures (marshalled :: AndroidHardwareBufferFormatPropertiesANDROID)) samplerYcbcrConversionComponents'' (suggestedYcbcrModel (marshalled :: AndroidHardwareBufferFormatPropertiesANDROID)) (suggestedYcbcrRange (marshalled :: AndroidHardwareBufferFormatPropertiesANDROID)) (suggestedXChromaOffset (marshalled :: AndroidHardwareBufferFormatPropertiesANDROID)) (suggestedYChromaOffset (marshalled :: AndroidHardwareBufferFormatPropertiesANDROID)))))

-- | A function to read a 'VkAndroidHardwareBufferFormatPropertiesANDROID' and all additional
-- structures in the pointer chain into a 'AndroidHardwareBufferFormatPropertiesANDROID'.
fromCStructAndroidHardwareBufferFormatPropertiesANDROID :: VkAndroidHardwareBufferFormatPropertiesANDROID -> IO AndroidHardwareBufferFormatPropertiesANDROID
fromCStructAndroidHardwareBufferFormatPropertiesANDROID c = AndroidHardwareBufferFormatPropertiesANDROID <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAndroidHardwareBufferFormatPropertiesANDROID)))
                                                                                                         <*> pure (vkFormat (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkExternalFormat (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkFormatFeatures (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> (fromCStructComponentMapping (vkSamplerYcbcrConversionComponents (c :: VkAndroidHardwareBufferFormatPropertiesANDROID)))
                                                                                                         <*> pure (vkSuggestedYcbcrModel (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkSuggestedYcbcrRange (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkSuggestedXChromaOffset (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                                                                                                         <*> pure (vkSuggestedYChromaOffset (c :: VkAndroidHardwareBufferFormatPropertiesANDROID))

instance Zero AndroidHardwareBufferFormatPropertiesANDROID where
  zero = AndroidHardwareBufferFormatPropertiesANDROID Nothing
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero



-- | VkAndroidHardwareBufferPropertiesANDROID - Properties of External Memory
-- Android Hardware Buffers
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID'
data AndroidHardwareBufferPropertiesANDROID = AndroidHardwareBufferPropertiesANDROID
  { -- Univalued member elided
  -- No documentation found for Nested "AndroidHardwareBufferPropertiesANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidHardwareBufferPropertiesANDROID" "allocationSize"
  allocationSize :: DeviceSize
  , -- No documentation found for Nested "AndroidHardwareBufferPropertiesANDROID" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAndroidHardwareBufferPropertiesANDROID' and
-- marshal a 'AndroidHardwareBufferPropertiesANDROID' into it. The 'VkAndroidHardwareBufferPropertiesANDROID' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAndroidHardwareBufferPropertiesANDROID :: AndroidHardwareBufferPropertiesANDROID -> (VkAndroidHardwareBufferPropertiesANDROID -> IO a) -> IO a
withCStructAndroidHardwareBufferPropertiesANDROID marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: AndroidHardwareBufferPropertiesANDROID)) (\pPNext -> cont (VkAndroidHardwareBufferPropertiesANDROID VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID pPNext (allocationSize (marshalled :: AndroidHardwareBufferPropertiesANDROID)) (memoryTypeBits (marshalled :: AndroidHardwareBufferPropertiesANDROID))))

-- | A function to read a 'VkAndroidHardwareBufferPropertiesANDROID' and all additional
-- structures in the pointer chain into a 'AndroidHardwareBufferPropertiesANDROID'.
fromCStructAndroidHardwareBufferPropertiesANDROID :: VkAndroidHardwareBufferPropertiesANDROID -> IO AndroidHardwareBufferPropertiesANDROID
fromCStructAndroidHardwareBufferPropertiesANDROID c = AndroidHardwareBufferPropertiesANDROID <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAndroidHardwareBufferPropertiesANDROID)))
                                                                                             <*> pure (vkAllocationSize (c :: VkAndroidHardwareBufferPropertiesANDROID))
                                                                                             <*> pure (vkMemoryTypeBits (c :: VkAndroidHardwareBufferPropertiesANDROID))

instance Zero AndroidHardwareBufferPropertiesANDROID where
  zero = AndroidHardwareBufferPropertiesANDROID Nothing
                                                zero
                                                zero



-- | VkAndroidHardwareBufferUsageANDROID - Struct containing Android hardware
-- buffer usage flags
--
-- = Description
--
-- The @androidHardwareBufferUsage@ field /must/ include Android hardware
-- buffer usage flags listed in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-external-android-hardware-buffer-usage AHardwareBuffer Usage Equivalence>
-- table when the corresponding Vulkan image usage or image creation flags
-- are included in the @usage@ or @flags@ fields of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'.
-- It /must/ include at least one GPU usage flag
-- (@AHARDWAREBUFFER_USAGE_GPU_@*), even if none of the corresponding
-- Vulkan usages or flags are requested.
--
-- __Note__
--
-- Requiring at least one GPU usage flag ensures that Android hardware
-- buffer memory will be allocated in a memory pool accessible to the
-- Vulkan implementation, and that specializing the memory layout based on
-- usage flags does not prevent it from being compatible with Vulkan.
-- Implementations /may/ avoid unnecessary restrictions caused by this
-- requirement by using vendor usage flags to indicate that only the Vulkan
-- uses indicated in
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2'
-- are required.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data AndroidHardwareBufferUsageANDROID = AndroidHardwareBufferUsageANDROID
  { -- Univalued member elided
  -- No documentation found for Nested "AndroidHardwareBufferUsageANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidHardwareBufferUsageANDROID" "androidHardwareBufferUsage"
  androidHardwareBufferUsage :: Word64
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAndroidHardwareBufferUsageANDROID' and
-- marshal a 'AndroidHardwareBufferUsageANDROID' into it. The 'VkAndroidHardwareBufferUsageANDROID' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAndroidHardwareBufferUsageANDROID :: AndroidHardwareBufferUsageANDROID -> (VkAndroidHardwareBufferUsageANDROID -> IO a) -> IO a
withCStructAndroidHardwareBufferUsageANDROID marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: AndroidHardwareBufferUsageANDROID)) (\pPNext -> cont (VkAndroidHardwareBufferUsageANDROID VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID pPNext (androidHardwareBufferUsage (marshalled :: AndroidHardwareBufferUsageANDROID))))

-- | A function to read a 'VkAndroidHardwareBufferUsageANDROID' and all additional
-- structures in the pointer chain into a 'AndroidHardwareBufferUsageANDROID'.
fromCStructAndroidHardwareBufferUsageANDROID :: VkAndroidHardwareBufferUsageANDROID -> IO AndroidHardwareBufferUsageANDROID
fromCStructAndroidHardwareBufferUsageANDROID c = AndroidHardwareBufferUsageANDROID <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAndroidHardwareBufferUsageANDROID)))
                                                                                   <*> pure (vkAndroidHardwareBufferUsage (c :: VkAndroidHardwareBufferUsageANDROID))

instance Zero AndroidHardwareBufferUsageANDROID where
  zero = AndroidHardwareBufferUsageANDROID Nothing
                                           zero



-- | VkExternalFormatANDROID - Structure containing an Android hardware
-- buffer external format
--
-- = Description
--
-- If @externalFormat@ is zero, the effect is as if the
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
-- structure was not present. Otherwise, the @image@ will have the
-- specified external format.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExternalFormatANDROID = ExternalFormatANDROID
  { -- Univalued member elided
  -- No documentation found for Nested "ExternalFormatANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalFormatANDROID" "externalFormat"
  externalFormat :: Word64
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExternalFormatANDROID' and
-- marshal a 'ExternalFormatANDROID' into it. The 'VkExternalFormatANDROID' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExternalFormatANDROID :: ExternalFormatANDROID -> (VkExternalFormatANDROID -> IO a) -> IO a
withCStructExternalFormatANDROID marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExternalFormatANDROID)) (\pPNext -> cont (VkExternalFormatANDROID VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID pPNext (externalFormat (marshalled :: ExternalFormatANDROID))))

-- | A function to read a 'VkExternalFormatANDROID' and all additional
-- structures in the pointer chain into a 'ExternalFormatANDROID'.
fromCStructExternalFormatANDROID :: VkExternalFormatANDROID -> IO ExternalFormatANDROID
fromCStructExternalFormatANDROID c = ExternalFormatANDROID <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalFormatANDROID)))
                                                           <*> pure (vkExternalFormat (c :: VkExternalFormatANDROID))

instance Zero ExternalFormatANDROID where
  zero = ExternalFormatANDROID Nothing
                               zero



-- | VkImportAndroidHardwareBufferInfoANDROID - Import memory from an Android
-- hardware buffer
--
-- = Description
--
-- If the 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory' command
-- succeeds, the implementation /must/ acquire a reference to the imported
-- hardware buffer, which it /must/ release when the device memory object
-- is freed. If the command fails, the implementation /must/ not retain a
-- reference.
--
-- == Valid Usage
--
-- -   If @buffer@ is not @NULL@, Android hardware buffers /must/ be
--     supported for import, as reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'.
--
-- -   If @buffer@ is not @NULL@, it /must/ be a valid Android hardware
--     buffer object with @AHardwareBuffer_Desc@::@format@ and
--     @AHardwareBuffer_Desc@::@usage@ compatible with Vulkan as described
--     in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-external-android-hardware-buffer Android Hardware Buffers>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID'
--
-- -   @buffer@ /must/ be a valid pointer to an
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AHardwareBuffer'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ImportAndroidHardwareBufferInfoANDROID = ImportAndroidHardwareBufferInfoANDROID
  { -- Univalued member elided
  -- No documentation found for Nested "ImportAndroidHardwareBufferInfoANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportAndroidHardwareBufferInfoANDROID" "buffer"
  buffer :: Ptr AHardwareBuffer
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImportAndroidHardwareBufferInfoANDROID' and
-- marshal a 'ImportAndroidHardwareBufferInfoANDROID' into it. The 'VkImportAndroidHardwareBufferInfoANDROID' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImportAndroidHardwareBufferInfoANDROID :: ImportAndroidHardwareBufferInfoANDROID -> (VkImportAndroidHardwareBufferInfoANDROID -> IO a) -> IO a
withCStructImportAndroidHardwareBufferInfoANDROID marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImportAndroidHardwareBufferInfoANDROID)) (\pPNext -> cont (VkImportAndroidHardwareBufferInfoANDROID VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID pPNext (buffer (marshalled :: ImportAndroidHardwareBufferInfoANDROID))))

-- | A function to read a 'VkImportAndroidHardwareBufferInfoANDROID' and all additional
-- structures in the pointer chain into a 'ImportAndroidHardwareBufferInfoANDROID'.
fromCStructImportAndroidHardwareBufferInfoANDROID :: VkImportAndroidHardwareBufferInfoANDROID -> IO ImportAndroidHardwareBufferInfoANDROID
fromCStructImportAndroidHardwareBufferInfoANDROID c = ImportAndroidHardwareBufferInfoANDROID <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportAndroidHardwareBufferInfoANDROID)))
                                                                                             <*> pure (vkBuffer (c :: VkImportAndroidHardwareBufferInfoANDROID))

instance Zero ImportAndroidHardwareBufferInfoANDROID where
  zero = ImportAndroidHardwareBufferInfoANDROID Nothing
                                                zero



-- | VkMemoryGetAndroidHardwareBufferInfoANDROID - Structure describing an
-- Android hardware buffer memory export operation
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     /must/ have been included in
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory.VkExportMemoryAllocateInfoKHR'::@handleTypes@
--     when @memory@ was created.
--
-- -   If the @pNext@ chain of the
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' used to
--     allocate @memory@ included a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with non-@NULL@ @image@ member, then that @image@ /must/ already be
--     bound to @memory@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetMemoryAndroidHardwareBufferANDROID'
data MemoryGetAndroidHardwareBufferInfoANDROID = MemoryGetAndroidHardwareBufferInfoANDROID
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryGetAndroidHardwareBufferInfoANDROID" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryGetAndroidHardwareBufferInfoANDROID" "memory"
  memory :: DeviceMemory
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryGetAndroidHardwareBufferInfoANDROID' and
-- marshal a 'MemoryGetAndroidHardwareBufferInfoANDROID' into it. The 'VkMemoryGetAndroidHardwareBufferInfoANDROID' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryGetAndroidHardwareBufferInfoANDROID :: MemoryGetAndroidHardwareBufferInfoANDROID -> (VkMemoryGetAndroidHardwareBufferInfoANDROID -> IO a) -> IO a
withCStructMemoryGetAndroidHardwareBufferInfoANDROID marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryGetAndroidHardwareBufferInfoANDROID)) (\pPNext -> cont (VkMemoryGetAndroidHardwareBufferInfoANDROID VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID pPNext (memory (marshalled :: MemoryGetAndroidHardwareBufferInfoANDROID))))

-- | A function to read a 'VkMemoryGetAndroidHardwareBufferInfoANDROID' and all additional
-- structures in the pointer chain into a 'MemoryGetAndroidHardwareBufferInfoANDROID'.
fromCStructMemoryGetAndroidHardwareBufferInfoANDROID :: VkMemoryGetAndroidHardwareBufferInfoANDROID -> IO MemoryGetAndroidHardwareBufferInfoANDROID
fromCStructMemoryGetAndroidHardwareBufferInfoANDROID c = MemoryGetAndroidHardwareBufferInfoANDROID <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryGetAndroidHardwareBufferInfoANDROID)))
                                                                                                   <*> pure (vkMemory (c :: VkMemoryGetAndroidHardwareBufferInfoANDROID))

instance Zero MemoryGetAndroidHardwareBufferInfoANDROID where
  zero = MemoryGetAndroidHardwareBufferInfoANDROID Nothing
                                                   zero



-- | vkGetAndroidHardwareBufferPropertiesANDROID - Get Properties of External
-- Memory Android Hardware Buffers
--
-- = Parameters
--
-- -   @device@ is the logical device that will be importing @buffer@.
--
-- -   @buffer@ is the Android hardware buffer which will be imported.
--
-- -   @pProperties@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferPropertiesANDROID'
--     structure in which the properties of @buffer@ are returned.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory.VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
getAndroidHardwareBufferPropertiesANDROID :: Device ->  Ptr AHardwareBuffer ->  IO (AndroidHardwareBufferPropertiesANDROID)
getAndroidHardwareBufferPropertiesANDROID = \(Device device' commandTable) -> \buffer' -> alloca (\pProperties' -> vkGetAndroidHardwareBufferPropertiesANDROID commandTable device' buffer' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructAndroidHardwareBufferPropertiesANDROID <=< peek) pProperties')))


-- | vkGetMemoryAndroidHardwareBufferANDROID - Get an Android hardware buffer
-- for a memory object
--
-- = Parameters
--
-- -   @device@ is the logical device that created the device memory being
--     exported.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkMemoryGetAndroidHardwareBufferInfoANDROID'
--     structure containing parameters of the export operation.
--
-- -   @pBuffer@ will return an Android hardware buffer representing the
--     underlying resources of the device memory object.
--
-- = Description
--
-- Each call to
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetMemoryAndroidHardwareBufferANDROID'
-- /must/ return an Android hardware buffer with a new reference acquired
-- in addition to the reference held by the
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory'. To avoid leaking
-- resources, the application /must/ release the reference by calling
-- @AHardwareBuffer_release@ when it is no longer needed. When called with
-- the same handle in
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkMemoryGetAndroidHardwareBufferInfoANDROID'::@memory@,
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetMemoryAndroidHardwareBufferANDROID'
-- /must/ return the same Android hardware buffer object. If the device
-- memory was created by importing an Android hardware buffer,
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetMemoryAndroidHardwareBufferANDROID'
-- /must/ return that same Android hardware buffer object.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_TOO_MANY_OBJECTS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkMemoryGetAndroidHardwareBufferInfoANDROID'
getMemoryAndroidHardwareBufferANDROID :: Device ->  MemoryGetAndroidHardwareBufferInfoANDROID ->  IO (Ptr AHardwareBuffer)
getMemoryAndroidHardwareBufferANDROID = \(Device device' commandTable) -> \info' -> alloca (\pBuffer' -> (\marshalled -> withCStructMemoryGetAndroidHardwareBufferInfoANDROID marshalled . flip with) info' (\pInfo' -> vkGetMemoryAndroidHardwareBufferANDROID commandTable device' pInfo' pBuffer' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pBuffer'))))

-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME"
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME = VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION"
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION :: Integral a => a
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION = VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
