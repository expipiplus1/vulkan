{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AHardwareBuffer
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
  , pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  , vkGetAndroidHardwareBufferPropertiesANDROID
  , vkGetMemoryAndroidHardwareBufferANDROID
  , VkImportAndroidHardwareBufferInfoANDROID(..)
  , VkAndroidHardwareBufferUsageANDROID(..)
  , VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
  , VkAndroidHardwareBufferFormatPropertiesANDROID(..)
  , VkExternalFormatANDROID(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  , VkDeviceSize
  , VkFormatFeatureFlags
  )
import Graphics.Vulkan.Core10.ImageView
  ( VkComponentMapping(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkChromaLocation(..)
  , VkSamplerYcbcrModelConversion(..)
  , VkSamplerYcbcrRange(..)
  )


-- | Opaque data
data AHardwareBuffer
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID"
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID = VkStructureType 1000129000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID"
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID = VkStructureType 1000129001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID"
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID = VkStructureType 1000129002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID = VkStructureType 1000129003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID = VkStructureType 1000129004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID"
pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID = VkStructureType 1000129005
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID = VkExternalMemoryHandleTypeFlagBits 0x00000400
-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION"
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION :: Integral a => a
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION = 3
-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME"
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME = "VK_ANDROID_external_memory_android_hardware_buffer"
-- | vkGetAndroidHardwareBufferPropertiesANDROID - Get Properties of External
-- Memory Android Hardware Buffers
--
-- = Parameters
--
-- -   @device@ is the logical device that will be importing @buffer@.
--
-- -   @buffer@ is the Android hardware buffer which will be imported.
--
-- -   @pProperties@ will return properties of @buffer@.
--
-- == Valid Usage
--
-- -   @buffer@ /must/ be a valid Android hardware buffer object with at
--     least one of the @AHARDWAREBUFFER_USAGE_GPU_@* usage flags.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @buffer@ /must/ be a valid pointer to a valid @AHardwareBuffer@
--     value
--
-- -   @pProperties@ /must/ be a valid pointer to a
--     @VkAndroidHardwareBufferPropertiesANDROID@ structure
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR@
--
-- = See Also
--
-- 'VkAndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetAndroidHardwareBufferPropertiesANDROID" vkGetAndroidHardwareBufferPropertiesANDROID :: ("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult
-- | vkGetMemoryAndroidHardwareBufferANDROID - Get an Android hardware buffer
-- for a memory object
--
-- = Parameters
--
-- -   @device@ is the logical device that created the device memory being
--     exported.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'VkMemoryGetAndroidHardwareBufferInfoANDROID' structure containing
--     parameters of the export operation.
--
-- -   @pBuffer@ will return an Android hardware buffer representing the
--     underlying resources of the device memory object.
--
-- = Description
--
-- Each call to @vkGetMemoryAndroidHardwareBufferANDROID@ /must/ return an
-- Android hardware buffer with a new reference acquired in addition to the
-- reference held by the @VkDeviceMemory@. To avoid leaking resources, the
-- application /must/ release the reference by calling
-- AHardwareBuffer_release when it is no longer needed. When called with
-- the same handle in
-- @VkMemoryGetAndroidHardwareBufferInfoANDROID@::@memory@,
-- @vkGetMemoryAndroidHardwareBufferANDROID@ /must/ return the same Android
-- hardware buffer object. If the device memory was created by importing an
-- Android hardware buffer, @vkGetMemoryAndroidHardwareBufferANDROID@
-- /must/ return that same Android hardware buffer object.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     @VkMemoryGetAndroidHardwareBufferInfoANDROID@ structure
--
-- -   @pBuffer@ /must/ be a valid pointer to a valid pointer to a
--     @AHardwareBuffer@ value
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_TOO_MANY_OBJECTS@
--
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkMemoryGetAndroidHardwareBufferInfoANDROID'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryAndroidHardwareBufferANDROID" vkGetMemoryAndroidHardwareBufferANDROID :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult
-- | VkImportAndroidHardwareBufferInfoANDROID - Import memory from an Android
-- hardware buffer
--
-- = Description
--
-- If the @vkAllocateMemory@ command succeeds, the implementation /must/
-- acquire a reference to the imported hardware buffer, which it /must/
-- release when the device memory object is freed. If the command fails,
-- the implementation /must/ not retain a reference.
--
-- == Valid Usage
--
-- -   If @buffer@ is not @NULL@, Android hardware buffers /must/ be
--     supported for import, as reported by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'.
--
-- -   If @buffer@ is not @NULL@, it /must/ be a valid Android hardware
--     buffer object with format and usage compatible with Vulkan as
--     described by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID@
--
-- -   @buffer@ /must/ be a valid pointer to a @AHardwareBuffer@ value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkImportAndroidHardwareBufferInfoANDROID = VkImportAndroidHardwareBufferInfoANDROID
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @buffer@ is the Android hardware buffer to import.
  vkBuffer :: Ptr AHardwareBuffer
  }
  deriving (Eq, Show)

instance Storable VkImportAndroidHardwareBufferInfoANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImportAndroidHardwareBufferInfoANDROID <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportAndroidHardwareBufferInfoANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportAndroidHardwareBufferInfoANDROID))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkImportAndroidHardwareBufferInfoANDROID))
-- | VkAndroidHardwareBufferUsageANDROID - Struct containing Android hardware
-- buffer usage flags
--
-- = Description
--
-- The @androidHardwareBufferUsage@ field /must/ include Android hardware
-- buffer usage flags listed in the [AHardwareBuffer Usage
-- Equivalence](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-external-android-hardware-buffer-usage)
-- table when the corresponding Vulkan image usage or image creation flags
-- are included in the @usage@ or @flags@ fields of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'.
-- It /must/ include at least one GPU usage flag
-- (@AHARDWAREBUFFER_USAGE_GPU_@*), even if none of the corresponding
-- Vulkan usages or flags are requested.
--
-- __Note__
--
-- Requiring at least one GPU usage flag ensures that Android hardware
-- buffer memory will be allocated in a memory pool accessible to the
-- Vulkan implementation, and that specializing the memory layout based on
-- usage flags doesn’t prevent it from being compatible with Vulkan.
-- Implementations /may/ avoid unnecessary restrictions caused by this
-- requirement by using vendor usage flags to indicate that only the Vulkan
-- uses indicated in
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2'
-- are required.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkAndroidHardwareBufferUsageANDROID = VkAndroidHardwareBufferUsageANDROID
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @androidHardwareBufferUsage@ returns the the Android hardware buffer
  -- usage flags.
  vkAndroidHardwareBufferUsage :: Word64
  }
  deriving (Eq, Show)

instance Storable VkAndroidHardwareBufferUsageANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkAndroidHardwareBufferUsageANDROID <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAndroidHardwareBufferUsageANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAndroidHardwareBufferUsageANDROID))
                *> poke (ptr `plusPtr` 16) (vkAndroidHardwareBufferUsage (poked :: VkAndroidHardwareBufferUsageANDROID))
-- | VkAndroidHardwareBufferPropertiesANDROID - Properties of External Memory
-- Android Hardware Buffers
--
-- = See Also
--
-- @VkDeviceSize@, 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetAndroidHardwareBufferPropertiesANDROID'
data VkAndroidHardwareBufferPropertiesANDROID = VkAndroidHardwareBufferPropertiesANDROID
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @allocationSize@ is the size of the external memory
  vkAllocationSize :: VkDeviceSize
  , -- | @memoryTypeBits@ is a bitmask containing one bit set for every memory
  -- type which the specified Android hardware buffer /can/ be imported as.
  vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkAndroidHardwareBufferPropertiesANDROID where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkAndroidHardwareBufferPropertiesANDROID <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
                                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAndroidHardwareBufferPropertiesANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAndroidHardwareBufferPropertiesANDROID))
                *> poke (ptr `plusPtr` 16) (vkAllocationSize (poked :: VkAndroidHardwareBufferPropertiesANDROID))
                *> poke (ptr `plusPtr` 24) (vkMemoryTypeBits (poked :: VkAndroidHardwareBufferPropertiesANDROID))
-- | VkMemoryGetAndroidHardwareBufferInfoANDROID - Structure describing an
-- Android hardware buffer memory export operation
--
-- == Valid Usage
--
-- -   @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@
--     /must/ have been included in
--     'Graphics.Vulkan.Extensions.VK_KHR_external_memory.VkExportMemoryAllocateInfoKHR'::@handleTypes@
--     when @memory@ was created.
--
-- -   If the @pNext@ chain of the @VkMemoryAllocateInfo@ used to allocate
--     @memory@ included a @VkMemoryDedicatedAllocateInfo@ with non-NULL
--     @image@ member, then that @image@ /must/ already be bound to
--     @memory@.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetMemoryAndroidHardwareBufferANDROID'
data VkMemoryGetAndroidHardwareBufferInfoANDROID = VkMemoryGetAndroidHardwareBufferInfoANDROID
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @memory@ is the memory object from which the Android hardware buffer
  -- will be exported.
  vkMemory :: VkDeviceMemory
  }
  deriving (Eq, Show)

instance Storable VkMemoryGetAndroidHardwareBufferInfoANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryGetAndroidHardwareBufferInfoANDROID <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryGetAndroidHardwareBufferInfoANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryGetAndroidHardwareBufferInfoANDROID))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMemoryGetAndroidHardwareBufferInfoANDROID))
-- | VkAndroidHardwareBufferFormatPropertiesANDROID - Structure describing
-- the image format properties of an Android hardware buffer
--
-- = Description
--
-- If the Android hardware buffer has one of the formats listed in the
-- [Format Equivalence
-- table](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-external-android-hardware-buffer-formats),
-- then @format@ /must/ have the equivalent Vulkan format listed in the
-- table. Otherwise, @format@ /may/ be @VK_FORMAT_UNDEFINED@, indicating
-- the Android hardware buffer can only be used with an external format.
--
-- The @formatFeatures@ member /must/ include
-- @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@ and at least one of
-- @VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT@ or
-- @VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT@, and /should/ include
-- @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ and
-- @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT@.
--
-- Android hardware buffers with the same external format /must/ have the
-- same support for @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@,
-- @VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT@,
-- @VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT@,
-- @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT@,
-- @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT@,
-- and
-- @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT@.
-- Other format features /may/ differ between Android hardware buffers that
-- have the same external format. This allows applications to use the same
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion'
-- object (and samplers and pipelines created from them) for any Android
-- hardware buffers that have the same external format.
--
-- If @format@ is not @VK_FORMAT_UNDEFINED@, then the value of
-- @samplerYcbcrConversionComponents@ /must/ be valid when used as the
-- @components@ member of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
-- with that format. If @format@ is @VK_FORMAT_UNDEFINED@, all members of
-- @samplerYcbcrConversionComponents@ /must/ be
-- @VK_COMPONENT_SWIZZLE_IDENTITY@.
--
-- Implementations may not always be able to determine the color model,
-- numerical range, or chroma offsets of the image contents, so the values
-- in @VkAndroidHardwareBufferFormatPropertiesANDROID@ are only
-- suggestions. Applications /should/ treat these values as sensible
-- defaults to use in the absence of more reliable information obtained
-- through some other means. If the underlying physical device is also
-- usable via OpenGL ES with the GL_OES_EGL_image_external extension, the
-- implementation /should/ suggest values that will produce similar sampled
-- values as would be obtained by sampling the same external image via
-- @samplerExternalOES@ in OpenGL ES using equivalent sampler parameters.
--
-- __Note__
--
-- Since GL_OES_EGL_image_external does not require the same sampling and
-- conversion calculations as Vulkan does, achieving identical results
-- between APIs /may/ not be possible on some implementations.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkChromaLocation',
-- 'Graphics.Vulkan.Core10.ImageView.VkComponentMapping',
-- 'Graphics.Vulkan.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkFormatFeatureFlags',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrModelConversion',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrRange',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkAndroidHardwareBufferFormatPropertiesANDROID = VkAndroidHardwareBufferFormatPropertiesANDROID
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @format@ is the Vulkan format corresponding to the Android hardware
  -- buffer’s format, or @VK_FORMAT_UNDEFINED@ if there isn’t an equivalent
  -- Vulkan format.
  vkFormat :: VkFormat
  , -- | @externalFormat@ is an implementation-defined external format identifier
  -- for use with 'VkExternalFormatANDROID'. It /must/ not be zero.
  vkExternalFormat :: Word64
  , -- | @formatFeatures@ describes the capabilities of this external format when
  -- used with an image bound to memory imported from @buffer@.
  vkFormatFeatures :: VkFormatFeatureFlags
  , -- | @samplerYcbcrConversionComponents@ is the component swizzle that
  -- /should/ be used in
  -- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'.
  vkSamplerYcbcrConversionComponents :: VkComponentMapping
  , -- | @suggestedYcbcrModel@ is a suggested color model to use in the
  -- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'.
  vkSuggestedYcbcrModel :: VkSamplerYcbcrModelConversion
  , -- | @suggestedYcbcrRange@ is a suggested numerical value range to use in
  -- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'.
  vkSuggestedYcbcrRange :: VkSamplerYcbcrRange
  , -- | @suggestedXChromaOffset@ is a suggested X chroma offset to use in
  -- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'.
  vkSuggestedXChromaOffset :: VkChromaLocation
  , -- | @suggestedYChromaOffset@ is a suggested Y chroma offset to use in
  -- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'.
  vkSuggestedYChromaOffset :: VkChromaLocation
  }
  deriving (Eq, Show)

instance Storable VkAndroidHardwareBufferFormatPropertiesANDROID where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkAndroidHardwareBufferFormatPropertiesANDROID <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 24)
                                                            <*> peek (ptr `plusPtr` 32)
                                                            <*> peek (ptr `plusPtr` 36)
                                                            <*> peek (ptr `plusPtr` 52)
                                                            <*> peek (ptr `plusPtr` 56)
                                                            <*> peek (ptr `plusPtr` 60)
                                                            <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 24) (vkExternalFormat (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 32) (vkFormatFeatures (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 36) (vkSamplerYcbcrConversionComponents (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 52) (vkSuggestedYcbcrModel (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 56) (vkSuggestedYcbcrRange (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 60) (vkSuggestedXChromaOffset (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
                *> poke (ptr `plusPtr` 64) (vkSuggestedYChromaOffset (poked :: VkAndroidHardwareBufferFormatPropertiesANDROID))
-- | VkExternalFormatANDROID - Structure containing an Android hardware
-- buffer external format
--
-- = Description
--
-- If @externalFormat@ is zero, the effect is as if the
-- @VkExternalFormatANDROID@ structure was not present. Otherwise, the
-- image will have the specified external format, and
-- @VkImageCreateInfo@::@format@ /must/ be @VK_FORMAT_UNDEFINED@.
--
-- == Valid Usage
--
-- -   @externalFormat@ /must/ be @0@ or a value returned in the
--     @externalFormat@ member of
--     'VkAndroidHardwareBufferFormatPropertiesANDROID' by an earlier call
--     to 'vkGetAndroidHardwareBufferPropertiesANDROID'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkExternalFormatANDROID = VkExternalFormatANDROID
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @externalFormat@ is an implementation-defined identifier for the
  -- external format
  vkExternalFormat :: Word64
  }
  deriving (Eq, Show)

instance Storable VkExternalFormatANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalFormatANDROID <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalFormatANDROID))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalFormatANDROID))
                *> poke (ptr `plusPtr` 16) (vkExternalFormat (poked :: VkExternalFormatANDROID))
