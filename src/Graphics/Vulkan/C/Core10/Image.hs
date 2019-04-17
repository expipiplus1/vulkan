{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo(..)
  , VkImageLayout(..)
  , pattern VK_IMAGE_LAYOUT_UNDEFINED
  , pattern VK_IMAGE_LAYOUT_GENERAL
  , pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_PREINITIALIZED
  , VkSubresourceLayout(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateImage
#endif
  , FN_vkCreateImage
  , PFN_vkCreateImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyImage
#endif
  , FN_vkDestroyImage
  , PFN_vkDestroyImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetImageSubresourceLayout
#endif
  , FN_vkGetImageSubresourceLayout
  , PFN_vkGetImageSubresourceLayout
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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkSharingMode(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkExtent3D(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkSampleCountFlagBits(..)
  , VkDevice
  , VkDeviceSize
  , VkImageCreateFlags
  , VkImageUsageFlags
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageSubresource(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkImageCreateInfo - Structure specifying the parameters of a newly
-- created image object
--
-- = Description
--
-- Images created with @tiling@ equal to @VK_IMAGE_TILING_LINEAR@ have
-- further restrictions on their limits and capabilities compared to images
-- created with @tiling@ equal to @VK_IMAGE_TILING_OPTIMAL@. Creation of
-- images with tiling @VK_IMAGE_TILING_LINEAR@ /may/ not be supported
-- unless other parameters meet all of the constraints:
--
-- -   @imageType@ is @VK_IMAGE_TYPE_2D@
--
-- -   @format@ is not a depth\/stencil format
--
-- -   @mipLevels@ is 1
--
-- -   @arrayLayers@ is 1
--
-- -   @samples@ is @VK_SAMPLE_COUNT_1_BIT@
--
-- -   @usage@ only includes @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ and\/or
--     @VK_IMAGE_USAGE_TRANSFER_DST_BIT@
--
-- Images created with a @format@ from one of those listed in
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion {html_spec_relative}#formats-requiring-sampler-ycbcr-conversion>
-- have further restrictions on their limits and capabilities compared to
-- images created with other formats. Creation of images with a format
-- requiring
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion Y’CBCR conversion>
-- /may/ not be supported unless other parameters meet all of the
-- constraints:
--
-- -   @imageType@ is @VK_IMAGE_TYPE_2D@
--
-- -   @mipLevels@ is 1
--
-- -   @arrayLayers@ is 1
--
-- -   @samples@ is @VK_SAMPLE_COUNT_1_BIT@
--
-- Implementations /may/ support additional limits and capabilities beyond
-- those listed above.
--
-- To determine the set of valid @usage@ bits for a given format, call
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'.
--
-- If the size of the resultant image would exceed @maxResourceSize@, then
-- @vkCreateImage@ /must/ fail and return @VK_ERROR_OUT_OF_DEVICE_MEMORY@.
-- This failure /may/ occur even when all image creation parameters satisfy
-- their valid usage requirements.
--
-- __Note__
--
-- For images created without @VK_IMAGE_CREATE_EXTENDED_USAGE_BIT@ a
-- @usage@ bit is valid if it is supported for the format the image is
-- created with.
--
-- For images created with @VK_IMAGE_CREATE_EXTENDED_USAGE_BIT@ a @usage@
-- bit is valid if it is supported for at least one of the formats a
-- @VkImageView@ created from the image /can/ have (see
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-views Image Views>
-- for more detail).
--
-- Valid values for some image creation parameters are limited by a
-- numerical upper bound or by inclusion in a bitset. For example,
-- @VkImageCreateInfo@::@arrayLayers@ is limited by
-- @imageCreateMaxArrayLayers@, defined below; and
-- @VkImageCreateInfo@::@samples@ is limited by @imageCreateSampleCounts@,
-- also defined below.
--
-- Several limiting values are defined below, as well as assisting values
-- from which the limiting values are derived. The limiting values are
-- referenced by the relevant valid usage statements of
-- @VkImageCreateInfo@.
--
-- -   Let @uint64_t imageCreateDrmFormatModifiers[]@ be the set of
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-drm-format-modifier Linux DRM format modifiers>
--     that the resultant image /may/ have.
--
--     -   If @tiling@ is not @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@,
--         then @imageCreateDrmFormatModifiers@ is empty.
--
--     -   If @VkImageCreateInfo@::@pNext@ contains
--         'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierExplicitCreateInfoEXT',
--         then @imageCreateDrmFormatModifiers@ contains exactly one
--         modifier,
--         @VkImageDrmFormatModifierExplicitCreateInfoEXT@::@drmFormatModifier@.
--
--     -   If @VkImageCreateInfo@::@pNext@ contains
--         'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierListCreateInfoEXT',
--         then @imageCreateDrmFormatModifiers@ contains the exactly the
--         modifiers in
--         @VkImageDrmFormatModifierListCreateInfoEXT@::@pDrmFormatModifiers@.
--
-- -   Let @VkBool32 imageCreateMaybeLinear@ indicate if the resultant
--     image may be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-linear-image linear>.
--
--     -   If @tiling@ is @VK_IMAGE_TILING_LINEAR@, then
--         @imageCreateMaybeLinear@ is @true@.
--
--     -   If @tiling@ is @VK_IMAGE_TILING_OPTIMAL@, then
--         @imageCreateMaybeLinear@ is @false@.
--
--     -   If @tiling@ is @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@, then
--         @imageCreateMaybeLinear_@ is @true@ if and only if
--         @imageCreateDrmFormatModifiers@ contains
--         @DRM_FORMAT_MOD_LINEAR@.
--
-- -   Let @VkFormatFeatureFlags imageCreateFormatFeatures@ be the set of
--     format features available during image creation.
--
--     -   If @tiling@ is @VK_IMAGE_TILING_LINEAR@, then
--         @imageCreateFormatFeatures@ is the value of
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@linearTilingFeatures@
--         found by calling
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--         with parameter @format@ equal to @VkImageCreateInfo@::@format@.
--
--     -   If @tiling@ is @VK_IMAGE_TILING_OPTIMAL@, and if the @pNext@
--         chain contains no instance of
--         'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--         with non-zero @externalFormat@, then @imageCreateFormatFeatures@
--         is value of
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@optimalTilingFeatures@
--         found by calling
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--         with parameter @format@ equal to @VkImageCreateInfo@::@format@.
--
--     -   If @tiling@ is @VK_IMAGE_TILING_OPTIMAL@, and if the @pNext@
--         chain contains an instance of
--         'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--         with non-zero @externalFormat@, then @imageCreateFormatFeatures@
--         is the value of
--         'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID'::@formatFeatures@
--         obtained by
--         'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID'
--         with a matching @externalFormat@ value.
--
--     -   If @tiling@ is @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@, then
--         the value of @imageCreateFormatFeatures@ is found by calling
--         'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2'
--         with
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@format@
--         equal to @VkImageCreateInfo@::@format@ and with
--         'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkDrmFormatModifierPropertiesListEXT'
--         chained into
--         'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2';
--         by collecting all members of the returned array
--         'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkDrmFormatModifierPropertiesListEXT'::pDrmFormatModifierProperties
--         whose @drmFormatModifier@ belongs to
--         @imageCreateDrmFormatModifiers@; and by taking the bitwise
--         intersection, over the collected array members, of
--         @drmFormatModifierTilingFeatures@. (The resultant
--         @imageCreateFormatFeatures@ /may/ be empty).
--
-- -   Let
--     @VkImageFormatProperties2 imageCreateImageFormatPropertiesList[]@ be
--     defined as follows.
--
--     -   If @VkImageCreateInfo@::@pNext@ contains no instance of
--         'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--         with non-zero @externalFormat@, then
--         @imageCreateImageFormatPropertiesList@ is the list of structures
--         obtained by calling
--         'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2',
--         possibly multiple times, as follows:
--
--         -   The parameters
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'::@format@,
--             @imageType@, @tiling@, @usage@, and @flags@ /must/ be equal
--             to those in @VkImageCreateInfo@.
--
--         -   If 'VkImageCreateInfo'::@pNext@ contains an instance of
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
--             where @handleTypes@ is not @0@, then
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ contain an instance of
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
--             where @handleType@ is not @0@; and
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--             /must/ be called for each handle type in
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'::@handleTypes@,
--             successively setting
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'::@handleType@
--             on each call.
--
--         -   If 'VkImageCreateInfo'::@pNext@ contains no instance of
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
--             or contains an instance where @handleTypes@ is @0@, then
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ either contain no instance of
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
--             or contain an instance where @handleType@ is @0@.
--
--         -   If @tiling@ is @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@,
--             then
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ contain an instance of
--             'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkPhysicalDeviceImageDrmFormatModifierInfoEXT'
--             where @sharingMode@ is equal to
--             'VkImageCreateInfo'::@sharingMode@; and, if @sharingMode@ is
--             @VK_SHARING_MODE_CONCURRENT@, then @queueFamilyIndexCount@
--             and @pQueueFamilyIndices@ /must/ be equal to those in
--             'VkImageCreateInfo'; and, if @flags@ contains
--             @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@, then the instance of
--             'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'
--             in the @pNext@ chain of
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
--             /must/ be equivalent to the one in the @pNext@ chain of
--             'VkImageCreateInfo'; and
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--             /must/ be called for each modifier in
--             @imageCreateDrmFormatModifiers@, successively setting
--             'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkPhysicalDeviceImageDrmFormatModifierInfoEXT'::@drmFormatModifier@
--             on each call.
--
--         -   If @tiling@ is not
--             @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@, then
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ contain no instance of
--             'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkPhysicalDeviceImageDrmFormatModifierInfoEXT'.
--
--         -   If any call to
--             'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--             returns an error, then
--             @imageCreateImageFormatPropertiesList@ is defined to be the
--             empty list.
--
--     -   If @VkImageCreateInfo@::@pNext@ contains an instance of
--         'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--         with non-zero @externalFormat@, then
--         @imageCreateImageFormatPropertiesList@ contains a single element
--         where:
--
--         -   @VkImageFormatProperties@::@maxMipLevels@ is
--             ⌊log2(max(@extent.width@, @extent.height@, @extent.depth@))⌋
--             + 1.
--
--         -   @VkImageFormatProperties@::@maxArrayLayers@ is
--             'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::maxImageArrayLayers.
--
--         -   Each component of @VkImageFormatProperties@::@maxExtent@ is
--             'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::maxImageDimension2D.
--
--         -   @VkImageFormatProperties@::@sampleCounts@ contains exactly
--             @VK_SAMPLE_COUNT_1_BIT@.
--
-- -   Let @uint32_t imageCreateMaxMipLevels@ be the minimum value of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@maxMipLevels@
--     in @imageCreateImageFormatPropertiesList@. The value is undefined if
--     @imageCreateImageFormatPropertiesList@ is empty.
--
-- -   Let @uint32_t imageCreateMaxArrayLayers@ be the minimum value of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@maxArrayLayers@
--     in @imageCreateImageFormatPropertiesList@. The value is undefined if
--     @imageCreateImageFormatPropertiesList@ is empty.
--
-- -   Let @VkExtent3D imageCreateMaxExtent@ be the component-wise minimum
--     over all
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@maxExtent@
--     values in @imageCreateImageFormatPropertiesList@. The value is
--     undefined if @imageCreateImageFormatPropertiesList@ is empty.
--
-- -   Let @VkSampleCountFlags imageCreateSampleCounts@ be the intersection
--     of each
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@sampleCounts@
--     in @imageCreateImageFormatPropertiesList@. The value is undefined if
--     @imageCreateImageFormatPropertiesList@ is empty.
--
-- = Valid Usage
--
-- -   Each of the following values (as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ not be undefined @imageCreateMaxMipLevels@,
--     @imageCreateMaxArrayLayers@, @imageCreateMaxExtent@, and
--     @imageCreateSampleCounts@.
--
-- -   If @sharingMode@ is @VK_SHARING_MODE_CONCURRENT@,
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   If @sharingMode@ is @VK_SHARING_MODE_CONCURRENT@,
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   If @sharingMode@ is @VK_SHARING_MODE_CONCURRENT@, each element of
--     @pQueueFamilyIndices@ /must/ be unique and /must/ be less than
--     @pQueueFamilyPropertyCount@ returned by either
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID',
--     and its member @externalFormat@ is non-zero the @format@ /must/ be
--     @VK_FORMAT_UNDEFINED@.
--
-- -   If the @pNext@ chain does not contain an instance of
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID',
--     or does and its member @externalFormat@ is @0@ the @format@ /must/
--     not be @VK_FORMAT_UNDEFINED@.
--
-- -   @extent@::@width@ /must/ be greater than @0@.
--
-- -   @extent@::@height@ /must/ be greater than @0@.
--
-- -   @extent@::@depth@ /must/ be greater than @0@.
--
-- -   @mipLevels@ /must/ be greater than @0@
--
-- -   @arrayLayers@ /must/ be greater than @0@
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@,
--     @imageType@ /must/ be @VK_IMAGE_TYPE_2D@
--
-- -   If @flags@ contains @VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT@,
--     @imageType@ /must/ be @VK_IMAGE_TYPE_2D@
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@,
--     @imageType@ /must/ be @VK_IMAGE_TYPE_3D@
--
-- -   @extent.width@ /must/ be less than or equal to
--     @imageCreateMaxExtent.width@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   @extent.height@ /must/ be less than or equal to
--     @imageCreateMaxExtent.height@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   @extent.depth@ /must/ be less than or equal to
--     @imageCreateMaxExtent.depth@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_2D@ and @flags@ contains
--     @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@, @extent.width@ and
--     @extent.height@ /must/ be equal and @arrayLayers@ /must/ be greater
--     than or equal to 6
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_1D@, both @extent.height@ and
--     @extent.depth@ /must/ be @1@
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_2D@, @extent.depth@ /must/ be @1@
--
-- -   @mipLevels@ /must/ be less than or equal to the number of levels in
--     the complete mipmap chain based on @extent.width@, @extent.height@,
--     and @extent.depth@.
--
-- -   @mipLevels@ /must/ be less than or equal to
--     @imageCreateMaxMipLevels@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   @arrayLayers@ /must/ be less than or equal to
--     @imageCreateMaxArrayLayers@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_3D@, @arrayLayers@ /must/ be @1@.
--
-- -   If @samples@ is not @VK_SAMPLE_COUNT_1_BIT@, then @imageType@ /must/
--     be @VK_IMAGE_TYPE_2D@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@, @mipLevels@ /must/ be equal
--     to @1@, and @imageCreateMaybeLinear@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ be @false@,
--
-- -   If @samples@ is not @VK_SAMPLE_COUNT_1_BIT@, @usage@ /must/ not
--     contain @VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT@
--
-- -   If @usage@ includes @VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT@, then
--     bits other than @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@,
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@, and
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ /must/ not be set
--
-- -   If @usage@ includes @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@,
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@,
--     @VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT@, or
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@, @extent.width@ /must/ be less
--     than or equal to @VkPhysicalDeviceLimits@::@maxFramebufferWidth@
--
-- -   If @usage@ includes @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@,
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@,
--     @VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT@, or
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@, @extent.height@ /must/ be
--     less than or equal to
--     @VkPhysicalDeviceLimits@::@maxFramebufferHeight@
--
-- -   If @usage@ includes @VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT@,
--     @extent.width@ /must/ be less than or equal to
--     \(\lceil{\frac{maxFramebufferWidth}{minFragmentDensityTexelSize_{width}}}\rceil\)
--
-- -   If @usage@ includes @VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT@,
--     @extent.height@ /must/ be less than or equal to
--     \(\lceil{\frac{maxFramebufferHeight}{minFragmentDensityTexelSize_{height}}}\rceil\)
--
-- -   If @usage@ includes @VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT@,
--     @usage@ /must/ also contain at least one of
--     @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@,
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@, or
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@.
--
-- -   @samples@ /must/ be a bit value that is set in
--     @imageCreateSampleCounts@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-shaderStorageImageMultisample multisampled storage images>
--     feature is not enabled, and @usage@ contains
--     @VK_IMAGE_USAGE_STORAGE_BIT@, @samples@ /must/ be
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-sparseBinding sparse bindings>
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-sparseResidencyAliased sparse aliased residency>
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_1D@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-sparseResidencyImage2D sparse residency for 2D images>
--     feature is not enabled, and @imageType@ is @VK_IMAGE_TYPE_2D@,
--     @flags@ /must/ not contain @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-sparseResidencyImage3D sparse residency for 3D images>
--     feature is not enabled, and @imageType@ is @VK_IMAGE_TYPE_3D@,
--     @flags@ /must/ not contain @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-sparseResidency2Samples sparse residency for images with 2 samples>
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_2_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-sparseResidency4Samples sparse residency for images with 4 samples>
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_4_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-sparseResidency8Samples sparse residency for images with 8 samples>
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_8_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-sparseResidency16Samples sparse residency for images with 16 samples>
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_16_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@ or
--     @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@, it /must/ also contain
--     @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@
--
-- -   If any of the bits @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@,
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@, or
--     @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@ are set,
--     @VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT@ /must/ not also be set
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV',
--     it /must/ not contain an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'.
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
--     its @handleTypes@ member /must/ only contain bits that are also in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'::@externalMemoryProperties.compatibleHandleTypes@,
--     as returned by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--     with @format@, @imageType@, @tiling@, @usage@, and @flags@ equal to
--     those in this structure, and with an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
--     in the @pNext@ chain, with a @handleType@ equal to any one of the
--     handle types specified in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'::@handleTypes@
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV',
--     its @handleTypes@ member /must/ only contain bits that are also in
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV'::@externalMemoryProperties.compatibleHandleTypes@,
--     as returned by
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV'
--     with @format@, @imageType@, @tiling@, @usage@, and @flags@ equal to
--     those in this structure, and with @externalHandleType@ equal to any
--     one of the handle types specified in
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV'::@handleTypes@
--
-- -   If the logical device was created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo'::@physicalDeviceCount@
--     equal to 1, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT@
--
-- -   If @flags@ contains
--     @VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT@, then @mipLevels@
--     /must/ be one, @arrayLayers@ /must/ be one, @imageType@ /must/ be
--     @VK_IMAGE_TYPE_2D@. and @imageCreateMaybeLinear@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ be @false@.
--
-- -   If @flags@ contains
--     @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@, then @format@
--     /must/ be a
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#appendix-compressedtex-bc block-compressed image format>,
--     an
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#appendix-compressedtex-etc2 ETC compressed image format>,
--     or an
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#appendix-compressedtex-astc ASTC compressed image format>.
--
-- -   If @flags@ contains
--     @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@, then @flags@
--     /must/ also contain @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@.
--
-- -   @initialLayout@ /must/ be @VK_IMAGE_LAYOUT_UNDEFINED@ or
--     @VK_IMAGE_LAYOUT_PREINITIALIZED@.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV'
--     structure whose @handleTypes@ member is not @0@, @initialLayout@
--     /must/ be @VK_IMAGE_LAYOUT_UNDEFINED@
--
-- -   If the image @format@ is one of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion {html_spec_relative}#formats-requiring-sampler-ycbcr-conversion>,
--     then @mipLevels@ /must/ be 1
--
-- -   If the image @format@ is one of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion {html_spec_relative}#formats-requiring-sampler-ycbcr-conversion>,
--     @samples@ must be @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If the image @format@ is one of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion {html_spec_relative}#formats-requiring-sampler-ycbcr-conversion>,
--     @imageType@ /must/ be @VK_IMAGE_TYPE_2D@
--
-- -   If the image @format@ is one of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion {html_spec_relative}#formats-requiring-sampler-ycbcr-conversion>,
--     and the @ycbcrImageArrays@ feature is not enabled, @arrayLayers@
--     /must/ be 1
--
-- -   If @format@ is a /multi-planar/ format, and if
--     @imageCreateFormatFeatures@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     does not contain @VK_FORMAT_FEATURE_DISJOINT_BIT@, then @flags@
--     /must/ not contain @VK_IMAGE_CREATE_DISJOINT_BIT@.
--
-- -   If @format@ is not a /multi-planar/ format, and @flags@ does not
--     include @VK_IMAGE_CREATE_ALIAS_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_DISJOINT_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@, then the
--     @pNext@ chain /must/ contain exactly one of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierListCreateInfoEXT'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierExplicitCreateInfoEXT'.
--
-- -   If the @pNext@ chain contains
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierListCreateInfoEXT'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierExplicitCreateInfoEXT',
--     then @tiling@ /must/ be @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@.
--
-- -   If @tiling@ is @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@ and @flags@
--     contains @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@, then the @pNext@
--     chain /must/ contain
--     'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'
--     with non-zero @viewFormatCount@.
--
-- -   If @flags@ contains
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ @format@
--     /must/ be a depth or depth\/stencil format
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
--     structure whose @handleTypes@ member includes
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@,
--     @imageType@ /must/ be @VK_IMAGE_TYPE_2D@.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
--     structure whose @handleTypes@ member includes
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@,
--     @mipLevels@ /must/ either be @1@ or equal to the number of levels in
--     the complete mipmap chain based on @extent.width@, @extent.height@,
--     and @extent.depth@.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@, @flags@ /must/
--     not include @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@, @usage@ /must/
--     not include any usages except @VK_IMAGE_USAGE_SAMPLED_BIT@.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@, @tiling@ /must/
--     be @VK_IMAGE_TILING_OPTIMAL@.
--
-- -   If @format@ is a depth-stencil format and the @pNext@ chain contains
--     an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT',
--     then its @stencilUsage@ member /must/ only include
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ if @usage@ also
--     includes it
--
-- -   If @format@ is a depth-stencil format and the @pNext@ chain contains
--     an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT',
--     then its @stencilUsage@ member /must/ only include
--     @VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT@ if @usage@ also includes
--     it
--
-- -   If @Format@ is a depth-stencil format and the @pNext@ chain contains
--     an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'
--     with its @stencilUsage@ member including
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@, @extent.width@ /must/ be less
--     than or equal to @VkPhysicalDeviceLimits@::@maxFramebufferWidth@
--
-- -   If @format@ is a depth-stencil format and the @pNext@ chain contains
--     an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'
--     with its @stencilUsage@ member including
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@, @extent.height@ /must/ be
--     less than or equal to
--     @VkPhysicalDeviceLimits@::@maxFramebufferHeight@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-shaderStorageImageMultisample multisampled storage images>
--     feature is not enabled, @format@ is a depth-stencil format and the
--     @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'
--     with its @stencilUsage@ including @VK_IMAGE_USAGE_STORAGE_BIT@,
--     @samples@ /must/ be @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV@,
--     @imageType@ /must/ be @VK_IMAGE_TYPE_2D@ or @VK_IMAGE_TYPE_3D@
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV@, it
--     /must/ not contain @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@ and the
--     @format@ /must/ not be a depth\/stencil format
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV@ and
--     @imageType@ is @VK_IMAGE_TYPE_2D@, @extent@::@width@ and
--     @extent@::@height@ /must/ be greater than @1@
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV@ and
--     @imageType@ is @VK_IMAGE_TYPE_3D@, @extent@::@width@,
--     @extent@::@height@, and @extent@::@depth@ /must/ be greater than @1@
--
-- -   If @usage@ includes @VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV@,
--     @imageType@ /must/ be @VK_IMAGE_TYPE_2D@.
--
-- -   If @usage@ includes @VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV@,
--     @samples@ /must/ be @VK_SAMPLE_COUNT_1_BIT@.
--
-- -   If @usage@ includes @VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV@,
--     @tiling@ /must/ be @VK_IMAGE_TILING_OPTIMAL@.
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT@, @tiling@
--     /must/ be @VK_IMAGE_TILING_OPTIMAL@
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT@,
--     @imageType@ /must/ be @VK_IMAGE_TYPE_2D@
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT@, @flags@
--     /must/ not contain @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@
--
-- -   If @flags@ contains @VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT@,
--     @mipLevels@ /must/ be @1@
--
-- = Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlagBits'
--     values
--
-- -   @imageType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType' value
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Core.VkFormat'
--     value
--
-- -   @samples@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'
--     value
--
-- -   @tiling@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling' value
--
-- -   @usage@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
--     values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @sharingMode@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Buffer.VkSharingMode' value
--
-- -   @initialLayout@ /must/ be a valid 'VkImageLayout' value
--
-- \<\/section>
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags',
-- 'VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkSharingMode',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkCreateImage'
data VkImageCreateInfo = VkImageCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlagBits'
  -- describing additional parameters of the image.
  vkFlags :: VkImageCreateFlags
  , -- | @imageType@ is a
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType' value
  -- specifying the basic dimensionality of the image. Layers in array
  -- textures do not count as a dimension for the purposes of the image type.
  vkImageType :: VkImageType
  , -- | @format@ is a 'Graphics.Vulkan.C.Core10.Core.VkFormat' describing the
  -- format and type of the texel blocks that will be contained in the image.
  vkFormat :: VkFormat
  , -- | @extent@ is a 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D'
  -- describing the number of data elements in each dimension of the base
  -- level.
  vkExtent :: VkExtent3D
  , -- | @mipLevels@ describes the number of levels of detail available for
  -- minified sampling of the image.
  vkMipLevels :: Word32
  , -- | @arrayLayers@ is the number of layers in the image.
  vkArrayLayers :: Word32
  , -- | @samples@ is a
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'
  -- specifying the number of
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primsrast-multisampling samples per texel>.
  vkSamples :: VkSampleCountFlagBits
  , -- | @tiling@ is a
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling' value
  -- specifying the tiling arrangement of the texel blocks in memory.
  vkTiling :: VkImageTiling
  , -- | @usage@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
  -- describing the intended usage of the image.
  vkUsage :: VkImageUsageFlags
  , -- | @sharingMode@ is a 'Graphics.Vulkan.C.Core10.Buffer.VkSharingMode' value
  -- specifying the sharing mode of the image when it will be accessed by
  -- multiple queue families.
  vkSharingMode :: VkSharingMode
  , -- | @queueFamilyIndexCount@ is the number of entries in the
  -- @pQueueFamilyIndices@ array.
  vkQueueFamilyIndexCount :: Word32
  , -- | @pQueueFamilyIndices@ is a list of queue families that will access this
  -- image (ignored if @sharingMode@ is not @VK_SHARING_MODE_CONCURRENT@).
  vkPQueueFamilyIndices :: Ptr Word32
  , -- | @initialLayout@ is a 'VkImageLayout' value specifying the initial
  -- 'VkImageLayout' of all image subresources of the image. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-layouts Image Layouts>.
  vkInitialLayout :: VkImageLayout
  }
  deriving (Eq, Show)

instance Storable VkImageCreateInfo where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = VkImageCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 20)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 28)
                               <*> peek (ptr `plusPtr` 40)
                               <*> peek (ptr `plusPtr` 44)
                               <*> peek (ptr `plusPtr` 48)
                               <*> peek (ptr `plusPtr` 52)
                               <*> peek (ptr `plusPtr` 56)
                               <*> peek (ptr `plusPtr` 60)
                               <*> peek (ptr `plusPtr` 64)
                               <*> peek (ptr `plusPtr` 72)
                               <*> peek (ptr `plusPtr` 80)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkImageType (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkFormat (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkExtent (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkMipLevels (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkArrayLayers (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkSamples (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkTiling (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkUsage (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 60) (vkSharingMode (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkQueueFamilyIndexCount (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkPQueueFamilyIndices (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 80) (vkInitialLayout (poked :: VkImageCreateInfo))

instance Zero VkImageCreateInfo where
  zero = VkImageCreateInfo zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
-- ** VkImageLayout

-- | VkImageLayout - Layout of image and image subresources
--
-- = Description
--
-- The type(s) of device access supported by each layout are:
--
-- The layout of each image subresource is not a state of the image
-- subresource itself, but is rather a property of how the data in memory
-- is organized, and thus for each mechanism of accessing an image in the
-- API the application /must/ specify a parameter or structure member that
-- indicates which image layout the image subresource(s) are considered to
-- be in when the image will be accessed. For transfer commands, this is a
-- parameter to the command (see
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#clears {html_spec_relative}#clears>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies {html_spec_relative}#copies>).
-- For use as a framebuffer attachment, this is a member in the
-- substructures of the @VkRenderPassCreateInfo@ (see
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass Render Pass>).
-- For use in a descriptor set, this is a member in the
-- @VkDescriptorImageInfo@ structure (see
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-updates {html_spec_relative}#descriptorsets-updates>).
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResolveImage'
newtype VkImageLayout = VkImageLayout Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkImageLayout where
  showsPrec _ VK_IMAGE_LAYOUT_UNDEFINED = showString "VK_IMAGE_LAYOUT_UNDEFINED"
  showsPrec _ VK_IMAGE_LAYOUT_GENERAL = showString "VK_IMAGE_LAYOUT_GENERAL"
  showsPrec _ VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = showString "VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = showString "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = showString "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = showString "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_PREINITIALIZED = showString "VK_IMAGE_LAYOUT_PREINITIALIZED"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageLayout 1000117000) = showString "VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL"
  showsPrec _ (VkImageLayout 1000117001) = showString "VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL"
  showsPrec _ (VkImageLayout 1000001002) = showString "VK_IMAGE_LAYOUT_PRESENT_SRC_KHR"
  showsPrec _ (VkImageLayout 1000111000) = showString "VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR"
  showsPrec _ (VkImageLayout 1000164003) = showString "VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV"
  showsPrec _ (VkImageLayout 1000218000) = showString "VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT"
  showsPrec p (VkImageLayout x) = showParen (p >= 11) (showString "VkImageLayout " . showsPrec 11 x)

instance Read VkImageLayout where
  readPrec = parens ( choose [ ("VK_IMAGE_LAYOUT_UNDEFINED",                        pure VK_IMAGE_LAYOUT_UNDEFINED)
                             , ("VK_IMAGE_LAYOUT_GENERAL",                          pure VK_IMAGE_LAYOUT_GENERAL)
                             , ("VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL",         pure VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL", pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL",  pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL",         pure VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL",             pure VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL",             pure VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_PREINITIALIZED",                   pure VK_IMAGE_LAYOUT_PREINITIALIZED)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL", pure (VkImageLayout 1000117000))
                             , ("VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL", pure (VkImageLayout 1000117001))
                             , ("VK_IMAGE_LAYOUT_PRESENT_SRC_KHR",                            pure (VkImageLayout 1000001002))
                             , ("VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR",                         pure (VkImageLayout 1000111000))
                             , ("VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV",                    pure (VkImageLayout 1000164003))
                             , ("VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT",           pure (VkImageLayout 1000218000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageLayout")
                        v <- step readPrec
                        pure (VkImageLayout v)
                        )
                    )

-- | @VK_IMAGE_LAYOUT_UNDEFINED@ does not support device access. This layout
-- /must/ only be used as the @initialLayout@ member of @VkImageCreateInfo@
-- or @VkAttachmentDescription@, or as the @oldLayout@ in an image
-- transition. When transitioning out of this layout, the contents of the
-- memory are not guaranteed to be preserved.
pattern VK_IMAGE_LAYOUT_UNDEFINED :: VkImageLayout
pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0

-- | @VK_IMAGE_LAYOUT_GENERAL@ supports all types of device access.
pattern VK_IMAGE_LAYOUT_GENERAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1

-- | @VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL@ /must/ only be used as a
-- color or resolve attachment in a @VkFramebuffer@. This layout is valid
-- only for image subresources of images created with the
-- @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@ usage bit enabled.
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VkImageLayout 2

-- | @VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL@ /must/ only be used
-- as a depth\/stencil or depth\/stencil resolve attachment in a
-- @VkFramebuffer@. This layout is valid only for image subresources of
-- images created with the @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@
-- usage bit enabled.
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = VkImageLayout 3

-- | @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@ /must/ only be used as
-- a read-only depth\/stencil attachment in a @VkFramebuffer@ and\/or as a
-- read-only image in a shader (which /can/ be read as a sampled image,
-- combined image\/sampler and\/or input attachment). This layout is valid
-- only for image subresources of images created with the
-- @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ usage bit enabled. Only
-- image views created with a @usage@ value including
-- @VK_IMAGE_USAGE_SAMPLED_BIT@ /can/ be used as a sampled image or
-- combined image\/sampler in a shader. Similarly, only image views created
-- with a @usage@ value including @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@
-- /can/ be used as input attachments.
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = VkImageLayout 4

-- | @VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL@ /must/ only be used as a
-- read-only image in a shader (which /can/ be read as a sampled image,
-- combined image\/sampler and\/or input attachment). This layout is valid
-- only for image subresources of images created with the
-- @VK_IMAGE_USAGE_SAMPLED_BIT@ or @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@
-- usage bit enabled.
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VkImageLayout 5

-- | @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ /must/ only be used as a source
-- image of a transfer command (see the definition of
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer VK_PIPELINE_STAGE_TRANSFER_BIT>).
-- This layout is valid only for image subresources of images created with
-- the @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ usage bit enabled.
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VkImageLayout 6

-- | @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ /must/ only be used as a
-- destination image of a transfer command. This layout is valid only for
-- image subresources of images created with the
-- @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ usage bit enabled.
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VkImageLayout 7

-- | @VK_IMAGE_LAYOUT_PREINITIALIZED@ does not support device access. This
-- layout /must/ only be used as the @initialLayout@ member of
-- @VkImageCreateInfo@ or @VkAttachmentDescription@, or as the @oldLayout@
-- in an image transition. When transitioning out of this layout, the
-- contents of the memory are preserved. This layout is intended to be used
-- as the initial layout for an image whose contents are written by the
-- host, and hence the data /can/ be written to memory immediately, without
-- first executing a layout transition. Currently,
-- @VK_IMAGE_LAYOUT_PREINITIALIZED@ is only useful with
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-linear-resource linear>
-- images because there is not a standard layout defined for
-- @VK_IMAGE_TILING_OPTIMAL@ images.
pattern VK_IMAGE_LAYOUT_PREINITIALIZED :: VkImageLayout
pattern VK_IMAGE_LAYOUT_PREINITIALIZED = VkImageLayout 8
-- | VkSubresourceLayout - Structure specifying subresource layout
--
-- = Description
--
-- If the image is
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-linear-resource linear>,
-- then @rowPitch@, @arrayPitch@ and @depthPitch@ describe the layout of
-- the image subresource in linear memory. For uncompressed formats,
-- @rowPitch@ is the number of bytes between texels with the same x
-- coordinate in adjacent rows (y coordinates differ by one). @arrayPitch@
-- is the number of bytes between texels with the same x and y coordinate
-- in adjacent array layers of the image (array layer values differ by
-- one). @depthPitch@ is the number of bytes between texels with the same x
-- and y coordinate in adjacent slices of a 3D image (z coordinates differ
-- by one). Expressed as an addressing formula, the starting byte of a
-- texel in the image subresource has address:
--
-- > // (x,y,z,layer) are in texel coordinates
-- > address(x,y,z,layer) = layer*arrayPitch + z*depthPitch + y*rowPitch + x*elementSize + offset
--
-- For compressed formats, the @rowPitch@ is the number of bytes between
-- compressed texel blocks in adjacent rows. @arrayPitch@ is the number of
-- bytes between compressed texel blocks in adjacent array layers.
-- @depthPitch@ is the number of bytes between compressed texel blocks in
-- adjacent slices of a 3D image.
--
-- > // (x,y,z,layer) are in compressed texel block coordinates
-- > address(x,y,z,layer) = layer*arrayPitch + z*depthPitch + y*rowPitch + x*compressedTexelBlockByteSize + offset;
--
-- The value of @arrayPitch@ is undefined for images that were not created
-- as arrays. @depthPitch@ is defined only for 3D images.
--
-- If the image has a /single-plane/ color format and its tiling is
-- @VK_IMAGE_TILING_LINEAR@ , then the @aspectMask@ member of
-- @VkImageSubresource@ /must/ be @VK_IMAGE_ASPECT_COLOR_BIT@.
--
-- If the image has a depth\/stencil format and its tiling is
-- @VK_IMAGE_TILING_LINEAR@ , then @aspectMask@ /must/ be either
-- @VK_IMAGE_ASPECT_DEPTH_BIT@ or @VK_IMAGE_ASPECT_STENCIL_BIT@. On
-- implementations that store depth and stencil aspects separately,
-- querying each of these image subresource layouts will return a different
-- @offset@ and @size@ representing the region of memory used for that
-- aspect. On implementations that store depth and stencil aspects
-- interleaved, the same @offset@ and @size@ are returned and represent the
-- interleaved memory allocation.
--
-- If the image has a
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
-- and its tiling is @VK_IMAGE_TILING_LINEAR@ , then the @aspectMask@
-- member of @VkImageSubresource@ /must/ be @VK_IMAGE_ASPECT_PLANE_0_BIT@,
-- @VK_IMAGE_ASPECT_PLANE_1_BIT@, or (for 3-plane formats only)
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@. Querying each of these image subresource
-- layouts will return a different @offset@ and @size@ representing the
-- region of memory used for that plane. If the image is /disjoint/, then
-- the @offset@ is relative to the base address of the plane. If the image
-- is /non-disjoint/, then the @offset@ is relative to the base address of
-- the image.
--
-- If the image’s tiling is @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@, then
-- the @aspectMask@ member of @VkImageSubresource@ /must/ be one of
-- @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@, where the maximum allowed
-- plane index @i@ is defined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkDrmFormatModifierPropertiesEXT drmFormatModifierPlaneCount>
-- associated with the image’s
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkImageCreateInfo format>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-drm-format-modifier modifier>.
-- The memory range used by the subresource is described by @offset@ and
-- @size@. If the image is /disjoint/, then the @offset@ is relative to the
-- base address of the /memory plane/. If the image is /non-disjoint/, then
-- the @offset@ is relative to the base address of the image. If the image
-- is
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-linear-resource non-linear>,
-- then @rowPitch@, @arrayPitch@, and @depthPitch@ have an
-- implementation-dependent meaning.
--
-- = See Also
--
-- @VkDeviceSize@, 'vkGetImageSubresourceLayout'
data VkSubresourceLayout = VkSubresourceLayout
  { -- | @offset@ is the byte offset from the start of the image or the plane
  -- where the image subresource begins.
  vkOffset :: VkDeviceSize
  , -- | @size@ is the size in bytes of the image subresource. @size@ includes
  -- any extra memory that is required based on @rowPitch@.
  vkSize :: VkDeviceSize
  , -- | @rowPitch@ describes the number of bytes between each row of texels in
  -- an image.
  vkRowPitch :: VkDeviceSize
  , -- | @arrayPitch@ describes the number of bytes between each array layer of
  -- an image.
  vkArrayPitch :: VkDeviceSize
  , -- | @depthPitch@ describes the number of bytes between each slice of 3D
  -- image.
  vkDepthPitch :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkSubresourceLayout where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSubresourceLayout <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkOffset (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 16) (vkRowPitch (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 24) (vkArrayPitch (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 32) (vkDepthPitch (poked :: VkSubresourceLayout))

instance Zero VkSubresourceLayout where
  zero = VkSubresourceLayout zero
                             zero
                             zero
                             zero
                             zero
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkCreateImage - Create a new image object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the image.
--
-- -   @pCreateInfo@ is a pointer to an instance of the @VkImageCreateInfo@
--     structure containing parameters to be used to create the image.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pImage@ points to a
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle in which
--     the resulting image object is returned.
--
-- == Valid Usage
--
-- -   If the @flags@ member of @pCreateInfo@ includes
--     @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@, creating this @VkImage@ /must/
--     not cause the total required sparse memory for all currently valid
--     sparse resources on the device to exceed
--     @VkPhysicalDeviceLimits@::@sparseAddressSpaceSize@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkImageCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pImage@ /must/ be a valid pointer to a @VkImage@ handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage', 'VkImageCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateImage" vkCreateImage :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult

#endif
type FN_vkCreateImage = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult
type PFN_vkCreateImage = FunPtr FN_vkCreateImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkDestroyImage - Destroy an image object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the image.
--
-- -   @image@ is the image to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @image@, either directly or via
--     a @VkImageView@, /must/ have completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @image@ was created, a
--     compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @image@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @image@ /must/
--     be a valid @VkImage@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @image@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @image@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyImage" vkDestroyImage :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyImage = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyImage = FunPtr FN_vkDestroyImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkGetImageSubresourceLayout - Retrieve information about an image
-- subresource
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @image@ is the image whose layout is being queried.
--
-- -   @pSubresource@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageSubresource'
--     structure selecting a specific image for the image subresource.
--
-- -   @pLayout@ points to a 'VkSubresourceLayout' structure in which the
--     layout is returned.
--
-- = Description
--
-- If the image is
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-linear-resource linear>,
-- then the returned layout is valid for
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-device-hostacces host access>.
--
-- If the image’s tiling is @VK_IMAGE_TILING_LINEAR@ and its format is a
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
-- then @vkGetImageSubresourceLayout@ describes one /format plane/ of the
-- image. If the image’s tiling is
-- @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@, then
-- @vkGetImageSubresourceLayout@ describes one /memory plane/ of the image.
-- If the image’s tiling is @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@ and
-- the image is
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-linear-resource non-linear>,
-- then the returned layout has an implementation-dependent meaning; the
-- vendor of the image’s
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-drm-format-modifier DRM format modifier>
-- /may/ provide documentation that explains how to interpret the returned
-- layout.
--
-- @vkGetImageSubresourceLayout@ is invariant for the lifetime of a single
-- image. However, the subresource layout of images in Android hardware
-- buffer external memory is not known until the image has been bound to
-- memory, so applications /must/ not call 'vkGetImageSubresourceLayout'
-- for such an image before it has been bound.
--
-- == Valid Usage
--
-- -   @image@ /must/ have been created with @tiling@ equal to
--     @VK_IMAGE_TILING_LINEAR@ or
--     @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@
--
-- -   The @aspectMask@ member of @pSubresource@ /must/ only have a single
--     bit set
--
-- -   The @mipLevel@ member of @pSubresource@ /must/ be less than the
--     @mipLevels@ specified in 'VkImageCreateInfo' when @image@ was
--     created
--
-- -   The @arrayLayer@ member of @pSubresource@ /must/ be less than the
--     @arrayLayers@ specified in 'VkImageCreateInfo' when @image@ was
--     created
--
-- -   If the @tiling@ of the @image@ is @VK_IMAGE_TILING_LINEAR@ and its
--     @format@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--     with two planes, the @aspectMask@ member of @pSubresource@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@ or @VK_IMAGE_ASPECT_PLANE_1_BIT@
--
-- -   If the @tiling@ of the @image@ is @VK_IMAGE_TILING_LINEAR@ and its
--     @format@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--     with three planes, the @aspectMask@ member of @pSubresource@ /must/
--     be @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@ or
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@
--
-- -   If @image@ was created with the
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@
--     external memory handle type, then @image@ /must/ be bound to memory.
--
-- -   If the @tiling@ of the @image@ is
--     @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@, then the @aspectMask@
--     member of @pSubresource@ /must/ be
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ and the index @i@ /must/ be
--     less than the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkDrmFormatModifierPropertiesEXT drmFormatModifierPlaneCount>
--     associated with the image’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkImageCreateInfo format>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkImageDrmFormatModifierPropertiesEXT drmFormatModifier>.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- -   @pSubresource@ /must/ be a valid pointer to a valid
--     @VkImageSubresource@ structure
--
-- -   @pLayout@ /must/ be a valid pointer to a @VkSubresourceLayout@
--     structure
--
-- -   @image@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageSubresource',
-- 'VkSubresourceLayout'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageSubresourceLayout" vkGetImageSubresourceLayout :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ()

#endif
type FN_vkGetImageSubresourceLayout = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ()
type PFN_vkGetImageSubresourceLayout = FunPtr FN_vkGetImageSubresourceLayout
