{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.Image
  ( withCStructImageCreateInfo
  , fromCStructImageCreateInfo
  , ImageCreateInfo(..)
  , ImageLayout
  , pattern IMAGE_LAYOUT_UNDEFINED
  , pattern IMAGE_LAYOUT_GENERAL
  , pattern IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , pattern IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , pattern IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
  , pattern IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , pattern IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
  , pattern IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , pattern IMAGE_LAYOUT_PREINITIALIZED
  , pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  , pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern IMAGE_LAYOUT_PRESENT_SRC_KHR
  , pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR
  , pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
  , pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  , withCStructSubresourceLayout
  , fromCStructSubresourceLayout
  , SubresourceLayout(..)
  , createImage
  , destroyImage
  , getImageSubresourceLayout
  , withImage
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
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
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo(..)
  , VkImageLayout(..)
  , VkSubresourceLayout(..)
  , vkCreateImage
  , vkDestroyImage
  , vkGetImageSubresourceLayout
  , pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_GENERAL
  , pattern VK_IMAGE_LAYOUT_PREINITIALIZED
  , pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_UNDEFINED
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
  )
import Graphics.Vulkan.Core10.Buffer
  ( SharingMode
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , Extent3D(..)
  , DeviceSize
  , ImageCreateFlags
  , ImageTiling
  , ImageType
  , ImageUsageFlags
  , SampleCountFlagBits
  , fromCStructExtent3D
  , withCStructAllocationCallbacks
  , withCStructExtent3D
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageSubresource(..)
  , withCStructImageSubresource
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )



-- | VkImageCreateInfo - Structure specifying the parameters of a newly
-- created image object
--
-- = Description
--
-- Images created with @tiling@ equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR'
-- have further restrictions on their limits and capabilities compared to
-- images created with @tiling@ equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL'.
-- Creation of images with tiling
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR'
-- /may/ not be supported unless other parameters meet all of the
-- constraints:
--
-- -   @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D'
--
-- -   @format@ is not a depth\/stencil format
--
-- -   @mipLevels@ is 1
--
-- -   @arrayLayers@ is 1
--
-- -   @samples@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   @usage@ only includes
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_SRC_BIT'
--     and\/or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- Implementations /may/ support additional limits and capabilities beyond
-- those listed above.
--
-- To determine the set of valid @usage@ bits for a given format, call
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'.
--
-- If the size of the resultant image would exceed @maxResourceSize@, then
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage' /must/ fail and return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'. This
-- failure /may/ occur even when all image creation parameters satisfy
-- their valid usage requirements.
--
-- Valid values for some image creation parameters are limited by a
-- numerical upper bound or by inclusion in a bitset. For example,
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@arrayLayers@ is
-- limited by @imageCreateMaxArrayLayers@, defined below; and
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@samples@ is limited
-- by @imageCreateSampleCounts@, also defined below.
--
-- Several limiting values are defined below, as well as assisting values
-- from which the limiting values are derived. The limiting values are
-- referenced by the relevant valid usage statements of
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'.
--
-- -   Let @VkBool32 imageCreateMaybeLinear@ indicate if the resultant
--     image may be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#glossary-linear-image linear>.
--     (The definition below is trivial because certain extensions are
--     disabled in this build of the specification).
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR',
--         then @imageCreateMaybeLinear@ is @true@.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL',
--         then @imageCreateMaybeLinear@ is @false@.
--
-- -   Let @VkFormatFeatureFlags imageCreateFormatFeatures@ be the set of
--     format features available during image creation.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR',
--         then @imageCreateFormatFeatures@ is the value of
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@linearTilingFeatures@
--         found by calling
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--         with parameter @format@ equal to
--         'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@format@.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL',
--         then @imageCreateFormatFeatures@ is value of
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@optimalTilingFeatures@
--         found by calling
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--         with parameter @format@ equal to
--         'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@format@.
--
-- -   Let @uint32_t imageCreateMaxMipLevels@ be the value of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@maxMipLevels@
--     found by calling
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
--     with parameters @format@, @imageType@, @tiling@, @usage@, and
--     @flags@ equal to those in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'. If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--     returns an error, then @imageCreateMaxMipLevels@ is undefined.
--
-- -   Let @uint32_t imageCreateMaxArrayLayers@ be defined analogously to
--     @imageCreateMaxMipLevels@.
--
-- -   Let @VkExtent3D imageCreateMaxExtent@ be defined analogously to
--     @imageCreateMaxMipLevels@.
--
-- -   Let @VkSampleCountFlags imageCreateSampleCounts@ be defined
--     analogously to @imageCreateMaxMipLevels@.
--
-- = Valid Usage
--
-- -   Each of the following values (as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ not be undefined @imageCreateMaxMipLevels@,
--     @imageCreateMaxArrayLayers@, @imageCreateMaxExtent@, and
--     @imageCreateSampleCounts@.
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT',
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT',
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT', each
--     element of @pQueueFamilyIndices@ /must/ be unique and /must/ be less
--     than @pQueueFamilyPropertyCount@ returned by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   @format@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED'
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
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @imageType@ /must/ be
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D'
--
-- -   @extent.width@ /must/ be less than or equal to
--     @imageCreateMaxExtent.width@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   @extent.height@ /must/ be less than or equal to
--     @imageCreateMaxExtent.height@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   @extent.depth@ /must/ be less than or equal to
--     @imageCreateMaxExtent.depth@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D' and
--     @flags@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @extent.width@ and @extent.height@ /must/ be equal and @arrayLayers@
--     /must/ be greater than or equal to 6
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     both @extent.height@ and @extent.depth@ /must/ be @1@
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     @extent.depth@ /must/ be @1@
--
-- -   @mipLevels@ /must/ be less than or equal to the number of levels in
--     the complete mipmap chain based on @extent.width@, @extent.height@,
--     and @extent.depth@.
--
-- -   @mipLevels@ /must/ be less than or equal to
--     @imageCreateMaxMipLevels@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   @arrayLayers@ /must/ be less than or equal to
--     @imageCreateMaxArrayLayers@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_3D',
--     @arrayLayers@ /must/ be @1@.
--
-- -   If @samples@ is not
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT',
--     then @imageType@ /must/ be
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @mipLevels@ /must/ be equal to @1@, and @imageCreateMaybeLinear@ (as
--     defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ be @false@,
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     then bits other than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     /must/ not be set
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.width@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxFramebufferWidth@
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.height@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxFramebufferHeight@
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     @usage@ /must/ also contain at least one of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'.
--
-- -   @samples@ /must/ be a bit value that is set in
--     @imageCreateSampleCounts@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-shaderStorageImageMultisample multisampled storage images>
--     feature is not enabled, and @usage@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_STORAGE_BIT',
--     @samples@ /must/ be
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseBinding sparse bindings>
--     feature is not enabled, @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidencyAliased sparse aliased residency>
--     feature is not enabled, @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_ALIASED_BIT'
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidencyImage2D sparse residency for 2D images>
--     feature is not enabled, and @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidencyImage3D sparse residency for 3D images>
--     feature is not enabled, and @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_3D',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidency2Samples sparse residency for images with 2 samples>
--     feature is not enabled, @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     and @samples@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_2_BIT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidency4Samples sparse residency for images with 4 samples>
--     feature is not enabled, @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     and @samples@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_4_BIT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidency8Samples sparse residency for images with 8 samples>
--     feature is not enabled, @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     and @samples@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_8_BIT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidency16Samples sparse residency for images with 16 samples>
--     feature is not enabled, @imageType@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     and @samples@ is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_16_BIT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_ALIASED_BIT',
--     it /must/ also contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT'
--
-- -   If any of the bits
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_ALIASED_BIT'
--     are set,
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT'
--     /must/ not also be set
--
-- -   @initialLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED'.
--
-- = Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationImageCreateInfoNV',
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierExplicitCreateInfoEXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierListCreateInfoEXT',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT',
--     or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkImageSwapchainCreateInfoKHR'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
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
-- -   @initialLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- \<\/section>
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkSharingMode',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage'
data ImageCreateInfo = ImageCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ImageCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageCreateInfo" "flags"
  flags :: ImageCreateFlags
  , -- No documentation found for Nested "ImageCreateInfo" "imageType"
  imageType :: ImageType
  , -- No documentation found for Nested "ImageCreateInfo" "format"
  format :: Format
  , -- No documentation found for Nested "ImageCreateInfo" "extent"
  extent :: Extent3D
  , -- No documentation found for Nested "ImageCreateInfo" "mipLevels"
  mipLevels :: Word32
  , -- No documentation found for Nested "ImageCreateInfo" "arrayLayers"
  arrayLayers :: Word32
  , -- No documentation found for Nested "ImageCreateInfo" "samples"
  samples :: SampleCountFlagBits
  , -- No documentation found for Nested "ImageCreateInfo" "tiling"
  tiling :: ImageTiling
  , -- No documentation found for Nested "ImageCreateInfo" "usage"
  usage :: ImageUsageFlags
  , -- No documentation found for Nested "ImageCreateInfo" "sharingMode"
  sharingMode :: SharingMode
  -- Length valued member elided
  , -- No documentation found for Nested "ImageCreateInfo" "pQueueFamilyIndices"
  queueFamilyIndices :: Vector Word32
  , -- No documentation found for Nested "ImageCreateInfo" "initialLayout"
  initialLayout :: ImageLayout
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageCreateInfo' and
-- marshal a 'ImageCreateInfo' into it. The 'VkImageCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageCreateInfo :: ImageCreateInfo -> (VkImageCreateInfo -> IO a) -> IO a
withCStructImageCreateInfo marshalled cont = withVec (&) (queueFamilyIndices (marshalled :: ImageCreateInfo)) (\pPQueueFamilyIndices -> withCStructExtent3D (extent (marshalled :: ImageCreateInfo)) (\extent'' -> maybeWith withSomeVkStruct (next (marshalled :: ImageCreateInfo)) (\pPNext -> cont (VkImageCreateInfo VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO pPNext (flags (marshalled :: ImageCreateInfo)) (imageType (marshalled :: ImageCreateInfo)) (format (marshalled :: ImageCreateInfo)) extent'' (mipLevels (marshalled :: ImageCreateInfo)) (arrayLayers (marshalled :: ImageCreateInfo)) (samples (marshalled :: ImageCreateInfo)) (tiling (marshalled :: ImageCreateInfo)) (usage (marshalled :: ImageCreateInfo)) (sharingMode (marshalled :: ImageCreateInfo)) (fromIntegral (Data.Vector.length (queueFamilyIndices (marshalled :: ImageCreateInfo)))) pPQueueFamilyIndices (initialLayout (marshalled :: ImageCreateInfo))))))

-- | A function to read a 'VkImageCreateInfo' and all additional
-- structures in the pointer chain into a 'ImageCreateInfo'.
fromCStructImageCreateInfo :: VkImageCreateInfo -> IO ImageCreateInfo
fromCStructImageCreateInfo c = ImageCreateInfo <$> -- Univalued Member elided
                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageCreateInfo)))
                                               <*> pure (vkFlags (c :: VkImageCreateInfo))
                                               <*> pure (vkImageType (c :: VkImageCreateInfo))
                                               <*> pure (vkFormat (c :: VkImageCreateInfo))
                                               <*> (fromCStructExtent3D (vkExtent (c :: VkImageCreateInfo)))
                                               <*> pure (vkMipLevels (c :: VkImageCreateInfo))
                                               <*> pure (vkArrayLayers (c :: VkImageCreateInfo))
                                               <*> pure (vkSamples (c :: VkImageCreateInfo))
                                               <*> pure (vkTiling (c :: VkImageCreateInfo))
                                               <*> pure (vkUsage (c :: VkImageCreateInfo))
                                               <*> pure (vkSharingMode (c :: VkImageCreateInfo))
                                               -- Length valued member elided
                                               <*> (Data.Vector.generateM (fromIntegral (vkQueueFamilyIndexCount (c :: VkImageCreateInfo))) (peekElemOff (vkPQueueFamilyIndices (c :: VkImageCreateInfo))))
                                               <*> pure (vkInitialLayout (c :: VkImageCreateInfo))

instance Zero ImageCreateInfo where
  zero = ImageCreateInfo Nothing
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
                         Data.Vector.empty
                         zero


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
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#clears>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies>).
-- For use as a framebuffer attachment, this is a member in the
-- substructures of the
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo' (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass Render Pass>).
-- For use in a descriptor set, this is a member in the
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo' structure
-- (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-updates>).
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkAttachmentDescription2KHR',
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkAttachmentReference2KHR',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdBindShadingRateImageNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResolveImage'
type ImageLayout = VkImageLayout


{-# complete IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_GENERAL, IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL, IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, IMAGE_LAYOUT_PREINITIALIZED, IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL, IMAGE_LAYOUT_PRESENT_SRC_KHR, IMAGE_LAYOUT_SHARED_PRESENT_KHR, IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV, IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT :: ImageLayout #-}


-- | 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' does not
-- support device access. This layout /must/ only be used as the
-- @initialLayout@ member of
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' or
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription', or as the
-- @oldLayout@ in an image transition. When transitioning out of this
-- layout, the contents of the memory are not guaranteed to be preserved.
pattern IMAGE_LAYOUT_UNDEFINED :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_UNDEFINED = VK_IMAGE_LAYOUT_UNDEFINED


-- | 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL' supports all
-- types of device access.
pattern IMAGE_LAYOUT_GENERAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_GENERAL = VK_IMAGE_LAYOUT_GENERAL


-- | 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
-- /must/ only be used as a color or resolve attachment in a
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer'. This layout is valid only
-- for image subresources of images created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL


-- | 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
-- /must/ only be used as a depth\/stencil attachment in a
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer'. This layout is valid only
-- for image subresources of images created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL


-- | 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
-- /must/ only be used as a read-only depth\/stencil attachment in a
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer' and\/or as a read-only
-- image in a shader (which /can/ be read as a sampled image, combined
-- image\/sampler and\/or input attachment). This layout is valid only for
-- image subresources of images created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
-- usage bit enabled. Only image views created with a @usage@ value
-- including
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT'
-- /can/ be used as a sampled image or combined image\/sampler in a shader.
-- Similarly, only image views created with a @usage@ value including
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
-- /can/ be used as input attachments.
pattern IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL


-- | 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
-- /must/ only be used as a read-only image in a shader (which /can/ be
-- read as a sampled image, combined image\/sampler and\/or input
-- attachment). This layout is valid only for image subresources of images
-- created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT'
-- or
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL


-- | 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
-- /must/ only be used as a source image of a transfer command (see the
-- definition of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer >).
-- This layout is valid only for image subresources of images created with
-- the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_SRC_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL


-- | 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
-- /must/ only be used as a destination image of a transfer command. This
-- layout is valid only for image subresources of images created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
-- usage bit enabled.
pattern IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL


-- | 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED' does not
-- support device access. This layout /must/ only be used as the
-- @initialLayout@ member of
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' or
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription', or as the
-- @oldLayout@ in an image transition. When transitioning out of this
-- layout, the contents of the memory are preserved. This layout is
-- intended to be used as the initial layout for an image whose contents
-- are written by the host, and hence the data /can/ be written to memory
-- immediately, without first executing a layout transition. Currently,
-- 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED' is only
-- useful with
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#glossary-linear-resource linear>
-- images because there is not a standard layout defined for
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL'
-- images.
pattern IMAGE_LAYOUT_PREINITIALIZED :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_PREINITIALIZED = VK_IMAGE_LAYOUT_PREINITIALIZED


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL = VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL = VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_PRESENT_SRC_KHR"
pattern IMAGE_LAYOUT_PRESENT_SRC_KHR :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_PRESENT_SRC_KHR = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_SHARED_PRESENT_KHR"
pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR = VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV"
pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV = VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT"
pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT = VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT


-- | VkSubresourceLayout - Structure specifying subresource layout
--
-- = Description
--
-- If the image is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#glossary-linear-resource linear>,
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
-- If the image has a color format , then the @aspectMask@ member of
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageSubresource'
-- /must/ be
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT'.
--
-- If the image has a depth\/stencil format , then @aspectMask@ /must/ be
-- either
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
-- or
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'.
-- On implementations that store depth and stencil aspects separately,
-- querying each of these image subresource layouts will return a different
-- @offset@ and @size@ representing the region of memory used for that
-- aspect. On implementations that store depth and stencil aspects
-- interleaved, the same @offset@ and @size@ are returned and represent the
-- interleaved memory allocation.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierExplicitCreateInfoEXT',
-- 'Graphics.Vulkan.C.Core10.Image.vkGetImageSubresourceLayout'
data SubresourceLayout = SubresourceLayout
  { -- No documentation found for Nested "SubresourceLayout" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "size"
  size :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "rowPitch"
  rowPitch :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "arrayPitch"
  arrayPitch :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "depthPitch"
  depthPitch :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubresourceLayout' and
-- marshal a 'SubresourceLayout' into it. The 'VkSubresourceLayout' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubresourceLayout :: SubresourceLayout -> (VkSubresourceLayout -> IO a) -> IO a
withCStructSubresourceLayout marshalled cont = cont (VkSubresourceLayout (offset (marshalled :: SubresourceLayout)) (size (marshalled :: SubresourceLayout)) (rowPitch (marshalled :: SubresourceLayout)) (arrayPitch (marshalled :: SubresourceLayout)) (depthPitch (marshalled :: SubresourceLayout)))

-- | A function to read a 'VkSubresourceLayout' and all additional
-- structures in the pointer chain into a 'SubresourceLayout'.
fromCStructSubresourceLayout :: VkSubresourceLayout -> IO SubresourceLayout
fromCStructSubresourceLayout c = SubresourceLayout <$> pure (vkOffset (c :: VkSubresourceLayout))
                                                   <*> pure (vkSize (c :: VkSubresourceLayout))
                                                   <*> pure (vkRowPitch (c :: VkSubresourceLayout))
                                                   <*> pure (vkArrayPitch (c :: VkSubresourceLayout))
                                                   <*> pure (vkDepthPitch (c :: VkSubresourceLayout))

instance Zero SubresourceLayout where
  zero = SubresourceLayout zero
                           zero
                           zero
                           zero
                           zero



-- | vkCreateImage - Create a new image object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the image.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure
--     containing parameters to be used to create the image.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pImage@ points to a
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle in which
--     the resulting image object is returned.
--
-- == Valid Usage
--
-- -   If the @flags@ member of @pCreateInfo@ includes
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT',
--     creating this 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage'
--     /must/ not cause the total required sparse memory for all currently
--     valid sparse resources on the device to exceed
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@sparseAddressSpaceSize@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pImage@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
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
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'
createImage :: Device ->  ImageCreateInfo ->  Maybe AllocationCallbacks ->  IO (Image)
createImage = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pImage' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructImageCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateImage commandTable device' pCreateInfo' pAllocator pImage' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pImage')))))


-- | vkDestroyImage - Destroy an image object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the image.
--
-- -   @image@ is the image to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @image@, either directly or via
--     a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView', /must/ have
--     completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @image@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @image@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @image@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
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
destroyImage :: Device ->  Image ->  Maybe AllocationCallbacks ->  IO ()
destroyImage = \(Device device' commandTable) -> \image' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyImage commandTable device' image' pAllocator *> (pure ()))


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
-- -   @pLayout@ points to a
--     'Graphics.Vulkan.C.Core10.Image.VkSubresourceLayout' structure in
--     which the layout is returned.
--
-- = Description
--
-- The image /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#glossary-linear-resource linear>.
-- The returned layout is valid for
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-device-hostacces host access>.
--
-- 'Graphics.Vulkan.C.Core10.Image.vkGetImageSubresourceLayout' is
-- invariant for the lifetime of a single image.
--
-- == Valid Usage
--
-- -   @image@ /must/ have been created with @tiling@ equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR'
--
-- -   The @aspectMask@ member of @pSubresource@ /must/ only have a single
--     bit set
--
-- -   The @mipLevel@ member of @pSubresource@ /must/ be less than the
--     @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   The @arrayLayer@ member of @pSubresource@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @pSubresource@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageSubresource'
--     structure
--
-- -   @pLayout@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.Image.VkSubresourceLayout' structure
--
-- -   @image@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageSubresource',
-- 'Graphics.Vulkan.C.Core10.Image.VkSubresourceLayout'
getImageSubresourceLayout :: Device ->  Image ->  ImageSubresource ->  IO (SubresourceLayout)
getImageSubresourceLayout = \(Device device' commandTable) -> \image' -> \subresource' -> alloca (\pLayout' -> (\marshalled -> withCStructImageSubresource marshalled . flip with) subresource' (\pSubresource' -> vkGetImageSubresourceLayout commandTable device' image' pSubresource' pLayout' *> ((fromCStructSubresourceLayout <=< peek) pLayout')))

-- | A safe wrapper for 'createImage' and 'destroyImage' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withImage
  :: Device -> ImageCreateInfo -> Maybe (AllocationCallbacks) -> (Image -> IO a) -> IO a
withImage device imageCreateInfo allocationCallbacks = bracket
  (createImage device imageCreateInfo allocationCallbacks)
  (\o -> destroyImage device o allocationCallbacks)
