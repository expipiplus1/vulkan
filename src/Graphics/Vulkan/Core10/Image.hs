{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Image
  ( VkImageLayout(..)
  , pattern VK_IMAGE_LAYOUT_UNDEFINED
  , pattern VK_IMAGE_LAYOUT_GENERAL
  , pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_PREINITIALIZED
  , vkCreateImage
  , vkDestroyImage
  , vkGetImageSubresourceLayout
  , VkImageCreateInfo(..)
  , VkSubresourceLayout(..)
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
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


import Graphics.Vulkan.Core10.Buffer
  ( VkSharingMode(..)
  )
import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
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
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkImage
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkImageSubresource(..)
  )


-- ** VkImageLayout

-- | VkImageLayout - Layout of image and image subresources
--
-- = Description
--
-- The type(s) of device access supported by each layout are:
--
-- -   @VK_IMAGE_LAYOUT_UNDEFINED@ does not support device access. This
--     layout /must/ only be used as the @initialLayout@ member of
--     @VkImageCreateInfo@ or @VkAttachmentDescription@, or as the
--     @oldLayout@ in an image transition. When transitioning out of this
--     layout, the contents of the memory are not guaranteed to be
--     preserved.
--
-- -   @VK_IMAGE_LAYOUT_PREINITIALIZED@ does not support device access.
--     This layout /must/ only be used as the @initialLayout@ member of
--     @VkImageCreateInfo@ or @VkAttachmentDescription@, or as the
--     @oldLayout@ in an image transition. When transitioning out of this
--     layout, the contents of the memory are preserved. This layout is
--     intended to be used as the initial layout for an image whose
--     contents are written by the host, and hence the data /can/ be
--     written to memory immediately, without first executing a layout
--     transition. Currently, @VK_IMAGE_LAYOUT_PREINITIALIZED@ is only
--     useful with @VK_IMAGE_TILING_LINEAR@ images because there is not a
--     standard layout defined for @VK_IMAGE_TILING_OPTIMAL@ images.
--
-- -   @VK_IMAGE_LAYOUT_GENERAL@ supports all types of device access.
--
-- -   @VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL@ /must/ only be used as a
--     color or resolve attachment in a @VkFramebuffer@. This layout is
--     valid only for image subresources of images created with the
--     @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@ usage bit enabled.
--
-- -   @VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL@ /must/ only be
--     used as a depth\/stencil attachment in a @VkFramebuffer@. This
--     layout is valid only for image subresources of images created with
--     the @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ usage bit enabled.
--
-- -   @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@ /must/ only be
--     used as a read-only depth\/stencil attachment in a @VkFramebuffer@
--     and\/or as a read-only image in a shader (which /can/ be read as a
--     sampled image, combined image\/sampler and\/or input attachment).
--     This layout is valid only for image subresources of images created
--     with the @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ usage bit
--     enabled. Only image subresources of images created with
--     @VK_IMAGE_USAGE_SAMPLED_BIT@ /can/ be used as a sampled image or
--     combined image\/sampler in a shader. Similarly, only image
--     subresources of images created with
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ /can/ be used as input
--     attachments.
--
-- -   @VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL@: /must/
--     only be used as a depth\/stencil attachment in a @VkFramebuffer@,
--     where the depth aspect is read-only, and\/or as a read-only image in
--     a shader (which /can/ be read as a sampled image, combined
--     image\/sampler and\/or input attachment) where only the depth aspect
--     is accessed. This layout is valid only for image subresources of
--     images created with the
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ usage bit enabled.
--     Only image subresources of images created with
--     @VK_IMAGE_USAGE_SAMPLED_BIT@ /can/ be used as a sampled image or
--     combined image\/sampler in a shader. Similarly, only image
--     subresources of images created with
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ /can/ be used as input
--     attachments.
--
-- -   @VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL@: /must/
--     only be used as a depth\/stencil attachment in a @VkFramebuffer@,
--     where the stencil aspect is read-only, and\/or as a read-only image
--     in a shader (which /can/ be read as a sampled image, combined
--     image\/sampler and\/or input attachment) where only the stencil
--     aspect is accessed. This layout is valid only for image subresources
--     of images created with the
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ usage bit enabled.
--     Only image subresources of images created with
--     @VK_IMAGE_USAGE_SAMPLED_BIT@ /can/ be used as a sampled image or
--     combined image\/sampler in a shader. Similarly, only image
--     subresources of images created with
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ /can/ be used as input
--     attachments.
--
-- -   @VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL@ /must/ only be used as a
--     read-only image in a shader (which /can/ be read as a sampled image,
--     combined image\/sampler and\/or input attachment). This layout is
--     valid only for image subresources of images created with the
--     @VK_IMAGE_USAGE_SAMPLED_BIT@ or
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ usage bit enabled.
--
-- -   @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ /must/ only be used as a
--     source image of a transfer command (see the definition of
--     [@VK_PIPELINE_STAGE_TRANSFER_BIT@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer)).
--     This layout is valid only for image subresources of images created
--     with the @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ usage bit enabled.
--
-- -   @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ /must/ only be used as a
--     destination image of a transfer command. This layout is valid only
--     for image subresources of images created with the
--     @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ usage bit enabled.
--
-- -   @VK_IMAGE_LAYOUT_PRESENT_SRC_KHR@ /must/ only be used for presenting
--     a presentable image for display. A swapchain’s image /must/ be
--     transitioned to this layout before calling
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkQueuePresentKHR', and
--     /must/ be transitioned away from this layout after calling
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR'.
--
-- -   @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@ is valid only for shared
--     presentable images, and /must/ be used for any usage the image
--     supports.
--
-- The layout of each image subresource is not a state of the image
-- subresource itself, but is rather a property of how the data in memory
-- is organized, and thus for each mechanism of accessing an image in the
-- API the application /must/ specify a parameter or structure member that
-- indicates which image layout the image subresource(s) are considered to
-- be in when the image will be accessed. For transfer commands, this is a
-- parameter to the command (see
-- [{html_spec_relative}#clears](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#clears)
-- and
-- [{html_spec_relative}#copies](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies)).
-- For use as a framebuffer attachment, this is a member in the
-- substructures of the @VkRenderPassCreateInfo@ (see [Render
-- Pass](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass)).
-- For use in a descriptor set, this is a member in the
-- @VkDescriptorImageInfo@ structure (see
-- [{html_spec_relative}#descriptorsets-updates](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-updates)).
-- At the time that any command buffer command accessing an image executes
-- on any queue, the layouts of the image subresources that are accessed
-- /must/ all match the layout specified via the API controlling those
-- accesses.
--
-- When performing a layout transition on an image subresource, the old
-- layout value /must/ either equal the current layout of the image
-- subresource (at the time the transition executes), or else be
-- @VK_IMAGE_LAYOUT_UNDEFINED@ (implying that the contents of the image
-- subresource need not be preserved). The new layout used in a transition
-- /must/ not be @VK_IMAGE_LAYOUT_UNDEFINED@ or
-- @VK_IMAGE_LAYOUT_PREINITIALIZED@.
--
-- The image layout of each image subresource of a depth\/stencil image
-- created with @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@
-- is dependent on the last sample locations used to render to the image
-- subresource as a depth\/stencil attachment, thus applications /must/
-- provide the same sample locations that were last used to render to the
-- given image subresource whenever a layout transition of the image
-- subresource happens, otherwise the contents of the depth aspect of the
-- image subresource become undefined.
--
-- In addition, depth reads from a depth\/stencil attachment referring to
-- an image subresource range of a depth\/stencil image created with
-- @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ using
-- different sample locations than what have been last used to perform
-- depth writes to the image subresources of the same image subresource
-- range produce undefined results.
--
-- Similarly, depth writes to a depth\/stencil attachment referring to an
-- image subresource range of a depth\/stencil image created with
-- @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ using
-- different sample locations than what have been last used to perform
-- depth writes to the image subresources of the same image subresource
-- range make the contents of the depth aspect of those image subresources
-- undefined.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.Core10.Pass.VkAttachmentReference',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'VkImageCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBlitImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdResolveImage'
newtype VkImageLayout = VkImageLayout Int32
  deriving (Eq, Ord, Storable)

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
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageLayout")
                        v <- step readPrec
                        pure (VkImageLayout v)
                        )
                    )

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_UNDEFINED"
pattern VK_IMAGE_LAYOUT_UNDEFINED :: VkImageLayout
pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_GENERAL"
pattern VK_IMAGE_LAYOUT_GENERAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VkImageLayout 2

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = VkImageLayout 3

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = VkImageLayout 4

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VkImageLayout 5

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VkImageLayout 6

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VkImageLayout 7

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_PREINITIALIZED"
pattern VK_IMAGE_LAYOUT_PREINITIALIZED :: VkImageLayout
pattern VK_IMAGE_LAYOUT_PREINITIALIZED = VkImageLayout 8
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
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pImage@ points to a @VkImage@ handle in which the resulting image
--     object is returned.
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
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage', 'VkImageCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateImage" vkCreateImage :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult
-- | vkDestroyImage - Destroy an image object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the image.
--
-- -   @image@ is the image to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
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
-- -   If @image@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @image@ /must/ be a valid @VkImage@ handle
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyImage" vkDestroyImage :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
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
--     'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageSubresource'
--     structure selecting a specific image for the image subresource.
--
-- -   @pLayout@ points to a 'VkSubresourceLayout' structure in which the
--     layout is returned.
--
-- = Description
--
-- If the 'Graphics.Vulkan.Core10.Core.VkFormat' of @image@ is a
-- [multi-planar
-- format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
-- @vkGetImageSubresourceLayout@ describes one plane of the image.
--
-- 'vkGetImageSubresourceLayout' is invariant for the lifetime of a single
-- image. However, the subresource layout of images in Android hardware
-- buffer external memory isn’t known until the image has been bound to
-- memory, so calling @vkGetImageSubresourceLayout@ for such an image
-- before it has been bound will result in undefined behavior.
--
-- == Valid Usage
--
-- -   @image@ /must/ have been created with @tiling@ equal to
--     @VK_IMAGE_TILING_LINEAR@
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
-- -   If the @format@ of @image@ is a [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     with two planes, the @aspectMask@ member of @pSubresource@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@ or @VK_IMAGE_ASPECT_PLANE_1_BIT@
--
-- -   If the @format@ of @image@ is a [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     with three planes, the @aspectMask@ member of @pSubresource@ /must/
--     be @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@ or
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@
--
-- -   If @image@ was created with the
--     VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
--     external memory handle type, then @image@ /must/ be bound to memory.
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageSubresource',
-- 'VkSubresourceLayout'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageSubresourceLayout" vkGetImageSubresourceLayout :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ()
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
-- Implementations /may/ support additional limits and capabilities beyond
-- those listed above.
--
-- To query an implementation’s specific capabilities for a given
-- combination of @format@, @imageType@, @tiling@, @usage@,
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'::@handleTypes@
-- and @flags@, call
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'.
-- The return value specifies whether that combination of image settings is
-- supported. On success, the @VkImageFormatProperties@ output parameter
-- specifies the set of valid @samples@ bits and the limits for @extent@,
-- @mipLevels@, @arrayLayers@, and @maxResourceSize@. Even if
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'.
-- returns success and the parameters to vkCreateImage are all within the
-- returned limits, @vkCreateImage@ /must/ fail and return
-- @VK_ERROR_OUT_OF_DEVICE_MEMORY@ if the resulting size of the image would
-- be larger than @maxResourceSize@.
--
-- To determine the set of valid @usage@ bits for a given format, call
-- 'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'.
--
-- __Note__
--
-- For images created without @VK_IMAGE_CREATE_EXTENDED_USAGE_BIT@ a
-- @usage@ bit is valid if it is supported for the format the image is
-- created with.
--
-- For images created with @VK_IMAGE_CREATE_EXTENDED_USAGE_BIT@ a @usage@
-- bit is valid if it is supported for at least one of the formats a
-- @VkImageView@ created from the image /can/ have (see [Image
-- Views](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-views)
-- for more detail).
--
-- == Valid Usage
--
-- -   If the @pNext@ chain doesn’t contain an instance of
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID',
--     or if @format@ is not VK_FORMAT_UNDEFINED, the combination of
--     @format@, @imageType@, @tiling@, @usage@, and @flags@ /must/ be
--     supported, as indicated by a @VK_SUCCESS@ return value from
--     @vkGetPhysicalDeviceImageFormatProperties@ invoked with the same
--     values passed to the corresponding parameters.
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
--     'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   @format@ /must/ not be @VK_FORMAT_UNDEFINED@
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
-- -   If @flags@ contains @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@,
--     @imageType@ /must/ be @VK_IMAGE_TYPE_3D@
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_1D@, @extent.width@ /must/ be less
--     than or equal to @VkPhysicalDeviceLimits@::@maxImageDimension1D@, or
--     @VkImageFormatProperties@::@maxExtent.width@ (as returned by
--     @vkGetPhysicalDeviceImageFormatProperties@ with @format@,
--     @imageType@, @tiling@, @usage@, and @flags@ equal to those in this
--     structure) - whichever is higher
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_2D@ and @flags@ does not contain
--     @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@, @extent.width@ and
--     @extent.height@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxImageDimension2D@, or
--     @VkImageFormatProperties@::@maxExtent.width@\/@height@ (as returned
--     by @vkGetPhysicalDeviceImageFormatProperties@ with @format@,
--     @imageType@, @tiling@, @usage@, and @flags@ equal to those in this
--     structure) - whichever is higher
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_2D@ and @flags@ contains
--     @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@, @extent.width@ and
--     @extent.height@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxImageDimensionCube@, or
--     @VkImageFormatProperties@::@maxExtent.width@\/@height@ (as returned
--     by @vkGetPhysicalDeviceImageFormatProperties@ with @format@,
--     @imageType@, @tiling@, @usage@, and @flags@ equal to those in this
--     structure) - whichever is higher
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_2D@ and @flags@ contains
--     @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@, @extent.width@ and
--     @extent.height@ /must/ be equal and @arrayLayers@ /must/ be greater
--     than or equal to 6
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_3D@, @extent.width@,
--     @extent.height@ and @extent.depth@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxImageDimension3D@, or
--     @VkImageFormatProperties@::@maxExtent.width@\/@height@\/@depth@ (as
--     returned by @vkGetPhysicalDeviceImageFormatProperties@ with
--     @format@, @imageType@, @tiling@, @usage@, and @flags@ equal to those
--     in this structure) - whichever is higher
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_1D@, both @extent.height@ and
--     @extent.depth@ /must/ be @1@
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_2D@, @extent.depth@ /must/ be @1@
--
-- -   @mipLevels@ /must/ be less than or equal to
--     ⌊log2(max(@extent.width@, @extent.height@, @extent.depth@))⌋ + 1.
--
-- -   @mipLevels@ /must/ be less than or equal to
--     @VkImageFormatProperties@::@maxMipLevels@ (as returned by
--     @vkGetPhysicalDeviceImageFormatProperties@ with @format@,
--     @imageType@, @tiling@, @usage@, and @flags@ equal to those in this
--     structure)
--
-- -   @arrayLayers@ /must/ be less than or equal to
--     @VkImageFormatProperties@::@maxArrayLayers@ (as returned by
--     @vkGetPhysicalDeviceImageFormatProperties@ with @format@,
--     @imageType@, @tiling@, @usage@, and @flags@ equal to those in this
--     structure)
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_3D@, @arrayLayers@ /must/ be @1@.
--
-- -   If @samples@ is not @VK_SAMPLE_COUNT_1_BIT@, @imageType@ /must/ be
--     @VK_IMAGE_TYPE_2D@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@, @tiling@ /must/ be
--     @VK_IMAGE_TILING_OPTIMAL@, and @mipLevels@ /must/ be equal to @1@
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
-- -   If @usage@ includes @VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT@,
--     @usage@ /must/ also contain at least one of
--     @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@,
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@, or
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@.
--
-- -   @samples@ /must/ be a bit value that is set in
--     @VkImageFormatProperties@::@sampleCounts@ returned by
--     @vkGetPhysicalDeviceImageFormatProperties@ with @format@,
--     @imageType@, @tiling@, @usage@, and @flags@ equal to those in this
--     structure
--
-- -   If the [multisampled storage
--     images](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-shaderStorageImageMultisample)
--     feature is not enabled, and @usage@ contains
--     @VK_IMAGE_USAGE_STORAGE_BIT@, @samples@ /must/ be
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If the [sparse
--     bindings](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseBinding)
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@
--
-- -   If the [sparse aliased
--     residency](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseResidencyAliased)
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_1D@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the [sparse residency for 2D
--     images](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseResidencyImage2D)
--     feature is not enabled, and @imageType@ is @VK_IMAGE_TYPE_2D@,
--     @flags@ /must/ not contain @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the [sparse residency for 3D
--     images](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseResidencyImage3D)
--     feature is not enabled, and @imageType@ is @VK_IMAGE_TYPE_3D@,
--     @flags@ /must/ not contain @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the [sparse residency for images with 2
--     samples](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseResidency2Samples)
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_2_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the [sparse residency for images with 4
--     samples](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseResidency4Samples)
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_4_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the [sparse residency for images with 8
--     samples](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseResidency8Samples)
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_8_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the [sparse residency for images with 16
--     samples](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sparseResidency16Samples)
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_16_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_LINEAR@, @format@ /must/ be a format
--     that has at least one supported feature bit present in the value of
--     @VkFormatProperties@::@linearTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_LINEAR@, and
--     @VkFormatProperties@::@linearTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@,
--     @usage@ /must/ not contain @VK_IMAGE_USAGE_SAMPLED_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_LINEAR@, and
--     @VkFormatProperties@::@linearTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include @VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT@,
--     @usage@ /must/ not contain @VK_IMAGE_USAGE_STORAGE_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_LINEAR@, and
--     @VkFormatProperties@::@linearTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include @VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT@,
--     @usage@ /must/ not contain @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_LINEAR@, and
--     @VkFormatProperties@::@linearTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include
--     @VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT@, @usage@ /must/ not
--     contain @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_OPTIMAL@, @format@ /must/ be a
--     format that has at least one supported feature bit present in the
--     value of @VkFormatProperties@::@optimalTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_OPTIMAL@, and
--     @VkFormatProperties@::@optimalTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@,
--     @usage@ /must/ not contain @VK_IMAGE_USAGE_SAMPLED_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_OPTIMAL@, and
--     @VkFormatProperties@::@optimalTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include @VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT@,
--     @usage@ /must/ not contain @VK_IMAGE_USAGE_STORAGE_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_OPTIMAL@, and
--     @VkFormatProperties@::@optimalTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include @VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT@,
--     @usage@ /must/ not contain @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_OPTIMAL@, and
--     @VkFormatProperties@::@optimalTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include
--     @VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT@, @usage@ /must/ not
--     contain @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@
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
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV',
--     it /must/ not contain an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'.
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
--     its @handleTypes@ member /must/ only contain bits that are also in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'::@externalMemoryProperties@::@compatibleHandleTypes@,
--     as returned by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--     with @format@, @imageType@, @tiling@, @usage@, and @flags@ equal to
--     those in this structure, and with an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
--     in the @pNext@ chain, with a @handleType@ equal to any one of the
--     handle types specified in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'::@handleTypes@
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV',
--     its @handleTypes@ member /must/ only contain bits that are also in
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV'::@externalMemoryProperties@::@compatibleHandleTypes@,
--     as returned by
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV'
--     with @format@, @imageType@, @tiling@, @usage@, and @flags@ equal to
--     those in this structure, and with @externalHandleType@ equal to any
--     one of the handle types specified in
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV'::@handleTypes@
--
-- -   If the logical device was created with
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo'::@physicalDeviceCount@
--     equal to 1, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT@
--
-- -   If @flags@ contains
--     @VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT@, then @mipLevels@
--     /must/ be one, @arrayLayers@ /must/ be one, @imageType@ /must/ be
--     @VK_IMAGE_TYPE_2D@, and @tiling@ /must/ be @VK_IMAGE_TILING_OPTIMAL@
--
-- -   If @flags@ contains
--     @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@, then @format@
--     /must/ be a [block-compressed image
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#appendix-compressedtex-bc),
--     an [ETC compressed image
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#appendix-compressedtex-etc2),
--     or an [ASTC compressed image
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#appendix-compressedtex-astc).
--
-- -   If @flags@ contains
--     @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@, then @flags@
--     /must/ also contain @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@.
--
-- -   @initialLayout@ /must/ be @VK_IMAGE_LAYOUT_UNDEFINED@ or
--     @VK_IMAGE_LAYOUT_PREINITIALIZED@.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
--     or
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV'
--     structure whose @handleTypes@ member is not @0@, @initialLayout@
--     /must/ be @VK_IMAGE_LAYOUT_UNDEFINED@
--
-- -   If the image @format@ is one of those listed in
--     [{html_spec_relative}#features-formats-requiring-sampler-ycbcr-conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion):
--
--     -   @mipLevels@ /must/ be 1
--
--     -   @samples@ must be @VK_SAMPLE_COUNT_1_BIT@
--
--     -   @imageType@ /must/ be @VK_IMAGE_TYPE_2D@
--
--     -   @arrayLayers@ /must/ be 1
--
-- -   If @tiling@ is @VK_IMAGE_TILING_OPTIMAL@, @format@ is a
--     /multi-planar/ format, and
--     @VkFormatProperties@::@optimalTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include @VK_FORMAT_FEATURE_DISJOINT_BIT@, @flags@
--     /must/ not contain @VK_IMAGE_CREATE_DISJOINT_BIT@
--
-- -   If @tiling@ is @VK_IMAGE_TILING_LINEAR@, @format@ is a
--     /multi-planar/ format, and
--     @VkFormatProperties@::@linearTilingFeatures@ (as returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@) does not include @VK_FORMAT_FEATURE_DISJOINT_BIT@, @flags@
--     /must/ not contain @VK_IMAGE_CREATE_DISJOINT_BIT@
--
-- -   If @format@ is not a /multi-planar/ format, and @flags@ does not
--     include @VK_IMAGE_CREATE_ALIAS_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_DISJOINT_BIT@
--
-- -   If @flags@ contains
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ @format@
--     /must/ be a depth or depth\/stencil format
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
--     structure whose @handleTypes@ member includes
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@:
--
--     -   @imageType@ /must/ be @VK_IMAGE_TYPE_2D@
--
--     -   @mipLevels@ /must/ either be @1@ or equal to
--         ⌊log2(max(@extent.width@, @extent.height@, @extent.depth@))⌋ +
--         1.
--
--     -   If @format@ is not @VK_FORMAT_UNDEFINED@, then @format@,
--         @imageType@, @tiling@, @usage@, @flags@, @mipLevels@, and
--         @samples@ /must/ be supported with
--         @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@
--         external memory handle types according to
--         'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--
--     -   If @format@ is @VK_FORMAT_UNDEFINED@, then the @pNext@ chain
--         /must/ include a
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--         structure whose @externalFormat@ member is not @0@
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@:
--
--     -   @format@ /must/ be @VK_FORMAT_UNDEFINED@
--
--     -   @flags@ /must/ not include VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
--
--     -   @usage@ /must/ not include any usages except
--         @VK_IMAGE_USAGE_SAMPLED_BIT@
--
--     -   @tiling@ /must/ be @VK_IMAGE_TILING_OPTIMAL@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationImageCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID',
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR',
--     or
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkImageSwapchainCreateInfoKHR'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkImageCreateFlagBits'
--     values
--
-- -   @imageType@ /must/ be a valid
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkImageType' value
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.Core10.Core.VkFormat'
--     value
--
-- -   @samples@ /must/ be a valid
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
--     value
--
-- -   @tiling@ /must/ be a valid
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkImageTiling' value
--
-- -   @usage@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkImageUsageFlagBits'
--     values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @sharingMode@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Buffer.VkSharingMode' value
--
-- -   @initialLayout@ /must/ be a valid 'VkImageLayout' value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageCreateFlags',
-- 'VkImageLayout',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- 'Graphics.Vulkan.Core10.Buffer.VkSharingMode',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateImage'
data VkImageCreateInfo = VkImageCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageCreateFlagBits'
  -- describing additional parameters of the image.
  vkFlags :: VkImageCreateFlags
  , -- | @imageType@ is a
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageType' value
  -- specifying the basic dimensionality of the image. Layers in array
  -- textures do not count as a dimension for the purposes of the image type.
  vkImageType :: VkImageType
  , -- | @format@ is a 'Graphics.Vulkan.Core10.Core.VkFormat' describing the
  -- format and type of the data elements that will be contained in the
  -- image.
  vkFormat :: VkFormat
  , -- | @extent@ is a 'Graphics.Vulkan.Core10.DeviceInitialization.VkExtent3D'
  -- describing the number of data elements in each dimension of the base
  -- level.
  vkExtent :: VkExtent3D
  , -- | @mipLevels@ describes the number of levels of detail available for
  -- minified sampling of the image.
  vkMipLevels :: Word32
  , -- | @arrayLayers@ is the number of layers in the image.
  vkArrayLayers :: Word32
  , -- | @samples@ is the number of sub-data element samples in the image as
  -- defined in
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'. See
  -- [Multisampling](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primsrast-multisampling).
  vkSamples :: VkSampleCountFlagBits
  , -- | @tiling@ is a
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageTiling' value
  -- specifying the tiling arrangement of the data elements in memory.
  vkTiling :: VkImageTiling
  , -- | @usage@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageUsageFlagBits'
  -- describing the intended usage of the image.
  vkUsage :: VkImageUsageFlags
  , -- | @sharingMode@ is a 'Graphics.Vulkan.Core10.Buffer.VkSharingMode' value
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
  -- 'VkImageLayout' of all image subresources of the image. See [Image
  -- Layouts](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-layouts).
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
-- | VkSubresourceLayout - Structure specifying subresource layout
--
-- = Description
--
-- For images created with linear tiling, @rowPitch@, @arrayPitch@ and
-- @depthPitch@ describe the layout of the image subresource in linear
-- memory. For uncompressed formats, @rowPitch@ is the number of bytes
-- between texels with the same x coordinate in adjacent rows (y
-- coordinates differ by one). @arrayPitch@ is the number of bytes between
-- texels with the same x and y coordinate in adjacent array layers of the
-- image (array layer values differ by one). @depthPitch@ is the number of
-- bytes between texels with the same x and y coordinate in adjacent slices
-- of a 3D image (z coordinates differ by one). Expressed as an addressing
-- formula, the starting byte of a texel in the image subresource has
-- address:
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
-- @arrayPitch@ is undefined for images that were not created as arrays.
-- @depthPitch@ is defined only for 3D images.
--
-- For /single-plane/ color formats, the @aspectMask@ member of
-- @VkImageSubresource@ /must/ be @VK_IMAGE_ASPECT_COLOR_BIT@. For
-- depth\/stencil formats, @aspectMask@ /must/ be either
-- @VK_IMAGE_ASPECT_DEPTH_BIT@ or @VK_IMAGE_ASPECT_STENCIL_BIT@. On
-- implementations that store depth and stencil aspects separately,
-- querying each of these image subresource layouts will return a different
-- @offset@ and @size@ representing the region of memory used for that
-- aspect. On implementations that store depth and stencil aspects
-- interleaved, the same @offset@ and @size@ are returned and represent the
-- interleaved memory allocation.
--
-- For [multi-planar
-- formats](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
-- the @aspectMask@ member of @VkImageSubresource@ /must/ be
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or (for
-- 3-plane formats only) @VK_IMAGE_ASPECT_PLANE_2_BIT@. Querying each of
-- these image subresource layouts will return a different @offset@ and
-- @size@ representing the region of memory used for that plane.
--
-- = See Also
--
-- @VkDeviceSize@, 'vkGetImageSubresourceLayout'
data VkSubresourceLayout = VkSubresourceLayout
  { -- | @offset@ is the byte offset from the start of the image where the image
  -- subresource begins.
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
