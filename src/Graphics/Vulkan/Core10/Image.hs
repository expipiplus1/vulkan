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


import Graphics.Vulkan.Core10.Buffer
  ( VkSharingMode(..)
  )
import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  , VkStructureType(..)
  , VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkImageUsageFlags
  , VkImageTiling(..)
  , VkSampleCountFlagBits(..)
  , VkExtent3D(..)
  , VkImageType(..)
  , VkImageCreateFlags
  , VkAllocationCallbacks(..)
  , VkDevice
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
-- #_description#
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
-- -   @VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL@ /must/ only be used as a
--     read-only image in a shader (which /can/ be read as a sampled image,
--     combined image\/sampler and\/or input attachment). This layout is
--     valid only for image subresources of images created with the
--     @VK_IMAGE_USAGE_SAMPLED_BIT@ or
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ usage bit enabled.
--
-- -   @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ /must/ only be used as a
--     source image of a transfer command (see the definition of
--     <{html_spec_relative}#synchronization-pipeline-stages-transfer VK_PIPELINE_STAGE_TRANSFER_BIT>).
--     This layout is valid only for image subresources of images created
--     with the @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ usage bit enabled.
--
-- -   @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ /must/ only be used as a
--     destination image of a transfer command. This layout is valid only
--     for image subresources of images created with the
--     @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ usage bit enabled.
--
-- The layout of each image subresource is not a state of the image
-- subresource itself, but is rather a property of how the data in memory
-- is organized, and thus for each mechanism of accessing an image in the
-- API the application /must/ specify a parameter or structure member that
-- indicates which image layout the image subresource(s) are considered to
-- be in when the image will be accessed. For transfer commands, this is a
-- parameter to the command (see
-- <{html_spec_relative}#clears {html_spec_relative}#clears> and
-- <{html_spec_relative}#copies {html_spec_relative}#copies>). For use as a
-- framebuffer attachment, this is a member in the substructures of the
-- @VkRenderPassCreateInfo@ (see
-- <{html_spec_relative}#renderpass Render Pass>). For use in a descriptor
-- set, this is a member in the @VkDescriptorImageInfo@ structure (see
-- <{html_spec_relative}#descriptorsets-updates {html_spec_relative}#descriptorsets-updates>).
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
-- = See Also
-- #_see_also#
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
-- #_parameters#
--
-- -   @device@ is the logical device that creates the image.
--
-- -   @pCreateInfo@ is a pointer to an instance of the @VkImageCreateInfo@
--     structure containing parameters to be used to create the image.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pImage@ points to a @VkImage@ handle in which the resulting image
--     object is returned.
--
-- = Description
-- #_description#
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
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage', 'VkImageCreateInfo'
foreign import ccall "vkCreateImage" vkCreateImage :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult
-- | vkDestroyImage - Destroy an image object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that destroys the image.
--
-- -   @image@ is the image to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- = Description
-- #_description#
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
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage'
foreign import ccall "vkDestroyImage" vkDestroyImage :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkGetImageSubresourceLayout - Retrieve information about an image
-- subresource
--
-- = Parameters
-- #_parameters#
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
-- #_description#
--
-- 'vkGetImageSubresourceLayout' is invariant for the lifetime of a single
-- image.
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
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageSubresource',
-- 'VkSubresourceLayout'
foreign import ccall "vkGetImageSubresourceLayout" vkGetImageSubresourceLayout :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ()
-- | VkImageCreateInfo - Structure specifying the parameters of a newly
-- created image object
--
-- = Description
-- #_description#
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
-- combination of @format@, @imageType@, @tiling@, @usage@, and @flags@,
-- call
-- 'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'.
-- The return value specifies whether that combination of image settings is
-- supported. On success, the @VkImageFormatProperties@ output parameter
-- specifies the set of valid @samples@ bits and the limits for @extent@,
-- @mipLevels@, @arrayLayers@, and @maxResourceSize@. Even if
-- 'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'.
-- returns success and the parameters to vkCreateImage are all within the
-- returned limits, @vkCreateImage@ /must/ fail and return
-- @VK_ERROR_OUT_OF_DEVICE_MEMORY@ if the resulting size of the image would
-- be larger than @maxResourceSize@.
--
-- To determine the set of valid @usage@ bits for a given format, call
-- 'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'.
--
-- == Valid Usage
--
-- -   The combination of @format@, @imageType@, @tiling@, @usage@, and
--     @flags@ /must/ be supported, as indicated by a @VK_SUCCESS@ return
--     value from @vkGetPhysicalDeviceImageFormatProperties@ invoked with
--     the same values passed to the corresponding parameters.
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
--     @pQueueFamilyPropertyCount@ returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
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
-- -   If the
--     <{html_spec_relative}#features-features-shaderStorageImageMultisample multisampled storage images>
--     feature is not enabled, and @usage@ contains
--     @VK_IMAGE_USAGE_STORAGE_BIT@, @samples@ /must/ be
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-sparseBinding sparse bindings>
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-sparseResidencyAliased sparse aliased residency>
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@
--
-- -   If @imageType@ is @VK_IMAGE_TYPE_1D@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-sparseResidencyImage2D sparse residency for 2D images>
--     feature is not enabled, and @imageType@ is @VK_IMAGE_TYPE_2D@,
--     @flags@ /must/ not contain @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-sparseResidencyImage3D sparse residency for 3D images>
--     feature is not enabled, and @imageType@ is @VK_IMAGE_TYPE_3D@,
--     @flags@ /must/ not contain @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-sparseResidency2Samples sparse residency for images with 2 samples>
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_2_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-sparseResidency4Samples sparse residency for images with 4 samples>
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_4_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-sparseResidency8Samples sparse residency for images with 8 samples>
--     feature is not enabled, @imageType@ is @VK_IMAGE_TYPE_2D@, and
--     @samples@ is @VK_SAMPLE_COUNT_8_BIT@, @flags@ /must/ not contain
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-sparseResidency16Samples sparse residency for images with 16 samples>
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
-- -   @initialLayout@ /must/ be @VK_IMAGE_LAYOUT_UNDEFINED@ or
--     @VK_IMAGE_LAYOUT_PREINITIALIZED@.
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
-- #_see_also#
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
  { -- No documentation found for Nested "VkImageCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageCreateInfo" "vkFlags"
  vkFlags :: VkImageCreateFlags
  , -- No documentation found for Nested "VkImageCreateInfo" "vkImageType"
  vkImageType :: VkImageType
  , -- No documentation found for Nested "VkImageCreateInfo" "vkFormat"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkImageCreateInfo" "vkExtent"
  vkExtent :: VkExtent3D
  , -- No documentation found for Nested "VkImageCreateInfo" "vkMipLevels"
  vkMipLevels :: Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "vkArrayLayers"
  vkArrayLayers :: Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "vkSamples"
  vkSamples :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkImageCreateInfo" "vkTiling"
  vkTiling :: VkImageTiling
  , -- No documentation found for Nested "VkImageCreateInfo" "vkUsage"
  vkUsage :: VkImageUsageFlags
  , -- No documentation found for Nested "VkImageCreateInfo" "vkSharingMode"
  vkSharingMode :: VkSharingMode
  , -- No documentation found for Nested "VkImageCreateInfo" "vkQueueFamilyIndexCount"
  vkQueueFamilyIndexCount :: Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "vkPQueueFamilyIndices"
  vkPQueueFamilyIndices :: Ptr Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "vkInitialLayout"
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
-- #_description#
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
-- For color formats, the @aspectMask@ member of @VkImageSubresource@
-- /must/ be @VK_IMAGE_ASPECT_COLOR_BIT@. For depth\/stencil formats,
-- @aspectMask@ /must/ be either @VK_IMAGE_ASPECT_DEPTH_BIT@ or
-- @VK_IMAGE_ASPECT_STENCIL_BIT@. On implementations that store depth and
-- stencil aspects separately, querying each of these image subresource
-- layouts will return a different @offset@ and @size@ representing the
-- region of memory used for that aspect. On implementations that store
-- depth and stencil aspects interleaved, the same @offset@ and @size@ are
-- returned and represent the interleaved memory allocation.
--
-- = See Also
-- #_see_also#
--
-- @VkDeviceSize@, 'vkGetImageSubresourceLayout'
data VkSubresourceLayout = VkSubresourceLayout
  { -- No documentation found for Nested "VkSubresourceLayout" "vkOffset"
  vkOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "vkSize"
  vkSize :: VkDeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "vkRowPitch"
  vkRowPitch :: VkDeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "vkArrayPitch"
  vkArrayPitch :: VkDeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "vkDepthPitch"
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
