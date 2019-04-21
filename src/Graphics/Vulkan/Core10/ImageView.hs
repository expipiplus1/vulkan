{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.ImageView
  ( withCStructComponentMapping
  , fromCStructComponentMapping
  , ComponentMapping(..)
  , ComponentSwizzle
  , pattern COMPONENT_SWIZZLE_IDENTITY
  , pattern COMPONENT_SWIZZLE_ZERO
  , pattern COMPONENT_SWIZZLE_ONE
  , pattern COMPONENT_SWIZZLE_R
  , pattern COMPONENT_SWIZZLE_G
  , pattern COMPONENT_SWIZZLE_B
  , pattern COMPONENT_SWIZZLE_A
  , withCStructImageSubresourceRange
  , fromCStructImageSubresourceRange
  , ImageSubresourceRange(..)
  , ImageView
  , ImageViewCreateFlagBits
  , ImageViewCreateFlags
  , withCStructImageViewCreateInfo
  , fromCStructImageViewCreateInfo
  , ImageViewCreateInfo(..)
  , ImageViewType
  , pattern IMAGE_VIEW_TYPE_1D
  , pattern IMAGE_VIEW_TYPE_2D
  , pattern IMAGE_VIEW_TYPE_3D
  , pattern IMAGE_VIEW_TYPE_CUBE
  , pattern IMAGE_VIEW_TYPE_1D_ARRAY
  , pattern IMAGE_VIEW_TYPE_2D_ARRAY
  , pattern IMAGE_VIEW_TYPE_CUBE_ARRAY
  , createImageView
  , destroyImageView
  , withImageView
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
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping(..)
  , VkComponentSwizzle(..)
  , VkImageSubresourceRange(..)
  , VkImageViewCreateFlagBits(..)
  , VkImageViewCreateInfo(..)
  , VkImageViewType(..)
  , VkImageView
  , vkCreateImageView
  , vkDestroyImageView
  , pattern VK_COMPONENT_SWIZZLE_A
  , pattern VK_COMPONENT_SWIZZLE_B
  , pattern VK_COMPONENT_SWIZZLE_G
  , pattern VK_COMPONENT_SWIZZLE_IDENTITY
  , pattern VK_COMPONENT_SWIZZLE_ONE
  , pattern VK_COMPONENT_SWIZZLE_R
  , pattern VK_COMPONENT_SWIZZLE_ZERO
  , pattern VK_IMAGE_VIEW_TYPE_1D
  , pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_2D
  , pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_3D
  , pattern VK_IMAGE_VIEW_TYPE_CUBE
  , pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlags
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )



-- | VkComponentMapping - Structure specifying a color component mapping
--
-- = Description
--
-- Unresolved directive in VkComponentMapping.txt -
-- include::{generated}\/validity\/structs\/VkComponentMapping.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkComponentSwizzle',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
data ComponentMapping = ComponentMapping
  { -- No documentation found for Nested "ComponentMapping" "r"
  r :: ComponentSwizzle
  , -- No documentation found for Nested "ComponentMapping" "g"
  g :: ComponentSwizzle
  , -- No documentation found for Nested "ComponentMapping" "b"
  b :: ComponentSwizzle
  , -- No documentation found for Nested "ComponentMapping" "a"
  a :: ComponentSwizzle
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkComponentMapping' and
-- marshal a 'ComponentMapping' into it. The 'VkComponentMapping' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructComponentMapping :: ComponentMapping -> (VkComponentMapping -> IO a) -> IO a
withCStructComponentMapping marshalled cont = cont (VkComponentMapping (r (marshalled :: ComponentMapping)) (g (marshalled :: ComponentMapping)) (b (marshalled :: ComponentMapping)) (a (marshalled :: ComponentMapping)))

-- | A function to read a 'VkComponentMapping' and all additional
-- structures in the pointer chain into a 'ComponentMapping'.
fromCStructComponentMapping :: VkComponentMapping -> IO ComponentMapping
fromCStructComponentMapping c = ComponentMapping <$> pure (vkR (c :: VkComponentMapping))
                                                 <*> pure (vkG (c :: VkComponentMapping))
                                                 <*> pure (vkB (c :: VkComponentMapping))
                                                 <*> pure (vkA (c :: VkComponentMapping))

instance Zero ComponentMapping where
  zero = ComponentMapping zero
                          zero
                          zero
                          zero


-- | VkComponentSwizzle - Specify how a component is swizzled
--
-- = Description
--
-- Setting the identity swizzle on a component is equivalent to setting the
-- identity mapping on that component. That is:
--
-- > +-----------------------------------+-----------------------------------+
-- > | Component                         | Identity Mapping                  |
-- > +===================================+===================================+
-- > | @components.r@                    | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |                                   | ew.VK_COMPONENT_SWIZZLE_R'        |
-- > +-----------------------------------+-----------------------------------+
-- > | @components.g@                    | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |                                   | ew.VK_COMPONENT_SWIZZLE_G'        |
-- > +-----------------------------------+-----------------------------------+
-- > | @components.b@                    | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |                                   | ew.VK_COMPONENT_SWIZZLE_B'        |
-- > +-----------------------------------+-----------------------------------+
-- > | @components.a@                    | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |                                   | ew.VK_COMPONENT_SWIZZLE_A'        |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Component Mappings Equivalent To
-- > 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkComponentMapping'
type ComponentSwizzle = VkComponentSwizzle


-- | 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
-- specifies that the component is set to the identity swizzle.
pattern COMPONENT_SWIZZLE_IDENTITY :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_IDENTITY = VK_COMPONENT_SWIZZLE_IDENTITY


-- | 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ZERO' specifies
-- that the component is set to zero.
pattern COMPONENT_SWIZZLE_ZERO :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_ZERO = VK_COMPONENT_SWIZZLE_ZERO


-- | 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ONE' specifies
-- that the component is set to either 1 or 1.0, depending on whether the
-- type of the image view format is integer or floating-point respectively,
-- as determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-definition Format Definition>
-- section for each 'Graphics.Vulkan.C.Core10.Core.VkFormat'.
pattern COMPONENT_SWIZZLE_ONE :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_ONE = VK_COMPONENT_SWIZZLE_ONE


-- | 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_R' specifies
-- that the component is set to the value of the R component of the image.
pattern COMPONENT_SWIZZLE_R :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_R = VK_COMPONENT_SWIZZLE_R


-- | 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_G' specifies
-- that the component is set to the value of the G component of the image.
pattern COMPONENT_SWIZZLE_G :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_G = VK_COMPONENT_SWIZZLE_G


-- | 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_B' specifies
-- that the component is set to the value of the B component of the image.
pattern COMPONENT_SWIZZLE_B :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_B = VK_COMPONENT_SWIZZLE_B


-- | 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_A' specifies
-- that the component is set to the value of the A component of the image.
pattern COMPONENT_SWIZZLE_A :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_A = VK_COMPONENT_SWIZZLE_A


-- | VkImageSubresourceRange - Structure specifying an image subresource
-- range
--
-- = Description
--
-- The number of mipmap levels and array layers /must/ be a subset of the
-- image subresources in the image. If an application wants to use all mip
-- levels or layers in an image after the @baseMipLevel@ or
-- @baseArrayLayer@, it /can/ set @levelCount@ and @layerCount@ to the
-- special values
-- 'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_MIP_LEVELS' and
-- 'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_ARRAY_LAYERS' without
-- knowing the exact number of mip levels or layers.
--
-- For cube and cube array image views, the layers of the image view
-- starting at @baseArrayLayer@ correspond to faces in the order +X, -X,
-- +Y, -Y, +Z, -Z. For cube arrays, each set of six sequential layers is a
-- single cube, so the number of cube maps in a cube map array view is
-- /@layerCount@ \/ 6/, and image array layer (@baseArrayLayer@ + i) is
-- face index (i mod 6) of cube /i \/ 6/. If the number of layers in the
-- view, whether set explicitly in @layerCount@ or implied by
-- 'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_ARRAY_LAYERS', is not a
-- multiple of 6, the last cube map in the array /must/ not be accessed.
--
-- @aspectMask@ /must/ be only
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
-- or
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'
-- if @format@ is a color, depth-only or stencil-only format, respectively,
-- except if @format@ is a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>.
-- If using a depth\/stencil format with both depth and stencil components,
-- @aspectMask@ /must/ include at least one of
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
-- and
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT',
-- and /can/ include both.
--
-- When the 'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'
-- structure is used to select a subset of the slices of a 3D image’s mip
-- level in order to create a 2D or 2D array image view of a 3D image
-- created with
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT',
-- @baseArrayLayer@ and @layerCount@ specify the first slice index and the
-- number of slices to include in the created image view. Such an image
-- view /can/ be used as a framebuffer attachment that refers only to the
-- specified range of slices of the selected mip level. However, any layout
-- transitions performed on such an attachment view during a render pass
-- instance still apply to the entire subresource referenced which includes
-- all the slices of the selected mip level.
--
-- When using an image view of a depth\/stencil image to populate a
-- descriptor set (e.g. for sampling in the shader, or for use as an input
-- attachment), the @aspectMask@ /must/ only include one bit and selects
-- whether the image view is used for depth reads (i.e. using a
-- floating-point sampler or input attachment in the shader) or stencil
-- reads (i.e. using an unsigned integer sampler or input attachment in the
-- shader). When an image view of a depth\/stencil image is used as a
-- depth\/stencil framebuffer attachment, the @aspectMask@ is ignored and
-- both depth and stencil image subresources are used.
--
-- The @components@ member is of type
-- 'Graphics.Vulkan.C.Core10.ImageView.VkComponentMapping', and describes a
-- remapping from components of the image to components of the vector
-- returned by shader image instructions. This remapping /must/ be identity
-- for storage image descriptors, input attachment descriptors, framebuffer
-- attachments, and any 'Graphics.Vulkan.C.Core10.ImageView.VkImageView'
-- used with a combined image sampler that enables
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>.
--
-- When creating a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView', if
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>
-- is enabled in the sampler, the @aspectMask@ of a @subresourceRange@ used
-- by the 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' /must/ be
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT'.
--
-- When creating a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView', if
-- sampler Y’CBCR conversion is not enabled in the sampler and the image
-- @format@ is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>,
-- the image /must/ have been created with
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT',
-- and the @aspectMask@ of the
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView'’s @subresourceRange@
-- /must/ be
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_0_BIT',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_1_BIT'
-- or
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_2_BIT'.
--
-- == Valid Usage
--
-- -   If @levelCount@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_MIP_LEVELS', it
--     /must/ be greater than @0@
--
-- -   If @layerCount@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_ARRAY_LAYERS', it
--     /must/ be greater than @0@
--
-- -   If @aspectMask@ includes
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT',
--     then it /must/ not include any of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   @aspectMask@ /must/ not include
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index @i@.
--
-- Unresolved directive in VkImageSubresourceRange.txt -
-- include::{generated}\/validity\/structs\/VkImageSubresourceRange.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage'
data ImageSubresourceRange = ImageSubresourceRange
  { -- No documentation found for Nested "ImageSubresourceRange" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ImageSubresourceRange" "baseMipLevel"
  baseMipLevel :: Word32
  , -- No documentation found for Nested "ImageSubresourceRange" "levelCount"
  levelCount :: Word32
  , -- No documentation found for Nested "ImageSubresourceRange" "baseArrayLayer"
  baseArrayLayer :: Word32
  , -- No documentation found for Nested "ImageSubresourceRange" "layerCount"
  layerCount :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageSubresourceRange' and
-- marshal a 'ImageSubresourceRange' into it. The 'VkImageSubresourceRange' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageSubresourceRange :: ImageSubresourceRange -> (VkImageSubresourceRange -> IO a) -> IO a
withCStructImageSubresourceRange marshalled cont = cont (VkImageSubresourceRange (aspectMask (marshalled :: ImageSubresourceRange)) (baseMipLevel (marshalled :: ImageSubresourceRange)) (levelCount (marshalled :: ImageSubresourceRange)) (baseArrayLayer (marshalled :: ImageSubresourceRange)) (layerCount (marshalled :: ImageSubresourceRange)))

-- | A function to read a 'VkImageSubresourceRange' and all additional
-- structures in the pointer chain into a 'ImageSubresourceRange'.
fromCStructImageSubresourceRange :: VkImageSubresourceRange -> IO ImageSubresourceRange
fromCStructImageSubresourceRange c = ImageSubresourceRange <$> pure (vkAspectMask (c :: VkImageSubresourceRange))
                                                           <*> pure (vkBaseMipLevel (c :: VkImageSubresourceRange))
                                                           <*> pure (vkLevelCount (c :: VkImageSubresourceRange))
                                                           <*> pure (vkBaseArrayLayer (c :: VkImageSubresourceRange))
                                                           <*> pure (vkLayerCount (c :: VkImageSubresourceRange))

instance Zero ImageSubresourceRange where
  zero = ImageSubresourceRange zero
                               zero
                               zero
                               zero
                               zero


-- | VkImageView - Opaque handle to an image view object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkCreateImageView',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkDestroyImageView'
type ImageView = VkImageView

-- | VkImageViewCreateFlagBits - Bitmask specifying additional parameters of
-- an image view
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateFlags'
type ImageViewCreateFlagBits = VkImageViewCreateFlagBits

-- | VkImageViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateFlags' is a bitmask
-- type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo'
type ImageViewCreateFlags = ImageViewCreateFlagBits


-- | VkImageViewCreateInfo - Structure specifying parameters of a newly
-- created image view
--
-- = Description
--
-- Some of the @image@ creation parameters are inherited by the view. In
-- particular, image view creation inherits the implicit parameter @usage@
-- specifying the allowed usages of the image view that, by default, takes
-- the value of the corresponding @usage@ parameter specified in
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' at image creation
-- time. If the image was has a depth-stencil format and was created with
-- an instance of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo', the usage is
-- calculated based on the @subresource.aspectMask@ provided: * If
-- @aspectMask@ includes only
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT',
-- the implicit @usage@ is equal to
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'::@stencilUsage@.
-- * If @aspectMask@ includes only
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT',
-- the implicit @usage@ is equal to
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@stencilUsage@. * If
-- both aspects are included in @aspectMask@, the implicit @usage@ is equal
-- to the intersection of slinkVkImageCreateInfo::@usage@ and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'::@stencilUsage@.
-- The implicit @usage@ /can/ be overriden by including an instance of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo'
-- structure in the @pNext@ chain.
--
-- If @image@ was created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- flag, and if the @format@ of the image is not
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>,
-- @format@ /can/ be different from the image’s format, but if @image@ was
-- created without the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
-- flag and they are not equal they /must/ be /compatible/. Image format
-- compatibility is defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
-- section. Views of compatible formats will have the same mapping between
-- texel coordinates and memory locations irrespective of the @format@,
-- with only the interpretation of the bit pattern changing.
--
-- __Note__
--
-- Values intended to be used with one view format /may/ not be exactly
-- preserved when written or read through a different format. For example,
-- an integer value that happens to have the bit pattern of a floating
-- point denorm or NaN /may/ be flushed or canonicalized when written or
-- read through a view with a floating point format. Similarly, a value
-- written through a signed normalized format that has a bit pattern
-- exactly equal to -2b /may/ be changed to -2b + 1 as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fundamentals-fixedfpconv Conversion from Normalized Fixed-Point to Floating-Point>.
--
-- If @image@ was created with the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
-- flag, @format@ /must/ be /compatible/ with the image’s format as
-- described above, or /must/ be an uncompressed format in which case it
-- /must/ be /size-compatible/ with the image’s format, as defined for
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies-images-format-size-compatibility copying data between images>
-- In this case the resulting image view’s texel dimensions equal the
-- dimensions of the selected mip level divided by the compressed texel
-- block size and rounded up.
--
-- If the image view is to be used with a sampler which supports
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>,
-- an /identically defined object/ of type
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion'
-- to that used to create the sampler /must/ be passed to
-- 'Graphics.Vulkan.C.Core10.ImageView.vkCreateImageView' in a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
-- added to the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo'.
--
-- If the image has a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- @format@ and @subresourceRange.aspectMask@ is
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT',
-- @format@ /must/ be identical to the image @format@, and the sampler to
-- be used with the image view /must/ enable
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>.
--
-- If @image@ was created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- and the image has a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- @format@, and if @subresourceRange.aspectMask@ is
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_0_BIT',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_1_BIT',
-- or
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_2_BIT',
-- @format@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatible-planes compatible>
-- with the corresponding plane of the image, and the sampler to be used
-- with the image view /must/ not enable
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>.
-- The @width@ and @height@ of the single-plane image view /must/ be
-- derived from the multi-planar image’s dimensions in the manner listed
-- for
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatible-planes plane compatibility>
-- for the plane.
--
-- Any view of an image plane will have the same mapping between texel
-- coordinates and memory locations as used by the channels of the color
-- aspect, subject to the formulae relating texel coordinates to
-- lower-resolution planes as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-chroma-reconstruction Chroma Reconstruction>.
-- That is, if an R or B plane has a reduced resolution relative to the G
-- plane of the multi-planar image, the image view operates using the
-- (/uplane/, /vplane/) unnormalized coordinates of the reduced-resolution
-- plane, and these coordinates access the same memory locations as the
-- (/ucolor/, /vcolor/) unnormalized coordinates of the color aspect for
-- which chroma reconstruction operations operate on the same (/uplane/,
-- /vplane/) or (/iplane/, /jplane/) coordinates.
--
-- > +---------+------------------------+-----------------------------------+
-- > | Dim,    | Image parameters       | View parameters                   |
-- > | Arrayed |                        |                                   |
-- > | ,       |                        |                                   |
-- > | MS      |                        |                                   |
-- > +=========+========================+===================================+
-- > |         | @imageType@ =          | @baseArrayLayer@, @layerCount@,   |
-- > |         | ci.@imageType@         | and @levelCount@ are members of   |
-- > |         | @width@ =              | the @subresourceRange@ member.    |
-- > |         | ci.@extent.width@      |                                   |
-- > |         | @height@ =             |                                   |
-- > |         | ci.@extent.height@     |                                   |
-- > |         | @depth@ =              |                                   |
-- > |         | ci.@extent.depth@      |                                   |
-- > |         | @arrayLayers@ =        |                                   |
-- > |         | ci.@arrayLayers@       |                                   |
-- > |         | @samples@ =            |                                   |
-- > |         | ci.@samples@           |                                   |
-- > |         | @flags@ = ci.@flags@   |                                   |
-- > |         | where ci is the        |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e10.Image.VkImageCreat |                                   |
-- > |         | eInfo'                 |                                   |
-- > |         | used to create         |                                   |
-- > |         | @image@.               |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __1D,   | @imageType@ =          | @viewType@ =                      |
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_1D'         |
-- > |         | on.VK_IMAGE_TYPE_1D'   | @baseArrayLayer@ ≥ 0              |
-- > |         | @width@ ≥ 1            | @layerCount@ = 1                  |
-- > |         | @height@ = 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __1D,   | @imageType@ =          | @viewType@ =                      |
-- > | 1, 0__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_1D_ARRAY'   |
-- > |         | on.VK_IMAGE_TYPE_1D'   | @baseArrayLayer@ ≥ 0              |
-- > |         | @width@ ≥ 1            | @layerCount@ ≥ 1                  |
-- > |         | @height@ = 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __2D,   | @imageType@ =          | @viewType@ =                      |
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_2D'         |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @baseArrayLayer@ ≥ 0              |
-- > |         | @width@ ≥ 1            | @layerCount@ = 1                  |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __2D,   | @imageType@ =          | @viewType@ =                      |
-- > | 1, 0__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_2D_ARRAY'   |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @baseArrayLayer@ ≥ 0              |
-- > |         | @width@ ≥ 1            | @layerCount@ ≥ 1                  |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __2D,   | @imageType@ =          | @viewType@ =                      |
-- > | 0, 1__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_2D'         |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @baseArrayLayer@ ≥ 0              |
-- > |         | @width@ ≥ 1            | @layerCount@ = 1                  |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ > 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __2D,   | @imageType@ =          | @viewType@ =                      |
-- > | 1, 1__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_2D_ARRAY'   |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @baseArrayLayer@ ≥ 0              |
-- > |         | @width@ ≥ 1            | @layerCount@ ≥ 1                  |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ > 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __CUBE, | @imageType@ =          | @viewType@ =                      |
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_CUBE'       |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @baseArrayLayer@ ≥ 0              |
-- > |         | @width@ ≥ 1            | @layerCount@ = 6                  |
-- > |         | @height@ = @width@     |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 6      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > |         | @flags@ includes       |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e10.DeviceInitializati |                                   |
-- > |         | on.VK_IMAGE_CREATE_CUB |                                   |
-- > |         | E_COMPATIBLE_BIT'      |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __CUBE, | @imageType@ =          | @viewType@ =                      |
-- > | 1, 0__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY' |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @baseArrayLayer@ ≥ 0              |
-- > |         | @width@ ≥ 1            | @layerCount@ = 6 × /N/, /N/ ≥ 1   |
-- > |         | @height@ = width       |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | /N/ ≥ 1                |                                   |
-- > |         | @arrayLayers@ ≥ 6 ×    |                                   |
-- > |         | /N/                    |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > |         | @flags@ includes       |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e10.DeviceInitializati |                                   |
-- > |         | on.VK_IMAGE_CREATE_CUB |                                   |
-- > |         | E_COMPATIBLE_BIT'      |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __3D,   | @imageType@ =          | @viewType@ =                      |
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_3D'         |
-- > |         | on.VK_IMAGE_TYPE_3D'   | @baseArrayLayer@ = 0              |
-- > |         | @width@ ≥ 1            | @layerCount@ = 1                  |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ ≥ 1            |                                   |
-- > |         | @arrayLayers@ = 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __3D,   | @imageType@ =          | @viewType@ =                      |
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_2D'         |
-- > |         | on.VK_IMAGE_TYPE_3D'   | @levelCount@ = 1                  |
-- > |         | @width@ ≥ 1            | @baseArrayLayer@ ≥ 0              |
-- > |         | @height@ ≥ 1           | @layerCount@ = 1                  |
-- > |         | @depth@ ≥ 1            |                                   |
-- > |         | @arrayLayers@ = 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > |         | @flags@ includes       |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e11.Promoted_from_VK_K |                                   |
-- > |         | HR_maintenance1.VK_IMA |                                   |
-- > |         | GE_CREATE_2D_ARRAY_COM |                                   |
-- > |         | PATIBLE_BIT'           |                                   |
-- > |         | @flags@ does not       |                                   |
-- > |         | include                |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e10.DeviceInitializati |                                   |
-- > |         | on.VK_IMAGE_CREATE_SPA |                                   |
-- > |         | RSE_BINDING_BIT',      |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e10.DeviceInitializati |                                   |
-- > |         | on.VK_IMAGE_CREATE_SPA |                                   |
-- > |         | RSE_RESIDENCY_BIT',    |                                   |
-- > |         | and                    |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e10.DeviceInitializati |                                   |
-- > |         | on.VK_IMAGE_CREATE_SPA |                                   |
-- > |         | RSE_ALIASED_BIT'       |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __3D,   | @imageType@ =          | @viewType@ =                      |
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |         | e10.DeviceInitializati | ew.VK_IMAGE_VIEW_TYPE_2D_ARRAY'   |
-- > |         | on.VK_IMAGE_TYPE_3D'   | @levelCount@ = 1                  |
-- > |         | @width@ ≥ 1            | @baseArrayLayer@ ≥ 0              |
-- > |         | @height@ ≥ 1           | @layerCount@ ≥ 1                  |
-- > |         | @depth@ ≥ 1            |                                   |
-- > |         | @arrayLayers@ = 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > |         | @flags@ includes       |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e11.Promoted_from_VK_K |                                   |
-- > |         | HR_maintenance1.VK_IMA |                                   |
-- > |         | GE_CREATE_2D_ARRAY_COM |                                   |
-- > |         | PATIBLE_BIT'           |                                   |
-- > |         | @flags@ does not       |                                   |
-- > |         | include                |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e10.DeviceInitializati |                                   |
-- > |         | on.VK_IMAGE_CREATE_SPA |                                   |
-- > |         | RSE_BINDING_BIT',      |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e10.DeviceInitializati |                                   |
-- > |         | on.VK_IMAGE_CREATE_SPA |                                   |
-- > |         | RSE_RESIDENCY_BIT',    |                                   |
-- > |         | and                    |                                   |
-- > |         | 'Graphics.Vulkan.C.Cor |                                   |
-- > |         | e10.DeviceInitializati |                                   |
-- > |         | on.VK_IMAGE_CREATE_SPA |                                   |
-- > |         | RSE_ALIASED_BIT'       |                                   |
-- > +---------+------------------------+-----------------------------------+
-- >
-- > Image and image view parameter compatibility requirements
--
-- == Valid Usage
--
-- -   If @image@ was not created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
--     then @viewType@ /must/ not be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-imageCubeArray image cubemap arrays>
--     feature is not enabled, @viewType@ /must/ not be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   If @image@ was created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_3D' but
--     without
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set then @viewType@ /must/ not be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY'
--
-- -   @image@ /must/ have been created with a @usage@ value containing at
--     least one of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_STORAGE_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     of the resultant image view /must/ contain at least one bit.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     of the resultant image view /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_STORAGE_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT'.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain at least one of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'.
--
-- -   @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when
--     @image@ was created
--
-- -   If @subresourceRange.levelCount@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @image@ was created with @usage@ containing
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @subresourceRange.levelCount@ /must/ be @1@
--
-- -   If @image@ is not a 3D image created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, or @viewType@ is not
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange@::@baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @subresourceRange@::@layerCount@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_ARRAY_LAYERS',
--     @image@ is not a 3D image created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, or @viewType@ is not
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange@::@layerCount@ /must/ be non-zero and
--     @subresourceRange@::@baseArrayLayer@ +
--     @subresourceRange@::@layerCount@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @image@ is a 3D image created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, and @viewType@ is
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange@::@baseArrayLayer@ /must/ be less than the
--     @extent.depth@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @subresourceRange@::@layerCount@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_ARRAY_LAYERS',
--     @image@ is a 3D image created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, and @viewType@ is
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange@::@layerCount@ /must/ be non-zero and
--     @subresourceRange@::@baseArrayLayer@ +
--     @subresourceRange@::@layerCount@ /must/ be less than or equal to the
--     @extent.depth@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @image@ was created with the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, @format@ /must/ be compatible with the @format@ used to create
--     @image@, as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
--
-- -   If @image@ was created with the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, but without the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     flag, and if the @format@ of the @image@ is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format, @format@ /must/ be compatible with the @format@ used to
--     create @image@, as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
--
-- -   If @image@ was created with the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     flag, @format@ /must/ be compatible with, or /must/ be an
--     uncompressed format that is size-compatible with, the @format@ used
--     to create @image@.
--
-- -   If @image@ was created with the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     flag, the @levelCount@ and @layerCount@ members of
--     @subresourceRange@ /must/ both be @1@.
--
-- -   If a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'
--     structure was included in the @pNext@ chain of the
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' struct used when
--     creating @image@ and the @viewFormatCount@ field of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'
--     is not zero then @format@ /must/ be one of the formats in
--     'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'::@pViewFormats@.
--
-- -   If @image@ was created with the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, if the @format@ of the @image@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format, and if @subresourceRange.aspectMask@ is one of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_2_BIT',
--     then @format@ /must/ be compatible with the
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' for the plane of the
--     @image@ @format@ indicated by @subresourceRange.aspectMask@, as
--     defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatible-planes>
--
-- -   If @image@ was not created with the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, or if the @format@ of the @image@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format and if @subresourceRange.aspectMask@ is
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT',
--     @format@ /must/ be identical to the @format@ used to create @image@
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--     with a @conversion@ value other than
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', all members of
--     @components@ /must/ have the value
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'.
--
-- -   If @image@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @subresourceRange@ and @viewType@ /must/ be compatible with the
--     image, as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-views-compatibility compatibility table>
--
-- -   If @image@ has an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>,
--     @format@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED'.
--
-- -   If @image@ has an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>,
--     the @pNext@ chain /must/ contain an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--     with a @conversion@ object created with the same external format as
--     @image@.
--
-- -   If @image@ has an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>,
--     all members of @components@ /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'.
--
-- -   If @image@ was created with @usage@ containing
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     @viewType@ /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY'
--
-- -   If @image@ was created with @usage@ containing
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     @format@ /must/ be 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_UINT'
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-fragmentdensitymapdynamic dynamic fragment density map>
--     feature is not enabled, @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT'
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-fragmentdensitymapdynamic dynamic fragment density map>
--     feature is not enabled and @image@ was created with @usage@
--     containing
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @flags@ /must/ not contain any of
--     'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VK_IMAGE_CREATE_PROTECTED_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_ALIASED_BIT'
--     ifdef::VK_VERSION_1_1,VK_KHR_maintenance2
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
--     and @image@ was not created with an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'
--     in the @pNext@ chain of
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo', its @usage@
--     member /must/ not include any bits that were not set in the @usage@
--     member of the 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'
--     structure used to create @image@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
--     @image@ was created with an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'
--     in the @pNext@ chain of
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo', and
--     @subResourceRange.aspectMask@ includes
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT',
--     the @usage@ member of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo'
--     instance /must/ not include any bits that were not set in the
--     @usage@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'
--     structure used to create @image@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
--     @image@ was created with an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT'
--     in the @pNext@ chain of
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo', and
--     @subResourceRange.aspectMask@ includes bits other than
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT',
--     the @usage@ member of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo'
--     instance /must/ not include any bits that were not set in the
--     @usage@ member of the
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure used to
--     create @image@ endif::VK_VERSION_1_1,VK_KHR_maintenance2
--
-- Unresolved directive in VkImageViewCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkImageViewCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkComponentMapping',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateFlags',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkCreateImageView'
data ImageViewCreateInfo = ImageViewCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ImageViewCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewCreateInfo" "flags"
  flags :: ImageViewCreateFlags
  , -- No documentation found for Nested "ImageViewCreateInfo" "image"
  image :: Image
  , -- No documentation found for Nested "ImageViewCreateInfo" "viewType"
  viewType :: ImageViewType
  , -- No documentation found for Nested "ImageViewCreateInfo" "format"
  format :: Format
  , -- No documentation found for Nested "ImageViewCreateInfo" "components"
  components :: ComponentMapping
  , -- No documentation found for Nested "ImageViewCreateInfo" "subresourceRange"
  subresourceRange :: ImageSubresourceRange
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageViewCreateInfo' and
-- marshal a 'ImageViewCreateInfo' into it. The 'VkImageViewCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageViewCreateInfo :: ImageViewCreateInfo -> (VkImageViewCreateInfo -> IO a) -> IO a
withCStructImageViewCreateInfo marshalled cont = withCStructImageSubresourceRange (subresourceRange (marshalled :: ImageViewCreateInfo)) (\subresourceRange'' -> withCStructComponentMapping (components (marshalled :: ImageViewCreateInfo)) (\components'' -> maybeWith withSomeVkStruct (next (marshalled :: ImageViewCreateInfo)) (\pPNext -> cont (VkImageViewCreateInfo VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO pPNext (flags (marshalled :: ImageViewCreateInfo)) (image (marshalled :: ImageViewCreateInfo)) (viewType (marshalled :: ImageViewCreateInfo)) (format (marshalled :: ImageViewCreateInfo)) components'' subresourceRange''))))

-- | A function to read a 'VkImageViewCreateInfo' and all additional
-- structures in the pointer chain into a 'ImageViewCreateInfo'.
fromCStructImageViewCreateInfo :: VkImageViewCreateInfo -> IO ImageViewCreateInfo
fromCStructImageViewCreateInfo c = ImageViewCreateInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageViewCreateInfo)))
                                                       <*> pure (vkFlags (c :: VkImageViewCreateInfo))
                                                       <*> pure (vkImage (c :: VkImageViewCreateInfo))
                                                       <*> pure (vkViewType (c :: VkImageViewCreateInfo))
                                                       <*> pure (vkFormat (c :: VkImageViewCreateInfo))
                                                       <*> (fromCStructComponentMapping (vkComponents (c :: VkImageViewCreateInfo)))
                                                       <*> (fromCStructImageSubresourceRange (vkSubresourceRange (c :: VkImageViewCreateInfo)))

instance Zero ImageViewCreateInfo where
  zero = ImageViewCreateInfo Nothing
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero


-- | VkImageViewType - Image view types
--
-- = Description
--
-- The exact image view type is partially implicit, based on the image’s
-- type and sample count, as well as the view creation parameters as
-- described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-views-compatibility image view compatibility table>
-- for 'Graphics.Vulkan.C.Core10.ImageView.vkCreateImageView'. This table
-- also shows which SPIR-V @OpTypeImage@ @Dim@ and @Arrayed@ parameters
-- correspond to each image view type.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo'
type ImageViewType = VkImageViewType


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_1D"
pattern IMAGE_VIEW_TYPE_1D :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_1D = VK_IMAGE_VIEW_TYPE_1D


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_2D"
pattern IMAGE_VIEW_TYPE_2D :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_2D = VK_IMAGE_VIEW_TYPE_2D


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_3D"
pattern IMAGE_VIEW_TYPE_3D :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_3D = VK_IMAGE_VIEW_TYPE_3D


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_CUBE"
pattern IMAGE_VIEW_TYPE_CUBE :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_CUBE = VK_IMAGE_VIEW_TYPE_CUBE


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_1D_ARRAY"
pattern IMAGE_VIEW_TYPE_1D_ARRAY :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_1D_ARRAY = VK_IMAGE_VIEW_TYPE_1D_ARRAY


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_2D_ARRAY"
pattern IMAGE_VIEW_TYPE_2D_ARRAY :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_2D_ARRAY = VK_IMAGE_VIEW_TYPE_2D_ARRAY


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_CUBE_ARRAY"
pattern IMAGE_VIEW_TYPE_CUBE_ARRAY :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_CUBE_ARRAY = VK_IMAGE_VIEW_TYPE_CUBE_ARRAY


-- | vkCreateImageView - Create an image view from an existing image
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the image view.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo' structure
--     containing parameters to be used to create the image view.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pView@ points to a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView'
--     handle in which the resulting image view object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateImageView.txt -
-- include::{generated}\/validity\/protos\/vkCreateImageView.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo'
createImageView :: Device ->  ImageViewCreateInfo ->  Maybe AllocationCallbacks ->  IO (ImageView)
createImageView = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pView' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructImageViewCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateImageView commandTable device' pCreateInfo' pAllocator pView' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pView')))))


-- | vkDestroyImageView - Destroy an image view object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the image view.
--
-- -   @imageView@ is the image view to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @imageView@ /must/ have
--     completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @imageView@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @imageView@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyImageView.txt -
-- include::{generated}\/validity\/protos\/vkDestroyImageView.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView'
destroyImageView :: Device ->  ImageView ->  Maybe AllocationCallbacks ->  IO ()
destroyImageView = \(Device device' commandTable) -> \imageView' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyImageView commandTable device' imageView' pAllocator *> (pure ()))

-- | A safe wrapper for 'createImageView' and 'destroyImageView' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withImageView
  :: Device -> ImageViewCreateInfo -> Maybe (AllocationCallbacks) -> (ImageView -> IO a) -> IO a
withImageView device imageViewCreateInfo allocationCallbacks = bracket
  (createImageView device imageViewCreateInfo allocationCallbacks)
  (\o -> destroyImageView device o allocationCallbacks)
