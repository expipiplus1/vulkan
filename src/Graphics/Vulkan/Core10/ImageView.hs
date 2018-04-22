{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.ImageView
  ( VkComponentSwizzle(..)
  , pattern VK_COMPONENT_SWIZZLE_IDENTITY
  , pattern VK_COMPONENT_SWIZZLE_ZERO
  , pattern VK_COMPONENT_SWIZZLE_ONE
  , pattern VK_COMPONENT_SWIZZLE_R
  , pattern VK_COMPONENT_SWIZZLE_G
  , pattern VK_COMPONENT_SWIZZLE_B
  , pattern VK_COMPONENT_SWIZZLE_A
  , VkImageViewType(..)
  , pattern VK_IMAGE_VIEW_TYPE_1D
  , pattern VK_IMAGE_VIEW_TYPE_2D
  , pattern VK_IMAGE_VIEW_TYPE_3D
  , pattern VK_IMAGE_VIEW_TYPE_CUBE
  , pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
  , VkImageViewCreateFlags(..)
  , VkImageView
  , vkCreateImageView
  , vkDestroyImageView
  , VkComponentMapping(..)
  , VkImageSubresourceRange(..)
  , VkImageViewCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
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


import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkImage
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlags
  )


-- ** VkComponentSwizzle

-- | VkComponentSwizzle - Specify how a component is swizzled
--
-- = Description
--
-- -   @VK_COMPONENT_SWIZZLE_IDENTITY@ specifies that the component is set
--     to the identity swizzle.
--
-- -   @VK_COMPONENT_SWIZZLE_ZERO@ specifies that the component is set to
--     zero.
--
-- -   @VK_COMPONENT_SWIZZLE_ONE@ specifies that the component is set to
--     either 1 or 1.0, depending on whether the type of the image view
--     format is integer or floating-point respectively, as determined by
--     the [Format
--     Definition](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-definition)
--     section for each 'Graphics.Vulkan.Core10.Core.VkFormat'.
--
-- -   @VK_COMPONENT_SWIZZLE_R@ specifies that the component is set to the
--     value of the R component of the image.
--
-- -   @VK_COMPONENT_SWIZZLE_G@ specifies that the component is set to the
--     value of the G component of the image.
--
-- -   @VK_COMPONENT_SWIZZLE_B@ specifies that the component is set to the
--     value of the B component of the image.
--
-- -   @VK_COMPONENT_SWIZZLE_A@ specifies that the component is set to the
--     value of the A component of the image.
--
-- Setting the identity swizzle on a component is equivalent to setting the
-- identity mapping on that component. That is:
--
-- +-----------------------------------+-----------------------------------+
-- | Component                         | Identity Mapping                  |
-- +===================================+===================================+
-- | @components.r@                    | @VK_COMPONENT_SWIZZLE_R@          |
-- +-----------------------------------+-----------------------------------+
-- | @components.g@                    | @VK_COMPONENT_SWIZZLE_G@          |
-- +-----------------------------------+-----------------------------------+
-- | @components.b@                    | @VK_COMPONENT_SWIZZLE_B@          |
-- +-----------------------------------+-----------------------------------+
-- | @components.a@                    | @VK_COMPONENT_SWIZZLE_A@          |
-- +-----------------------------------+-----------------------------------+
--
-- Component Mappings Equivalent To @VK_COMPONENT_SWIZZLE_IDENTITY@
--
-- = See Also
--
-- 'VkComponentMapping'
newtype VkComponentSwizzle = VkComponentSwizzle Int32
  deriving (Eq, Ord, Storable)

instance Show VkComponentSwizzle where
  showsPrec _ VK_COMPONENT_SWIZZLE_IDENTITY = showString "VK_COMPONENT_SWIZZLE_IDENTITY"
  showsPrec _ VK_COMPONENT_SWIZZLE_ZERO = showString "VK_COMPONENT_SWIZZLE_ZERO"
  showsPrec _ VK_COMPONENT_SWIZZLE_ONE = showString "VK_COMPONENT_SWIZZLE_ONE"
  showsPrec _ VK_COMPONENT_SWIZZLE_R = showString "VK_COMPONENT_SWIZZLE_R"
  showsPrec _ VK_COMPONENT_SWIZZLE_G = showString "VK_COMPONENT_SWIZZLE_G"
  showsPrec _ VK_COMPONENT_SWIZZLE_B = showString "VK_COMPONENT_SWIZZLE_B"
  showsPrec _ VK_COMPONENT_SWIZZLE_A = showString "VK_COMPONENT_SWIZZLE_A"
  showsPrec p (VkComponentSwizzle x) = showParen (p >= 11) (showString "VkComponentSwizzle " . showsPrec 11 x)

instance Read VkComponentSwizzle where
  readPrec = parens ( choose [ ("VK_COMPONENT_SWIZZLE_IDENTITY", pure VK_COMPONENT_SWIZZLE_IDENTITY)
                             , ("VK_COMPONENT_SWIZZLE_ZERO",     pure VK_COMPONENT_SWIZZLE_ZERO)
                             , ("VK_COMPONENT_SWIZZLE_ONE",      pure VK_COMPONENT_SWIZZLE_ONE)
                             , ("VK_COMPONENT_SWIZZLE_R",        pure VK_COMPONENT_SWIZZLE_R)
                             , ("VK_COMPONENT_SWIZZLE_G",        pure VK_COMPONENT_SWIZZLE_G)
                             , ("VK_COMPONENT_SWIZZLE_B",        pure VK_COMPONENT_SWIZZLE_B)
                             , ("VK_COMPONENT_SWIZZLE_A",        pure VK_COMPONENT_SWIZZLE_A)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkComponentSwizzle")
                        v <- step readPrec
                        pure (VkComponentSwizzle v)
                        )
                    )

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_IDENTITY"
pattern VK_COMPONENT_SWIZZLE_IDENTITY :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_IDENTITY = VkComponentSwizzle 0

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_ZERO"
pattern VK_COMPONENT_SWIZZLE_ZERO :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_ZERO = VkComponentSwizzle 1

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_ONE"
pattern VK_COMPONENT_SWIZZLE_ONE :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_ONE = VkComponentSwizzle 2

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_R"
pattern VK_COMPONENT_SWIZZLE_R :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_R = VkComponentSwizzle 3

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_G"
pattern VK_COMPONENT_SWIZZLE_G :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_G = VkComponentSwizzle 4

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_B"
pattern VK_COMPONENT_SWIZZLE_B :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_B = VkComponentSwizzle 5

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_A"
pattern VK_COMPONENT_SWIZZLE_A :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_A = VkComponentSwizzle 6
-- ** VkImageViewType

-- | VkImageViewType - Image view types
--
-- = Description
--
-- The exact image view type is partially implicit, based on the image’s
-- type and sample count, as well as the view creation parameters as
-- described in the [image view compatibility
-- table](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-views-compatibility)
-- for 'vkCreateImageView'. This table also shows which SPIR-V
-- @OpTypeImage@ @Dim@ and @Arrayed@ parameters correspond to each image
-- view type.
--
-- = See Also
--
-- 'VkImageViewCreateInfo'
newtype VkImageViewType = VkImageViewType Int32
  deriving (Eq, Ord, Storable)

instance Show VkImageViewType where
  showsPrec _ VK_IMAGE_VIEW_TYPE_1D = showString "VK_IMAGE_VIEW_TYPE_1D"
  showsPrec _ VK_IMAGE_VIEW_TYPE_2D = showString "VK_IMAGE_VIEW_TYPE_2D"
  showsPrec _ VK_IMAGE_VIEW_TYPE_3D = showString "VK_IMAGE_VIEW_TYPE_3D"
  showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE = showString "VK_IMAGE_VIEW_TYPE_CUBE"
  showsPrec _ VK_IMAGE_VIEW_TYPE_1D_ARRAY = showString "VK_IMAGE_VIEW_TYPE_1D_ARRAY"
  showsPrec _ VK_IMAGE_VIEW_TYPE_2D_ARRAY = showString "VK_IMAGE_VIEW_TYPE_2D_ARRAY"
  showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = showString "VK_IMAGE_VIEW_TYPE_CUBE_ARRAY"
  showsPrec p (VkImageViewType x) = showParen (p >= 11) (showString "VkImageViewType " . showsPrec 11 x)

instance Read VkImageViewType where
  readPrec = parens ( choose [ ("VK_IMAGE_VIEW_TYPE_1D",         pure VK_IMAGE_VIEW_TYPE_1D)
                             , ("VK_IMAGE_VIEW_TYPE_2D",         pure VK_IMAGE_VIEW_TYPE_2D)
                             , ("VK_IMAGE_VIEW_TYPE_3D",         pure VK_IMAGE_VIEW_TYPE_3D)
                             , ("VK_IMAGE_VIEW_TYPE_CUBE",       pure VK_IMAGE_VIEW_TYPE_CUBE)
                             , ("VK_IMAGE_VIEW_TYPE_1D_ARRAY",   pure VK_IMAGE_VIEW_TYPE_1D_ARRAY)
                             , ("VK_IMAGE_VIEW_TYPE_2D_ARRAY",   pure VK_IMAGE_VIEW_TYPE_2D_ARRAY)
                             , ("VK_IMAGE_VIEW_TYPE_CUBE_ARRAY", pure VK_IMAGE_VIEW_TYPE_CUBE_ARRAY)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageViewType")
                        v <- step readPrec
                        pure (VkImageViewType v)
                        )
                    )

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_1D"
pattern VK_IMAGE_VIEW_TYPE_1D :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_1D = VkImageViewType 0

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_2D"
pattern VK_IMAGE_VIEW_TYPE_2D :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_2D = VkImageViewType 1

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_3D"
pattern VK_IMAGE_VIEW_TYPE_3D :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_3D = VkImageViewType 2

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_CUBE"
pattern VK_IMAGE_VIEW_TYPE_CUBE :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_CUBE = VkImageViewType 3

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_1D_ARRAY"
pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY = VkImageViewType 4

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_2D_ARRAY"
pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY = VkImageViewType 5

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_CUBE_ARRAY"
pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = VkImageViewType 6
-- ** VkImageViewCreateFlags

-- | VkImageViewCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkImageViewCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkImageViewCreateInfo'
newtype VkImageViewCreateFlags = VkImageViewCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkImageViewCreateFlags where
  
  showsPrec p (VkImageViewCreateFlags x) = showParen (p >= 11) (showString "VkImageViewCreateFlags " . showsPrec 11 x)

instance Read VkImageViewCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageViewCreateFlags")
                        v <- step readPrec
                        pure (VkImageViewCreateFlags v)
                        )
                    )


-- | Dummy data to tag the 'Ptr' with
data VkImageView_T
-- | VkImageView - Opaque handle to a image view object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.Core10.Pass.VkFramebufferCreateInfo',
-- 'vkCreateImageView', 'vkDestroyImageView'
type VkImageView = Ptr VkImageView_T
-- | vkCreateImageView - Create an image view from an existing image
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the image view.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkImageViewCreateInfo@ structure containing parameters to be used
--     to create the image view.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pView@ points to a @VkImageView@ handle in which the resulting
--     image view object is returned.
--
-- = Description
--
-- Some of the image creation parameters are inherited by the view. In
-- particular, image view creation inherits the implicit parameter @usage@
-- specifying the allowed usages of the image view that, by default, takes
-- the value of the corresponding @usage@ parameter specified in
-- @VkImageCreateInfo@ at image creation time. This implicit parameter
-- /can/ be overriden by chaining a
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo'
-- structure through the @pNext@ member to @VkImageViewCreateInfo@ as
-- described later in this section.
--
-- The remaining parameters are contained in the @pCreateInfo@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkImageViewCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pView@ /must/ be a valid pointer to a @VkImageView@ handle
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkImageView',
-- 'VkImageViewCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateImageView" vkCreateImageView :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult
-- | vkDestroyImageView - Destroy an image view object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the image view.
--
-- -   @imageView@ is the image view to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @imageView@ /must/ have
--     completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @imageView@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @imageView@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @imageView@
--     /must/ be a valid @VkImageView@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @imageView@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @imageView@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkImageView'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyImageView" vkDestroyImageView :: ("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkComponentMapping - Structure specifying a color component mapping
--
-- == Valid Usage (Implicit)
--
-- -   @r@ /must/ be a valid 'VkComponentSwizzle' value
--
-- -   @g@ /must/ be a valid 'VkComponentSwizzle' value
--
-- -   @b@ /must/ be a valid 'VkComponentSwizzle' value
--
-- -   @a@ /must/ be a valid 'VkComponentSwizzle' value
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'VkComponentSwizzle', 'VkImageViewCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
data VkComponentMapping = VkComponentMapping
  { -- | @r@ is a 'VkComponentSwizzle' specifying the component value placed in
  -- the R component of the output vector.
  vkR :: VkComponentSwizzle
  , -- | @g@ is a 'VkComponentSwizzle' specifying the component value placed in
  -- the G component of the output vector.
  vkG :: VkComponentSwizzle
  , -- | @b@ is a 'VkComponentSwizzle' specifying the component value placed in
  -- the B component of the output vector.
  vkB :: VkComponentSwizzle
  , -- | @a@ is a 'VkComponentSwizzle' specifying the component value placed in
  -- the A component of the output vector.
  vkA :: VkComponentSwizzle
  }
  deriving (Eq, Show)

instance Storable VkComponentMapping where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkComponentMapping <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkR (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 4) (vkG (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 8) (vkB (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 12) (vkA (poked :: VkComponentMapping))
-- | VkImageSubresourceRange - Structure specifying a image subresource range
--
-- = Description
--
-- The number of mipmap levels and array layers /must/ be a subset of the
-- image subresources in the image. If an application wants to use all mip
-- levels or layers in an image after the @baseMipLevel@ or
-- @baseArrayLayer@, it /can/ set @levelCount@ and @layerCount@ to the
-- special values @VK_REMAINING_MIP_LEVELS@ and @VK_REMAINING_ARRAY_LAYERS@
-- without knowing the exact number of mip levels or layers.
--
-- For cube and cube array image views, the layers of the image view
-- starting at @baseArrayLayer@ correspond to faces in the order +X, -X,
-- +Y, -Y, +Z, -Z. For cube arrays, each set of six sequential layers is a
-- single cube, so the number of cube maps in a cube map array view is
-- /@layerCount@ \/ 6/, and image array layer (@baseArrayLayer@ + i) is
-- face index (i mod 6) of cube /i \/ 6/. If the number of layers in the
-- view, whether set explicitly in @layerCount@ or implied by
-- @VK_REMAINING_ARRAY_LAYERS@, is not a multiple of 6, behavior when
-- indexing the last cube is undefined.
--
-- @aspectMask@ /must/ be only @VK_IMAGE_ASPECT_COLOR_BIT@,
-- @VK_IMAGE_ASPECT_DEPTH_BIT@ or @VK_IMAGE_ASPECT_STENCIL_BIT@ if @format@
-- is a color, depth-only or stencil-only format, respectively, except if
-- @format@ is a [multi-planar
-- format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion).
-- If using a depth\/stencil format with both depth and stencil components,
-- @aspectMask@ /must/ include at least one of @VK_IMAGE_ASPECT_DEPTH_BIT@
-- and @VK_IMAGE_ASPECT_STENCIL_BIT@, and /can/ include both.
--
-- When the @VkImageSubresourceRange@ structure is used to select a subset
-- of the slices of a 3D image’s mip level in order to create a 2D or 2D
-- array image view of a 3D image created with
-- @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@, @baseArrayLayer@ and
-- @layerCount@ specify the first slice index and the number of slices to
-- include in the created image view. Such an image view /can/ be used as a
-- framebuffer attachment that refers only to the specified range of slices
-- of the selected mip level. However, any layout transitions performed on
-- such an attachment view during a render pass instance still apply to the
-- entire subresource referenced which includes all the slices of the
-- selected mip level.
--
-- When using an imageView of a depth\/stencil image to populate a
-- descriptor set (e.g. for sampling in the shader, or for use as an input
-- attachment), the @aspectMask@ /must/ only include one bit and selects
-- whether the imageView is used for depth reads (i.e. using a
-- floating-point sampler or input attachment in the shader) or stencil
-- reads (i.e. using an unsigned integer sampler or input attachment in the
-- shader). When an imageView of a depth\/stencil image is used as a
-- depth\/stencil framebuffer attachment, the @aspectMask@ is ignored and
-- both depth and stencil image subresources are used.
--
-- The @components@ member is of type 'VkComponentMapping', and describes a
-- remapping from components of the image to components of the vector
-- returned by shader image instructions. This remapping /must/ be identity
-- for storage image descriptors, input attachment descriptors, framebuffer
-- attachments, and any @VkImageView@ used with a combined image sampler
-- that enables [sampler Y’CBCR
-- conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion).
--
-- When creating a @VkImageView@, if [sampler Y’CBCR
-- conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion)
-- is enabled in the sampler, the @aspectMask@ of a @subresourceRange@ used
-- by the @VkImageView@ /must/ be @VK_IMAGE_ASPECT_COLOR_BIT@.
--
-- When creating a @VkImageView@, if sampler Y’CBCR conversion is not
-- enabled in the sampler and the image @format@ is
-- [multi-planar](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
-- the image /must/ have been created with
-- @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@, and the @aspectMask@ of the
-- @VkImageView@’s @subresourceRange@ /must/ be
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@ or
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@.
--
-- == Valid Usage
--
-- -   If @levelCount@ is not @VK_REMAINING_MIP_LEVELS@, it /must/ be
--     greater than @0@
--
-- -   If @layerCount@ is not @VK_REMAINING_ARRAY_LAYERS@, it /must/ be
--     greater than @0@
--
-- -   If @aspectMask@ includes @VK_IMAGE_ASPECT_COLOR_BIT@, then it /must/
--     not include any of @VK_IMAGE_ASPECT_PLANE_0_BIT@,
--     @VK_IMAGE_ASPECT_PLANE_1_BIT@, or @VK_IMAGE_ASPECT_PLANE_2_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   @aspectMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     values
--
-- -   @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'VkImageViewCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage'
data VkImageSubresourceRange = VkImageSubresourceRange
  { -- | @aspectMask@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
  -- specifying which aspect(s) of the image are included in the view.
  vkAspectMask :: VkImageAspectFlags
  , -- | @baseMipLevel@ is the first mipmap level accessible to the view.
  vkBaseMipLevel :: Word32
  , -- | @levelCount@ is the number of mipmap levels (starting from
  -- @baseMipLevel@) accessible to the view.
  vkLevelCount :: Word32
  , -- | @baseArrayLayer@ is the first array layer accessible to the view.
  vkBaseArrayLayer :: Word32
  , -- | @layerCount@ is the number of array layers (starting from
  -- @baseArrayLayer@) accessible to the view.
  vkLayerCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkImageSubresourceRange where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkImageSubresourceRange <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 4) (vkBaseMipLevel (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 8) (vkLevelCount (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 12) (vkBaseArrayLayer (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 16) (vkLayerCount (poked :: VkImageSubresourceRange))
-- | VkImageViewCreateInfo - Structure specifying parameters of a newly
-- created image view
--
-- = Description
--
-- If @image@ was created with the @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@
-- flag, and if the @format@ of the image is not
-- [multi-planar](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
-- @format@ /can/ be different from the image’s format, but if @image@ was
-- created without the @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@
-- flag and they are not equal they /must/ be /compatible/. Image format
-- compatibility is defined in the [Format Compatibility
-- Classes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatibility-classes)
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
-- [Conversion from Normalized Fixed-Point to
-- Floating-Point](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-fixedfpconv).
--
-- If @image@ was created with the
-- @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@ flag, @format@ /must/
-- be /compatible/ with the image’s format as described above, or /must/ be
-- an uncompressed format in which case it /must/ be /size-compatible/ with
-- the image’s format, as defined for [copying data between
-- images](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies-images-format-size-compatibility)
-- In this case the resulting image view’s texel dimensions equal the
-- dimensions of the selected mip level divided by the compressed texel
-- block size and rounded up.
--
-- If the image view is to be used with a sampler which supports [sampler
-- Y’CBCR
-- conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion),
-- an /identically defined object/ of type
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversion'
-- to that used to create the sampler /must/ be passed to
-- 'vkCreateImageView' in a
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
-- added to the @pNext@ chain of 'VkImageViewCreateInfo'.
--
-- If the image has a
-- [multi-planar](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
-- @format@ and @subresourceRange.aspectMask@ is
-- @VK_IMAGE_ASPECT_COLOR_BIT@, @format@ /must/ be identical to the image
-- @format@, and the sampler to be used with the image view /must/ enable
-- [sampler Y’CBCR
-- conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion).
--
-- If @image@ was created with the @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@ and
-- the image has a
-- [multi-planar](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
-- @format@, and if @subresourceRange.aspectMask@ is
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@, @format@ /must/ be
-- [compatible](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatible-planes)
-- with the corresponding plane of the image, and the sampler to be used
-- with the image view /must/ not enable [sampler Y’CBCR
-- conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion).
-- The @width@ and @height@ of the single-plane image view /must/ be
-- derived from the multi-planar image’s dimensions in the manner listed
-- for [plane
-- compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatible-planes)
-- for the plane.
--
-- Any view of an image plane will have the same mapping between texel
-- coordinates and memory locations as used by the channels of the color
-- aspect, subject to the formulae relating texel coordinates to
-- lower-resolution planes as described in [Chroma
-- Reconstruction](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-chroma-reconstruction).
-- That is, if an R or B plane has a reduced resolution relative to the G
-- plane of the multi-planar image, the image view operates using the
-- (/uplane/, /vplane/) unnormalized coordinates of the reduced-resolution
-- plane, and these coordinates access the same memory locations as the
-- (/ucolor/, /vcolor/) unnormalized coordinates of the color aspect for
-- which chroma reconstruction operations operate on the same (/uplane/,
-- /vplane/) or (/iplane/, /jplane/) coordinates.
--
-- +----------+--------------------------------------------------+-----------------------------------+
-- | Dim,     | Image parameters                                 | View parameters                   |
-- | Arrayed, |                                                  |                                   |
-- | MS       |                                                  |                                   |
-- +==========+==================================================+===================================+
-- |          | @imageType@ = ci.@imageType@                     | @baseArrayLayer@, @layerCount@,   |
-- |          | @width@ = ci.@extent.width@                      | and @levelCount@ are members of   |
-- |          | @height@ = ci.@extent.height@                    | the @subresourceRange@ member.    |
-- |          | @depth@ = ci.@extent.depth@                      |                                   |
-- |          | @arrayLayers@ = ci.@arrayLayers@                 |                                   |
-- |          | @samples@ = ci.@samples@                         |                                   |
-- |          | @flags@ = ci.@flags@                             |                                   |
-- |          | where ci is the                                  |                                   |
-- |          | 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' |                                   |
-- |          | used to create @image@.                          |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __1D, 0, | @imageType@ = @VK_IMAGE_TYPE_1D@                 | @viewType@ =                      |
-- | 0__      | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_1D@           |
-- |          | @height@ = 1                                     | @baseArrayLayer@ ≥ 0              |
-- |          | @depth@ = 1                                      | @layerCount@ = 1                  |
-- |          | @arrayLayers@ ≥ 1                                |                                   |
-- |          | @samples@ = 1                                    |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __1D, 1, | @imageType@ = @VK_IMAGE_TYPE_1D@                 | @viewType@ =                      |
-- | 0__      | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_1D_ARRAY@     |
-- |          | @height@ = 1                                     | @baseArrayLayer@ ≥ 0              |
-- |          | @depth@ = 1                                      | @layerCount@ ≥ 1                  |
-- |          | @arrayLayers@ ≥ 1                                |                                   |
-- |          | @samples@ = 1                                    |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __2D, 0, | @imageType@ = @VK_IMAGE_TYPE_2D@                 | @viewType@ =                      |
-- | 0__      | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_2D@           |
-- |          | @height@ ≥ 1                                     | @baseArrayLayer@ ≥ 0              |
-- |          | @depth@ = 1                                      | @layerCount@ = 1                  |
-- |          | @arrayLayers@ ≥ 1                                |                                   |
-- |          | @samples@ = 1                                    |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __2D, 1, | @imageType@ = @VK_IMAGE_TYPE_2D@                 | @viewType@ =                      |
-- | 0__      | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_2D_ARRAY@     |
-- |          | @height@ ≥ 1                                     | @baseArrayLayer@ ≥ 0              |
-- |          | @depth@ = 1                                      | @layerCount@ ≥ 1                  |
-- |          | @arrayLayers@ ≥ 1                                |                                   |
-- |          | @samples@ = 1                                    |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __2D, 0, | @imageType@ = @VK_IMAGE_TYPE_2D@                 | @viewType@ =                      |
-- | 1__      | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_2D@           |
-- |          | @height@ ≥ 1                                     | @baseArrayLayer@ ≥ 0              |
-- |          | @depth@ = 1                                      | @layerCount@ = 1                  |
-- |          | @arrayLayers@ ≥ 1                                |                                   |
-- |          | @samples@ > 1                                    |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __2D, 1, | @imageType@ = @VK_IMAGE_TYPE_2D@                 | @viewType@ =                      |
-- | 1__      | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_2D_ARRAY@     |
-- |          | @height@ ≥ 1                                     | @baseArrayLayer@ ≥ 0              |
-- |          | @depth@ = 1                                      | @layerCount@ ≥ 1                  |
-- |          | @arrayLayers@ ≥ 1                                |                                   |
-- |          | @samples@ > 1                                    |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __CUBE,  | @imageType@ = @VK_IMAGE_TYPE_2D@                 | @viewType@ =                      |
-- | 0, 0__   | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_CUBE@         |
-- |          | @height@ = @width@                               | @baseArrayLayer@ ≥ 0              |
-- |          | @depth@ = 1                                      | @layerCount@ = 6                  |
-- |          | @arrayLayers@ ≥ 6                                |                                   |
-- |          | @samples@ = 1                                    |                                   |
-- |          | @flags@ includes                                 |                                   |
-- |          | @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@            |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __CUBE,  | @imageType@ = @VK_IMAGE_TYPE_2D@                 | @viewType@ =                      |
-- | 1, 0__   | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@   |
-- |          | @height@ = width                                 | @baseArrayLayer@ ≥ 0              |
-- |          | @depth@ = 1                                      | @layerCount@ = 6 × /N/, /N/ ≥ 1   |
-- |          | /N/ ≥ 1                                          |                                   |
-- |          | @arrayLayers@ ≥ 6 × /N/                          |                                   |
-- |          | @samples@ = 1                                    |                                   |
-- |          | @flags@ includes                                 |                                   |
-- |          | @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@            |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __3D, 0, | @imageType@ = @VK_IMAGE_TYPE_3D@                 | @viewType@ =                      |
-- | 0__      | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_3D@           |
-- |          | @height@ ≥ 1                                     | @baseArrayLayer@ = 0              |
-- |          | @depth@ ≥ 1                                      | @layerCount@ = 1                  |
-- |          | @arrayLayers@ = 1                                |                                   |
-- |          | @samples@ = 1                                    |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __3D, 0, | @imageType@ = @VK_IMAGE_TYPE_3D@                 | @viewType@ =                      |
-- | 0__      | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_2D@           |
-- |          | @height@ ≥ 1                                     | @levelCount@ = 1                  |
-- |          | @depth@ ≥ 1                                      | @baseArrayLayer@ ≥ 0              |
-- |          | @arrayLayers@ = 1                                | @layerCount@ = 1                  |
-- |          | @samples@ = 1                                    |                                   |
-- |          | @flags@ includes                                 |                                   |
-- |          | @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@        |                                   |
-- |          | @flags@ does not include                         |                                   |
-- |          | @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@,            |                                   |
-- |          | @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@, and      |                                   |
-- |          | @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@             |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
-- | __3D, 0, | @imageType@ = @VK_IMAGE_TYPE_3D@                 | @viewType@ =                      |
-- | 0__      | @width@ ≥ 1                                      | @VK_IMAGE_VIEW_TYPE_2D_ARRAY@     |
-- |          | @height@ ≥ 1                                     | @levelCount@ = 1                  |
-- |          | @depth@ ≥ 1                                      | @baseArrayLayer@ ≥ 0              |
-- |          | @arrayLayers@ = 1                                | @layerCount@ ≥ 1                  |
-- |          | @samples@ = 1                                    |                                   |
-- |          | @flags@ includes                                 |                                   |
-- |          | @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@        |                                   |
-- |          | @flags@ does not include                         |                                   |
-- |          | @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@,            |                                   |
-- |          | @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@, and      |                                   |
-- |          | @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@             |                                   |
-- +----------+--------------------------------------------------+-----------------------------------+
--
-- Image and image view parameter compatibility requirements
--
-- == Valid Usage
--
-- -   If @image@ was not created with
--     @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@ then @viewType@ /must/ not be
--     @VK_IMAGE_VIEW_TYPE_CUBE@ or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- -   If the [image cubemap
--     arrays](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-imageCubeArray)
--     feature is not enabled, @viewType@ /must/ not be
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- -   If @image@ was created with @VK_IMAGE_TYPE_3D@ but without
--     @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@ set then @viewType@ /must/
--     not be @VK_IMAGE_VIEW_TYPE_2D@ or @VK_IMAGE_VIEW_TYPE_2D_ARRAY@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_LINEAR@, @format@
--     /must/ be format that has at least one supported feature bit present
--     in the value of @VkFormatProperties@::@linearTilingFeatures@
--     returned by @vkGetPhysicalDeviceFormatProperties@ with the same
--     value of @format@
--
-- -   @image@ /must/ have been created with a @usage@ value containing at
--     least one of @VK_IMAGE_USAGE_SAMPLED_BIT@,
--     @VK_IMAGE_USAGE_STORAGE_BIT@, @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@,
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@, or
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_LINEAR@ and @usage@
--     contains @VK_IMAGE_USAGE_SAMPLED_BIT@, @format@ /must/ be supported
--     for sampled images, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_LINEAR@ and @usage@
--     contains @VK_IMAGE_USAGE_STORAGE_BIT@, @format@ /must/ be supported
--     for storage images, as specified by the
--     @VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_LINEAR@ and @usage@
--     contains @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@, @format@ /must/ be
--     supported for color attachments, as specified by the
--     @VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_LINEAR@ and @usage@
--     contains @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@, @format@
--     /must/ be supported for depth\/stencil attachments, as specified by
--     the @VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_OPTIMAL@, @format@
--     /must/ be format that has at least one supported feature bit present
--     in the value of @VkFormatProperties@::@optimalTilingFeatures@
--     returned by @vkGetPhysicalDeviceFormatProperties@ with the same
--     value of @format@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_OPTIMAL@ and @usage@
--     contains @VK_IMAGE_USAGE_SAMPLED_BIT@, @format@ /must/ be supported
--     for sampled images, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@ flag in
--     @VkFormatProperties@::@optimalTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_OPTIMAL@ and @usage@
--     contains @VK_IMAGE_USAGE_STORAGE_BIT@, @format@ /must/ be supported
--     for storage images, as specified by the
--     @VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT@ flag in
--     @VkFormatProperties@::@optimalTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_OPTIMAL@ and @usage@
--     contains @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@, @format@ /must/ be
--     supported for color attachments, as specified by the
--     @VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT@ flag in
--     @VkFormatProperties@::@optimalTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   If @image@ was created with @VK_IMAGE_TILING_OPTIMAL@ and @usage@
--     contains @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@, @format@
--     /must/ be supported for depth\/stencil attachments, as specified by
--     the @VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT@ flag in
--     @VkFormatProperties@::@optimalTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@ with the same value of
--     @format@
--
-- -   @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when
--     @image@ was created
--
-- -   If @subresourceRange.levelCount@ is not @VK_REMAINING_MIP_LEVELS@,
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @image@ is not a 3D image created with
--     @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@ set, or @viewType@ is not
--     @VK_IMAGE_VIEW_TYPE_2D@ or @VK_IMAGE_VIEW_TYPE_2D_ARRAY@,
--     @subresourceRange@::@baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @subresourceRange@::@layerCount@ is not
--     @VK_REMAINING_ARRAY_LAYERS@, @image@ is not a 3D image created with
--     @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@ set, or @viewType@ is not
--     @VK_IMAGE_VIEW_TYPE_2D@ or @VK_IMAGE_VIEW_TYPE_2D_ARRAY@,
--     @subresourceRange@::@layerCount@ /must/ be non-zero and
--     @subresourceRange@::@baseArrayLayer@ +
--     @subresourceRange@::@layerCount@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @image@ is a 3D image created with
--     @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@ set, and @viewType@ is
--     @VK_IMAGE_VIEW_TYPE_2D@ or @VK_IMAGE_VIEW_TYPE_2D_ARRAY@,
--     @subresourceRange@::@baseArrayLayer@ /must/ be less than the
--     @extent.depth@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @subresourceRange@::@layerCount@ is not
--     @VK_REMAINING_ARRAY_LAYERS@, @image@ is a 3D image created with
--     @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@ set, and @viewType@ is
--     @VK_IMAGE_VIEW_TYPE_2D@ or @VK_IMAGE_VIEW_TYPE_2D_ARRAY@,
--     @subresourceRange@::@layerCount@ /must/ be non-zero and
--     @subresourceRange@::@baseArrayLayer@ +
--     @subresourceRange@::@layerCount@ /must/ be less than or equal to the
--     @extent.depth@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @image@ was created with the @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@
--     flag, @format@ /must/ be compatible with the @format@ used to create
--     @image@, as defined in [Format Compatibility
--     Classes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatibility-classes)
--
-- -   If @image@ was created with the @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@
--     flag, but without the
--     @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@ flag, and if the
--     @format@ of the @image@ is not a
--     [multi-planar](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     format, @format@ /must/ be compatible with the @format@ used to
--     create @image@, as defined in [Format Compatibility
--     Classes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatibility-classes)
--
-- -   If @image@ was created with the
--     @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@ flag, @format@
--     /must/ be compatible with, or /must/ be an uncompressed format that
--     is size-compatible with, the @format@ used to create @image@.
--
-- -   If @image@ was created with the
--     @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@ flag, the
--     @levelCount@ and @layerCount@ members of @subresourceRange@ /must/
--     both be @1@.
--
-- -   If a @VkImageFormatListCreateInfoKHR@ structure was included in the
--     @pNext@ chain of the @VkImageCreateInfo@ struct used when creating
--     @image@ and the @viewFormatCount@ field of
--     @VkImageFormatListCreateInfoKHR@ is not zero then @format@ /must/ be
--     one of the formats in
--     @VkImageFormatListCreateInfoKHR@::@pViewFormats@.
--
-- -   If @image@ was created with the @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@
--     flag, if the @format@ of the @image@ is a
--     [multi-planar](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     format, and if @subresourceRange.aspectMask@ is one of
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@, then @format@ /must/ be compatible
--     with the 'Graphics.Vulkan.Core10.Core.VkFormat' for the plane of the
--     @image@ @format@ indicated by @subresourceRange.aspectMask@, as
--     defined in
--     [{html_spec_relative}#features-formats-compatible-planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatible-planes)
--
-- -   If @image@ was not created with the
--     @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@ flag, or if the @format@ of the
--     @image@ is a
--     [multi-planar](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     format and if @subresourceRange.aspectMask@ is
--     @VK_IMAGE_ASPECT_COLOR_BIT@, @format@ /must/ be identical to the
--     @format@ used to create @image@
--
-- -   If @image@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @subresourceRange@ and @viewType@ /must/ be compatible with the
--     image, as described in the [compatibility
--     table](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-views-compatibility)
--
-- -   If @image@ has an [external
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats):
--
--     -   @format@ must be @VK_FORMAT_UNDEFINED@
--
--     -   The @pNext@ chain must contain an instance of
--         'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--         with a @conversion@ object created with the same external format
--         as @image@
--
--     -   All members of @components@ must be
--         @VK_COMPONENT_SWIZZLE_IDENTITY@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo'
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- -   @viewType@ /must/ be a valid 'VkImageViewType' value
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.Core10.Core.VkFormat'
--     value
--
-- -   @components@ /must/ be a valid @VkComponentMapping@ structure
--
-- -   @subresourceRange@ /must/ be a valid @VkImageSubresourceRange@
--     structure
--
-- = See Also
--
-- 'VkComponentMapping', 'Graphics.Vulkan.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'VkImageSubresourceRange', 'VkImageViewCreateFlags', 'VkImageViewType',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateImageView'
data VkImageViewCreateInfo = VkImageViewCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkImageViewCreateFlags
  , -- | @image@ is a @VkImage@ on which the view will be created.
  vkImage :: VkImage
  , -- | @viewType@ is an 'VkImageViewType' value specifying the type of the
  -- image view.
  vkViewType :: VkImageViewType
  , -- | @format@ is a 'Graphics.Vulkan.Core10.Core.VkFormat' describing the
  -- format and type used to interpret data elements in the image.
  vkFormat :: VkFormat
  , -- | @components@ is a 'VkComponentMapping' specifies a remapping of color
  -- components (or of depth or stencil components after they have been
  -- converted into color components).
  vkComponents :: VkComponentMapping
  , -- | @subresourceRange@ is a 'VkImageSubresourceRange' selecting the set of
  -- mipmap levels and array layers to be accessible to the view.
  vkSubresourceRange :: VkImageSubresourceRange
  }
  deriving (Eq, Show)

instance Storable VkImageViewCreateInfo where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek ptr = VkImageViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 36)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkImage (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkViewType (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkFormat (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkComponents (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkSubresourceRange (poked :: VkImageViewCreateInfo))
