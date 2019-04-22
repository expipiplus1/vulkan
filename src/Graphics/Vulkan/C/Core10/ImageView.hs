{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping(..)
  , VkComponentSwizzle(..)
  , pattern VK_COMPONENT_SWIZZLE_IDENTITY
  , pattern VK_COMPONENT_SWIZZLE_ZERO
  , pattern VK_COMPONENT_SWIZZLE_ONE
  , pattern VK_COMPONENT_SWIZZLE_R
  , pattern VK_COMPONENT_SWIZZLE_G
  , pattern VK_COMPONENT_SWIZZLE_B
  , pattern VK_COMPONENT_SWIZZLE_A
  , VkImageSubresourceRange(..)
  , VkImageView
  , VkImageViewCreateFlagBits(..)
  , VkImageViewCreateFlags
  , VkImageViewCreateInfo(..)
  , VkImageViewType(..)
  , pattern VK_IMAGE_VIEW_TYPE_1D
  , pattern VK_IMAGE_VIEW_TYPE_2D
  , pattern VK_IMAGE_VIEW_TYPE_3D
  , pattern VK_IMAGE_VIEW_TYPE_CUBE
  , pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
  , FN_vkCreateImageView
  , PFN_vkCreateImageView
  , vkCreateImageView
  , FN_vkDestroyImageView
  , PFN_vkDestroyImageView
  , vkDestroyImageView
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


import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlags
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkComponentMapping - Structure specifying a color component mapping
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'VkComponentSwizzle', 'VkImageViewCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
data VkComponentMapping = VkComponentMapping
  { -- | @r@ /must/ be a valid 'VkComponentSwizzle' value
  vkR :: VkComponentSwizzle
  , -- | @g@ /must/ be a valid 'VkComponentSwizzle' value
  vkG :: VkComponentSwizzle
  , -- | @b@ /must/ be a valid 'VkComponentSwizzle' value
  vkB :: VkComponentSwizzle
  , -- | @a@ /must/ be a valid 'VkComponentSwizzle' value
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

instance Zero VkComponentMapping where
  zero = VkComponentMapping zero
                            zero
                            zero
                            zero

-- ** VkComponentSwizzle

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
-- > | @components.r@                    | 'VK_COMPONENT_SWIZZLE_R'          |
-- > +-----------------------------------+-----------------------------------+
-- > | @components.g@                    | 'VK_COMPONENT_SWIZZLE_G'          |
-- > +-----------------------------------+-----------------------------------+
-- > | @components.b@                    | 'VK_COMPONENT_SWIZZLE_B'          |
-- > +-----------------------------------+-----------------------------------+
-- > | @components.a@                    | 'VK_COMPONENT_SWIZZLE_A'          |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Component Mappings Equivalent To 'VK_COMPONENT_SWIZZLE_IDENTITY'
--
-- = See Also
--
-- 'VkComponentMapping'
newtype VkComponentSwizzle = VkComponentSwizzle Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- | 'VK_COMPONENT_SWIZZLE_IDENTITY' specifies that the component is set to
-- the identity swizzle.
pattern VK_COMPONENT_SWIZZLE_IDENTITY :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_IDENTITY = VkComponentSwizzle 0

-- | 'VK_COMPONENT_SWIZZLE_ZERO' specifies that the component is set to zero.
pattern VK_COMPONENT_SWIZZLE_ZERO :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_ZERO = VkComponentSwizzle 1

-- | 'VK_COMPONENT_SWIZZLE_ONE' specifies that the component is set to either
-- 1 or 1.0, depending on whether the type of the image view format is
-- integer or floating-point respectively, as determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-definition Format Definition>
-- section for each 'Graphics.Vulkan.C.Core10.Core.VkFormat'.
pattern VK_COMPONENT_SWIZZLE_ONE :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_ONE = VkComponentSwizzle 2

-- | 'VK_COMPONENT_SWIZZLE_R' specifies that the component is set to the
-- value of the R component of the image.
pattern VK_COMPONENT_SWIZZLE_R :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_R = VkComponentSwizzle 3

-- | 'VK_COMPONENT_SWIZZLE_G' specifies that the component is set to the
-- value of the G component of the image.
pattern VK_COMPONENT_SWIZZLE_G :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_G = VkComponentSwizzle 4

-- | 'VK_COMPONENT_SWIZZLE_B' specifies that the component is set to the
-- value of the B component of the image.
pattern VK_COMPONENT_SWIZZLE_B :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_B = VkComponentSwizzle 5

-- | 'VK_COMPONENT_SWIZZLE_A' specifies that the component is set to the
-- value of the A component of the image.
pattern VK_COMPONENT_SWIZZLE_A :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_A = VkComponentSwizzle 6

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
-- if @format@ is a color, depth-only or stencil-only format, respectively.
-- If using a depth\/stencil format with both depth and stencil components,
-- @aspectMask@ /must/ include at least one of
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
-- and
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT',
-- and /can/ include both.
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
-- The @components@ member is of type 'VkComponentMapping', and describes a
-- remapping from components of the image to components of the vector
-- returned by shader image instructions. This remapping /must/ be identity
-- for storage image descriptors, input attachment descriptors, and
-- framebuffer attachments.
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
-- == Valid Usage (Implicit)
--
-- -   @aspectMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     values
--
-- -   @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'VkImageViewCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage'
data VkImageSubresourceRange = VkImageSubresourceRange
  { -- | @aspectMask@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
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

instance Zero VkImageSubresourceRange where
  zero = VkImageSubresourceRange zero
                                 zero
                                 zero
                                 zero
                                 zero

-- | Dummy data to tag the 'Ptr' with
data VkImageView_T
-- | VkImageView - Opaque handle to an image view object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle.VkImageViewHandleInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdBindShadingRateImageNV',
-- 'vkCreateImageView', 'vkDestroyImageView'
type VkImageView = Ptr VkImageView_T

-- ** VkImageViewCreateFlagBits

-- | VkImageViewCreateFlagBits - Bitmask specifying additional parameters of
-- an image view
--
-- = See Also
--
-- 'VkImageViewCreateFlags'
newtype VkImageViewCreateFlagBits = VkImageViewCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkImageViewCreateFlagBits where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageViewCreateFlagBits 0x00000001) = showString "VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT"
  showsPrec p (VkImageViewCreateFlagBits x) = showParen (p >= 11) (showString "VkImageViewCreateFlagBits " . showsPrec 11 x)

instance Read VkImageViewCreateFlagBits where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT", pure (VkImageViewCreateFlagBits 0x00000001))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageViewCreateFlagBits")
                        v <- step readPrec
                        pure (VkImageViewCreateFlagBits v)
                        )
                    )



-- | VkImageViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'VkImageViewCreateFlags' is a bitmask type for setting a mask of zero or
-- more 'VkImageViewCreateFlagBits'.
--
-- = See Also
--
-- 'VkImageViewCreateFlagBits', 'VkImageViewCreateInfo'
type VkImageViewCreateFlags = VkImageViewCreateFlagBits

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
-- time.
--
-- If @image@ was created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- flag, @format@ /can/ be different from the image’s format, but if they
-- are not equal they /must/ be /compatible/. Image format compatibility is
-- defined in the
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
-- > +---------+------------------------+-----------------------------------+
-- > | Dim,    | Image parameters       | View parameters                   |
-- > | Arrayed |                        |                                   |
-- > | ,       |                        |                                   |
-- > | MS      |                        |                                   |
-- > +=========+========================+===================================+
-- > |         | @imageType@ =          | @baseArrayLayer@ and @layerCount@ |
-- > |         | ci.@imageType@         | are members of the                |
-- > |         | @width@ =              | @subresourceRange@ member.        |
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
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'VK_IMAGE_VIEW_TYPE_1D'           |
-- > |         | e10.DeviceInitializati | @baseArrayLayer@ ≥ 0              |
-- > |         | on.VK_IMAGE_TYPE_1D'   | @layerCount@ = 1                  |
-- > |         | @width@ ≥ 1            |                                   |
-- > |         | @height@ = 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __1D,   | @imageType@ =          | @viewType@ =                      |
-- > | 1, 0__  | 'Graphics.Vulkan.C.Cor | 'VK_IMAGE_VIEW_TYPE_1D_ARRAY'     |
-- > |         | e10.DeviceInitializati | @baseArrayLayer@ ≥ 0              |
-- > |         | on.VK_IMAGE_TYPE_1D'   | @layerCount@ ≥ 1                  |
-- > |         | @width@ ≥ 1            |                                   |
-- > |         | @height@ = 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __2D,   | @imageType@ =          | @viewType@ =                      |
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'VK_IMAGE_VIEW_TYPE_2D'           |
-- > |         | e10.DeviceInitializati | @baseArrayLayer@ ≥ 0              |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @layerCount@ = 1                  |
-- > |         | @width@ ≥ 1            |                                   |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __2D,   | @imageType@ =          | @viewType@ =                      |
-- > | 1, 0__  | 'Graphics.Vulkan.C.Cor | 'VK_IMAGE_VIEW_TYPE_2D_ARRAY'     |
-- > |         | e10.DeviceInitializati | @baseArrayLayer@ ≥ 0              |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @layerCount@ ≥ 1                  |
-- > |         | @width@ ≥ 1            |                                   |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __2D,   | @imageType@ =          | @viewType@ =                      |
-- > | 0, 1__  | 'Graphics.Vulkan.C.Cor | 'VK_IMAGE_VIEW_TYPE_2D'           |
-- > |         | e10.DeviceInitializati | @baseArrayLayer@ ≥ 0              |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @layerCount@ = 1                  |
-- > |         | @width@ ≥ 1            |                                   |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ > 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __2D,   | @imageType@ =          | @viewType@ =                      |
-- > | 1, 1__  | 'Graphics.Vulkan.C.Cor | 'VK_IMAGE_VIEW_TYPE_2D_ARRAY'     |
-- > |         | e10.DeviceInitializati | @baseArrayLayer@ ≥ 0              |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @layerCount@ ≥ 1                  |
-- > |         | @width@ ≥ 1            |                                   |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ = 1            |                                   |
-- > |         | @arrayLayers@ ≥ 1      |                                   |
-- > |         | @samples@ > 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- > | __CUBE, | @imageType@ =          | @viewType@ =                      |
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'VK_IMAGE_VIEW_TYPE_CUBE'         |
-- > |         | e10.DeviceInitializati | @baseArrayLayer@ ≥ 0              |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @layerCount@ = 6                  |
-- > |         | @width@ ≥ 1            |                                   |
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
-- > | 1, 0__  | 'Graphics.Vulkan.C.Cor | 'VK_IMAGE_VIEW_TYPE_CUBE_ARRAY'   |
-- > |         | e10.DeviceInitializati | @baseArrayLayer@ ≥ 0              |
-- > |         | on.VK_IMAGE_TYPE_2D'   | @layerCount@ = 6 × /N/, /N/ ≥ 1   |
-- > |         | @width@ ≥ 1            |                                   |
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
-- > | 0, 0__  | 'Graphics.Vulkan.C.Cor | 'VK_IMAGE_VIEW_TYPE_3D'           |
-- > |         | e10.DeviceInitializati | @baseArrayLayer@ = 0              |
-- > |         | on.VK_IMAGE_TYPE_3D'   | @layerCount@ = 1                  |
-- > |         | @width@ ≥ 1            |                                   |
-- > |         | @height@ ≥ 1           |                                   |
-- > |         | @depth@ ≥ 1            |                                   |
-- > |         | @arrayLayers@ = 1      |                                   |
-- > |         | @samples@ = 1          |                                   |
-- > +---------+------------------------+-----------------------------------+
-- >
-- > Image and image view parameter compatibility requirements
--
-- == Valid Usage
--
-- -   If @image@ was not created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
--     then @viewType@ /must/ not be 'VK_IMAGE_VIEW_TYPE_CUBE' or
--     'VK_IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-imageCubeArray image cubemap arrays>
--     feature is not enabled, @viewType@ /must/ not be
--     'VK_IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   @image@ /must/ have been created with a @usage@ value containing at
--     least one of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_STORAGE_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
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
-- -   @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @subresourceRange.layerCount@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_ARRAY_LAYERS',
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @image@ was created with the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, @format@ /must/ be compatible with the @format@ used to create
--     @image@, as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
--
-- -   If @image@ was not created with the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, @format@ /must/ be identical to the @format@ used to create
--     @image@
--
-- -   If @image@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @subresourceRange@ and @viewType@ /must/ be compatible with the
--     image, as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-views-compatibility compatibility table>
--     ifdef::VK_VERSION_1_1,VK_KHR_maintenance2
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
--     its @usage@ member /must/ not include any bits that were not set in
--     the @usage@ member of the
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure used to
--     create @image@. endif::VK_VERSION_1_1,VK_KHR_maintenance2
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode.VkImageViewASTCDecodeModeEXT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be a valid combination of 'VkImageViewCreateFlagBits'
--     values
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @viewType@ /must/ be a valid 'VkImageViewType' value
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Core.VkFormat'
--     value
--
-- -   @components@ /must/ be a valid 'VkComponentMapping' structure
--
-- -   @subresourceRange@ /must/ be a valid 'VkImageSubresourceRange'
--     structure
--
-- = See Also
--
-- 'VkComponentMapping', 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'VkImageSubresourceRange', 'VkImageViewCreateFlags', 'VkImageViewType',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkCreateImageView'
data VkImageViewCreateInfo = VkImageViewCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkImageViewCreateFlagBits' describing
  -- additional parameters of the image view.
  vkFlags :: VkImageViewCreateFlags
  , -- | @image@ is a 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' on
  -- which the view will be created.
  vkImage :: VkImage
  , -- | @viewType@ is a 'VkImageViewType' value specifying the type of the image
  -- view.
  vkViewType :: VkImageViewType
  , -- | @format@ is a 'Graphics.Vulkan.C.Core10.Core.VkFormat' describing the
  -- format and type used to interpret texel blocks in the image.
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

instance Zero VkImageViewCreateInfo where
  zero = VkImageViewCreateInfo VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
                               zero
                               zero
                               zero
                               zero
                               zero
                               zero
                               zero

-- ** VkImageViewType

-- | VkImageViewType - Image view types
--
-- = Description
--
-- The exact image view type is partially implicit, based on the image’s
-- type and sample count, as well as the view creation parameters as
-- described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-views-compatibility image view compatibility table>
-- for 'vkCreateImageView'. This table also shows which SPIR-V
-- @OpTypeImage@ @Dim@ and @Arrayed@ parameters correspond to each image
-- view type.
--
-- = See Also
--
-- 'VkImageViewCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic.VkPhysicalDeviceImageViewImageFormatInfoEXT'
newtype VkImageViewType = VkImageViewType Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- | vkCreateImageView - Create an image view from an existing image
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the image view.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkImageViewCreateInfo' structure containing parameters to be used
--     to create the image view.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pView@ points to a 'VkImageView' handle in which the resulting
--     image view object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'VkImageViewCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pView@ /must/ be a valid pointer to a 'VkImageView' handle
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkImageView',
-- 'VkImageViewCreateInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateImageView" vkCreateImageView :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult
#else
vkCreateImageView :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult
vkCreateImageView deviceCmds = mkVkCreateImageView (pVkCreateImageView deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImageView
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult)
#endif

type FN_vkCreateImageView = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult
type PFN_vkCreateImageView = FunPtr FN_vkCreateImageView

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
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @imageView@
--     /must/ be a valid 'VkImageView' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkImageView'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyImageView" vkDestroyImageView :: ("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyImageView :: DeviceCmds -> ("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyImageView deviceCmds = mkVkDestroyImageView (pVkDestroyImageView deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyImageView
  :: FunPtr (("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyImageView = ("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyImageView = FunPtr FN_vkDestroyImageView
