{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Sampler
  ( VkBorderColor(..)
  , pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
  , pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE
  , pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE
  , VkSamplerAddressMode(..)
  , pattern VK_SAMPLER_ADDRESS_MODE_REPEAT
  , pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  , VkFilter(..)
  , pattern VK_FILTER_NEAREST
  , pattern VK_FILTER_LINEAR
  , VkSamplerMipmapMode(..)
  , pattern VK_SAMPLER_MIPMAP_MODE_NEAREST
  , pattern VK_SAMPLER_MIPMAP_MODE_LINEAR
  , VkSamplerCreateFlags(..)
  , VkSampler
  , vkCreateSampler
  , vkDestroySampler
  , VkSamplerCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Foreign.C.Types
  ( CFloat(..)
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkCompareOp(..)
  )


-- ** VkBorderColor

-- | VkBorderColor - Specify border color used for texture lookups
--
-- = Description
--
-- -   @VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK@ specifies a transparent,
--     floating-point format, black color.
--
-- -   @VK_BORDER_COLOR_INT_TRANSPARENT_BLACK@ specifies a transparent,
--     integer format, black color.
--
-- -   @VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK@ specifies an opaque,
--     floating-point format, black color.
--
-- -   @VK_BORDER_COLOR_INT_OPAQUE_BLACK@ specifies an opaque, integer
--     format, black color.
--
-- -   @VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE@ specifies an opaque,
--     floating-point format, white color.
--
-- -   @VK_BORDER_COLOR_INT_OPAQUE_WHITE@ specifies an opaque, integer
--     format, white color.
--
-- These colors are described in detail in [Texel
-- Replacement](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-texel-replacement).
--
-- = See Also
--
-- 'VkSamplerCreateInfo'
newtype VkBorderColor = VkBorderColor Int32
  deriving (Eq, Ord, Storable)

instance Show VkBorderColor where
  showsPrec _ VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = showString "VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK"
  showsPrec _ VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = showString "VK_BORDER_COLOR_INT_TRANSPARENT_BLACK"
  showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK"
  showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_BLACK = showString "VK_BORDER_COLOR_INT_OPAQUE_BLACK"
  showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE"
  showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_WHITE = showString "VK_BORDER_COLOR_INT_OPAQUE_WHITE"
  showsPrec p (VkBorderColor x) = showParen (p >= 11) (showString "VkBorderColor " . showsPrec 11 x)

instance Read VkBorderColor where
  readPrec = parens ( choose [ ("VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK", pure VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK)
                             , ("VK_BORDER_COLOR_INT_TRANSPARENT_BLACK",   pure VK_BORDER_COLOR_INT_TRANSPARENT_BLACK)
                             , ("VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK",      pure VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK)
                             , ("VK_BORDER_COLOR_INT_OPAQUE_BLACK",        pure VK_BORDER_COLOR_INT_OPAQUE_BLACK)
                             , ("VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE",      pure VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE)
                             , ("VK_BORDER_COLOR_INT_OPAQUE_WHITE",        pure VK_BORDER_COLOR_INT_OPAQUE_WHITE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBorderColor")
                        v <- step readPrec
                        pure (VkBorderColor v)
                        )
                    )

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK"
pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VkBorderColor 0

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_TRANSPARENT_BLACK"
pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = VkBorderColor 1

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK"
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = VkBorderColor 2

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_OPAQUE_BLACK"
pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = VkBorderColor 3

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE"
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE :: VkBorderColor
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = VkBorderColor 4

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_OPAQUE_WHITE"
pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE :: VkBorderColor
pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = VkBorderColor 5
-- ** VkSamplerAddressMode

-- | VkSamplerAddressMode - Specify behavior of sampling with texture
-- coordinates outside an image
--
-- = See Also
--
-- 'VkSamplerCreateInfo'
newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerAddressMode where
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_REPEAT = showString "VK_SAMPLER_ADDRESS_MODE_REPEAT"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = showString "VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkSamplerAddressMode 4) = showString "VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE"
  showsPrec p (VkSamplerAddressMode x) = showParen (p >= 11) (showString "VkSamplerAddressMode " . showsPrec 11 x)

instance Read VkSamplerAddressMode where
  readPrec = parens ( choose [ ("VK_SAMPLER_ADDRESS_MODE_REPEAT",          pure VK_SAMPLER_ADDRESS_MODE_REPEAT)
                             , ("VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT", pure VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT)
                             , ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE",   pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)
                             , ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER", pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE", pure (VkSamplerAddressMode 4))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerAddressMode")
                        v <- step readPrec
                        pure (VkSamplerAddressMode v)
                        )
                    )

-- | @VK_SAMPLER_ADDRESS_MODE_REPEAT@ specifies that the repeat wrap mode
-- will be used.
pattern VK_SAMPLER_ADDRESS_MODE_REPEAT :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0

-- | @VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT@ specifies that the mirrored
-- repeat wrap mode will be used.
pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = VkSamplerAddressMode 1

-- | @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE@ specifies that the clamp to edge
-- wrap mode will be used.
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = VkSamplerAddressMode 2

-- | @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER@ specifies that the clamp to
-- border wrap mode will be used.
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = VkSamplerAddressMode 3
-- ** VkFilter

-- | VkFilter - Specify filters used for texture lookups
--
-- = Description
--
-- -   @VK_FILTER_NEAREST@ specifies nearest filtering.
--
-- -   @VK_FILTER_LINEAR@ specifies linear filtering.
--
-- -   @VK_FILTER_CUBIC_IMG@ specifies cubic filtering.
--
-- These filters are described in detail in [Texel
-- Filtering](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-texel-filtering).
--
-- = See Also
--
-- 'VkSamplerCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBlitImage'
newtype VkFilter = VkFilter Int32
  deriving (Eq, Ord, Storable)

instance Show VkFilter where
  showsPrec _ VK_FILTER_NEAREST = showString "VK_FILTER_NEAREST"
  showsPrec _ VK_FILTER_LINEAR = showString "VK_FILTER_LINEAR"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkFilter 1000015000) = showString "VK_FILTER_CUBIC_IMG"
  showsPrec p (VkFilter x) = showParen (p >= 11) (showString "VkFilter " . showsPrec 11 x)

instance Read VkFilter where
  readPrec = parens ( choose [ ("VK_FILTER_NEAREST", pure VK_FILTER_NEAREST)
                             , ("VK_FILTER_LINEAR",  pure VK_FILTER_LINEAR)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_FILTER_CUBIC_IMG", pure (VkFilter 1000015000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFilter")
                        v <- step readPrec
                        pure (VkFilter v)
                        )
                    )

-- No documentation found for Nested "VkFilter" "VK_FILTER_NEAREST"
pattern VK_FILTER_NEAREST :: VkFilter
pattern VK_FILTER_NEAREST = VkFilter 0

-- No documentation found for Nested "VkFilter" "VK_FILTER_LINEAR"
pattern VK_FILTER_LINEAR :: VkFilter
pattern VK_FILTER_LINEAR = VkFilter 1
-- ** VkSamplerMipmapMode

-- | VkSamplerMipmapMode - Specify mipmap mode used for texture lookups
--
-- = Description
--
-- -   @VK_SAMPLER_MIPMAP_MODE_NEAREST@ specifies nearest filtering.
--
-- -   @VK_SAMPLER_MIPMAP_MODE_LINEAR@ specifies linear filtering.
--
-- These modes are described in detail in [Texel
-- Filtering](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-texel-filtering).
--
-- = See Also
--
-- 'VkSamplerCreateInfo'
newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerMipmapMode where
  showsPrec _ VK_SAMPLER_MIPMAP_MODE_NEAREST = showString "VK_SAMPLER_MIPMAP_MODE_NEAREST"
  showsPrec _ VK_SAMPLER_MIPMAP_MODE_LINEAR = showString "VK_SAMPLER_MIPMAP_MODE_LINEAR"
  showsPrec p (VkSamplerMipmapMode x) = showParen (p >= 11) (showString "VkSamplerMipmapMode " . showsPrec 11 x)

instance Read VkSamplerMipmapMode where
  readPrec = parens ( choose [ ("VK_SAMPLER_MIPMAP_MODE_NEAREST", pure VK_SAMPLER_MIPMAP_MODE_NEAREST)
                             , ("VK_SAMPLER_MIPMAP_MODE_LINEAR",  pure VK_SAMPLER_MIPMAP_MODE_LINEAR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerMipmapMode")
                        v <- step readPrec
                        pure (VkSamplerMipmapMode v)
                        )
                    )

-- No documentation found for Nested "VkSamplerMipmapMode" "VK_SAMPLER_MIPMAP_MODE_NEAREST"
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST :: VkSamplerMipmapMode
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0

-- No documentation found for Nested "VkSamplerMipmapMode" "VK_SAMPLER_MIPMAP_MODE_LINEAR"
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR :: VkSamplerMipmapMode
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1
-- ** VkSamplerCreateFlags

-- | VkSamplerCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkSamplerCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkSamplerCreateInfo'
newtype VkSamplerCreateFlags = VkSamplerCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSamplerCreateFlags where
  
  showsPrec p (VkSamplerCreateFlags x) = showParen (p >= 11) (showString "VkSamplerCreateFlags " . showsPrec 11 x)

instance Read VkSamplerCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerCreateFlags")
                        v <- step readPrec
                        pure (VkSamplerCreateFlags v)
                        )
                    )


-- | Dummy data to tag the 'Ptr' with
data VkSampler_T
-- | VkSampler - Opaque handle to a sampler object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'vkCreateSampler', 'vkDestroySampler'
type VkSampler = Ptr VkSampler_T
-- | vkCreateSampler - Create a new sampler object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the sampler.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkSamplerCreateInfo' structure specifying the state of the sampler
--     object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pSampler@ points to a 'VkSampler' handle in which the resulting
--     sampler object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkSamplerCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pSampler@ /must/ be a valid pointer to a @VkSampler@ handle
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
--     -   @VK_ERROR_TOO_MANY_OBJECTS@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkSampler',
-- 'VkSamplerCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSampler" vkCreateSampler :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult
-- | vkDestroySampler - Destroy a sampler object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the sampler.
--
-- -   @sampler@ is the sampler to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @sampler@ /must/ have completed
--     execution
--
-- -   If @VkAllocationCallbacks@ were provided when @sampler@ was created,
--     a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @sampler@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @sampler@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @sampler@ /must/
--     be a valid @VkSampler@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @sampler@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @sampler@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkSampler'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroySampler" vkDestroySampler :: ("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkSamplerCreateInfo - Structure specifying parameters of a newly created
-- sampler
--
-- = Members
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to an extension-specific structure.
--
-- -   @flags@ is reserved for future use.
--
-- -   @magFilter@ is a 'VkFilter' value specifying the magnification
--     filter to apply to lookups.
--
-- -   @minFilter@ is a 'VkFilter' value specifying the minification filter
--     to apply to lookups.
--
-- -   @mipmapMode@ is a 'VkSamplerMipmapMode' value specifying the mipmap
--     filter to apply to lookups.
--
-- -   @addressModeU@ is a 'VkSamplerAddressMode' value specifying the
--     addressing mode for outside [0..1] range for U coordinate.
--
-- -   @addressModeV@ is a 'VkSamplerAddressMode' value specifying the
--     addressing mode for outside [0..1] range for V coordinate.
--
-- -   @addressModeW@ is a 'VkSamplerAddressMode' value specifying the
--     addressing mode for outside [0..1] range for W coordinate.
--
-- -   @mipLodBias@ is the bias to be added to mipmap LOD (level-of-detail)
--     calculation and bias provided by image sampling functions in SPIR-V,
--     as described in the [Level-of-Detail
--     Operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-level-of-detail-operation)
--     section.
--
-- -   @anisotropyEnable@ is @VK_TRUE@ to enable anisotropic filtering, as
--     described in the [Texel Anisotropic
--     Filtering](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-texel-anisotropic-filtering)
--     section, or @VK_FALSE@ otherwise.
--
-- -   @maxAnisotropy@ is the anisotropy value clamp used by the sampler
--     when @anisotropyEnable@ is @VK_TRUE@. If @anisotropyEnable@ is
--     @VK_FALSE@, @maxAnisotropy@ is ignored.
--
-- -   @compareEnable@ is @VK_TRUE@ to enable comparison against a
--     reference value during lookups, or @VK_FALSE@ otherwise.
--
--     -   Note: Some implementations will default to shader state if this
--         member does not match.
--
-- -   @compareOp@ is a 'Graphics.Vulkan.Core10.Pipeline.VkCompareOp' value
--     specifying the comparison function to apply to fetched data before
--     filtering as described in the [Depth Compare
--     Operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-depth-compare-operation)
--     section.
--
-- -   @minLod@ and @maxLod@ are the values used to clamp the computed LOD
--     value, as described in the [Level-of-Detail
--     Operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-level-of-detail-operation)
--     section. @maxLod@ /must/ be greater than or equal to @minLod@.
--
-- -   @borderColor@ is a 'VkBorderColor' value specifying the predefined
--     border color to use.
--
-- -   @unnormalizedCoordinates@ controls whether to use unnormalized or
--     normalized texel coordinates to address texels of the image. When
--     set to @VK_TRUE@, the range of the image coordinates used to lookup
--     the texel is in the range of zero to the image dimensions for x, y
--     and z. When set to @VK_FALSE@ the range of image coordinates is zero
--     to one. When @unnormalizedCoordinates@ is @VK_TRUE@, samplers have
--     the following requirements:
--
--     -   @minFilter@ and @magFilter@ /must/ be equal.
--
--     -   @mipmapMode@ /must/ be @VK_SAMPLER_MIPMAP_MODE_NEAREST@.
--
--     -   @minLod@ and @maxLod@ /must/ be zero.
--
--     -   @addressModeU@ and @addressModeV@ /must/ each be either
--         @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE@ or
--         @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER@.
--
--     -   @anisotropyEnable@ /must/ be @VK_FALSE@.
--
--     -   @compareEnable@ /must/ be @VK_FALSE@.
--
--     -   The sampler /must/ not enable sampler Y’CBCR conversion.
--
-- -   When @unnormalizedCoordinates@ is @VK_TRUE@, images the sampler is
--     used with in the shader have the following requirements:
--
--     -   The @viewType@ /must/ be either @VK_IMAGE_VIEW_TYPE_1D@ or
--         @VK_IMAGE_VIEW_TYPE_2D@.
--
--     -   The image view /must/ have a single layer and a single mip
--         level.
--
-- -   When @unnormalizedCoordinates@ is @VK_TRUE@, image built-in
--     functions in the shader that use the sampler have the following
--     requirements:
--
--     -   The functions /must/ not use projection.
--
--     -   The functions /must/ not use offsets.
--
-- = Description
--
-- __Note__
--
-- @magFilter@ values of @VK_FILTER_NEAREST@ and @VK_FILTER_LINEAR@
-- directly correspond to @GL_NEAREST@ and @GL_LINEAR@ magnification
-- filters. @minFilter@ and @mipmapMode@ combine to correspond to the
-- similarly named OpenGL minification filter of
-- @GL_minFilter_MIPMAP_mipmapMode@ (e.g. @minFilter@ of @VK_FILTER_LINEAR@
-- and @mipmapMode@ of @VK_SAMPLER_MIPMAP_MODE_NEAREST@ correspond to
-- @GL_LINEAR_MIPMAP_NEAREST@).
--
-- There are no Vulkan filter modes that directly correspond to OpenGL
-- minification filters of @GL_LINEAR@ or @GL_NEAREST@, but they /can/ be
-- emulated using @VK_SAMPLER_MIPMAP_MODE_NEAREST@, @minLod@ = 0, and
-- @maxLod@ = 0.25, and using @minFilter@ = @VK_FILTER_LINEAR@ or
-- @minFilter@ = @VK_FILTER_NEAREST@, respectively.
--
-- Note that using a @maxLod@ of zero would cause
-- [magnification](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-texel-filtering)
-- to always be performed, and the @magFilter@ to always be used. This is
-- valid, just not an exact match for OpenGL behavior. Clamping the maximum
-- LOD to 0.25 allows the λ value to be non-zero and minification to be
-- performed, while still always rounding down to the base level. If the
-- @minFilter@ and @magFilter@ are equal, then using a @maxLod@ of zero
-- also works.
--
-- The maximum number of sampler objects which /can/ be simultaneously
-- created on a device is implementation-dependent and specified by the
-- [maxSamplerAllocationCount](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-maxSamplerAllocationCount)
-- member of the
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'
-- structure. If @maxSamplerAllocationCount@ is exceeded, @vkCreateSampler@
-- will return @VK_ERROR_TOO_MANY_OBJECTS@.
--
-- Since 'VkSampler' is a non-dispatchable handle type, implementations
-- /may/ return the same handle for sampler state vectors that are
-- identical. In such cases, all such objects would only count once against
-- the @maxSamplerAllocationCount@ limit.
--
-- == Valid Usage
--
-- -   The absolute value of @mipLodBias@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxSamplerLodBias@
--
-- -   If the [anisotropic
--     sampling](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-samplerAnisotropy)
--     feature is not enabled, @anisotropyEnable@ /must/ be @VK_FALSE@
--
-- -   If @anisotropyEnable@ is @VK_TRUE@, @maxAnisotropy@ /must/ be
--     between @1.0@ and @VkPhysicalDeviceLimits@::@maxSamplerAnisotropy@,
--     inclusive
--
-- -   If [sampler Y’CBCR
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion)
--     is enabled and
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT@
--     is not set for the format, @minFilter@ and @magFilter@ /must/ be
--     equal to the sampler Y’CBCR conversion’s @chromaFilter@
--
-- -   If @unnormalizedCoordinates@ is @VK_TRUE@, @minFilter@ and
--     @magFilter@ /must/ be equal
--
-- -   If @unnormalizedCoordinates@ is @VK_TRUE@, @mipmapMode@ /must/ be
--     @VK_SAMPLER_MIPMAP_MODE_NEAREST@
--
-- -   If @unnormalizedCoordinates@ is @VK_TRUE@, @minLod@ and @maxLod@
--     /must/ be zero
--
-- -   If @unnormalizedCoordinates@ is @VK_TRUE@, @addressModeU@ and
--     @addressModeV@ /must/ each be either
--     @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE@ or
--     @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER@
--
-- -   If @unnormalizedCoordinates@ is @VK_TRUE@, @anisotropyEnable@ /must/
--     be @VK_FALSE@
--
-- -   If @unnormalizedCoordinates@ is @VK_TRUE@, @compareEnable@ /must/ be
--     @VK_FALSE@
--
-- -   If any of @addressModeU@, @addressModeV@ or @addressModeW@ are
--     @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER@, @borderColor@ /must/ be a
--     valid 'VkBorderColor' value
--
-- -   If [sampler Y’CBCR
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion)
--     is enabled, @addressModeU@, @addressModeV@, and @addressModeW@
--     /must/ be @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE@,
--     @anisotropyEnable@ /must/ be @VK_FALSE@, and
--     @unnormalizedCoordinates@ /must/ be @VK_FALSE@
--
-- -   The sampler reduction mode /must/ be set to
--     @VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT@ if [sampler Y’CBCR
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion)
--     is enabled
--
-- -   If the @{html_spec_relative}#VK_KHR_sampler_mirror_clamp_to_edge@
--     extension is not enabled, @addressModeU@, @addressModeV@ and
--     @addressModeW@ /must/ not be
--     @VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE@
--
-- -   If @compareEnable@ is @VK_TRUE@, @compareOp@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Pipeline.VkCompareOp' value
--
-- -   If either @magFilter@ or @minFilter@ is @VK_FILTER_CUBIC_IMG@,
--     @anisotropyEnable@ /must/ be @VK_FALSE@
--
-- -   If either @magFilter@ or @minFilter@ is @VK_FILTER_CUBIC_IMG@, the
--     @reductionMode@ member of
--     'Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax.VkSamplerReductionModeCreateInfoEXT'
--     /must/ be @VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT@
--
-- -   If @compareEnable@ is @VK_TRUE@, the @reductionMode@ member of
--     'Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax.VkSamplerReductionModeCreateInfoEXT'
--     /must/ be @VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax.VkSamplerReductionModeCreateInfoEXT'
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @magFilter@ /must/ be a valid 'VkFilter' value
--
-- -   @minFilter@ /must/ be a valid 'VkFilter' value
--
-- -   @mipmapMode@ /must/ be a valid 'VkSamplerMipmapMode' value
--
-- -   @addressModeU@ /must/ be a valid 'VkSamplerAddressMode' value
--
-- -   @addressModeV@ /must/ be a valid 'VkSamplerAddressMode' value
--
-- -   @addressModeW@ /must/ be a valid 'VkSamplerAddressMode' value
--
-- = See Also
--
-- @VkBool32@, 'VkBorderColor',
-- 'Graphics.Vulkan.Core10.Pipeline.VkCompareOp', 'VkFilter',
-- 'VkSamplerAddressMode', 'VkSamplerCreateFlags', 'VkSamplerMipmapMode',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateSampler'
data VkSamplerCreateInfo = VkSamplerCreateInfo
  { -- No documentation found for Nested "VkSamplerCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSamplerCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSamplerCreateInfo" "flags"
  vkFlags :: VkSamplerCreateFlags
  , -- No documentation found for Nested "VkSamplerCreateInfo" "magFilter"
  vkMagFilter :: VkFilter
  , -- No documentation found for Nested "VkSamplerCreateInfo" "minFilter"
  vkMinFilter :: VkFilter
  , -- No documentation found for Nested "VkSamplerCreateInfo" "mipmapMode"
  vkMipmapMode :: VkSamplerMipmapMode
  , -- No documentation found for Nested "VkSamplerCreateInfo" "addressModeU"
  vkAddressModeU :: VkSamplerAddressMode
  , -- No documentation found for Nested "VkSamplerCreateInfo" "addressModeV"
  vkAddressModeV :: VkSamplerAddressMode
  , -- No documentation found for Nested "VkSamplerCreateInfo" "addressModeW"
  vkAddressModeW :: VkSamplerAddressMode
  , -- No documentation found for Nested "VkSamplerCreateInfo" "mipLodBias"
  vkMipLodBias :: CFloat
  , -- No documentation found for Nested "VkSamplerCreateInfo" "anisotropyEnable"
  vkAnisotropyEnable :: VkBool32
  , -- No documentation found for Nested "VkSamplerCreateInfo" "maxAnisotropy"
  vkMaxAnisotropy :: CFloat
  , -- No documentation found for Nested "VkSamplerCreateInfo" "compareEnable"
  vkCompareEnable :: VkBool32
  , -- No documentation found for Nested "VkSamplerCreateInfo" "compareOp"
  vkCompareOp :: VkCompareOp
  , -- No documentation found for Nested "VkSamplerCreateInfo" "minLod"
  vkMinLod :: CFloat
  , -- No documentation found for Nested "VkSamplerCreateInfo" "maxLod"
  vkMaxLod :: CFloat
  , -- No documentation found for Nested "VkSamplerCreateInfo" "borderColor"
  vkBorderColor :: VkBorderColor
  , -- No documentation found for Nested "VkSamplerCreateInfo" "unnormalizedCoordinates"
  vkUnnormalizedCoordinates :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkSamplerCreateInfo where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek ptr = VkSamplerCreateInfo <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 20)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 28)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 36)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 44)
                                 <*> peek (ptr `plusPtr` 48)
                                 <*> peek (ptr `plusPtr` 52)
                                 <*> peek (ptr `plusPtr` 56)
                                 <*> peek (ptr `plusPtr` 60)
                                 <*> peek (ptr `plusPtr` 64)
                                 <*> peek (ptr `plusPtr` 68)
                                 <*> peek (ptr `plusPtr` 72)
                                 <*> peek (ptr `plusPtr` 76)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkMagFilter (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkMinFilter (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkMipmapMode (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkAddressModeU (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkAddressModeV (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkAddressModeW (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkMipLodBias (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkAnisotropyEnable (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkMaxAnisotropy (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkCompareEnable (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 60) (vkCompareOp (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkMinLod (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 68) (vkMaxLod (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkBorderColor (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 76) (vkUnnormalizedCoordinates (poked :: VkSamplerCreateInfo))
