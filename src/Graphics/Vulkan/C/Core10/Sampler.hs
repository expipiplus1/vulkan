{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Sampler
  ( VkBorderColor(..)
  , pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
  , pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE
  , pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE
  , VkFilter(..)
  , pattern VK_FILTER_NEAREST
  , pattern VK_FILTER_LINEAR
  , VkSampler
  , VkSamplerAddressMode(..)
  , pattern VK_SAMPLER_ADDRESS_MODE_REPEAT
  , pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  , VkSamplerCreateFlagBits(..)
  , VkSamplerCreateFlags
  , VkSamplerCreateInfo(..)
  , VkSamplerMipmapMode(..)
  , pattern VK_SAMPLER_MIPMAP_MODE_NEAREST
  , pattern VK_SAMPLER_MIPMAP_MODE_LINEAR
  , FN_vkCreateSampler
  , PFN_vkCreateSampler
  , vkCreateSampler
  , FN_vkDestroySampler
  , PFN_vkDestroySampler
  , vkDestroySampler
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkCompareOp(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkBorderColor

-- | VkBorderColor - Specify border color used for texture lookups
--
-- = Description
--
-- These colors are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-replacement Texel Replacement>.
--
-- = See Also
--
-- 'VkSamplerCreateInfo'
newtype VkBorderColor = VkBorderColor Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- | 'VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK' specifies a transparent,
-- floating-point format, black color.
pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VkBorderColor 0

-- | 'VK_BORDER_COLOR_INT_TRANSPARENT_BLACK' specifies a transparent, integer
-- format, black color.
pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = VkBorderColor 1

-- | 'VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK' specifies an opaque, floating-point
-- format, black color.
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = VkBorderColor 2

-- | 'VK_BORDER_COLOR_INT_OPAQUE_BLACK' specifies an opaque, integer format,
-- black color.
pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = VkBorderColor 3

-- | 'VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE' specifies an opaque, floating-point
-- format, white color.
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE :: VkBorderColor
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = VkBorderColor 4

-- | 'VK_BORDER_COLOR_INT_OPAQUE_WHITE' specifies an opaque, integer format,
-- white color.
pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE :: VkBorderColor
pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = VkBorderColor 5

-- ** VkFilter

-- | VkFilter - Specify filters used for texture lookups
--
-- = Description
--
-- These filters are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-filtering Texel Filtering>.
--
-- = See Also
--
-- 'VkSamplerCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
newtype VkFilter = VkFilter Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- | 'VK_FILTER_NEAREST' specifies nearest filtering.
pattern VK_FILTER_NEAREST :: VkFilter
pattern VK_FILTER_NEAREST = VkFilter 0

-- | 'VK_FILTER_LINEAR' specifies linear filtering.
pattern VK_FILTER_LINEAR :: VkFilter
pattern VK_FILTER_LINEAR = VkFilter 1

-- | Dummy data to tag the 'Ptr' with
data VkSampler_T
-- | VkSampler - Opaque handle to a sampler object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'vkCreateSampler', 'vkDestroySampler'
type VkSampler = Ptr VkSampler_T

-- ** VkSamplerAddressMode

-- | VkSamplerAddressMode - Specify behavior of sampling with texture
-- coordinates outside an image
--
-- = See Also
--
-- 'VkSamplerCreateInfo'
newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- | 'VK_SAMPLER_ADDRESS_MODE_REPEAT' specifies that the repeat wrap mode
-- will be used.
pattern VK_SAMPLER_ADDRESS_MODE_REPEAT :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0

-- | 'VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT' specifies that the mirrored
-- repeat wrap mode will be used.
pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = VkSamplerAddressMode 1

-- | 'VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE' specifies that the clamp to edge
-- wrap mode will be used.
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = VkSamplerAddressMode 2

-- | 'VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER' specifies that the clamp to
-- border wrap mode will be used.
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = VkSamplerAddressMode 3

-- ** VkSamplerCreateFlagBits

-- | VkSamplerCreateFlagBits - Bitmask specifying additional parameters of
-- sampler
--
-- = Description
--
-- __Note__
--
-- The approximations used when
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT'
-- is specified are implementation defined. Some implementations /may/
-- interpolate between fragment density levels in a subsampled image. In
-- that case, this bit /may/ be used to decide whether the interpolation
-- factors are calculated per fragment or at a coarser granularity.
--
-- = See Also
--
-- 'VkSamplerCreateFlags'
newtype VkSamplerCreateFlagBits = VkSamplerCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkSamplerCreateFlagBits where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkSamplerCreateFlagBits 0x00000001) = showString "VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT"
  showsPrec _ (VkSamplerCreateFlagBits 0x00000002) = showString "VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT"
  showsPrec p (VkSamplerCreateFlagBits x) = showParen (p >= 11) (showString "VkSamplerCreateFlagBits " . showsPrec 11 x)

instance Read VkSamplerCreateFlagBits where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT",                       pure (VkSamplerCreateFlagBits 0x00000001))
                             , ("VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT", pure (VkSamplerCreateFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerCreateFlagBits")
                        v <- step readPrec
                        pure (VkSamplerCreateFlagBits v)
                        )
                    )



-- | VkSamplerCreateFlags - Reserved for future use
--
-- = Description
--
-- 'VkSamplerCreateFlags' is a bitmask type for setting a mask of zero or
-- more 'VkSamplerCreateFlagBits'.
--
-- = See Also
--
-- 'VkSamplerCreateFlagBits', 'VkSamplerCreateInfo'
type VkSamplerCreateFlags = VkSamplerCreateFlagBits

-- | VkSamplerCreateInfo - Structure specifying parameters of a newly created
-- sampler
--
-- = Description
--
-- __Note__
--
-- @magFilter@ values of 'VK_FILTER_NEAREST' and 'VK_FILTER_LINEAR'
-- directly correspond to @GL_NEAREST@ and @GL_LINEAR@ magnification
-- filters. @minFilter@ and @mipmapMode@ combine to correspond to the
-- similarly named OpenGL minification filter of
-- @GL_minFilter_MIPMAP_mipmapMode@ (e.g. @minFilter@ of 'VK_FILTER_LINEAR'
-- and @mipmapMode@ of 'VK_SAMPLER_MIPMAP_MODE_NEAREST' correspond to
-- @GL_LINEAR_MIPMAP_NEAREST@).
--
-- There are no Vulkan filter modes that directly correspond to OpenGL
-- minification filters of @GL_LINEAR@ or @GL_NEAREST@, but they /can/ be
-- emulated using 'VK_SAMPLER_MIPMAP_MODE_NEAREST', @minLod@ = 0, and
-- @maxLod@ = 0.25, and using @minFilter@ = 'VK_FILTER_LINEAR' or
-- @minFilter@ = 'VK_FILTER_NEAREST', respectively.
--
-- Note that using a @maxLod@ of zero would cause
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-filtering magnification>
-- to always be performed, and the @magFilter@ to always be used. This is
-- valid, just not an exact match for OpenGL behavior. Clamping the maximum
-- LOD to 0.25 allows the λ value to be non-zero and minification to be
-- performed, while still always rounding down to the base level. If the
-- @minFilter@ and @magFilter@ are equal, then using a @maxLod@ of zero
-- also works.
--
-- The maximum number of sampler objects which /can/ be simultaneously
-- created on a device is implementation-dependent and specified by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-maxSamplerAllocationCount maxSamplerAllocationCount>
-- member of the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'
-- structure. If @maxSamplerAllocationCount@ is exceeded, 'vkCreateSampler'
-- will return 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_TOO_MANY_OBJECTS'.
--
-- Since 'VkSampler' is a non-dispatchable handle type, implementations
-- /may/ return the same handle for sampler state vectors that are
-- identical. In such cases, all such objects would only count once against
-- the @maxSamplerAllocationCount@ limit.
--
-- == Valid Usage
--
-- -   The absolute value of @mipLodBias@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxSamplerLodBias@
--
-- -   @maxLod@ /must/ be greater than or equal to @minLod@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-samplerAnisotropy anisotropic sampling>
--     feature is not enabled, @anisotropyEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If @anisotropyEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     @maxAnisotropy@ /must/ be between @1.0@ and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxSamplerAnisotropy@,
--     inclusive
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>
--     is enabled and
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT'
--     is not set for the format, @minFilter@ and @magFilter@ /must/ be
--     equal to the sampler Y’CBCR conversion’s @chromaFilter@
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @minFilter@ and @magFilter@
--     /must/ be equal
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @mipmapMode@ /must/ be
--     'VK_SAMPLER_MIPMAP_MODE_NEAREST'
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @minLod@ and @maxLod@
--     /must/ be zero
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @addressModeU@ and
--     @addressModeV@ /must/ each be either
--     'VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE' or
--     'VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @anisotropyEnable@ /must/
--     be 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @compareEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If any of @addressModeU@, @addressModeV@ or @addressModeW@ are
--     'VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER', @borderColor@ /must/ be a
--     valid 'VkBorderColor' value
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>
--     is enabled, @addressModeU@, @addressModeV@, and @addressModeW@
--     /must/ be 'VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE',
--     @anisotropyEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE', and
--     @unnormalizedCoordinates@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   The sampler reduction mode /must/ be set to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT'
--     if
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>
--     is enabled
--
-- -   If the
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_KHR_sampler_mirror_clamp_to_edge@
--     extension is not enabled, @addressModeU@, @addressModeV@ and
--     @addressModeW@ /must/ not be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_mirror_clamp_to_edge.VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
--
-- -   If @compareEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     @compareOp@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkCompareOp' value
--
-- -   If either @magFilter@ or @minFilter@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic.VK_FILTER_CUBIC_EXT',
--     @anisotropyEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If @compareEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the
--     @reductionMode@ member of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VkSamplerReductionModeCreateInfoEXT'
--     /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT'
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @minFilter@ and @magFilter@ /must/ be equal.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @mipmapMode@ /must/ be 'VK_SAMPLER_MIPMAP_MODE_NEAREST'.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @minLod@ and @maxLod@ /must/ be zero.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @addressModeU@ and @addressModeV@ /must/ each be either
--     'VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE' or
--     'VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @anisotropyEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @compareEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @unnormalizedCoordinates@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- Unresolved directive in VkSamplerCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkSamplerCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32', 'VkBorderColor',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkCompareOp', 'VkFilter',
-- 'VkSamplerAddressMode', 'VkSamplerCreateFlags', 'VkSamplerMipmapMode',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkCreateSampler'
data VkSamplerCreateInfo = VkSamplerCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkSamplerCreateFlagBits' describing additional
  -- parameters of the sampler.
  vkFlags :: VkSamplerCreateFlags
  , -- | @magFilter@ is a 'VkFilter' value specifying the magnification filter to
  -- apply to lookups.
  vkMagFilter :: VkFilter
  , -- | @minFilter@ is a 'VkFilter' value specifying the minification filter to
  -- apply to lookups.
  vkMinFilter :: VkFilter
  , -- | @mipmapMode@ is a 'VkSamplerMipmapMode' value specifying the mipmap
  -- filter to apply to lookups.
  vkMipmapMode :: VkSamplerMipmapMode
  , -- | @addressModeU@ is a 'VkSamplerAddressMode' value specifying the
  -- addressing mode for outside [0..1] range for U coordinate.
  vkAddressModeU :: VkSamplerAddressMode
  , -- | @addressModeV@ is a 'VkSamplerAddressMode' value specifying the
  -- addressing mode for outside [0..1] range for V coordinate.
  vkAddressModeV :: VkSamplerAddressMode
  , -- | @addressModeW@ is a 'VkSamplerAddressMode' value specifying the
  -- addressing mode for outside [0..1] range for W coordinate.
  vkAddressModeW :: VkSamplerAddressMode
  , -- | @mipLodBias@ is the bias to be added to mipmap LOD (level-of-detail)
  -- calculation and bias provided by image sampling functions in SPIR-V, as
  -- described in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-level-of-detail-operation Level-of-Detail Operation>
  -- section.
  vkMipLodBias :: CFloat
  , -- | @anisotropyEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' to enable
  -- anisotropic filtering, as described in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-anisotropic-filtering Texel Anisotropic Filtering>
  -- section, or 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' otherwise.
  vkAnisotropyEnable :: VkBool32
  , -- | @maxAnisotropy@ is the anisotropy value clamp used by the sampler when
  -- @anisotropyEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'. If
  -- @anisotropyEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE',
  -- @maxAnisotropy@ is ignored.
  vkMaxAnisotropy :: CFloat
  , -- | @compareEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' to enable
  -- comparison against a reference value during lookups, or
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' otherwise.
  --
  -- -   Note: Some implementations will default to shader state if this
  --     member does not match.
  --
  vkCompareEnable :: VkBool32
  , -- | @compareOp@ is a 'Graphics.Vulkan.C.Core10.Pipeline.VkCompareOp' value
  -- specifying the comparison function to apply to fetched data before
  -- filtering as described in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-depth-compare-operation Depth Compare Operation>
  -- section.
  vkCompareOp :: VkCompareOp
  , -- | @minLod@ and @maxLod@ are the values used to clamp the computed LOD
  -- value, as described in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-level-of-detail-operation Level-of-Detail Operation>
  -- section.
  vkMinLod :: CFloat
  , -- No documentation found for Nested "VkSamplerCreateInfo" "maxLod"
  vkMaxLod :: CFloat
  , -- | @borderColor@ is a 'VkBorderColor' value specifying the predefined
  -- border color to use.
  vkBorderColor :: VkBorderColor
  , -- | @unnormalizedCoordinates@ controls whether to use unnormalized or
  -- normalized texel coordinates to address texels of the image. When set to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the range of the image
  -- coordinates used to lookup the texel is in the range of zero to the
  -- image dimensions for x, y and z. When set to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' the range of image coordinates
  -- is zero to one.
  --
  -- When @unnormalizedCoordinates@ is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', images the sampler is used with
  -- in the shader have the following requirements:
  --
  -- -   The @viewType@ /must/ be either
  --     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D' or
  --     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D'.
  --
  -- -   The image view /must/ have a single layer and a single mip level.
  --
  -- When @unnormalizedCoordinates@ is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', image built-in functions in the
  -- shader that use the sampler have the following requirements:
  --
  -- -   The functions /must/ not use projection.
  --
  -- -   The functions /must/ not use offsets.
  --
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

instance Zero VkSamplerCreateInfo where
  zero = VkSamplerCreateInfo VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
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
                             zero
                             zero
                             zero

-- ** VkSamplerMipmapMode

-- | VkSamplerMipmapMode - Specify mipmap mode used for texture lookups
--
-- = Description
--
-- These modes are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-filtering Texel Filtering>.
--
-- = See Also
--
-- 'VkSamplerCreateInfo'
newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- | 'VK_SAMPLER_MIPMAP_MODE_NEAREST' specifies nearest filtering.
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST :: VkSamplerMipmapMode
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0

-- | 'VK_SAMPLER_MIPMAP_MODE_LINEAR' specifies linear filtering.
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR :: VkSamplerMipmapMode
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1

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
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pSampler@ points to a 'VkSampler' handle in which the resulting
--     sampler object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateSampler.txt -
-- include::{generated}\/validity\/protos\/vkCreateSampler.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkSampler',
-- 'VkSamplerCreateInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSampler" vkCreateSampler :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult
#else
vkCreateSampler :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult
vkCreateSampler deviceCmds = mkVkCreateSampler (pVkCreateSampler deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSampler
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult)
#endif

type FN_vkCreateSampler = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult
type PFN_vkCreateSampler = FunPtr FN_vkCreateSampler

-- | vkDestroySampler - Destroy a sampler object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the sampler.
--
-- -   @sampler@ is the sampler to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @sampler@ /must/ have completed
--     execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @sampler@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @sampler@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroySampler.txt -
-- include::{generated}\/validity\/protos\/vkDestroySampler.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkSampler'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroySampler" vkDestroySampler :: ("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroySampler :: DeviceCmds -> ("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroySampler deviceCmds = mkVkDestroySampler (pVkDestroySampler deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySampler
  :: FunPtr (("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroySampler = ("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySampler = FunPtr FN_vkDestroySampler
