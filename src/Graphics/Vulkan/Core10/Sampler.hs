{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Sampler
  ( BorderColor
  , pattern BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
  , pattern BORDER_COLOR_INT_TRANSPARENT_BLACK
  , pattern BORDER_COLOR_FLOAT_OPAQUE_BLACK
  , pattern BORDER_COLOR_INT_OPAQUE_BLACK
  , pattern BORDER_COLOR_FLOAT_OPAQUE_WHITE
  , pattern BORDER_COLOR_INT_OPAQUE_WHITE
  , Filter
  , pattern FILTER_NEAREST
  , pattern FILTER_LINEAR
  , Sampler
  , SamplerAddressMode
  , pattern SAMPLER_ADDRESS_MODE_REPEAT
  , pattern SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
  , pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  , SamplerCreateFlagBits
  , SamplerCreateFlags
  , withCStructSamplerCreateInfo
  , fromCStructSamplerCreateInfo
  , SamplerCreateInfo(..)
  , SamplerMipmapMode
  , pattern SAMPLER_MIPMAP_MODE_NEAREST
  , pattern SAMPLER_MIPMAP_MODE_LINEAR
  , createSampler
  , destroySampler
  , withSampler
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.C.Types
  ( CFloat(..)
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
  , pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkBorderColor(..)
  , VkFilter(..)
  , VkSamplerAddressMode(..)
  , VkSamplerCreateFlagBits(..)
  , VkSamplerCreateInfo(..)
  , VkSamplerMipmapMode(..)
  , VkSampler
  , vkCreateSampler
  , vkDestroySampler
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE
  , pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
  , pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE
  , pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK
  , pattern VK_FILTER_LINEAR
  , pattern VK_FILTER_NEAREST
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
  , pattern VK_SAMPLER_ADDRESS_MODE_REPEAT
  , pattern VK_SAMPLER_MIPMAP_MODE_LINEAR
  , pattern VK_SAMPLER_MIPMAP_MODE_NEAREST
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( CompareOp
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- | VkBorderColor - Specify border color used for texture lookups
--
-- = Description
--
-- These colors are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-replacement Texel Replacement>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo'
type BorderColor = VkBorderColor


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK'
-- specifies a transparent, floating-point format, black color.
pattern BORDER_COLOR_FLOAT_TRANSPARENT_BLACK :: (a ~ BorderColor) => a
pattern BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_BORDER_COLOR_INT_TRANSPARENT_BLACK'
-- specifies a transparent, integer format, black color.
pattern BORDER_COLOR_INT_TRANSPARENT_BLACK :: (a ~ BorderColor) => a
pattern BORDER_COLOR_INT_TRANSPARENT_BLACK = VK_BORDER_COLOR_INT_TRANSPARENT_BLACK


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK'
-- specifies an opaque, floating-point format, black color.
pattern BORDER_COLOR_FLOAT_OPAQUE_BLACK :: (a ~ BorderColor) => a
pattern BORDER_COLOR_FLOAT_OPAQUE_BLACK = VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_BORDER_COLOR_INT_OPAQUE_BLACK'
-- specifies an opaque, integer format, black color.
pattern BORDER_COLOR_INT_OPAQUE_BLACK :: (a ~ BorderColor) => a
pattern BORDER_COLOR_INT_OPAQUE_BLACK = VK_BORDER_COLOR_INT_OPAQUE_BLACK


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE'
-- specifies an opaque, floating-point format, white color.
pattern BORDER_COLOR_FLOAT_OPAQUE_WHITE :: (a ~ BorderColor) => a
pattern BORDER_COLOR_FLOAT_OPAQUE_WHITE = VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_BORDER_COLOR_INT_OPAQUE_WHITE'
-- specifies an opaque, integer format, white color.
pattern BORDER_COLOR_INT_OPAQUE_WHITE :: (a ~ BorderColor) => a
pattern BORDER_COLOR_INT_OPAQUE_WHITE = VK_BORDER_COLOR_INT_OPAQUE_WHITE

-- | VkFilter - Specify filters used for texture lookups
--
-- = Description
--
-- These filters are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-filtering Texel Filtering>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
type Filter = VkFilter


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_NEAREST' specifies nearest
-- filtering.
pattern FILTER_NEAREST :: (a ~ Filter) => a
pattern FILTER_NEAREST = VK_FILTER_NEAREST


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' specifies linear
-- filtering.
pattern FILTER_LINEAR :: (a ~ Filter) => a
pattern FILTER_LINEAR = VK_FILTER_LINEAR

-- | VkSampler - Opaque handle to a sampler object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.C.Core10.Sampler.vkCreateSampler',
-- 'Graphics.Vulkan.C.Core10.Sampler.vkDestroySampler'
type Sampler = VkSampler

-- | VkSamplerAddressMode - Specify behavior of sampling with texture
-- coordinates outside an image
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo'
type SamplerAddressMode = VkSamplerAddressMode


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_REPEAT'
-- specifies that the repeat wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_REPEAT :: (a ~ SamplerAddressMode) => a
pattern SAMPLER_ADDRESS_MODE_REPEAT = VK_SAMPLER_ADDRESS_MODE_REPEAT


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT'
-- specifies that the mirrored repeat wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT :: (a ~ SamplerAddressMode) => a
pattern SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
-- specifies that the clamp to edge wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE :: (a ~ SamplerAddressMode) => a
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'
-- specifies that the clamp to border wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER :: (a ~ SamplerAddressMode) => a
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER

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
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateFlags'
type SamplerCreateFlagBits = VkSamplerCreateFlagBits

-- | VkSamplerCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateFlags' is a bitmask
-- type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo'
type SamplerCreateFlags = SamplerCreateFlagBits


-- | VkSamplerCreateInfo - Structure specifying parameters of a newly created
-- sampler
--
-- = Description
--
-- __Note__
--
-- @magFilter@ values of
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_NEAREST' and
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' directly correspond
-- to @GL_NEAREST@ and @GL_LINEAR@ magnification filters. @minFilter@ and
-- @mipmapMode@ combine to correspond to the similarly named OpenGL
-- minification filter of @GL_minFilter_MIPMAP_mipmapMode@ (e.g.
-- @minFilter@ of 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' and
-- @mipmapMode@ of
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_NEAREST'
-- correspond to @GL_LINEAR_MIPMAP_NEAREST@).
--
-- There are no Vulkan filter modes that directly correspond to OpenGL
-- minification filters of @GL_LINEAR@ or @GL_NEAREST@, but they /can/ be
-- emulated using
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_NEAREST',
-- @minLod@ = 0, and @maxLod@ = 0.25, and using @minFilter@ =
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' or @minFilter@ =
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_NEAREST', respectively.
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
-- structure. If @maxSamplerAllocationCount@ is exceeded,
-- 'Graphics.Vulkan.C.Core10.Sampler.vkCreateSampler' will return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_TOO_MANY_OBJECTS'.
--
-- Since 'Graphics.Vulkan.C.Core10.Sampler.VkSampler' is a non-dispatchable
-- handle type, implementations /may/ return the same handle for sampler
-- state vectors that are identical. In such cases, all such objects would
-- only count once against the @maxSamplerAllocationCount@ limit.
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
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_NEAREST'
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @minLod@ and @maxLod@
--     /must/ be zero
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @addressModeU@ and
--     @addressModeV@ /must/ each be either
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--     or
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'
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
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER',
--     @borderColor@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Sampler.VkBorderColor' value
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>
--     is enabled, @addressModeU@, @addressModeV@, and @addressModeW@
--     /must/ be
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE',
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
--     then @mipmapMode@ /must/ be
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_NEAREST'.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @minLod@ and @maxLod@ /must/ be zero.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @addressModeU@ and @addressModeV@ /must/ each be either
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--     or
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'.
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
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkBorderColor',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkCompareOp',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkFilter',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerAddressMode',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerMipmapMode',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Sampler.vkCreateSampler'
data SamplerCreateInfo = SamplerCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "SamplerCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerCreateInfo" "flags"
  flags :: SamplerCreateFlags
  , -- No documentation found for Nested "SamplerCreateInfo" "magFilter"
  magFilter :: Filter
  , -- No documentation found for Nested "SamplerCreateInfo" "minFilter"
  minFilter :: Filter
  , -- No documentation found for Nested "SamplerCreateInfo" "mipmapMode"
  mipmapMode :: SamplerMipmapMode
  , -- No documentation found for Nested "SamplerCreateInfo" "addressModeU"
  addressModeU :: SamplerAddressMode
  , -- No documentation found for Nested "SamplerCreateInfo" "addressModeV"
  addressModeV :: SamplerAddressMode
  , -- No documentation found for Nested "SamplerCreateInfo" "addressModeW"
  addressModeW :: SamplerAddressMode
  , -- No documentation found for Nested "SamplerCreateInfo" "mipLodBias"
  mipLodBias :: CFloat
  , -- No documentation found for Nested "SamplerCreateInfo" "anisotropyEnable"
  anisotropyEnable :: Bool
  , -- No documentation found for Nested "SamplerCreateInfo" "maxAnisotropy"
  maxAnisotropy :: CFloat
  , -- No documentation found for Nested "SamplerCreateInfo" "compareEnable"
  compareEnable :: Bool
  , -- No documentation found for Nested "SamplerCreateInfo" "compareOp"
  compareOp :: CompareOp
  , -- No documentation found for Nested "SamplerCreateInfo" "minLod"
  minLod :: CFloat
  , -- No documentation found for Nested "SamplerCreateInfo" "maxLod"
  maxLod :: CFloat
  , -- No documentation found for Nested "SamplerCreateInfo" "borderColor"
  borderColor :: BorderColor
  , -- No documentation found for Nested "SamplerCreateInfo" "unnormalizedCoordinates"
  unnormalizedCoordinates :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSamplerCreateInfo' and
-- marshal a 'SamplerCreateInfo' into it. The 'VkSamplerCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSamplerCreateInfo :: SamplerCreateInfo -> (VkSamplerCreateInfo -> IO a) -> IO a
withCStructSamplerCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SamplerCreateInfo)) (\pPNext -> cont (VkSamplerCreateInfo VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO pPNext (flags (marshalled :: SamplerCreateInfo)) (magFilter (marshalled :: SamplerCreateInfo)) (minFilter (marshalled :: SamplerCreateInfo)) (mipmapMode (marshalled :: SamplerCreateInfo)) (addressModeU (marshalled :: SamplerCreateInfo)) (addressModeV (marshalled :: SamplerCreateInfo)) (addressModeW (marshalled :: SamplerCreateInfo)) (mipLodBias (marshalled :: SamplerCreateInfo)) (boolToBool32 (anisotropyEnable (marshalled :: SamplerCreateInfo))) (maxAnisotropy (marshalled :: SamplerCreateInfo)) (boolToBool32 (compareEnable (marshalled :: SamplerCreateInfo))) (compareOp (marshalled :: SamplerCreateInfo)) (minLod (marshalled :: SamplerCreateInfo)) (maxLod (marshalled :: SamplerCreateInfo)) (borderColor (marshalled :: SamplerCreateInfo)) (boolToBool32 (unnormalizedCoordinates (marshalled :: SamplerCreateInfo)))))

-- | A function to read a 'VkSamplerCreateInfo' and all additional
-- structures in the pointer chain into a 'SamplerCreateInfo'.
fromCStructSamplerCreateInfo :: VkSamplerCreateInfo -> IO SamplerCreateInfo
fromCStructSamplerCreateInfo c = SamplerCreateInfo <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSamplerCreateInfo)))
                                                   <*> pure (vkFlags (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMagFilter (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMinFilter (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMipmapMode (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkAddressModeU (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkAddressModeV (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkAddressModeW (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMipLodBias (c :: VkSamplerCreateInfo))
                                                   <*> pure (bool32ToBool (vkAnisotropyEnable (c :: VkSamplerCreateInfo)))
                                                   <*> pure (vkMaxAnisotropy (c :: VkSamplerCreateInfo))
                                                   <*> pure (bool32ToBool (vkCompareEnable (c :: VkSamplerCreateInfo)))
                                                   <*> pure (vkCompareOp (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMinLod (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMaxLod (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkBorderColor (c :: VkSamplerCreateInfo))
                                                   <*> pure (bool32ToBool (vkUnnormalizedCoordinates (c :: VkSamplerCreateInfo)))

instance Zero SamplerCreateInfo where
  zero = SamplerCreateInfo Nothing
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           False
                           zero
                           False
                           zero
                           zero
                           zero
                           zero
                           False


-- | VkSamplerMipmapMode - Specify mipmap mode used for texture lookups
--
-- = Description
--
-- These modes are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-filtering Texel Filtering>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo'
type SamplerMipmapMode = VkSamplerMipmapMode


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_NEAREST'
-- specifies nearest filtering.
pattern SAMPLER_MIPMAP_MODE_NEAREST :: (a ~ SamplerMipmapMode) => a
pattern SAMPLER_MIPMAP_MODE_NEAREST = VK_SAMPLER_MIPMAP_MODE_NEAREST


-- | 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_LINEAR'
-- specifies linear filtering.
pattern SAMPLER_MIPMAP_MODE_LINEAR :: (a ~ SamplerMipmapMode) => a
pattern SAMPLER_MIPMAP_MODE_LINEAR = VK_SAMPLER_MIPMAP_MODE_LINEAR


-- | vkCreateSampler - Create a new sampler object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the sampler.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo' structure
--     specifying the state of the sampler object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pSampler@ points to a 'Graphics.Vulkan.C.Core10.Sampler.VkSampler'
--     handle in which the resulting sampler object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateSampler.txt -
-- include::{generated}\/validity\/protos\/vkCreateSampler.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSampler',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo'
createSampler :: Device ->  SamplerCreateInfo ->  Maybe AllocationCallbacks ->  IO (Sampler)
createSampler = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pSampler' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructSamplerCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateSampler commandTable device' pCreateInfo' pAllocator pSampler' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSampler')))))


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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSampler'
destroySampler :: Device ->  Sampler ->  Maybe AllocationCallbacks ->  IO ()
destroySampler = \(Device device' commandTable) -> \sampler' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroySampler commandTable device' sampler' pAllocator *> (pure ()))

-- | A safe wrapper for 'createSampler' and 'destroySampler' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withSampler
  :: Device -> SamplerCreateInfo -> Maybe (AllocationCallbacks) -> (Sampler -> IO a) -> IO a
withSampler device samplerCreateInfo allocationCallbacks = bracket
  (createSampler device samplerCreateInfo allocationCallbacks)
  (\o -> destroySampler device o allocationCallbacks)
