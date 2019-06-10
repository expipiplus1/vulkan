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
  , pattern FILTER_CUBIC_IMG
  , Sampler
  , SamplerAddressMode
  , pattern SAMPLER_ADDRESS_MODE_REPEAT
  , pattern SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
  , pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  , pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
  , SamplerCreateFlagBits
  , pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
  , pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
  , SamplerCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , SamplerCreateInfo(..)
#endif
  , SamplerMipmapMode
  , pattern SAMPLER_MIPMAP_MODE_NEAREST
  , pattern SAMPLER_MIPMAP_MODE_LINEAR
  , createSampler
  , destroySampler
  , withSampler
  ) where

import Control.Exception
  ( bracket
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Sampler
  ( VkBorderColor(..)
  , VkFilter(..)
  , VkSamplerAddressMode(..)
  , VkSamplerCreateFlagBits(..)
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
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
  , pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_IMG_filter_cubic
  ( pattern VK_FILTER_CUBIC_IMG
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_sampler_mirror_clamp_to_edge
  ( pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( CompareOp
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "BorderColor"
type BorderColor = VkBorderColor


{-# complete BORDER_COLOR_FLOAT_TRANSPARENT_BLACK, BORDER_COLOR_INT_TRANSPARENT_BLACK, BORDER_COLOR_FLOAT_OPAQUE_BLACK, BORDER_COLOR_INT_OPAQUE_BLACK, BORDER_COLOR_FLOAT_OPAQUE_WHITE, BORDER_COLOR_INT_OPAQUE_WHITE :: BorderColor #-}


-- No documentation found for Nested "BorderColor" "BORDER_COLOR_FLOAT_TRANSPARENT_BLACK"
pattern BORDER_COLOR_FLOAT_TRANSPARENT_BLACK :: (a ~ BorderColor) => a
pattern BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK


-- No documentation found for Nested "BorderColor" "BORDER_COLOR_INT_TRANSPARENT_BLACK"
pattern BORDER_COLOR_INT_TRANSPARENT_BLACK :: (a ~ BorderColor) => a
pattern BORDER_COLOR_INT_TRANSPARENT_BLACK = VK_BORDER_COLOR_INT_TRANSPARENT_BLACK


-- No documentation found for Nested "BorderColor" "BORDER_COLOR_FLOAT_OPAQUE_BLACK"
pattern BORDER_COLOR_FLOAT_OPAQUE_BLACK :: (a ~ BorderColor) => a
pattern BORDER_COLOR_FLOAT_OPAQUE_BLACK = VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK


-- No documentation found for Nested "BorderColor" "BORDER_COLOR_INT_OPAQUE_BLACK"
pattern BORDER_COLOR_INT_OPAQUE_BLACK :: (a ~ BorderColor) => a
pattern BORDER_COLOR_INT_OPAQUE_BLACK = VK_BORDER_COLOR_INT_OPAQUE_BLACK


-- No documentation found for Nested "BorderColor" "BORDER_COLOR_FLOAT_OPAQUE_WHITE"
pattern BORDER_COLOR_FLOAT_OPAQUE_WHITE :: (a ~ BorderColor) => a
pattern BORDER_COLOR_FLOAT_OPAQUE_WHITE = VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE


-- No documentation found for Nested "BorderColor" "BORDER_COLOR_INT_OPAQUE_WHITE"
pattern BORDER_COLOR_INT_OPAQUE_WHITE :: (a ~ BorderColor) => a
pattern BORDER_COLOR_INT_OPAQUE_WHITE = VK_BORDER_COLOR_INT_OPAQUE_WHITE

-- No documentation found for TopLevel "Filter"
type Filter = VkFilter


{-# complete FILTER_NEAREST, FILTER_LINEAR, FILTER_CUBIC_IMG :: Filter #-}


-- No documentation found for Nested "Filter" "FILTER_NEAREST"
pattern FILTER_NEAREST :: (a ~ Filter) => a
pattern FILTER_NEAREST = VK_FILTER_NEAREST


-- No documentation found for Nested "Filter" "FILTER_LINEAR"
pattern FILTER_LINEAR :: (a ~ Filter) => a
pattern FILTER_LINEAR = VK_FILTER_LINEAR


-- No documentation found for Nested "Filter" "FILTER_CUBIC_IMG"
pattern FILTER_CUBIC_IMG :: (a ~ Filter) => a
pattern FILTER_CUBIC_IMG = VK_FILTER_CUBIC_IMG

-- No documentation found for TopLevel "Sampler"
type Sampler = VkSampler

-- No documentation found for TopLevel "SamplerAddressMode"
type SamplerAddressMode = VkSamplerAddressMode


{-# complete SAMPLER_ADDRESS_MODE_REPEAT, SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER, SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE :: SamplerAddressMode #-}


-- No documentation found for Nested "SamplerAddressMode" "SAMPLER_ADDRESS_MODE_REPEAT"
pattern SAMPLER_ADDRESS_MODE_REPEAT :: (a ~ SamplerAddressMode) => a
pattern SAMPLER_ADDRESS_MODE_REPEAT = VK_SAMPLER_ADDRESS_MODE_REPEAT


-- No documentation found for Nested "SamplerAddressMode" "SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
pattern SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT :: (a ~ SamplerAddressMode) => a
pattern SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT


-- No documentation found for Nested "SamplerAddressMode" "SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE :: (a ~ SamplerAddressMode) => a
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE


-- No documentation found for Nested "SamplerAddressMode" "SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER :: (a ~ SamplerAddressMode) => a
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER


-- No documentation found for Nested "SamplerAddressMode" "SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE"
pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE :: (a ~ SamplerAddressMode) => a
pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE

-- No documentation found for TopLevel "SamplerCreateFlagBits"
type SamplerCreateFlagBits = VkSamplerCreateFlagBits


{-# complete SAMPLER_CREATE_SUBSAMPLED_BIT_EXT, SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT :: SamplerCreateFlagBits #-}


-- No documentation found for Nested "SamplerCreateFlagBits" "SAMPLER_CREATE_SUBSAMPLED_BIT_EXT"
pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT :: (a ~ SamplerCreateFlagBits) => a
pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT = VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT


-- No documentation found for Nested "SamplerCreateFlagBits" "SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT"
pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT :: (a ~ SamplerCreateFlagBits) => a
pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT = VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT

-- No documentation found for TopLevel "SamplerCreateFlags"
type SamplerCreateFlags = SamplerCreateFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSamplerCreateInfo"
data SamplerCreateInfo = SamplerCreateInfo
  { -- No documentation found for Nested "SamplerCreateInfo" "pNext"
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
  mipLodBias :: Float
  , -- No documentation found for Nested "SamplerCreateInfo" "anisotropyEnable"
  anisotropyEnable :: Bool
  , -- No documentation found for Nested "SamplerCreateInfo" "maxAnisotropy"
  maxAnisotropy :: Float
  , -- No documentation found for Nested "SamplerCreateInfo" "compareEnable"
  compareEnable :: Bool
  , -- No documentation found for Nested "SamplerCreateInfo" "compareOp"
  compareOp :: CompareOp
  , -- No documentation found for Nested "SamplerCreateInfo" "minLod"
  minLod :: Float
  , -- No documentation found for Nested "SamplerCreateInfo" "maxLod"
  maxLod :: Float
  , -- No documentation found for Nested "SamplerCreateInfo" "borderColor"
  borderColor :: BorderColor
  , -- No documentation found for Nested "SamplerCreateInfo" "unnormalizedCoordinates"
  unnormalizedCoordinates :: Bool
  }
  deriving (Show, Eq)

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

#endif

-- No documentation found for TopLevel "SamplerMipmapMode"
type SamplerMipmapMode = VkSamplerMipmapMode


{-# complete SAMPLER_MIPMAP_MODE_NEAREST, SAMPLER_MIPMAP_MODE_LINEAR :: SamplerMipmapMode #-}


-- No documentation found for Nested "SamplerMipmapMode" "SAMPLER_MIPMAP_MODE_NEAREST"
pattern SAMPLER_MIPMAP_MODE_NEAREST :: (a ~ SamplerMipmapMode) => a
pattern SAMPLER_MIPMAP_MODE_NEAREST = VK_SAMPLER_MIPMAP_MODE_NEAREST


-- No documentation found for Nested "SamplerMipmapMode" "SAMPLER_MIPMAP_MODE_LINEAR"
pattern SAMPLER_MIPMAP_MODE_LINEAR :: (a ~ SamplerMipmapMode) => a
pattern SAMPLER_MIPMAP_MODE_LINEAR = VK_SAMPLER_MIPMAP_MODE_LINEAR


-- No documentation found for TopLevel "vkCreateSampler"
createSampler :: Device ->  SamplerCreateInfo ->  Maybe AllocationCallbacks ->  IO (Sampler)
createSampler = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroySampler"
destroySampler :: Device ->  Sampler ->  Maybe AllocationCallbacks ->  IO ()
destroySampler = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createSampler' and 'destroySampler' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withSampler
  :: Device -> SamplerCreateInfo -> Maybe AllocationCallbacks -> (Sampler -> IO a) -> IO a
withSampler device samplerCreateInfo allocationCallbacks = bracket
  (createSampler device samplerCreateInfo allocationCallbacks)
  (\o -> destroySampler device o allocationCallbacks)
