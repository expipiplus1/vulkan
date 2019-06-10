{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
  ( PipelineViewportSwizzleStateCreateFlagsNV
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineViewportSwizzleStateCreateInfoNV(..)
#endif
  , ViewportCoordinateSwizzleNV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
  , ViewportSwizzleNV(..)
  , pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  , pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle
  ( VkPipelineViewportSwizzleStateCreateFlagsNV(..)
  , VkViewportCoordinateSwizzleNV(..)
  , pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  , pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
  )


-- No documentation found for TopLevel "PipelineViewportSwizzleStateCreateFlagsNV"
type PipelineViewportSwizzleStateCreateFlagsNV = VkPipelineViewportSwizzleStateCreateFlagsNV


-- No complete pragma for PipelineViewportSwizzleStateCreateFlagsNV as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineViewportSwizzleStateCreateInfoNV"
data PipelineViewportSwizzleStateCreateInfoNV = PipelineViewportSwizzleStateCreateInfoNV
  { -- No documentation found for Nested "PipelineViewportSwizzleStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportSwizzleStateCreateInfoNV" "flags"
  flags :: PipelineViewportSwizzleStateCreateFlagsNV
  , -- No documentation found for Nested "PipelineViewportSwizzleStateCreateInfoNV" "pViewportSwizzles"
  viewportSwizzles :: Vector ViewportSwizzleNV
  }
  deriving (Show, Eq)

instance Zero PipelineViewportSwizzleStateCreateInfoNV where
  zero = PipelineViewportSwizzleStateCreateInfoNV Nothing
                                                  zero
                                                  mempty

#endif

-- No documentation found for TopLevel "ViewportCoordinateSwizzleNV"
type ViewportCoordinateSwizzleNV = VkViewportCoordinateSwizzleNV


{-# complete VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV, VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV, VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV, VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV, VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV, VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV, VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV, VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV :: ViewportCoordinateSwizzleNV #-}


-- No documentation found for Nested "ViewportCoordinateSwizzleNV" "VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV :: (a ~ ViewportCoordinateSwizzleNV) => a
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV = VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV


-- No documentation found for Nested "ViewportCoordinateSwizzleNV" "VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV :: (a ~ ViewportCoordinateSwizzleNV) => a
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV = VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV


-- No documentation found for Nested "ViewportCoordinateSwizzleNV" "VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV :: (a ~ ViewportCoordinateSwizzleNV) => a
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV = VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV


-- No documentation found for Nested "ViewportCoordinateSwizzleNV" "VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV :: (a ~ ViewportCoordinateSwizzleNV) => a
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV = VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV


-- No documentation found for Nested "ViewportCoordinateSwizzleNV" "VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV :: (a ~ ViewportCoordinateSwizzleNV) => a
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV = VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV


-- No documentation found for Nested "ViewportCoordinateSwizzleNV" "VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV :: (a ~ ViewportCoordinateSwizzleNV) => a
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV = VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV


-- No documentation found for Nested "ViewportCoordinateSwizzleNV" "VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV :: (a ~ ViewportCoordinateSwizzleNV) => a
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV = VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV


-- No documentation found for Nested "ViewportCoordinateSwizzleNV" "VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV :: (a ~ ViewportCoordinateSwizzleNV) => a
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV = VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV


-- No documentation found for TopLevel "VkViewportSwizzleNV"
data ViewportSwizzleNV = ViewportSwizzleNV
  { -- No documentation found for Nested "ViewportSwizzleNV" "x"
  x :: ViewportCoordinateSwizzleNV
  , -- No documentation found for Nested "ViewportSwizzleNV" "y"
  y :: ViewportCoordinateSwizzleNV
  , -- No documentation found for Nested "ViewportSwizzleNV" "z"
  z :: ViewportCoordinateSwizzleNV
  , -- No documentation found for Nested "ViewportSwizzleNV" "w"
  w :: ViewportCoordinateSwizzleNV
  }
  deriving (Show, Eq)

instance Zero ViewportSwizzleNV where
  zero = ViewportSwizzleNV zero
                           zero
                           zero
                           zero


-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME"
pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION"
pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION :: Integral a => a
pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION = VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION
