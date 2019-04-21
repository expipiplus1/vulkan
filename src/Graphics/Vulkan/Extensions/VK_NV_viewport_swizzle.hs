{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
  ( PipelineViewportSwizzleStateCreateFlagsNV
  , withCStructPipelineViewportSwizzleStateCreateInfoNV
  , fromCStructPipelineViewportSwizzleStateCreateInfoNV
  , PipelineViewportSwizzleStateCreateInfoNV(..)
  , ViewportCoordinateSwizzleNV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
  , pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
  , withCStructViewportSwizzleNV
  , fromCStructViewportSwizzleNV
  , ViewportSwizzleNV(..)
  , pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION
  , pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle
  ( VkPipelineViewportSwizzleStateCreateFlagsNV(..)
  , VkPipelineViewportSwizzleStateCreateInfoNV(..)
  , VkViewportCoordinateSwizzleNV(..)
  , VkViewportSwizzleNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle
  ( pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  , pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION
  )


-- | VkPipelineViewportSwizzleStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle.VkPipelineViewportSwizzleStateCreateFlagsNV'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- No cross-references are available
type PipelineViewportSwizzleStateCreateFlagsNV = VkPipelineViewportSwizzleStateCreateFlagsNV


-- | VkPipelineViewportSwizzleStateCreateInfoNV - Structure specifying
-- swizzle applied to primitive clip coordinates
--
-- == Valid Usage
--
-- Unresolved directive in VkPipelineViewportSwizzleStateCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkPipelineViewportSwizzleStateCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineViewportSwizzleStateCreateInfoNV = PipelineViewportSwizzleStateCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineViewportSwizzleStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportSwizzleStateCreateInfoNV" "flags"
  flags :: PipelineViewportSwizzleStateCreateFlagsNV
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineViewportSwizzleStateCreateInfoNV" "pViewportSwizzles"
  viewportSwizzles :: Vector ViewportSwizzleNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineViewportSwizzleStateCreateInfoNV' and
-- marshal a 'PipelineViewportSwizzleStateCreateInfoNV' into it. The 'VkPipelineViewportSwizzleStateCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineViewportSwizzleStateCreateInfoNV :: PipelineViewportSwizzleStateCreateInfoNV -> (VkPipelineViewportSwizzleStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportSwizzleStateCreateInfoNV marshalled cont = withVec withCStructViewportSwizzleNV (viewportSwizzles (marshalled :: PipelineViewportSwizzleStateCreateInfoNV)) (\pPViewportSwizzles -> maybeWith withSomeVkStruct (next (marshalled :: PipelineViewportSwizzleStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportSwizzleStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV pPNext (flags (marshalled :: PipelineViewportSwizzleStateCreateInfoNV)) (fromIntegral (Data.Vector.length (viewportSwizzles (marshalled :: PipelineViewportSwizzleStateCreateInfoNV)))) pPViewportSwizzles)))

-- | A function to read a 'VkPipelineViewportSwizzleStateCreateInfoNV' and all additional
-- structures in the pointer chain into a 'PipelineViewportSwizzleStateCreateInfoNV'.
fromCStructPipelineViewportSwizzleStateCreateInfoNV :: VkPipelineViewportSwizzleStateCreateInfoNV -> IO PipelineViewportSwizzleStateCreateInfoNV
fromCStructPipelineViewportSwizzleStateCreateInfoNV c = PipelineViewportSwizzleStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportSwizzleStateCreateInfoNV)))
                                                                                                 <*> pure (vkFlags (c :: VkPipelineViewportSwizzleStateCreateInfoNV))
                                                                                                 -- Length valued member elided
                                                                                                 <*> (Data.Vector.generateM (fromIntegral (vkViewportCount (c :: VkPipelineViewportSwizzleStateCreateInfoNV))) (((fromCStructViewportSwizzleNV <=<) . peekElemOff) (vkPViewportSwizzles (c :: VkPipelineViewportSwizzleStateCreateInfoNV))))

instance Zero PipelineViewportSwizzleStateCreateInfoNV where
  zero = PipelineViewportSwizzleStateCreateInfoNV Nothing
                                                  zero
                                                  Data.Vector.empty


-- | VkViewportCoordinateSwizzleNV - Specify how a viewport coordinate is
-- swizzled
--
-- = Description
--
-- These values are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vertexpostproc-viewport-swizzle Viewport Swizzle>.
--
-- = See Also
--
-- No cross-references are available
type ViewportCoordinateSwizzleNV = VkViewportCoordinateSwizzleNV


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


-- | VkViewportSwizzleNV - Structure specifying a viewport swizzle
--
-- = Description
--
-- Unresolved directive in VkViewportSwizzleNV.txt -
-- include::{generated}\/validity\/structs\/VkViewportSwizzleNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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

-- | A function to temporarily allocate memory for a 'VkViewportSwizzleNV' and
-- marshal a 'ViewportSwizzleNV' into it. The 'VkViewportSwizzleNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructViewportSwizzleNV :: ViewportSwizzleNV -> (VkViewportSwizzleNV -> IO a) -> IO a
withCStructViewportSwizzleNV marshalled cont = cont (VkViewportSwizzleNV (x (marshalled :: ViewportSwizzleNV)) (y (marshalled :: ViewportSwizzleNV)) (z (marshalled :: ViewportSwizzleNV)) (w (marshalled :: ViewportSwizzleNV)))

-- | A function to read a 'VkViewportSwizzleNV' and all additional
-- structures in the pointer chain into a 'ViewportSwizzleNV'.
fromCStructViewportSwizzleNV :: VkViewportSwizzleNV -> IO ViewportSwizzleNV
fromCStructViewportSwizzleNV c = ViewportSwizzleNV <$> pure (vkX (c :: VkViewportSwizzleNV))
                                                   <*> pure (vkY (c :: VkViewportSwizzleNV))
                                                   <*> pure (vkZ (c :: VkViewportSwizzleNV))
                                                   <*> pure (vkW (c :: VkViewportSwizzleNV))

instance Zero ViewportSwizzleNV where
  zero = ViewportSwizzleNV zero
                           zero
                           zero
                           zero

