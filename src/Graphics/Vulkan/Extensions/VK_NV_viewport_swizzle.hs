{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
  ( PipelineViewportSwizzleStateCreateFlagsNV
  , withCStructPipelineViewportSwizzleStateCreateInfoNV
  , fromCStructPipelineViewportSwizzleStateCreateInfoNV
  , PipelineViewportSwizzleStateCreateInfoNV(..)
  , ViewportCoordinateSwizzleNV
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


-- No documentation found for TopLevel "PipelineViewportSwizzleStateCreateFlagsNV"
type PipelineViewportSwizzleStateCreateFlagsNV = VkPipelineViewportSwizzleStateCreateFlagsNV
-- No documentation found for TopLevel "PipelineViewportSwizzleStateCreateInfoNV"
data PipelineViewportSwizzleStateCreateInfoNV = PipelineViewportSwizzleStateCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineViewportSwizzleStateCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportSwizzleStateCreateInfoNV" "flags"
  vkFlags :: PipelineViewportSwizzleStateCreateFlagsNV
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineViewportSwizzleStateCreateInfoNV" "pViewportSwizzles"
  vkPViewportSwizzles :: Vector ViewportSwizzleNV
  }
  deriving (Show, Eq)
withCStructPipelineViewportSwizzleStateCreateInfoNV :: PipelineViewportSwizzleStateCreateInfoNV -> (VkPipelineViewportSwizzleStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportSwizzleStateCreateInfoNV from cont = withVec withCStructViewportSwizzleNV (vkPViewportSwizzles (from :: PipelineViewportSwizzleStateCreateInfoNV)) (\pViewportSwizzles -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineViewportSwizzleStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportSwizzleStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV pPNext (vkFlags (from :: PipelineViewportSwizzleStateCreateInfoNV)) (fromIntegral (Data.Vector.length (vkPViewportSwizzles (from :: PipelineViewportSwizzleStateCreateInfoNV)))) pViewportSwizzles)))
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
-- No documentation found for TopLevel "ViewportCoordinateSwizzleNV"
type ViewportCoordinateSwizzleNV = VkViewportCoordinateSwizzleNV
-- No documentation found for TopLevel "ViewportSwizzleNV"
data ViewportSwizzleNV = ViewportSwizzleNV
  { -- No documentation found for Nested "ViewportSwizzleNV" "x"
  vkX :: ViewportCoordinateSwizzleNV
  , -- No documentation found for Nested "ViewportSwizzleNV" "y"
  vkY :: ViewportCoordinateSwizzleNV
  , -- No documentation found for Nested "ViewportSwizzleNV" "z"
  vkZ :: ViewportCoordinateSwizzleNV
  , -- No documentation found for Nested "ViewportSwizzleNV" "w"
  vkW :: ViewportCoordinateSwizzleNV
  }
  deriving (Show, Eq)
withCStructViewportSwizzleNV :: ViewportSwizzleNV -> (VkViewportSwizzleNV -> IO a) -> IO a
withCStructViewportSwizzleNV from cont = cont (VkViewportSwizzleNV (vkX (from :: ViewportSwizzleNV)) (vkY (from :: ViewportSwizzleNV)) (vkZ (from :: ViewportSwizzleNV)) (vkW (from :: ViewportSwizzleNV)))
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
