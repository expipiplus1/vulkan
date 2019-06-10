{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PipelineViewportWScalingStateCreateInfoNV(..)
  , 
#endif
  ViewportWScalingNV(..)
  , cmdSetViewportWScalingNV
  , pattern NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  , pattern NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  , pattern DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  ) where

import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( vkCmdSetViewportWScalingNV
  , pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  , pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineViewportWScalingStateCreateInfoNV"
data PipelineViewportWScalingStateCreateInfoNV = PipelineViewportWScalingStateCreateInfoNV
  { -- No documentation found for Nested "PipelineViewportWScalingStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportWScalingStateCreateInfoNV" "viewportWScalingEnable"
  viewportWScalingEnable :: Bool
  , -- No documentation found for Nested "PipelineViewportWScalingStateCreateInfoNV" "pViewportWScalings"
  viewportWScalings :: Either Word32 (Vector ViewportWScalingNV)
  }
  deriving (Show, Eq)

instance Zero PipelineViewportWScalingStateCreateInfoNV where
  zero = PipelineViewportWScalingStateCreateInfoNV Nothing
                                                   False
                                                   (Left 0)

#endif


-- No documentation found for TopLevel "VkViewportWScalingNV"
data ViewportWScalingNV = ViewportWScalingNV
  { -- No documentation found for Nested "ViewportWScalingNV" "xcoeff"
  xcoeff :: Float
  , -- No documentation found for Nested "ViewportWScalingNV" "ycoeff"
  ycoeff :: Float
  }
  deriving (Show, Eq)

instance Zero ViewportWScalingNV where
  zero = ViewportWScalingNV zero
                            zero



-- No documentation found for TopLevel "vkCmdSetViewportWScalingNV"
cmdSetViewportWScalingNV :: CommandBuffer ->  Word32 ->  Vector ViewportWScalingNV ->  IO ()
cmdSetViewportWScalingNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME"
pattern NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME = VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION"
pattern NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: Integral a => a
pattern NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
