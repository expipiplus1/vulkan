{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  ( withCStructPipelineViewportWScalingStateCreateInfoNV
  , fromCStructPipelineViewportWScalingStateCreateInfoNV
  , PipelineViewportWScalingStateCreateInfoNV(..)
  , withCStructViewportWScalingNV
  , fromCStructViewportWScalingNV
  , ViewportWScalingNV(..)
  , cmdSetViewportWScalingNV
  , pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
  , pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
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
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdSetViewportWScalingNV
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkPipelineViewportWScalingStateCreateInfoNV(..)
  , VkViewportWScalingNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  , pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  , pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
  )


-- No documentation found for TopLevel "PipelineViewportWScalingStateCreateInfoNV"
data PipelineViewportWScalingStateCreateInfoNV = PipelineViewportWScalingStateCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineViewportWScalingStateCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportWScalingStateCreateInfoNV" "viewportWScalingEnable"
  vkViewportWScalingEnable :: Bool
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportWScalingStateCreateInfoNV" "pViewportWScalings"
  vkPViewportWScalings :: Maybe (Vector ViewportWScalingNV)
  }
  deriving (Show, Eq)
withCStructPipelineViewportWScalingStateCreateInfoNV :: PipelineViewportWScalingStateCreateInfoNV -> (VkPipelineViewportWScalingStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportWScalingStateCreateInfoNV from cont = maybeWith (withVec withCStructViewportWScalingNV) (vkPViewportWScalings (from :: PipelineViewportWScalingStateCreateInfoNV)) (\pViewportWScalings -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineViewportWScalingStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportWScalingStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV pPNext (boolToBool32 (vkViewportWScalingEnable (from :: PipelineViewportWScalingStateCreateInfoNV))) (maybe 0 (fromIntegral . Data.Vector.length) (vkPViewportWScalings (from :: PipelineViewportWScalingStateCreateInfoNV))) pViewportWScalings)))
fromCStructPipelineViewportWScalingStateCreateInfoNV :: VkPipelineViewportWScalingStateCreateInfoNV -> IO PipelineViewportWScalingStateCreateInfoNV
fromCStructPipelineViewportWScalingStateCreateInfoNV c = PipelineViewportWScalingStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportWScalingStateCreateInfoNV)))
                                                                                                   <*> pure (bool32ToBool (vkViewportWScalingEnable (c :: VkPipelineViewportWScalingStateCreateInfoNV)))
                                                                                                   -- Optional length valued member elided
                                                                                                   <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkViewportCount (c :: VkPipelineViewportWScalingStateCreateInfoNV))) (((fromCStructViewportWScalingNV <=<) . peekElemOff) p)) (vkPViewportWScalings (c :: VkPipelineViewportWScalingStateCreateInfoNV))
instance Zero PipelineViewportWScalingStateCreateInfoNV where
  zero = PipelineViewportWScalingStateCreateInfoNV Nothing
                                                   False
                                                   Nothing
-- No documentation found for TopLevel "ViewportWScalingNV"
data ViewportWScalingNV = ViewportWScalingNV
  { -- No documentation found for Nested "ViewportWScalingNV" "xcoeff"
  vkXcoeff :: CFloat
  , -- No documentation found for Nested "ViewportWScalingNV" "ycoeff"
  vkYcoeff :: CFloat
  }
  deriving (Show, Eq)
withCStructViewportWScalingNV :: ViewportWScalingNV -> (VkViewportWScalingNV -> IO a) -> IO a
withCStructViewportWScalingNV from cont = cont (VkViewportWScalingNV (vkXcoeff (from :: ViewportWScalingNV)) (vkYcoeff (from :: ViewportWScalingNV)))
fromCStructViewportWScalingNV :: VkViewportWScalingNV -> IO ViewportWScalingNV
fromCStructViewportWScalingNV c = ViewportWScalingNV <$> pure (vkXcoeff (c :: VkViewportWScalingNV))
                                                     <*> pure (vkYcoeff (c :: VkViewportWScalingNV))
instance Zero ViewportWScalingNV where
  zero = ViewportWScalingNV zero
                            zero

-- | Wrapper for 'vkCmdSetViewportWScalingNV'
cmdSetViewportWScalingNV :: CommandBuffer ->  Word32 ->  Vector ViewportWScalingNV ->  IO ()
cmdSetViewportWScalingNV = \(CommandBuffer commandBuffer commandTable) -> \firstViewport -> \viewportWScalings -> withVec withCStructViewportWScalingNV viewportWScalings (\pViewportWScalings -> Graphics.Vulkan.C.Dynamic.cmdSetViewportWScalingNV commandTable commandBuffer firstViewport (fromIntegral $ Data.Vector.length viewportWScalings) pViewportWScalings *> (pure ()))
