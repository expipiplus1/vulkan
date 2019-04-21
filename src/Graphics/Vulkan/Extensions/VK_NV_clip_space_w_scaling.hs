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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkPipelineViewportWScalingStateCreateInfoNV(..)
  , VkViewportWScalingNV(..)
  , vkCmdSetViewportWScalingNV
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



-- | VkPipelineViewportWScalingStateCreateInfoNV - Structure specifying
-- parameters of a newly created pipeline viewport W scaling state
--
-- = Description
--
-- Unresolved directive in VkPipelineViewportWScalingStateCreateInfoNV.txt
-- -
-- include::{generated}\/validity\/structs\/VkPipelineViewportWScalingStateCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineViewportWScalingStateCreateInfoNV = PipelineViewportWScalingStateCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineViewportWScalingStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportWScalingStateCreateInfoNV" "viewportWScalingEnable"
  viewportWScalingEnable :: Bool
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportWScalingStateCreateInfoNV" "pViewportWScalings"
  viewportWScalings :: Maybe (Vector ViewportWScalingNV)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineViewportWScalingStateCreateInfoNV' and
-- marshal a 'PipelineViewportWScalingStateCreateInfoNV' into it. The 'VkPipelineViewportWScalingStateCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineViewportWScalingStateCreateInfoNV :: PipelineViewportWScalingStateCreateInfoNV -> (VkPipelineViewportWScalingStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportWScalingStateCreateInfoNV marshalled cont = maybeWith (withVec withCStructViewportWScalingNV) (viewportWScalings (marshalled :: PipelineViewportWScalingStateCreateInfoNV)) (\pPViewportWScalings -> maybeWith withSomeVkStruct (next (marshalled :: PipelineViewportWScalingStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportWScalingStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV pPNext (boolToBool32 (viewportWScalingEnable (marshalled :: PipelineViewportWScalingStateCreateInfoNV))) (maybe 0 (fromIntegral . Data.Vector.length) (viewportWScalings (marshalled :: PipelineViewportWScalingStateCreateInfoNV))) pPViewportWScalings)))

-- | A function to read a 'VkPipelineViewportWScalingStateCreateInfoNV' and all additional
-- structures in the pointer chain into a 'PipelineViewportWScalingStateCreateInfoNV'.
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



-- | VkViewportWScalingNV - Structure specifying a viewport
--
-- = Description
--
-- Unresolved directive in VkViewportWScalingNV.txt -
-- include::{generated}\/validity\/structs\/VkViewportWScalingNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data ViewportWScalingNV = ViewportWScalingNV
  { -- No documentation found for Nested "ViewportWScalingNV" "xcoeff"
  xcoeff :: CFloat
  , -- No documentation found for Nested "ViewportWScalingNV" "ycoeff"
  ycoeff :: CFloat
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkViewportWScalingNV' and
-- marshal a 'ViewportWScalingNV' into it. The 'VkViewportWScalingNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructViewportWScalingNV :: ViewportWScalingNV -> (VkViewportWScalingNV -> IO a) -> IO a
withCStructViewportWScalingNV marshalled cont = cont (VkViewportWScalingNV (xcoeff (marshalled :: ViewportWScalingNV)) (ycoeff (marshalled :: ViewportWScalingNV)))

-- | A function to read a 'VkViewportWScalingNV' and all additional
-- structures in the pointer chain into a 'ViewportWScalingNV'.
fromCStructViewportWScalingNV :: VkViewportWScalingNV -> IO ViewportWScalingNV
fromCStructViewportWScalingNV c = ViewportWScalingNV <$> pure (vkXcoeff (c :: VkViewportWScalingNV))
                                                     <*> pure (vkYcoeff (c :: VkViewportWScalingNV))

instance Zero ViewportWScalingNV where
  zero = ViewportWScalingNV zero
                            zero



-- | vkCmdSetViewportWScalingNV - Set the viewport W scaling on a command
-- buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstViewport@ is the index of the first viewport whose parameters
--     are updated by the command.
--
-- -   @viewportCount@ is the number of viewports whose parameters are
--     updated by the command.
--
-- -   @pViewportWScalings@ is a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.VkViewportWScalingNV'
--     structures specifying viewport parameters.
--
-- = Description
--
-- The viewport parameters taken from element i of @pViewportWScalings@
-- replace the current state for the viewport index @firstViewport@ + i,
-- for i in [0, @viewportCount@).
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled
--
-- -   @firstViewport@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- Unresolved directive in vkCmdSetViewportWScalingNV.txt -
-- include::{generated}\/validity\/protos\/vkCmdSetViewportWScalingNV.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdSetViewportWScalingNV :: CommandBuffer ->  Word32 ->  Vector ViewportWScalingNV ->  IO ()
cmdSetViewportWScalingNV = \(CommandBuffer commandBuffer' commandTable) -> \firstViewport' -> \viewportWScalings' -> withVec withCStructViewportWScalingNV viewportWScalings' (\pViewportWScalings' -> vkCmdSetViewportWScalingNV commandTable commandBuffer' firstViewport' (fromIntegral $ Data.Vector.length viewportWScalings') pViewportWScalings' *> (pure ()))
