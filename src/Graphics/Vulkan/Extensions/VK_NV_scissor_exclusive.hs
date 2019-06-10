{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceExclusiveScissorFeaturesNV(..)
  , 
  PipelineViewportExclusiveScissorStateCreateInfoNV(..)
#endif
  , cmdSetExclusiveScissorNV
  , pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  , pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  , pattern DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
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



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( vkCmdSetExclusiveScissorNV
  , pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
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
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceExclusiveScissorFeaturesNV"
data PhysicalDeviceExclusiveScissorFeaturesNV = PhysicalDeviceExclusiveScissorFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceExclusiveScissorFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExclusiveScissorFeaturesNV" "exclusiveScissor"
  exclusiveScissor :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceExclusiveScissorFeaturesNV where
  zero = PhysicalDeviceExclusiveScissorFeaturesNV Nothing
                                                  False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineViewportExclusiveScissorStateCreateInfoNV"
data PipelineViewportExclusiveScissorStateCreateInfoNV = PipelineViewportExclusiveScissorStateCreateInfoNV
  { -- No documentation found for Nested "PipelineViewportExclusiveScissorStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportExclusiveScissorStateCreateInfoNV" "pExclusiveScissors"
  exclusiveScissors :: Either Word32 (Vector Rect2D)
  }
  deriving (Show, Eq)

instance Zero PipelineViewportExclusiveScissorStateCreateInfoNV where
  zero = PipelineViewportExclusiveScissorStateCreateInfoNV Nothing
                                                           (Left 0)

#endif


-- No documentation found for TopLevel "vkCmdSetExclusiveScissorNV"
cmdSetExclusiveScissorNV :: CommandBuffer ->  Word32 ->  Vector Rect2D ->  IO ()
cmdSetExclusiveScissorNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME"
pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION"
pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION :: Integral a => a
pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
