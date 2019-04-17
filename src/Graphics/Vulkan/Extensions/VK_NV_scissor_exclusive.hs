{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive
  ( withCStructPhysicalDeviceExclusiveScissorFeaturesNV
  , fromCStructPhysicalDeviceExclusiveScissorFeaturesNV
  , PhysicalDeviceExclusiveScissorFeaturesNV(..)
  , withCStructPipelineViewportExclusiveScissorStateCreateInfoNV
  , fromCStructPipelineViewportExclusiveScissorStateCreateInfoNV
  , PipelineViewportExclusiveScissorStateCreateInfoNV(..)
  , cmdSetExclusiveScissorNV
  , pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
  , pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  , pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
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
  ( cmdSetExclusiveScissorNV
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( VkPhysicalDeviceExclusiveScissorFeaturesNV(..)
  , VkPipelineViewportExclusiveScissorStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , fromCStructRect2D
  , withCStructRect2D
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
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  , pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceExclusiveScissorFeaturesNV"
data PhysicalDeviceExclusiveScissorFeaturesNV = PhysicalDeviceExclusiveScissorFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceExclusiveScissorFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExclusiveScissorFeaturesNV" "exclusiveScissor"
  vkExclusiveScissor :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceExclusiveScissorFeaturesNV :: PhysicalDeviceExclusiveScissorFeaturesNV -> (VkPhysicalDeviceExclusiveScissorFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceExclusiveScissorFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceExclusiveScissorFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceExclusiveScissorFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV pPNext (boolToBool32 (vkExclusiveScissor (from :: PhysicalDeviceExclusiveScissorFeaturesNV)))))
fromCStructPhysicalDeviceExclusiveScissorFeaturesNV :: VkPhysicalDeviceExclusiveScissorFeaturesNV -> IO PhysicalDeviceExclusiveScissorFeaturesNV
fromCStructPhysicalDeviceExclusiveScissorFeaturesNV c = PhysicalDeviceExclusiveScissorFeaturesNV <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExclusiveScissorFeaturesNV)))
                                                                                                 <*> pure (bool32ToBool (vkExclusiveScissor (c :: VkPhysicalDeviceExclusiveScissorFeaturesNV)))
instance Zero PhysicalDeviceExclusiveScissorFeaturesNV where
  zero = PhysicalDeviceExclusiveScissorFeaturesNV Nothing
                                                  False
-- No documentation found for TopLevel "PipelineViewportExclusiveScissorStateCreateInfoNV"
data PipelineViewportExclusiveScissorStateCreateInfoNV = PipelineViewportExclusiveScissorStateCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineViewportExclusiveScissorStateCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportExclusiveScissorStateCreateInfoNV" "pExclusiveScissors"
  vkPExclusiveScissors :: Maybe (Vector Rect2D)
  }
  deriving (Show, Eq)
withCStructPipelineViewportExclusiveScissorStateCreateInfoNV :: PipelineViewportExclusiveScissorStateCreateInfoNV -> (VkPipelineViewportExclusiveScissorStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportExclusiveScissorStateCreateInfoNV from cont = maybeWith (withVec withCStructRect2D) (vkPExclusiveScissors (from :: PipelineViewportExclusiveScissorStateCreateInfoNV)) (\pExclusiveScissors -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineViewportExclusiveScissorStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportExclusiveScissorStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV pPNext (maybe 0 (fromIntegral . Data.Vector.length) (vkPExclusiveScissors (from :: PipelineViewportExclusiveScissorStateCreateInfoNV))) pExclusiveScissors)))
fromCStructPipelineViewportExclusiveScissorStateCreateInfoNV :: VkPipelineViewportExclusiveScissorStateCreateInfoNV -> IO PipelineViewportExclusiveScissorStateCreateInfoNV
fromCStructPipelineViewportExclusiveScissorStateCreateInfoNV c = PipelineViewportExclusiveScissorStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportExclusiveScissorStateCreateInfoNV)))
                                                                                                                   -- Optional length valued member elided
                                                                                                                   <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkExclusiveScissorCount (c :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))) (((fromCStructRect2D <=<) . peekElemOff) p)) (vkPExclusiveScissors (c :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))
instance Zero PipelineViewportExclusiveScissorStateCreateInfoNV where
  zero = PipelineViewportExclusiveScissorStateCreateInfoNV Nothing
                                                           Nothing

-- | Wrapper for 'vkCmdSetExclusiveScissorNV'
cmdSetExclusiveScissorNV :: CommandBuffer ->  Word32 ->  Vector Rect2D ->  IO ()
cmdSetExclusiveScissorNV = \(CommandBuffer commandBuffer commandTable) -> \firstExclusiveScissor -> \exclusiveScissors -> withVec withCStructRect2D exclusiveScissors (\pExclusiveScissors -> Graphics.Vulkan.C.Dynamic.cmdSetExclusiveScissorNV commandTable commandBuffer firstExclusiveScissor (fromIntegral $ Data.Vector.length exclusiveScissors) pExclusiveScissors *> (pure ()))
