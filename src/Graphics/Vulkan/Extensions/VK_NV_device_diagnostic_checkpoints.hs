{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints
  ( withCStructCheckpointDataNV
  , fromCStructCheckpointDataNV
  , CheckpointDataNV(..)
  , withCStructQueueFamilyCheckpointPropertiesNV
  , fromCStructQueueFamilyCheckpointPropertiesNV
  , QueueFamilyCheckpointPropertiesNV(..)
  , cmdSetCheckpointNV
  , getNumQueueCheckpointDataNV
  , getQueueCheckpointDataNV
  , getAllQueueCheckpointDataNV
  , pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
  , pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdSetCheckpointNV
  , getQueueCheckpointDataNV
  )


import Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( VkCheckpointDataNV(..)
  , VkQueueFamilyCheckpointPropertiesNV(..)
  , pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , Queue(..)
  , PipelineStageFlagBits
  , PipelineStageFlags
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
  , pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
  )


-- No documentation found for TopLevel "CheckpointDataNV"
data CheckpointDataNV = CheckpointDataNV
  { -- Univalued Member elided
  -- No documentation found for Nested "CheckpointDataNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CheckpointDataNV" "stage"
  vkStage :: PipelineStageFlagBits
  , -- No documentation found for Nested "CheckpointDataNV" "pCheckpointMarker"
  vkPCheckpointMarker :: Ptr ()
  }
  deriving (Show, Eq)
withCStructCheckpointDataNV :: CheckpointDataNV -> (VkCheckpointDataNV -> IO a) -> IO a
withCStructCheckpointDataNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: CheckpointDataNV)) (\pPNext -> cont (VkCheckpointDataNV VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV pPNext (vkStage (from :: CheckpointDataNV)) (vkPCheckpointMarker (from :: CheckpointDataNV))))
fromCStructCheckpointDataNV :: VkCheckpointDataNV -> IO CheckpointDataNV
fromCStructCheckpointDataNV c = CheckpointDataNV <$> -- Univalued Member elided
                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCheckpointDataNV)))
                                                 <*> pure (vkStage (c :: VkCheckpointDataNV))
                                                 <*> pure (vkPCheckpointMarker (c :: VkCheckpointDataNV))
-- No documentation found for TopLevel "QueueFamilyCheckpointPropertiesNV"
data QueueFamilyCheckpointPropertiesNV = QueueFamilyCheckpointPropertiesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "QueueFamilyCheckpointPropertiesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "QueueFamilyCheckpointPropertiesNV" "checkpointExecutionStageMask"
  vkCheckpointExecutionStageMask :: PipelineStageFlags
  }
  deriving (Show, Eq)
withCStructQueueFamilyCheckpointPropertiesNV :: QueueFamilyCheckpointPropertiesNV -> (VkQueueFamilyCheckpointPropertiesNV -> IO a) -> IO a
withCStructQueueFamilyCheckpointPropertiesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: QueueFamilyCheckpointPropertiesNV)) (\pPNext -> cont (VkQueueFamilyCheckpointPropertiesNV VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV pPNext (vkCheckpointExecutionStageMask (from :: QueueFamilyCheckpointPropertiesNV))))
fromCStructQueueFamilyCheckpointPropertiesNV :: VkQueueFamilyCheckpointPropertiesNV -> IO QueueFamilyCheckpointPropertiesNV
fromCStructQueueFamilyCheckpointPropertiesNV c = QueueFamilyCheckpointPropertiesNV <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkQueueFamilyCheckpointPropertiesNV)))
                                                                                   <*> pure (vkCheckpointExecutionStageMask (c :: VkQueueFamilyCheckpointPropertiesNV))

-- | Wrapper for 'vkCmdSetCheckpointNV'
cmdSetCheckpointNV :: CommandBuffer ->  Ptr () ->  IO ()
cmdSetCheckpointNV = \(CommandBuffer commandBuffer commandTable) -> \pCheckpointMarker -> Graphics.Vulkan.C.Dynamic.cmdSetCheckpointNV commandTable commandBuffer pCheckpointMarker *> (pure ())

-- | Wrapper for 'vkGetQueueCheckpointDataNV'
getNumQueueCheckpointDataNV :: Queue ->  IO (Word32)
getNumQueueCheckpointDataNV = \(Queue queue commandTable) -> alloca (\pCheckpointDataCount -> Graphics.Vulkan.C.Dynamic.getQueueCheckpointDataNV commandTable queue pCheckpointDataCount nullPtr *> (peek pCheckpointDataCount))

-- | Wrapper for 'vkGetQueueCheckpointDataNV'
getQueueCheckpointDataNV :: Queue ->  Word32 ->  IO (Vector CheckpointDataNV)
getQueueCheckpointDataNV = \(Queue queue commandTable) -> \checkpointDataCount -> allocaArray (fromIntegral checkpointDataCount) (\pCheckpointData -> with checkpointDataCount (\pCheckpointDataCount -> Graphics.Vulkan.C.Dynamic.getQueueCheckpointDataNV commandTable queue pCheckpointDataCount pCheckpointData *> ((flip Data.Vector.generateM ((\p -> fromCStructCheckpointDataNV <=< peekElemOff p) pCheckpointData) =<< (fromIntegral <$> (peek pCheckpointDataCount))))))
-- | Call 'getNumQueueCheckpointDataNV' to get the number of return values, then use that
-- number to call 'getQueueCheckpointDataNV' to get all the values.
getAllQueueCheckpointDataNV :: Queue ->  IO (Vector CheckpointDataNV)
getAllQueueCheckpointDataNV queue =
  getNumQueueCheckpointDataNV queue
    >>= \num -> getQueueCheckpointDataNV queue num

