{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  CheckpointDataNV(..)
  , 
  QueueFamilyCheckpointPropertiesNV(..)
#endif
  , cmdSetCheckpointNV
#if defined(VK_USE_PLATFORM_GGP)
  , getNumQueueCheckpointDataNV
  , getQueueCheckpointDataNV
  , getAllQueueCheckpointDataNV
#endif
  , pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
  , pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import qualified Data.Vector
  ( generateM
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Array
  ( allocaArray
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif
import Foreign.Ptr
  ( Ptr
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peekElemOff
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( vkCmdSetCheckpointNV
  , pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
  , pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( vkGetQueueCheckpointDataNV
  )
#endif
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Queue
  ( PipelineStageFlagBits
  , PipelineStageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Queue
  ( Queue(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCheckpointDataNV"
data CheckpointDataNV = CheckpointDataNV
  { -- No documentation found for Nested "CheckpointDataNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CheckpointDataNV" "stage"
  stage :: PipelineStageFlagBits
  , -- No documentation found for Nested "CheckpointDataNV" "pCheckpointMarker"
  checkpointMarker :: Ptr ()
  }
  deriving (Show, Eq)

instance Zero CheckpointDataNV where
  zero = CheckpointDataNV Nothing
                          zero
                          nullPtr

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkQueueFamilyCheckpointPropertiesNV"
data QueueFamilyCheckpointPropertiesNV = QueueFamilyCheckpointPropertiesNV
  { -- No documentation found for Nested "QueueFamilyCheckpointPropertiesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "QueueFamilyCheckpointPropertiesNV" "checkpointExecutionStageMask"
  checkpointExecutionStageMask :: PipelineStageFlags
  }
  deriving (Show, Eq)

instance Zero QueueFamilyCheckpointPropertiesNV where
  zero = QueueFamilyCheckpointPropertiesNV Nothing
                                           zero

#endif


-- No documentation found for TopLevel "vkCmdSetCheckpointNV"
cmdSetCheckpointNV :: CommandBuffer ->  Ptr () ->  IO ()
cmdSetCheckpointNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetQueueCheckpointDataNV"
getNumQueueCheckpointDataNV :: Queue ->  IO (Word32)
getNumQueueCheckpointDataNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetQueueCheckpointDataNV"
getQueueCheckpointDataNV :: Queue ->  Word32 ->  IO (Vector CheckpointDataNV)
getQueueCheckpointDataNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getQueueCheckpointDataNV'.
getAllQueueCheckpointDataNV :: Queue ->  IO (Vector CheckpointDataNV)
getAllQueueCheckpointDataNV queue' =
  getNumQueueCheckpointDataNV queue'
    >>= \num -> getQueueCheckpointDataNV queue' num

#endif

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME"
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME = VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION"
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION :: Integral a => a
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION = VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
