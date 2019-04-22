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
  , pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
  , pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.String
  ( IsString
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( VkCheckpointDataNV(..)
  , VkQueueFamilyCheckpointPropertiesNV(..)
  , vkCmdSetCheckpointNV
  , vkGetQueueCheckpointDataNV
  , pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
  , pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
  )



-- | VkCheckpointDataNV - return structure for command buffer checkpoint data
--
-- == Valid Usage (Implicit)
--
-- Note that the stages at which a checkpoint marker /can/ be executed are
-- implementation-defined and /can/ be queried by calling
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.vkGetQueueCheckpointDataNV'
data CheckpointDataNV = CheckpointDataNV
  { -- Univalued member elided
  -- No documentation found for Nested "CheckpointDataNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CheckpointDataNV" "stage"
  stage :: PipelineStageFlagBits
  , -- No documentation found for Nested "CheckpointDataNV" "pCheckpointMarker"
  checkpointMarker :: Ptr ()
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCheckpointDataNV' and
-- marshal a 'CheckpointDataNV' into it. The 'VkCheckpointDataNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCheckpointDataNV :: CheckpointDataNV -> (VkCheckpointDataNV -> IO a) -> IO a
withCStructCheckpointDataNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: CheckpointDataNV)) (\pPNext -> cont (VkCheckpointDataNV VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV pPNext (stage (marshalled :: CheckpointDataNV)) (checkpointMarker (marshalled :: CheckpointDataNV))))

-- | A function to read a 'VkCheckpointDataNV' and all additional
-- structures in the pointer chain into a 'CheckpointDataNV'.
fromCStructCheckpointDataNV :: VkCheckpointDataNV -> IO CheckpointDataNV
fromCStructCheckpointDataNV c = CheckpointDataNV <$> -- Univalued Member elided
                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCheckpointDataNV)))
                                                 <*> pure (vkStage (c :: VkCheckpointDataNV))
                                                 <*> pure (vkPCheckpointMarker (c :: VkCheckpointDataNV))

instance Zero CheckpointDataNV where
  zero = CheckpointDataNV Nothing
                          zero
                          zero



-- | VkQueueFamilyCheckpointPropertiesNV - return structure for queue family
-- checkpoint info query
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data QueueFamilyCheckpointPropertiesNV = QueueFamilyCheckpointPropertiesNV
  { -- Univalued member elided
  -- No documentation found for Nested "QueueFamilyCheckpointPropertiesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "QueueFamilyCheckpointPropertiesNV" "checkpointExecutionStageMask"
  checkpointExecutionStageMask :: PipelineStageFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkQueueFamilyCheckpointPropertiesNV' and
-- marshal a 'QueueFamilyCheckpointPropertiesNV' into it. The 'VkQueueFamilyCheckpointPropertiesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructQueueFamilyCheckpointPropertiesNV :: QueueFamilyCheckpointPropertiesNV -> (VkQueueFamilyCheckpointPropertiesNV -> IO a) -> IO a
withCStructQueueFamilyCheckpointPropertiesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: QueueFamilyCheckpointPropertiesNV)) (\pPNext -> cont (VkQueueFamilyCheckpointPropertiesNV VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV pPNext (checkpointExecutionStageMask (marshalled :: QueueFamilyCheckpointPropertiesNV))))

-- | A function to read a 'VkQueueFamilyCheckpointPropertiesNV' and all additional
-- structures in the pointer chain into a 'QueueFamilyCheckpointPropertiesNV'.
fromCStructQueueFamilyCheckpointPropertiesNV :: VkQueueFamilyCheckpointPropertiesNV -> IO QueueFamilyCheckpointPropertiesNV
fromCStructQueueFamilyCheckpointPropertiesNV c = QueueFamilyCheckpointPropertiesNV <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkQueueFamilyCheckpointPropertiesNV)))
                                                                                   <*> pure (vkCheckpointExecutionStageMask (c :: VkQueueFamilyCheckpointPropertiesNV))

instance Zero QueueFamilyCheckpointPropertiesNV where
  zero = QueueFamilyCheckpointPropertiesNV Nothing
                                           zero



-- | vkCmdSetCheckpointNV - insert diagnostic checkpoint in command stream
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that will receive the marker
--
-- -   @pCheckpointMarker@ is an opaque application-provided value that
--     will be associated with the checkpoint.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, compute,
--     or transfer operations
--
-- == Host Synchronization
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > |                 |                 | Transfer        |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdSetCheckpointNV :: CommandBuffer ->  Ptr () ->  IO ()
cmdSetCheckpointNV = \(CommandBuffer commandBuffer' commandTable) -> \pCheckpointMarker' -> vkCmdSetCheckpointNV commandTable commandBuffer' pCheckpointMarker' *> (pure ())


-- | vkGetQueueCheckpointDataNV - retrieve diagnostic checkpoint data
--
-- = Parameters
--
-- -   @queue@ is the 'Graphics.Vulkan.C.Core10.Queue.VkQueue' object the
--     caller would like to retrieve checkpoint data for
--
-- -   @pCheckpointDataCount@ is a pointer to an integer related to the
--     number of checkpoint markers available or queried, as described
--     below.
--
-- -   @pCheckpointData@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.VkCheckpointDataNV'
--     structures.
--
-- = Description
--
-- If @pCheckpointData@ is @NULL@, then the number of checkpoint markers
-- available is returned in @pCheckpointDataCount@.
--
-- Otherwise, @pCheckpointDataCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pCheckpointData@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pCheckpointData@.
--
-- If @pCheckpointDataCount@ is less than the number of checkpoint markers
-- available, at most @pCheckpointDataCount@ structures will be written.
--
-- == Valid Usage
--
-- -   The device that @queue@ belongs to /must/ be in the lost state
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
--     handle
--
-- -   @pCheckpointDataCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pCheckpointDataCount@ is not @0@, and
--     @pCheckpointData@ is not @NULL@, @pCheckpointData@ /must/ be a valid
--     pointer to an array of @pCheckpointDataCount@
--     'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.VkCheckpointDataNV'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.VkCheckpointDataNV',
-- 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
getNumQueueCheckpointDataNV :: Queue ->  IO (Word32)
getNumQueueCheckpointDataNV = \(Queue queue' commandTable) -> alloca (\pCheckpointDataCount' -> vkGetQueueCheckpointDataNV commandTable queue' pCheckpointDataCount' nullPtr *> (peek pCheckpointDataCount'))

-- | vkGetQueueCheckpointDataNV - retrieve diagnostic checkpoint data
--
-- = Parameters
--
-- -   @queue@ is the 'Graphics.Vulkan.C.Core10.Queue.VkQueue' object the
--     caller would like to retrieve checkpoint data for
--
-- -   @pCheckpointDataCount@ is a pointer to an integer related to the
--     number of checkpoint markers available or queried, as described
--     below.
--
-- -   @pCheckpointData@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.VkCheckpointDataNV'
--     structures.
--
-- = Description
--
-- If @pCheckpointData@ is @NULL@, then the number of checkpoint markers
-- available is returned in @pCheckpointDataCount@.
--
-- Otherwise, @pCheckpointDataCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pCheckpointData@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pCheckpointData@.
--
-- If @pCheckpointDataCount@ is less than the number of checkpoint markers
-- available, at most @pCheckpointDataCount@ structures will be written.
--
-- == Valid Usage
--
-- -   The device that @queue@ belongs to /must/ be in the lost state
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
--     handle
--
-- -   @pCheckpointDataCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pCheckpointDataCount@ is not @0@, and
--     @pCheckpointData@ is not @NULL@, @pCheckpointData@ /must/ be a valid
--     pointer to an array of @pCheckpointDataCount@
--     'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.VkCheckpointDataNV'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.VkCheckpointDataNV',
-- 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
getQueueCheckpointDataNV :: Queue ->  Word32 ->  IO (Vector CheckpointDataNV)
getQueueCheckpointDataNV = \(Queue queue' commandTable) -> \checkpointDataCount' -> allocaArray (fromIntegral checkpointDataCount') (\pCheckpointData' -> with checkpointDataCount' (\pCheckpointDataCount' -> vkGetQueueCheckpointDataNV commandTable queue' pCheckpointDataCount' pCheckpointData' *> ((flip Data.Vector.generateM ((\p -> fromCStructCheckpointDataNV <=< peekElemOff p) pCheckpointData') =<< (fromIntegral <$> (peek pCheckpointDataCount'))))))
-- | Returns all the values available from 'getQueueCheckpointDataNV'.
getAllQueueCheckpointDataNV :: Queue ->  IO (Vector CheckpointDataNV)
getAllQueueCheckpointDataNV queue' =
  getNumQueueCheckpointDataNV queue'
    >>= \num -> getQueueCheckpointDataNV queue' num


-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME"
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME = VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION"
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION :: Integral a => a
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION = VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
