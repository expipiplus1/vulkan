{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( VkCheckpointDataNV(..)
  , VkQueueFamilyCheckpointPropertiesNV(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdSetCheckpointNV
#endif
  , FN_vkCmdSetCheckpointNV
  , PFN_vkCmdSetCheckpointNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetQueueCheckpointDataNV
#endif
  , FN_vkGetQueueCheckpointDataNV
  , PFN_vkGetQueueCheckpointDataNV
  , pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
  , pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  , VkPipelineStageFlags
  , VkQueue
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkCheckpointDataNV - return structure for command buffer checkpoint data
--
-- = Description
--
-- Unresolved directive in VkCheckpointDataNV.txt -
-- include::..\/validity\/structs\/VkCheckpointDataNV.txt[]
--
-- Note that the stages at which a checkpoint marker /can/ be executed are
-- implementation-defined and /can/ be queried by calling
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'.
--
-- = See Also
--
-- No cross-references are available
data VkCheckpointDataNV = VkCheckpointDataNV
  { -- | @sType@ is the type of this structure
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @stage@ indicates which pipeline stage the checkpoint marker data refers
  -- to.
  vkStage :: VkPipelineStageFlagBits
  , -- | @pCheckpointMarker@ contains the value of the last checkpoint marker
  -- executed in the stage that @stage@ refers to.
  vkPCheckpointMarker :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkCheckpointDataNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkCheckpointDataNV <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCheckpointDataNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCheckpointDataNV))
                *> poke (ptr `plusPtr` 16) (vkStage (poked :: VkCheckpointDataNV))
                *> poke (ptr `plusPtr` 24) (vkPCheckpointMarker (poked :: VkCheckpointDataNV))

instance Zero VkCheckpointDataNV where
  zero = VkCheckpointDataNV zero
                            zero
                            zero
                            zero
-- | VkQueueFamilyCheckpointPropertiesNV - return structure for queue family
-- checkpoint info query
--
-- = Description
--
-- Unresolved directive in VkQueueFamilyCheckpointPropertiesNV.txt -
-- include::..\/validity\/structs\/VkQueueFamilyCheckpointPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkQueueFamilyCheckpointPropertiesNV = VkQueueFamilyCheckpointPropertiesNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @checkpointExecutionStageMask@ is a mask indicating which pipeline
  -- stages the implementation can execute checkpoint markers in.
  vkCheckpointExecutionStageMask :: VkPipelineStageFlags
  }
  deriving (Eq, Show)

instance Storable VkQueueFamilyCheckpointPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkQueueFamilyCheckpointPropertiesNV <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkQueueFamilyCheckpointPropertiesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkQueueFamilyCheckpointPropertiesNV))
                *> poke (ptr `plusPtr` 16) (vkCheckpointExecutionStageMask (poked :: VkQueueFamilyCheckpointPropertiesNV))

instance Zero VkQueueFamilyCheckpointPropertiesNV where
  zero = VkQueueFamilyCheckpointPropertiesNV zero
                                             zero
                                             zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCmdSetCheckpointNV - insert diagnostic checkpoint in command stream
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that will receive the marker
--
-- -   @pCheckpointMarker@ is an opaque application-provided value that
--     will be associated with the checkpoint.
--
-- = Description
--
-- Unresolved directive in vkCmdSetCheckpointNV.txt -
-- include::..\/validity\/protos\/vkCmdSetCheckpointNV.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetCheckpointNV" vkCmdSetCheckpointNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("pCheckpointMarker" ::: Ptr ()) -> IO ()

#endif
type FN_vkCmdSetCheckpointNV = ("commandBuffer" ::: VkCommandBuffer) -> ("pCheckpointMarker" ::: Ptr ()) -> IO ()
type PFN_vkCmdSetCheckpointNV = FunPtr FN_vkCmdSetCheckpointNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
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
--     @VkCheckpointDataNV@ structures.
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
-- Unresolved directive in vkGetQueueCheckpointDataNV.txt -
-- include::..\/validity\/protos\/vkGetQueueCheckpointDataNV.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetQueueCheckpointDataNV" vkGetQueueCheckpointDataNV :: ("queue" ::: VkQueue) -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr VkCheckpointDataNV) -> IO ()

#endif
type FN_vkGetQueueCheckpointDataNV = ("queue" ::: VkQueue) -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr VkCheckpointDataNV) -> IO ()
type PFN_vkGetQueueCheckpointDataNV = FunPtr FN_vkGetQueueCheckpointDataNV
-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME"
pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME = "VK_NV_device_diagnostic_checkpoints"
-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION"
pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION :: Integral a => a
pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION = 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV"
pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV = VkStructureType 1000206000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV"
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV = VkStructureType 1000206001
