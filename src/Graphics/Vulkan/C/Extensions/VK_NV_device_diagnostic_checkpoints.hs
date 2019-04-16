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


-- No documentation found for TopLevel "VkCheckpointDataNV"
data VkCheckpointDataNV = VkCheckpointDataNV
  { -- No documentation found for Nested "VkCheckpointDataNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCheckpointDataNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCheckpointDataNV" "stage"
  vkStage :: VkPipelineStageFlagBits
  , -- No documentation found for Nested "VkCheckpointDataNV" "pCheckpointMarker"
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
-- No documentation found for TopLevel "VkQueueFamilyCheckpointPropertiesNV"
data VkQueueFamilyCheckpointPropertiesNV = VkQueueFamilyCheckpointPropertiesNV
  { -- No documentation found for Nested "VkQueueFamilyCheckpointPropertiesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkQueueFamilyCheckpointPropertiesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkQueueFamilyCheckpointPropertiesNV" "checkpointExecutionStageMask"
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetCheckpointNV"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetCheckpointNV" vkCmdSetCheckpointNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("pCheckpointMarker" ::: Ptr ()) -> IO ()

#endif
type FN_vkCmdSetCheckpointNV = ("commandBuffer" ::: VkCommandBuffer) -> ("pCheckpointMarker" ::: Ptr ()) -> IO ()
type PFN_vkCmdSetCheckpointNV = FunPtr FN_vkCmdSetCheckpointNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetQueueCheckpointDataNV"
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
