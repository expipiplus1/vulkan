{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( VkPhysicalDeviceExclusiveScissorFeaturesNV(..)
  , VkPipelineViewportExclusiveScissorStateCreateInfoNV(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdSetExclusiveScissorNV
#endif
  , FN_vkCmdSetExclusiveScissorNV
  , PFN_vkCmdSetExclusiveScissorNV
  , pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  , pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
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
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkDynamicState(..)
  , VkRect2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceExclusiveScissorFeaturesNV"
data VkPhysicalDeviceExclusiveScissorFeaturesNV = VkPhysicalDeviceExclusiveScissorFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceExclusiveScissorFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceExclusiveScissorFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceExclusiveScissorFeaturesNV" "exclusiveScissor"
  vkExclusiveScissor :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExclusiveScissorFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExclusiveScissorFeaturesNV <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExclusiveScissorFeaturesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExclusiveScissorFeaturesNV))
                *> poke (ptr `plusPtr` 16) (vkExclusiveScissor (poked :: VkPhysicalDeviceExclusiveScissorFeaturesNV))
-- No documentation found for TopLevel "VkPipelineViewportExclusiveScissorStateCreateInfoNV"
data VkPipelineViewportExclusiveScissorStateCreateInfoNV = VkPipelineViewportExclusiveScissorStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineViewportExclusiveScissorStateCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineViewportExclusiveScissorStateCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineViewportExclusiveScissorStateCreateInfoNV" "exclusiveScissorCount"
  vkExclusiveScissorCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportExclusiveScissorStateCreateInfoNV" "pExclusiveScissors"
  vkPExclusiveScissors :: Ptr VkRect2D
  }
  deriving (Eq, Show)

instance Storable VkPipelineViewportExclusiveScissorStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportExclusiveScissorStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                                 <*> peek (ptr `plusPtr` 8)
                                                                 <*> peek (ptr `plusPtr` 16)
                                                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkExclusiveScissorCount (poked :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPExclusiveScissors (poked :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetExclusiveScissorNV"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetExclusiveScissorNV" vkCmdSetExclusiveScissorNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ()

#endif
type FN_vkCmdSetExclusiveScissorNV = ("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ()
type PFN_vkCmdSetExclusiveScissorNV = FunPtr FN_vkCmdSetExclusiveScissorNV
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV"
pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV :: VkDynamicState
pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV = VkDynamicState 1000205001
-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME"
pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = "VK_NV_scissor_exclusive"
-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION"
pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION :: Integral a => a
pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV = VkStructureType 1000205002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV = VkStructureType 1000205000
