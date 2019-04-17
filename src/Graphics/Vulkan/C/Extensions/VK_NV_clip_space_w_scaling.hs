{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkPipelineViewportWScalingStateCreateInfoNV(..)
  , VkViewportWScalingNV(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdSetViewportWScalingNV
#endif
  , FN_vkCmdSetViewportWScalingNV
  , PFN_vkCmdSetViewportWScalingNV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  , pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  , pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
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
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkDynamicState(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkPipelineViewportWScalingStateCreateInfoNV"
data VkPipelineViewportWScalingStateCreateInfoNV = VkPipelineViewportWScalingStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "viewportWScalingEnable"
  vkViewportWScalingEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "viewportCount"
  vkViewportCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "pViewportWScalings"
  vkPViewportWScalings :: Ptr VkViewportWScalingNV
  }
  deriving (Eq, Show)

instance Storable VkPipelineViewportWScalingStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportWScalingStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 20)
                                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkViewportWScalingEnable (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPViewportWScalings (poked :: VkPipelineViewportWScalingStateCreateInfoNV))

instance Zero VkPipelineViewportWScalingStateCreateInfoNV where
  zero = VkPipelineViewportWScalingStateCreateInfoNV zero
                                                     zero
                                                     zero
                                                     zero
                                                     zero
-- No documentation found for TopLevel "VkViewportWScalingNV"
data VkViewportWScalingNV = VkViewportWScalingNV
  { -- No documentation found for Nested "VkViewportWScalingNV" "xcoeff"
  vkXcoeff :: CFloat
  , -- No documentation found for Nested "VkViewportWScalingNV" "ycoeff"
  vkYcoeff :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkViewportWScalingNV where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkViewportWScalingNV <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkXcoeff (poked :: VkViewportWScalingNV))
                *> poke (ptr `plusPtr` 4) (vkYcoeff (poked :: VkViewportWScalingNV))

instance Zero VkViewportWScalingNV where
  zero = VkViewportWScalingNV zero
                              zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetViewportWScalingNV"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetViewportWScalingNV" vkCmdSetViewportWScalingNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ()

#endif
type FN_vkCmdSetViewportWScalingNV = ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ()
type PFN_vkCmdSetViewportWScalingNV = FunPtr FN_vkCmdSetViewportWScalingNV
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV"
pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: VkDynamicState
pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV = VkDynamicState 1000087000
-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME"
pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME = "VK_NV_clip_space_w_scaling"
-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION"
pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: Integral a => a
pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV = VkStructureType 1000087000
