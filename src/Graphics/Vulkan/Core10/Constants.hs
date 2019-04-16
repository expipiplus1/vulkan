{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.Constants
  ( PipelineCacheHeaderVersion
  , pattern VK_LOD_CLAMP_NONE
  , pattern VK_REMAINING_MIP_LEVELS
  , pattern VK_REMAINING_ARRAY_LAYERS
  , pattern VK_WHOLE_SIZE
  , pattern VK_ATTACHMENT_UNUSED
  , pattern VK_TRUE
  , pattern VK_FALSE
  , pattern VK_NULL_HANDLE
  , pattern VK_QUEUE_FAMILY_IGNORED
  , pattern VK_SUBPASS_EXTERNAL
  ) where




import Graphics.Vulkan.C.Core10.Constants
  ( VkPipelineCacheHeaderVersion(..)
  )
import Graphics.Vulkan.C.Core10.Constants
  ( pattern VK_ATTACHMENT_UNUSED
  , pattern VK_LOD_CLAMP_NONE
  , pattern VK_NULL_HANDLE
  , pattern VK_QUEUE_FAMILY_IGNORED
  , pattern VK_REMAINING_ARRAY_LAYERS
  , pattern VK_REMAINING_MIP_LEVELS
  , pattern VK_SUBPASS_EXTERNAL
  , pattern VK_WHOLE_SIZE
  )
import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_FALSE
  , pattern VK_TRUE
  )


-- No documentation found for TopLevel "PipelineCacheHeaderVersion"
type PipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion
