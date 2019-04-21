{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback
  ( PipelineCreationFeedbackFlagBitsEXT
  , PipelineCreationFeedbackFlagsEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackFlagBitsEXT
  )


-- | VkPipelineCreationFeedbackFlagBitsEXT - Bitmask specifying pipeline or
-- pipeline stage creation feedback
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackCreateInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT'
type PipelineCreationFeedbackFlagBitsEXT = VkPipelineCreationFeedbackFlagBitsEXT

-- | VkPipelineCreationFeedbackFlagsEXT - Bitmask of
-- VkPipelineCreationFeedbackFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackFlagsEXT'
-- is a bitmask type for providing zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackFlagBitsEXT'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackFlagBitsEXT'
type PipelineCreationFeedbackFlagsEXT = PipelineCreationFeedbackFlagBitsEXT
