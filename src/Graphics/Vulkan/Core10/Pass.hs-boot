{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Pass
  ( AccessFlagBits
  , AccessFlags
  , AttachmentDescriptionFlagBits
  , AttachmentDescriptionFlags
  , AttachmentLoadOp
  , AttachmentStoreOp
  , DependencyFlagBits
  , DependencyFlags
  , Framebuffer
  , FramebufferCreateFlags
  , PipelineBindPoint
  , RenderPassCreateFlags
  , SubpassDescriptionFlagBits
  , SubpassDescriptionFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits
  , VkAttachmentDescriptionFlagBits
  , VkAttachmentLoadOp
  , VkAttachmentStoreOp
  , VkDependencyFlagBits
  , VkFramebuffer
  , VkFramebufferCreateFlags
  , VkPipelineBindPoint
  , VkRenderPassCreateFlags
  , VkSubpassDescriptionFlagBits
  )


-- No documentation found for TopLevel "AccessFlagBits"
type AccessFlagBits = VkAccessFlagBits

-- No documentation found for TopLevel "AccessFlags"
type AccessFlags = AccessFlagBits

-- No documentation found for TopLevel "AttachmentDescriptionFlagBits"
type AttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits

-- No documentation found for TopLevel "AttachmentDescriptionFlags"
type AttachmentDescriptionFlags = AttachmentDescriptionFlagBits

-- No documentation found for TopLevel "AttachmentLoadOp"
type AttachmentLoadOp = VkAttachmentLoadOp

-- No documentation found for TopLevel "AttachmentStoreOp"
type AttachmentStoreOp = VkAttachmentStoreOp

-- No documentation found for TopLevel "DependencyFlagBits"
type DependencyFlagBits = VkDependencyFlagBits

-- No documentation found for TopLevel "DependencyFlags"
type DependencyFlags = DependencyFlagBits

-- No documentation found for TopLevel "Framebuffer"
type Framebuffer = VkFramebuffer

-- No documentation found for TopLevel "FramebufferCreateFlags"
type FramebufferCreateFlags = VkFramebufferCreateFlags

-- No documentation found for TopLevel "PipelineBindPoint"
type PipelineBindPoint = VkPipelineBindPoint

-- No documentation found for TopLevel "RenderPassCreateFlags"
type RenderPassCreateFlags = VkRenderPassCreateFlags

-- No documentation found for TopLevel "SubpassDescriptionFlagBits"
type SubpassDescriptionFlagBits = VkSubpassDescriptionFlagBits

-- No documentation found for TopLevel "SubpassDescriptionFlags"
type SubpassDescriptionFlags = SubpassDescriptionFlagBits
