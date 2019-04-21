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
  , VkFramebufferCreateFlags
  , VkPipelineBindPoint
  , VkRenderPassCreateFlags
  , VkSubpassDescriptionFlagBits
  , VkFramebuffer
  )


-- | VkAccessFlagBits - Bitmask specifying memory access types that will
-- participate in a memory dependency
--
-- = Description
--
-- Certain access types are only performed by a subset of pipeline stages.
-- Any synchronization command that takes both stage masks and access masks
-- uses both to define the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
-- - only the specified access types performed by the specified stages are
-- included in the access scope. An application /must/ not specify an
-- access flag in a synchronization command if it does not include a
-- pipeline stage in the corresponding stage mask that is able to perform
-- accesses of that type. The following table lists, for each access flag,
-- which pipeline stages /can/ perform that type of access.
--
-- > +-----------------------------------+-----------------------------------+
-- > | Access flag                       | Supported pipeline stages         |
-- > +===================================+===================================+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_INDIRECT_COMMAND_READ_BIT | K_PIPELINE_STAGE_DRAW_INDIRECT_BI |
-- > | '                                 | T'                                |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_INDEX_READ_BIT'           | K_PIPELINE_STAGE_VERTEX_INPUT_BIT |
-- > |                                   | '                                 |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_VERTEX_ATTRIBUTE_READ_BIT | K_PIPELINE_STAGE_VERTEX_INPUT_BIT |
-- > | '                                 | '                                 |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | _ACCESS_UNIFORM_READ_BIT'         | NV_mesh_shader.VK_PIPELINE_STAGE_ |
-- > |                                   | TASK_SHADER_BIT_NV',              |
-- > |                                   | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_mesh_shader.VK_PIPELINE_STAGE_ |
-- > |                                   | MESH_SHADER_BIT_NV',              |
-- > |                                   | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_ray_tracing.VK_PIPELINE_STAGE_ |
-- > |                                   | RAY_TRACING_SHADER_BIT_NV',       |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_VERTEX_SHADER_BI |
-- > |                                   | T',                               |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_TESSELLATION_CON |
-- > |                                   | TROL_SHADER_BIT',                 |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_TESSELLATION_EVA |
-- > |                                   | LUATION_SHADER_BIT',              |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_GEOMETRY_SHADER_ |
-- > |                                   | BIT',                             |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_FRAGMENT_SHADER_ |
-- > |                                   | BIT',                             |
-- > |                                   | or                                |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_COMPUTE_SHADER_B |
-- > |                                   | IT'                               |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | _ACCESS_SHADER_READ_BIT'          | NV_mesh_shader.VK_PIPELINE_STAGE_ |
-- > |                                   | TASK_SHADER_BIT_NV',              |
-- > |                                   | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_mesh_shader.VK_PIPELINE_STAGE_ |
-- > |                                   | MESH_SHADER_BIT_NV',              |
-- > |                                   | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_ray_tracing.VK_PIPELINE_STAGE_ |
-- > |                                   | RAY_TRACING_SHADER_BIT_NV',       |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_VERTEX_SHADER_BI |
-- > |                                   | T',                               |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_TESSELLATION_CON |
-- > |                                   | TROL_SHADER_BIT',                 |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_TESSELLATION_EVA |
-- > |                                   | LUATION_SHADER_BIT',              |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_GEOMETRY_SHADER_ |
-- > |                                   | BIT',                             |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_FRAGMENT_SHADER_ |
-- > |                                   | BIT',                             |
-- > |                                   | or                                |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_COMPUTE_SHADER_B |
-- > |                                   | IT'                               |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | _ACCESS_SHADER_WRITE_BIT'         | NV_mesh_shader.VK_PIPELINE_STAGE_ |
-- > |                                   | TASK_SHADER_BIT_NV',              |
-- > |                                   | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_mesh_shader.VK_PIPELINE_STAGE_ |
-- > |                                   | MESH_SHADER_BIT_NV',              |
-- > |                                   | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_ray_tracing.VK_PIPELINE_STAGE_ |
-- > |                                   | RAY_TRACING_SHADER_BIT_NV',       |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_VERTEX_SHADER_BI |
-- > |                                   | T',                               |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_TESSELLATION_CON |
-- > |                                   | TROL_SHADER_BIT',                 |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_TESSELLATION_EVA |
-- > |                                   | LUATION_SHADER_BIT',              |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_GEOMETRY_SHADER_ |
-- > |                                   | BIT',                             |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_FRAGMENT_SHADER_ |
-- > |                                   | BIT',                             |
-- > |                                   | or                                |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_COMPUTE_SHADER_B |
-- > |                                   | IT'                               |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_INPUT_ATTACHMENT_READ_BIT | K_PIPELINE_STAGE_FRAGMENT_SHADER_ |
-- > | '                                 | BIT'                              |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_COLOR_ATTACHMENT_READ_BIT | K_PIPELINE_STAGE_COLOR_ATTACHMENT |
-- > | '                                 | _OUTPUT_BIT'                      |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_COLOR_ATTACHMENT_WRITE_BI | K_PIPELINE_STAGE_COLOR_ATTACHMENT |
-- > | T'                                | _OUTPUT_BIT'                      |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_DEPTH_STENCIL_ATTACHMENT_ | K_PIPELINE_STAGE_EARLY_FRAGMENT_T |
-- > | READ_BIT'                         | ESTS_BIT',                        |
-- > |                                   | or                                |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_LATE_FRAGMENT_TE |
-- > |                                   | STS_BIT'                          |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_DEPTH_STENCIL_ATTACHMENT_ | K_PIPELINE_STAGE_EARLY_FRAGMENT_T |
-- > | WRITE_BIT'                        | ESTS_BIT',                        |
-- > |                                   | or                                |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_LATE_FRAGMENT_TE |
-- > |                                   | STS_BIT'                          |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_TRANSFER_READ_BIT'        | K_PIPELINE_STAGE_TRANSFER_BIT'    |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_TRANSFER_WRITE_BIT'       | K_PIPELINE_STAGE_TRANSFER_BIT'    |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_HOST_READ_BIT'            | K_PIPELINE_STAGE_HOST_BIT'        |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _ACCESS_HOST_WRITE_BIT'           | K_PIPELINE_STAGE_HOST_BIT'        |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | N\/A                              |
-- > | _ACCESS_MEMORY_READ_BIT'          |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pass.VK | N\/A                              |
-- > | _ACCESS_MEMORY_WRITE_BIT'         |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | EXT_blend_operation_advanced.VK_A | K_PIPELINE_STAGE_COLOR_ATTACHMENT |
-- > | CCESS_COLOR_ATTACHMENT_READ_NONCO | _OUTPUT_BIT'                      |
-- > | HERENT_BIT_EXT'                   |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | NVX_device_generated_commands.VK_ | NVX_device_generated_commands.VK_ |
-- > | ACCESS_COMMAND_PROCESS_READ_BIT_N | PIPELINE_STAGE_COMMAND_PROCESS_BI |
-- > | VX'                               | T_NVX'                            |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | NVX_device_generated_commands.VK_ | NVX_device_generated_commands.VK_ |
-- > | ACCESS_COMMAND_PROCESS_WRITE_BIT_ | PIPELINE_STAGE_COMMAND_PROCESS_BI |
-- > | NVX'                              | T_NVX'                            |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | EXT_conditional_rendering.VK_ACCE | EXT_conditional_rendering.VK_PIPE |
-- > | SS_CONDITIONAL_RENDERING_READ_BIT | LINE_STAGE_CONDITIONAL_RENDERING_ |
-- > | _EXT'                             | BIT_EXT'                          |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | NV_shading_rate_image.VK_ACCESS_S | NV_shading_rate_image.VK_PIPELINE |
-- > | HADING_RATE_IMAGE_READ_BIT_NV'    | _STAGE_SHADING_RATE_IMAGE_BIT_NV' |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | EXT_transform_feedback.VK_ACCESS_ | EXT_transform_feedback.VK_PIPELIN |
-- > | TRANSFORM_FEEDBACK_WRITE_BIT_EXT' | E_STAGE_TRANSFORM_FEEDBACK_BIT_EX |
-- > |                                   | T'                                |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | EXT_transform_feedback.VK_ACCESS_ | EXT_transform_feedback.VK_PIPELIN |
-- > | TRANSFORM_FEEDBACK_COUNTER_WRITE_ | E_STAGE_TRANSFORM_FEEDBACK_BIT_EX |
-- > | BIT_EXT'                          | T'                                |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | EXT_transform_feedback.VK_ACCESS_ | K_PIPELINE_STAGE_DRAW_INDIRECT_BI |
-- > | TRANSFORM_FEEDBACK_COUNTER_READ_B | T'                                |
-- > | IT_EXT'                           |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | NV_ray_tracing.VK_ACCESS_ACCELERA | NV_ray_tracing.VK_PIPELINE_STAGE_ |
-- > | TION_STRUCTURE_READ_BIT_NV'       | RAY_TRACING_SHADER_BIT_NV',       |
-- > |                                   | or                                |
-- > |                                   | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_ray_tracing.VK_PIPELINE_STAGE_ |
-- > |                                   | ACCELERATION_STRUCTURE_BUILD_BIT_ |
-- > |                                   | NV'                               |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | NV_ray_tracing.VK_ACCESS_ACCELERA | NV_ray_tracing.VK_PIPELINE_STAGE_ |
-- > | TION_STRUCTURE_WRITE_BIT_NV'      | ACCELERATION_STRUCTURE_BUILD_BIT_ |
-- > |                                   | NV'                               |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | EXT_fragment_density_map.VK_ACCES | EXT_fragment_density_map.VK_PIPEL |
-- > | S_FRAGMENT_DENSITY_MAP_READ_BIT_E | INE_STAGE_FRAGMENT_DENSITY_PROCES |
-- > | XT'                               | S_BIT_EXT'                        |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Supported access types
--
-- If a memory object does not have the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
-- property, then
-- 'Graphics.Vulkan.C.Core10.Memory.vkFlushMappedMemoryRanges' /must/ be
-- called in order to guarantee that writes to the memory object from the
-- host are made available to the host domain, where they /can/ be further
-- made available to the device domain via a domain operation. Similarly,
-- 'Graphics.Vulkan.C.Core10.Memory.vkInvalidateMappedMemoryRanges' /must/
-- be called to guarantee that writes which are available to the host
-- domain are made visible to host operations.
--
-- If the memory object does have the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
-- property flag, writes to the memory object from the host are
-- automatically made available to the host domain. Similarly, writes made
-- available to the host domain are automatically made visible to the host.
--
-- __Note__
--
-- The 'Graphics.Vulkan.C.Core10.Queue.vkQueueSubmit' command
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-host-writes automatically performs a domain operation from host to device>
-- for all writes performed before the command executes, so in most cases
-- an explicit memory barrier is not needed for this case. In the few
-- circumstances where a submit does not occur between the host write and
-- the device read access, writes /can/ be made available by using an
-- explicit memory barrier.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlags'
type AccessFlagBits = VkAccessFlagBits

-- | VkAccessFlags - Bitmask of VkAccessFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlags' is a bitmask type for
-- setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency'
type AccessFlags = AccessFlagBits

-- | VkAttachmentDescriptionFlagBits - Bitmask specifying additional
-- properties of an attachment
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescriptionFlags'
type AttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits

-- | VkAttachmentDescriptionFlags - Bitmask of
-- VkAttachmentDescriptionFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescriptionFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescriptionFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescriptionFlagBits'
type AttachmentDescriptionFlags = AttachmentDescriptionFlagBits

-- | VkAttachmentLoadOp - Specify how contents of an attachment are treated
-- at the beginning of a subpass
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription'
type AttachmentLoadOp = VkAttachmentLoadOp

-- | VkAttachmentStoreOp - Specify how contents of an attachment are treated
-- at the end of a subpass
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription'
type AttachmentStoreOp = VkAttachmentStoreOp

-- | VkDependencyFlagBits - Bitmask specifying how execution and memory
-- dependencies are formed
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlags'
type DependencyFlagBits = VkDependencyFlagBits

-- | VkDependencyFlags - Bitmask of VkDependencyFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlags' is a bitmask type for
-- setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlagBits',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
type DependencyFlags = DependencyFlagBits

-- | VkFramebuffer - Opaque handle to a framebuffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateFramebuffer',
-- 'Graphics.Vulkan.C.Core10.Pass.vkDestroyFramebuffer'
type Framebuffer = VkFramebuffer

-- | VkFramebufferCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateFlags' is a bitmask
-- type for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo'
type FramebufferCreateFlags = VkFramebufferCreateFlags

-- | VkPipelineBindPoint - Specify the bind point of a pipeline object to a
-- command buffer
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindPipeline'
type PipelineBindPoint = VkPipelineBindPoint

-- | VkRenderPassCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateFlags' is a bitmask
-- type for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo'
type RenderPassCreateFlags = VkRenderPassCreateFlags

-- | VkSubpassDescriptionFlagBits - Bitmask specifying usage of a subpass
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescriptionFlags'
type SubpassDescriptionFlagBits = VkSubpassDescriptionFlagBits

-- | VkSubpassDescriptionFlags - Bitmask of VkSubpassDescriptionFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescriptionFlags' is a bitmask
-- type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescriptionFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescriptionFlagBits'
type SubpassDescriptionFlags = SubpassDescriptionFlagBits
