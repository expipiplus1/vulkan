{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Pass
  ( AccessFlagBits
  , pattern ACCESS_INDIRECT_COMMAND_READ_BIT
  , pattern ACCESS_INDEX_READ_BIT
  , pattern ACCESS_VERTEX_ATTRIBUTE_READ_BIT
  , pattern ACCESS_UNIFORM_READ_BIT
  , pattern ACCESS_INPUT_ATTACHMENT_READ_BIT
  , pattern ACCESS_SHADER_READ_BIT
  , pattern ACCESS_SHADER_WRITE_BIT
  , pattern ACCESS_COLOR_ATTACHMENT_READ_BIT
  , pattern ACCESS_COLOR_ATTACHMENT_WRITE_BIT
  , pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
  , pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
  , pattern ACCESS_TRANSFER_READ_BIT
  , pattern ACCESS_TRANSFER_WRITE_BIT
  , pattern ACCESS_HOST_READ_BIT
  , pattern ACCESS_HOST_WRITE_BIT
  , pattern ACCESS_MEMORY_READ_BIT
  , pattern ACCESS_MEMORY_WRITE_BIT
  , AccessFlags
  , withCStructAttachmentDescription
  , fromCStructAttachmentDescription
  , AttachmentDescription(..)
  , AttachmentDescriptionFlagBits
  , pattern ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
  , AttachmentDescriptionFlags
  , AttachmentLoadOp
  , pattern ATTACHMENT_LOAD_OP_LOAD
  , pattern ATTACHMENT_LOAD_OP_CLEAR
  , pattern ATTACHMENT_LOAD_OP_DONT_CARE
  , withCStructAttachmentReference
  , fromCStructAttachmentReference
  , AttachmentReference(..)
  , AttachmentStoreOp
  , pattern ATTACHMENT_STORE_OP_STORE
  , pattern ATTACHMENT_STORE_OP_DONT_CARE
  , DependencyFlagBits
  , pattern DEPENDENCY_BY_REGION_BIT
  , DependencyFlags
  , Framebuffer
  , FramebufferCreateFlags
  , withCStructFramebufferCreateInfo
  , fromCStructFramebufferCreateInfo
  , FramebufferCreateInfo(..)
  , PipelineBindPoint
  , pattern PIPELINE_BIND_POINT_GRAPHICS
  , pattern PIPELINE_BIND_POINT_COMPUTE
  , RenderPassCreateFlags
  , withCStructRenderPassCreateInfo
  , fromCStructRenderPassCreateInfo
  , RenderPassCreateInfo(..)
  , withCStructSubpassDependency
  , fromCStructSubpassDependency
  , SubpassDependency(..)
  , withCStructSubpassDescription
  , fromCStructSubpassDescription
  , SubpassDescription(..)
  , SubpassDescriptionFlagBits
  , SubpassDescriptionFlags
  , createFramebuffer
  , createRenderPass
  , destroyFramebuffer
  , destroyRenderPass
  , getRenderAreaGranularity
  , withFramebuffer
  , withRenderPass
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.List
  ( minimum
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  , VkAttachmentDescription(..)
  , VkAttachmentDescriptionFlagBits(..)
  , VkAttachmentLoadOp(..)
  , VkAttachmentReference(..)
  , VkAttachmentStoreOp(..)
  , VkDependencyFlagBits(..)
  , VkFramebufferCreateFlags(..)
  , VkFramebufferCreateInfo(..)
  , VkPipelineBindPoint(..)
  , VkRenderPassCreateFlags(..)
  , VkRenderPassCreateInfo(..)
  , VkSubpassDependency(..)
  , VkSubpassDescription(..)
  , VkSubpassDescriptionFlagBits(..)
  , VkFramebuffer
  , vkCreateFramebuffer
  , vkCreateRenderPass
  , vkDestroyFramebuffer
  , vkDestroyRenderPass
  , vkGetRenderAreaGranularity
  , pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
  , pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
  , pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
  , pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
  , pattern VK_ACCESS_HOST_READ_BIT
  , pattern VK_ACCESS_HOST_WRITE_BIT
  , pattern VK_ACCESS_INDEX_READ_BIT
  , pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT
  , pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
  , pattern VK_ACCESS_MEMORY_READ_BIT
  , pattern VK_ACCESS_MEMORY_WRITE_BIT
  , pattern VK_ACCESS_SHADER_READ_BIT
  , pattern VK_ACCESS_SHADER_WRITE_BIT
  , pattern VK_ACCESS_TRANSFER_READ_BIT
  , pattern VK_ACCESS_TRANSFER_WRITE_BIT
  , pattern VK_ACCESS_UNIFORM_READ_BIT
  , pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT
  , pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
  , pattern VK_ATTACHMENT_LOAD_OP_CLEAR
  , pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE
  , pattern VK_ATTACHMENT_LOAD_OP_LOAD
  , pattern VK_ATTACHMENT_STORE_OP_DONT_CARE
  , pattern VK_ATTACHMENT_STORE_OP_STORE
  , pattern VK_DEPENDENCY_BY_REGION_BIT
  , pattern VK_PIPELINE_BIND_POINT_COMPUTE
  , pattern VK_PIPELINE_BIND_POINT_GRAPHICS
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , SampleCountFlagBits
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageView
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , RenderPass
  , fromCStructExtent2D
  )
import Graphics.Vulkan.Core10.Queue
  ( PipelineStageFlags
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
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


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_INDIRECT_COMMAND_READ_BIT'
-- specifies read access to indirect command data read as part of an
-- indirect drawing or dispatch command.
pattern ACCESS_INDIRECT_COMMAND_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_INDIRECT_COMMAND_READ_BIT = VK_ACCESS_INDIRECT_COMMAND_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_INDEX_READ_BIT' specifies read
-- access to an index buffer as part of an indexed drawing command, bound
-- by
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'.
pattern ACCESS_INDEX_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_INDEX_READ_BIT = VK_ACCESS_INDEX_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT'
-- specifies read access to a vertex buffer as part of a drawing command,
-- bound by
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers'.
pattern ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_UNIFORM_READ_BIT' specifies
-- read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer>.
pattern ACCESS_UNIFORM_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_UNIFORM_READ_BIT = VK_ACCESS_UNIFORM_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_INPUT_ATTACHMENT_READ_BIT'
-- specifies read access to an
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass input attachment>
-- within a render pass during fragment shading.
pattern ACCESS_INPUT_ATTACHMENT_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_INPUT_ATTACHMENT_READ_BIT = VK_ACCESS_INPUT_ATTACHMENT_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_SHADER_READ_BIT' specifies read
-- access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage image>.
pattern ACCESS_SHADER_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_SHADER_READ_BIT = VK_ACCESS_SHADER_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_SHADER_WRITE_BIT' specifies
-- write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage image>.
pattern ACCESS_SHADER_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_SHADER_WRITE_BIT = VK_ACCESS_SHADER_WRITE_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT'
-- specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass color attachment>,
-- such as via
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blending blending>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-logicop logic operations>,
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
-- It does not include
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>.
pattern ACCESS_COLOR_ATTACHMENT_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_COLOR_ATTACHMENT_READ_BIT = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'
-- specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass color, resolve, or depth\/stencil resolve attachment>
-- during a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass render pass>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
pattern ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT'
-- specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'
-- specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_TRANSFER_READ_BIT' specifies
-- read access to an image or buffer in a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies copy>
-- operation.
pattern ACCESS_TRANSFER_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_TRANSFER_READ_BIT = VK_ACCESS_TRANSFER_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_TRANSFER_WRITE_BIT' specifies
-- write access to an image or buffer in a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#clears clear>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies copy>
-- operation.
pattern ACCESS_TRANSFER_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_TRANSFER_WRITE_BIT = VK_ACCESS_TRANSFER_WRITE_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_READ_BIT' specifies read
-- access by a host operation. Accesses of this type are not performed
-- through a resource, but directly on memory.
pattern ACCESS_HOST_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_HOST_READ_BIT = VK_ACCESS_HOST_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_WRITE_BIT' specifies write
-- access by a host operation. Accesses of this type are not performed
-- through a resource, but directly on memory.
pattern ACCESS_HOST_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_HOST_WRITE_BIT = VK_ACCESS_HOST_WRITE_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_MEMORY_READ_BIT' specifies read
-- access via non-specific entities. These entities include the Vulkan
-- device and host, but /may/ also include entities external to the Vulkan
-- device or otherwise not part of the core Vulkan pipeline. When included
-- in a destination access mask, makes all available writes visible to all
-- future read accesses on entities known to the Vulkan device.
pattern ACCESS_MEMORY_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_MEMORY_READ_BIT = VK_ACCESS_MEMORY_READ_BIT


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_MEMORY_WRITE_BIT' specifies
-- write access via non-specific entities. These entities include the
-- Vulkan device and host, but /may/ also include entities external to the
-- Vulkan device or otherwise not part of the core Vulkan pipeline. When
-- included in a source access mask, all writes that are performed by
-- entities known to the Vulkan device are made available. When included in
-- a destination access mask, makes all available writes visible to all
-- future write accesses on entities known to the Vulkan device.
pattern ACCESS_MEMORY_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_MEMORY_WRITE_BIT = VK_ACCESS_MEMORY_WRITE_BIT

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


-- | VkAttachmentDescription - Structure specifying an attachment description
--
-- = Description
--
-- If the attachment uses a color format, then @loadOp@ and @storeOp@ are
-- used, and @stencilLoadOp@ and @stencilStoreOp@ are ignored. If the
-- format has depth and\/or stencil components, @loadOp@ and @storeOp@
-- apply only to the depth data, while @stencilLoadOp@ and @stencilStoreOp@
-- define how the stencil data is handled. @loadOp@ and @stencilLoadOp@
-- define the /load operations/ that execute as part of the first subpass
-- that uses the attachment. @storeOp@ and @stencilStoreOp@ define the
-- /store operations/ that execute as part of the last subpass that uses
-- the attachment.
--
-- The load operation for each sample in an attachment happens-before any
-- recorded command which accesses the sample in the first subpass where
-- the attachment is used. Load operations for attachments with a
-- depth\/stencil format execute in the
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
-- pipeline stage. Load operations for attachments with a color format
-- execute in the
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage.
--
-- The store operation for each sample in an attachment happens-after any
-- recorded command which accesses the sample in the last subpass where the
-- attachment is used. Store operations for attachments with a
-- depth\/stencil format execute in the
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
-- pipeline stage. Store operations for attachments with a color format
-- execute in the
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage.
--
-- If an attachment is not used by any subpass, then @loadOp@, @storeOp@,
-- @stencilStoreOp@, and @stencilLoadOp@ are ignored, and the attachment’s
-- memory contents will not be modified by execution of a render pass
-- instance.
--
-- The load and store operations apply on the first and last use of each
-- view in the render pass, respectively. If a view index of an attachment
-- is not included in the view mask in any subpass that uses it, then the
-- load and store operations are ignored, and the attachment’s memory
-- contents will not be modified by execution of a render pass instance.
--
-- During a render pass instance, input\/color attachments with color
-- formats that have a component size of 8, 16, or 32 bits /must/ be
-- represented in the attachment’s format throughout the instance.
-- Attachments with other floating- or fixed-point color formats, or with
-- depth components /may/ be represented in a format with a precision
-- higher than the attachment format, but /must/ be represented with the
-- same range. When such a component is loaded via the @loadOp@, it will be
-- converted into an implementation-dependent format used by the render
-- pass. Such components /must/ be converted from the render pass format,
-- to the format of the attachment, before they are resolved or stored at
-- the end of a render pass instance via @storeOp@. Conversions occur as
-- described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fundamentals-numerics Numeric Representation and Computation>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fundamentals-fixedconv Fixed-Point Data Conversions>.
--
-- If @flags@ includes
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT',
-- then the attachment is treated as if it shares physical memory with
-- another attachment in the same render pass. This information limits the
-- ability of the implementation to reorder certain operations (like layout
-- transitions and the @loadOp@) such that it is not improperly reordered
-- against other uses of the same physical memory via a different
-- attachment. This is described in more detail below.
--
-- == Valid Usage
--
-- Unresolved directive in VkAttachmentDescription.txt -
-- include::{generated}\/validity\/structs\/VkAttachmentDescription.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescriptionFlags',
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentLoadOp',
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentStoreOp',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'
data AttachmentDescription = AttachmentDescription
  { -- No documentation found for Nested "AttachmentDescription" "flags"
  flags :: AttachmentDescriptionFlags
  , -- No documentation found for Nested "AttachmentDescription" "format"
  format :: Format
  , -- No documentation found for Nested "AttachmentDescription" "samples"
  samples :: SampleCountFlagBits
  , -- No documentation found for Nested "AttachmentDescription" "loadOp"
  loadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "AttachmentDescription" "storeOp"
  storeOp :: AttachmentStoreOp
  , -- No documentation found for Nested "AttachmentDescription" "stencilLoadOp"
  stencilLoadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "AttachmentDescription" "stencilStoreOp"
  stencilStoreOp :: AttachmentStoreOp
  , -- No documentation found for Nested "AttachmentDescription" "initialLayout"
  initialLayout :: ImageLayout
  , -- No documentation found for Nested "AttachmentDescription" "finalLayout"
  finalLayout :: ImageLayout
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAttachmentDescription' and
-- marshal a 'AttachmentDescription' into it. The 'VkAttachmentDescription' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAttachmentDescription :: AttachmentDescription -> (VkAttachmentDescription -> IO a) -> IO a
withCStructAttachmentDescription marshalled cont = cont (VkAttachmentDescription (flags (marshalled :: AttachmentDescription)) (format (marshalled :: AttachmentDescription)) (samples (marshalled :: AttachmentDescription)) (loadOp (marshalled :: AttachmentDescription)) (storeOp (marshalled :: AttachmentDescription)) (stencilLoadOp (marshalled :: AttachmentDescription)) (stencilStoreOp (marshalled :: AttachmentDescription)) (initialLayout (marshalled :: AttachmentDescription)) (finalLayout (marshalled :: AttachmentDescription)))

-- | A function to read a 'VkAttachmentDescription' and all additional
-- structures in the pointer chain into a 'AttachmentDescription'.
fromCStructAttachmentDescription :: VkAttachmentDescription -> IO AttachmentDescription
fromCStructAttachmentDescription c = AttachmentDescription <$> pure (vkFlags (c :: VkAttachmentDescription))
                                                           <*> pure (vkFormat (c :: VkAttachmentDescription))
                                                           <*> pure (vkSamples (c :: VkAttachmentDescription))
                                                           <*> pure (vkLoadOp (c :: VkAttachmentDescription))
                                                           <*> pure (vkStoreOp (c :: VkAttachmentDescription))
                                                           <*> pure (vkStencilLoadOp (c :: VkAttachmentDescription))
                                                           <*> pure (vkStencilStoreOp (c :: VkAttachmentDescription))
                                                           <*> pure (vkInitialLayout (c :: VkAttachmentDescription))
                                                           <*> pure (vkFinalLayout (c :: VkAttachmentDescription))

instance Zero AttachmentDescription where
  zero = AttachmentDescription zero
                               zero
                               zero
                               zero
                               zero
                               zero
                               zero
                               zero
                               zero


-- | VkAttachmentDescriptionFlagBits - Bitmask specifying additional
-- properties of an attachment
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescriptionFlags'
type AttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
-- specifies that the attachment aliases the same device memory as other
-- attachments.
pattern ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT :: (a ~ AttachmentDescriptionFlagBits) => a
pattern ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT

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


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_LOAD' specifies
-- that the previous contents of the image within the render area will be
-- preserved. For attachments with a depth\/stencil format, this uses the
-- access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT'.
-- For attachments with a color format, this uses the access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT'.
pattern ATTACHMENT_LOAD_OP_LOAD :: (a ~ AttachmentLoadOp) => a
pattern ATTACHMENT_LOAD_OP_LOAD = VK_ATTACHMENT_LOAD_OP_LOAD


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR' specifies
-- that the contents within the render area will be cleared to a uniform
-- value, which is specified when a render pass instance is begun. For
-- attachments with a depth\/stencil format, this uses the access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'.
-- For attachments with a color format, this uses the access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern ATTACHMENT_LOAD_OP_CLEAR :: (a ~ AttachmentLoadOp) => a
pattern ATTACHMENT_LOAD_OP_CLEAR = VK_ATTACHMENT_LOAD_OP_CLEAR


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_DONT_CARE'
-- specifies that the previous contents within the area need not be
-- preserved; the contents of the attachment will be undefined inside the
-- render area. For attachments with a depth\/stencil format, this uses the
-- access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'.
-- For attachments with a color format, this uses the access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern ATTACHMENT_LOAD_OP_DONT_CARE :: (a ~ AttachmentLoadOp) => a
pattern ATTACHMENT_LOAD_OP_DONT_CARE = VK_ATTACHMENT_LOAD_OP_DONT_CARE


-- | VkAttachmentReference - Structure specifying an attachment reference
--
-- == Valid Usage
--
-- -   If @attachment@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', @layout@
--     /must/ not be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED'
--
-- Unresolved directive in VkAttachmentReference.txt -
-- include::{generated}\/validity\/structs\/VkAttachmentReference.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription'
data AttachmentReference = AttachmentReference
  { -- No documentation found for Nested "AttachmentReference" "attachment"
  attachment :: Word32
  , -- No documentation found for Nested "AttachmentReference" "layout"
  layout :: ImageLayout
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAttachmentReference' and
-- marshal a 'AttachmentReference' into it. The 'VkAttachmentReference' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAttachmentReference :: AttachmentReference -> (VkAttachmentReference -> IO a) -> IO a
withCStructAttachmentReference marshalled cont = cont (VkAttachmentReference (attachment (marshalled :: AttachmentReference)) (layout (marshalled :: AttachmentReference)))

-- | A function to read a 'VkAttachmentReference' and all additional
-- structures in the pointer chain into a 'AttachmentReference'.
fromCStructAttachmentReference :: VkAttachmentReference -> IO AttachmentReference
fromCStructAttachmentReference c = AttachmentReference <$> pure (vkAttachment (c :: VkAttachmentReference))
                                                       <*> pure (vkLayout (c :: VkAttachmentReference))

instance Zero AttachmentReference where
  zero = AttachmentReference zero
                             zero


-- | VkAttachmentStoreOp - Specify how contents of an attachment are treated
-- at the end of a subpass
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription'
type AttachmentStoreOp = VkAttachmentStoreOp


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_STORE_OP_STORE' specifies
-- the contents generated during the render pass and within the render area
-- are written to memory. For attachments with a depth\/stencil format,
-- this uses the access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'.
-- For attachments with a color format, this uses the access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern ATTACHMENT_STORE_OP_STORE :: (a ~ AttachmentStoreOp) => a
pattern ATTACHMENT_STORE_OP_STORE = VK_ATTACHMENT_STORE_OP_STORE


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_STORE_OP_DONT_CARE'
-- specifies the contents within the render area are not needed after
-- rendering, and /may/ be discarded; the contents of the attachment will
-- be undefined inside the render area. For attachments with a
-- depth\/stencil format, this uses the access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'.
-- For attachments with a color format, this uses the access type
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern ATTACHMENT_STORE_OP_DONT_CARE :: (a ~ AttachmentStoreOp) => a
pattern ATTACHMENT_STORE_OP_DONT_CARE = VK_ATTACHMENT_STORE_OP_DONT_CARE

-- | VkDependencyFlagBits - Bitmask specifying how execution and memory
-- dependencies are formed
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlags'
type DependencyFlagBits = VkDependencyFlagBits


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_DEPENDENCY_BY_REGION_BIT' specifies
-- that dependencies will be
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local>.
pattern DEPENDENCY_BY_REGION_BIT :: (a ~ DependencyFlagBits) => a
pattern DEPENDENCY_BY_REGION_BIT = VK_DEPENDENCY_BY_REGION_BIT

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


-- | VkFramebufferCreateInfo - Structure specifying parameters of a newly
-- created framebuffer
--
-- = Description
--
-- Applications /must/ ensure that all accesses to memory that backs image
-- subresources used as attachments in a given renderpass instance either
-- happen-before the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops load operations>
-- for those attachments, or happen-after the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops store operations>
-- for those attachments.
--
-- For depth\/stencil attachments, each aspect /can/ be used separately as
-- attachments and non-attachments as long as the non-attachment accesses
-- are also via an image subresource in either the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
-- layout or the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
-- layout, and the attachment resource uses whichever of those two layouts
-- the image accesses do not. Use of non-attachment aspects in this case is
-- only well defined if the attachment is used in the subpass where the
-- non-attachment access is being made, or the layout of the image
-- subresource is constant throughout the entire render pass instance,
-- including the @initialLayout@ and @finalLayout@.
--
-- __Note__
--
-- These restrictions mean that the render pass has full knowledge of all
-- uses of all of the attachments, so that the implementation is able to
-- make correct decisions about when and how to perform layout transitions,
-- when to overlap execution of subpasses, etc.
--
-- It is legal for a subpass to use no color or depth\/stencil attachments,
-- and rather use shader side effects such as image stores and atomics to
-- produce an output. In this case, the subpass continues to use the
-- @width@, @height@, and @layers@ of the framebuffer to define the
-- dimensions of the rendering area, and the @rasterizationSamples@ from
-- each pipeline’s
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'
-- to define the number of samples used in rasterization; however, if
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceFeatures'::@variableMultisampleRate@
-- is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then all pipelines to be
-- bound with a given zero-attachment subpass /must/ have the same value
-- for
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'::@rasterizationSamples@.
--
-- == Valid Usage
--
-- -   @attachmentCount@ /must/ be equal to the attachment count specified
--     in @renderPass@
--
-- -   Each element of @pAttachments@ that is used as a color attachment or
--     resolve attachment by @renderPass@ /must/ have been created with a
--     @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   Each element of @pAttachments@ that is used as a depth\/stencil
--     attachment by @renderPass@ /must/ have been created with a @usage@
--     value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   Each element of @pAttachments@ that is used as a depth\/stencil
--     resolve attachment by @renderPass@ /must/ have been created with a
--     @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   Each element of @pAttachments@ that is used as an input attachment
--     by @renderPass@ /must/ have been created with a @usage@ value
--     including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   Each element of @pAttachments@ that is used as a fragment density
--     map attachment by @renderPass@ /must/ not have been created with a
--     @flags@ value including
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT'.
--
-- -   If @renderPass@ has a fragment density map attachment and
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-nonsubsampledimages non-subsample image feature>
--     is not enabled, each element of @pAttachments@ /must/ have been
--     created with a @flags@ value including
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--     unless that element is the fragment density map attachment.
--
-- -   Each element of @pAttachments@ /must/ have been created with an
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' value that matches the
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' specified by the
--     corresponding
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' in
--     @renderPass@
--
-- -   Each element of @pAttachments@ /must/ have been created with a
--     @samples@ value that matches the @samples@ value specified by the
--     corresponding
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' in
--     @renderPass@
--
-- -   Each element of @pAttachments@ /must/ have dimensions at least as
--     large as the corresponding framebuffer dimension except for any
--     element that is referenced by @fragmentDensityMapAttachment@
--
-- -   An element of @pAttachments@ that is referenced by
--     @fragmentDensityMapAttachment@ /must/ have a width at least as large
--     as
--     \(\lceil{\frac{width}{maxFragmentDensityTexelSize_{width}}}\rceil\)
--
-- -   An element of @pAttachments@ that is referenced by
--     @fragmentDensityMapAttachment@ /must/ have a height at least as
--     large as
--     \(\lceil{\frac{height}{maxFragmentDensityTexelSize_{height}}}\rceil\)
--
-- -   Each element of @pAttachments@ /must/ only specify a single mip
--     level
--
-- -   Each element of @pAttachments@ /must/ have been created with the
--     identity swizzle
--
-- -   @width@ /must/ be greater than @0@.
--
-- -   @width@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxFramebufferWidth@
--
-- -   @height@ /must/ be greater than @0@.
--
-- -   @height@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxFramebufferHeight@
--
-- -   @layers@ /must/ be greater than @0@.
--
-- -   @layers@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxFramebufferLayers@
--
-- -   Each element of @pAttachments@ that is a 2D or 2D array image view
--     taken from a 3D image /must/ not be a depth\/stencil format
--
-- -   If @renderPass@ was specified with non-zero view masks, @layers@
--     /must/ be greater than or equal to the greatest position of any bit
--     included in any of those view masks
--
-- Unresolved directive in VkFramebufferCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkFramebufferCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateFlags',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateFramebuffer'
data FramebufferCreateInfo = FramebufferCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "FramebufferCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FramebufferCreateInfo" "flags"
  flags :: FramebufferCreateFlags
  , -- No documentation found for Nested "FramebufferCreateInfo" "renderPass"
  renderPass :: RenderPass
  -- Length valued member elided
  , -- No documentation found for Nested "FramebufferCreateInfo" "pAttachments"
  attachments :: Vector ImageView
  , -- No documentation found for Nested "FramebufferCreateInfo" "width"
  width :: Word32
  , -- No documentation found for Nested "FramebufferCreateInfo" "height"
  height :: Word32
  , -- No documentation found for Nested "FramebufferCreateInfo" "layers"
  layers :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkFramebufferCreateInfo' and
-- marshal a 'FramebufferCreateInfo' into it. The 'VkFramebufferCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructFramebufferCreateInfo :: FramebufferCreateInfo -> (VkFramebufferCreateInfo -> IO a) -> IO a
withCStructFramebufferCreateInfo marshalled cont = withVec (&) (attachments (marshalled :: FramebufferCreateInfo)) (\pPAttachments -> maybeWith withSomeVkStruct (next (marshalled :: FramebufferCreateInfo)) (\pPNext -> cont (VkFramebufferCreateInfo VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO pPNext (flags (marshalled :: FramebufferCreateInfo)) (renderPass (marshalled :: FramebufferCreateInfo)) (fromIntegral (Data.Vector.length (attachments (marshalled :: FramebufferCreateInfo)))) pPAttachments (width (marshalled :: FramebufferCreateInfo)) (height (marshalled :: FramebufferCreateInfo)) (layers (marshalled :: FramebufferCreateInfo)))))

-- | A function to read a 'VkFramebufferCreateInfo' and all additional
-- structures in the pointer chain into a 'FramebufferCreateInfo'.
fromCStructFramebufferCreateInfo :: VkFramebufferCreateInfo -> IO FramebufferCreateInfo
fromCStructFramebufferCreateInfo c = FramebufferCreateInfo <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFramebufferCreateInfo)))
                                                           <*> pure (vkFlags (c :: VkFramebufferCreateInfo))
                                                           <*> pure (vkRenderPass (c :: VkFramebufferCreateInfo))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkAttachmentCount (c :: VkFramebufferCreateInfo))) (peekElemOff (vkPAttachments (c :: VkFramebufferCreateInfo))))
                                                           <*> pure (vkWidth (c :: VkFramebufferCreateInfo))
                                                           <*> pure (vkHeight (c :: VkFramebufferCreateInfo))
                                                           <*> pure (vkLayers (c :: VkFramebufferCreateInfo))

instance Zero FramebufferCreateInfo where
  zero = FramebufferCreateInfo Nothing
                               zero
                               zero
                               Data.Vector.empty
                               zero
                               zero
                               zero


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


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
-- specifies binding as a graphics pipeline.
pattern PIPELINE_BIND_POINT_GRAPHICS :: (a ~ PipelineBindPoint) => a
pattern PIPELINE_BIND_POINT_GRAPHICS = VK_PIPELINE_BIND_POINT_GRAPHICS


-- | 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_COMPUTE' specifies
-- binding as a compute pipeline.
pattern PIPELINE_BIND_POINT_COMPUTE :: (a ~ PipelineBindPoint) => a
pattern PIPELINE_BIND_POINT_COMPUTE = VK_PIPELINE_BIND_POINT_COMPUTE

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


-- | VkRenderPassCreateInfo - Structure specifying parameters of a newly
-- created render pass
--
-- = Description
--
-- __Note__
--
-- Care should be taken to avoid a data race here; if any subpasses access
-- attachments with overlapping memory locations, and one of those accesses
-- is a write, a subpass dependency needs to be included between them.
--
-- == Valid Usage
--
-- -   If the @attachment@ member of any element of @pInputAttachments@,
--     @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@, or any element of @pPreserveAttachments@
--     in any element of @pSubpasses@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', it /must/
--     be less than @attachmentCount@
--
-- -   For any member of @pAttachments@ with a @loadOp@ equal to
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'.
--
-- -   For any member of @pAttachments@ with a @stencilLoadOp@ equal to
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'.
--
-- -   For any member of @pAttachments@ with a @loadOp@ equal to
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'.
--
-- -   For any member of @pAttachments@ with a @stencilLoadOp@ equal to
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'.
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo',
--     the @subpass@ member of each element of its @pAspectReferences@
--     member /must/ be less than @subpassCount@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo',
--     the @inputAttachmentIndex@ member of each element of its
--     @pAspectReferences@ member /must/ be less than the value of
--     @inputAttachmentCount@ in the member of @pSubpasses@ identified by
--     its @subpass@ member
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo',
--     for any element of the @pInputAttachments@ member of any element of
--     @pSubpasses@ where the @attachment@ member is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', the
--     @aspectMask@ member of the corresponding element of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo'::@pAspectReferences@
--     /must/ only include aspects that are present in images of the format
--     specified by the element of @pAttachments@ at @attachment@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
--     and its @subpassCount@ member is not zero, that member /must/ be
--     equal to the value of @subpassCount@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
--     if its @dependencyCount@ member is not zero, it /must/ be equal to
--     @dependencyCount@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
--     for each non-zero element of @pViewOffsets@, the @srcSubpass@ and
--     @dstSubpass@ members of @pDependencies@ at the same index /must/ not
--     be equal
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
--     for any element of @pDependencies@ with a @dependencyFlags@ member
--     that does not include
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT',
--     the corresponding element of the @pViewOffsets@ member of that
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo'
--     instance /must/ be @0@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
--     elements of its @pViewMasks@ member /must/ either all be @0@, or all
--     not be @0@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
--     and each element of its @pViewMasks@ member is @0@, the
--     @dependencyFlags@ member of each element of @pDependencies@ /must/
--     not include
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
--     and each element of its @pViewMasks@ member is @0@,
--     @correlatedViewMaskCount@ /must/ be @0@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
--     each element of its @pViewMask@ member /must/ not include a bit at a
--     position greater than the value of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxFramebufferLayers@
--
-- -   For any element of @pDependencies@, if the @srcSubpass@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL', all stage
--     flags included in the @srcStageMask@ member of that dependency
--     /must/ be a pipeline stage supported by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the source subpass
--
-- -   For any element of @pDependencies@, if the @dstSubpass@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL', all stage
--     flags included in the @dstStageMask@ member of that dependency
--     /must/ be a pipeline stage supported by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the source subpass
--
-- -   The @srcSubpass@ member of each element of @pDependencies@ /must/ be
--     less than @subpassCount@
--
-- -   The @dstSubpass@ member of each element of @pDependencies@ /must/ be
--     less than @subpassCount@
--
-- Unresolved directive in VkRenderPassCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkRenderPassCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateRenderPass'
data RenderPassCreateInfo = RenderPassCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "RenderPassCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassCreateInfo" "flags"
  flags :: RenderPassCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo" "pAttachments"
  attachments :: Vector AttachmentDescription
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo" "pSubpasses"
  subpasses :: Vector SubpassDescription
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo" "pDependencies"
  dependencies :: Vector SubpassDependency
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRenderPassCreateInfo' and
-- marshal a 'RenderPassCreateInfo' into it. The 'VkRenderPassCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRenderPassCreateInfo :: RenderPassCreateInfo -> (VkRenderPassCreateInfo -> IO a) -> IO a
withCStructRenderPassCreateInfo marshalled cont = withVec withCStructSubpassDependency (dependencies (marshalled :: RenderPassCreateInfo)) (\pPDependencies -> withVec withCStructSubpassDescription (subpasses (marshalled :: RenderPassCreateInfo)) (\pPSubpasses -> withVec withCStructAttachmentDescription (attachments (marshalled :: RenderPassCreateInfo)) (\pPAttachments -> maybeWith withSomeVkStruct (next (marshalled :: RenderPassCreateInfo)) (\pPNext -> cont (VkRenderPassCreateInfo VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO pPNext (flags (marshalled :: RenderPassCreateInfo)) (fromIntegral (Data.Vector.length (attachments (marshalled :: RenderPassCreateInfo)))) pPAttachments (fromIntegral (Data.Vector.length (subpasses (marshalled :: RenderPassCreateInfo)))) pPSubpasses (fromIntegral (Data.Vector.length (dependencies (marshalled :: RenderPassCreateInfo)))) pPDependencies)))))

-- | A function to read a 'VkRenderPassCreateInfo' and all additional
-- structures in the pointer chain into a 'RenderPassCreateInfo'.
fromCStructRenderPassCreateInfo :: VkRenderPassCreateInfo -> IO RenderPassCreateInfo
fromCStructRenderPassCreateInfo c = RenderPassCreateInfo <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassCreateInfo)))
                                                         <*> pure (vkFlags (c :: VkRenderPassCreateInfo))
                                                         -- Length valued member elided
                                                         <*> (Data.Vector.generateM (fromIntegral (vkAttachmentCount (c :: VkRenderPassCreateInfo))) (((fromCStructAttachmentDescription <=<) . peekElemOff) (vkPAttachments (c :: VkRenderPassCreateInfo))))
                                                         -- Length valued member elided
                                                         <*> (Data.Vector.generateM (fromIntegral (vkSubpassCount (c :: VkRenderPassCreateInfo))) (((fromCStructSubpassDescription <=<) . peekElemOff) (vkPSubpasses (c :: VkRenderPassCreateInfo))))
                                                         -- Length valued member elided
                                                         <*> (Data.Vector.generateM (fromIntegral (vkDependencyCount (c :: VkRenderPassCreateInfo))) (((fromCStructSubpassDependency <=<) . peekElemOff) (vkPDependencies (c :: VkRenderPassCreateInfo))))

instance Zero RenderPassCreateInfo where
  zero = RenderPassCreateInfo Nothing
                              zero
                              Data.Vector.empty
                              Data.Vector.empty
                              Data.Vector.empty



-- | VkSubpassDependency - Structure specifying a subpass dependency
--
-- = Description
--
-- If @srcSubpass@ is equal to @dstSubpass@ then the
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency' describes a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-barriers-subpass-self-dependencies subpass self-dependency>,
-- and only constrains the pipeline barriers allowed within a subpass
-- instance. Otherwise, when a render pass instance which includes a
-- subpass dependency is submitted to a queue, it defines a memory
-- dependency between the subpasses identified by @srcSubpass@ and
-- @dstSubpass@.
--
-- If @srcSubpass@ is equal to
-- 'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL', the first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- than the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass'
-- used to begin the render pass instance. Otherwise, the first set of
-- commands includes all commands submitted as part of the subpass instance
-- identified by @srcSubpass@ and any load, store or multisample resolve
-- operations on attachments used in @srcSubpass@. In either case, the
-- first synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@.
--
-- If @dstSubpass@ is equal to
-- 'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL', the second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes commands that occur later in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- than the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndRenderPass' used
-- to end the render pass instance. Otherwise, the second set of commands
-- includes all commands submitted as part of the subpass instance
-- identified by @dstSubpass@ and any load, store or multisample resolve
-- operations on attachments used in @dstSubpass@. In either case, the
-- second synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. It is also limited to access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. It is also limited to access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@.
--
-- The
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-available-and-visible availability and visibility operations>
-- defined by a subpass dependency affect the execution of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-layout-transitions image layout transitions>
-- within the render pass.
--
-- __Note__
--
-- For non-attachment resources, the memory dependency expressed by subpass
-- dependency is nearly identical to that of a
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier' (with
-- matching @srcAccessMask@\/@dstAccessMask@ parameters) submitted as a
-- part of a
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
-- (with matching @srcStageMask@\/@dstStageMask@ parameters). The only
-- difference being that its scopes are limited to the identified subpasses
-- rather than potentially affecting everything before and after.
--
-- For attachments however, subpass dependencies work more like a
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier'
-- defined similarly to the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier' above,
-- the queue family indices set to
-- 'Graphics.Vulkan.C.Core10.Constants.VK_QUEUE_FAMILY_IGNORED', and
-- layouts as follows:
--
-- -   The equivalent to @oldLayout@ is the attachment’s layout according
--     to the subpass description for @srcSubpass@.
--
-- -   The equivalent to @newLayout@ is the attachment’s layout according
--     to the subpass description for @dstSubpass@.
--
-- == Valid Usage
--
-- -   If @srcSubpass@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL',
--     @srcStageMask@ /must/ not include
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT'
--
-- -   If @dstSubpass@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL',
--     @dstStageMask@ /must/ not include
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   @srcSubpass@ /must/ be less than or equal to @dstSubpass@, unless
--     one of them is
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL', to avoid
--     cyclic dependencies and ensure a valid execution order
--
-- -   @srcSubpass@ and @dstSubpass@ /must/ not both be equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'
--
-- -   If @srcSubpass@ is equal to @dstSubpass@, @srcStageMask@ and
--     @dstStageMask@ /must/ not set any bits that are neither
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT',
--     nor one of the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-types graphics pipeline stages>
--
-- -   If @srcSubpass@ is equal to @dstSubpass@ and not all of the stages
--     in @srcStageMask@ and @dstStageMask@ are
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stages>,
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
--     pipeline stage in @srcStageMask@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>
--     than or equal to the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earliest>
--     pipeline stage in @dstStageMask@
--
-- -   Any access flag included in @srcAccessMask@ /must/ be supported by
--     one of the pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   Any access flag included in @dstAccessMask@ /must/ be supported by
--     one of the pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   If @srcSubpass@ equals @dstSubpass@, and @srcStageMask@ and
--     @dstStageMask@ both include a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stage>,
--     then @dependencyFlags@ /must/ include
--     'Graphics.Vulkan.C.Core10.Pass.VK_DEPENDENCY_BY_REGION_BIT'
--
-- -   If @dependencyFlags@ includes
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT',
--     @srcSubpass@ /must/ not be equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'
--
-- -   If @dependencyFlags@ includes
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT',
--     @dstSubpass@ /must/ not be equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'
--
-- -   If @srcSubpass@ equals @dstSubpass@ and that subpass has more than
--     one bit set in the view mask, then @dependencyFlags@ /must/ include
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- Unresolved directive in VkSubpassDependency.txt -
-- include::{generated}\/validity\/structs\/VkSubpassDependency.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlags',
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags',
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo'
data SubpassDependency = SubpassDependency
  { -- No documentation found for Nested "SubpassDependency" "srcSubpass"
  srcSubpass :: Word32
  , -- No documentation found for Nested "SubpassDependency" "dstSubpass"
  dstSubpass :: Word32
  , -- No documentation found for Nested "SubpassDependency" "srcStageMask"
  srcStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "SubpassDependency" "dstStageMask"
  dstStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "SubpassDependency" "srcAccessMask"
  srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "SubpassDependency" "dstAccessMask"
  dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "SubpassDependency" "dependencyFlags"
  dependencyFlags :: DependencyFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubpassDependency' and
-- marshal a 'SubpassDependency' into it. The 'VkSubpassDependency' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubpassDependency :: SubpassDependency -> (VkSubpassDependency -> IO a) -> IO a
withCStructSubpassDependency marshalled cont = cont (VkSubpassDependency (srcSubpass (marshalled :: SubpassDependency)) (dstSubpass (marshalled :: SubpassDependency)) (srcStageMask (marshalled :: SubpassDependency)) (dstStageMask (marshalled :: SubpassDependency)) (srcAccessMask (marshalled :: SubpassDependency)) (dstAccessMask (marshalled :: SubpassDependency)) (dependencyFlags (marshalled :: SubpassDependency)))

-- | A function to read a 'VkSubpassDependency' and all additional
-- structures in the pointer chain into a 'SubpassDependency'.
fromCStructSubpassDependency :: VkSubpassDependency -> IO SubpassDependency
fromCStructSubpassDependency c = SubpassDependency <$> pure (vkSrcSubpass (c :: VkSubpassDependency))
                                                   <*> pure (vkDstSubpass (c :: VkSubpassDependency))
                                                   <*> pure (vkSrcStageMask (c :: VkSubpassDependency))
                                                   <*> pure (vkDstStageMask (c :: VkSubpassDependency))
                                                   <*> pure (vkSrcAccessMask (c :: VkSubpassDependency))
                                                   <*> pure (vkDstAccessMask (c :: VkSubpassDependency))
                                                   <*> pure (vkDependencyFlags (c :: VkSubpassDependency))

instance Zero SubpassDependency where
  zero = SubpassDependency zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero



-- | VkSubpassDescription - Structure specifying a subpass description
--
-- = Description
--
-- Each element of the @pInputAttachments@ array corresponds to an input
-- attachment index in a fragment shader, i.e. if a shader declares an
-- image variable decorated with a @InputAttachmentIndex@ value of __X__,
-- then it uses the attachment provided in @pInputAttachments@[__X__].
-- Input attachments /must/ also be bound to the pipeline in a descriptor
-- set. If the @attachment@ member of any element of @pInputAttachments@ is
-- 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', the
-- application /must/ not read from the corresponding input attachment
-- index. Fragment shaders /can/ use subpass input variables to access the
-- contents of an input attachment at the fragment’s (x, y, layer)
-- framebuffer coordinates.
--
-- Each element of the @pColorAttachments@ array corresponds to an output
-- location in the shader, i.e. if the shader declares an output variable
-- decorated with a @Location@ value of __X__, then it uses the attachment
-- provided in @pColorAttachments@[__X__]. If the @attachment@ member of
-- any element of @pColorAttachments@ is
-- 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', writes to the
-- corresponding location by a fragment are discarded.
--
-- If @pResolveAttachments@ is not @NULL@, each of its elements corresponds
-- to a color attachment (the element in @pColorAttachments@ at the same
-- index), and a multisample resolve operation is defined for each
-- attachment. At the end of each subpass, multisample resolve operations
-- read the subpass’s color attachments, and resolve the samples for each
-- pixel to the same pixel location in the corresponding resolve
-- attachments, unless the resolve attachment index is
-- 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'.
--
-- Similarly, if
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkSubpassDescriptionDepthStencilResolveKHR'::@pDepthStencilResolveAttachment@
-- is not @NULL@ and does not have the value
-- 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', it
-- corresponds to the depth\/stencil attachment in
-- @pDepthStencilAttachment@, and multisample resolve operations for depth
-- and stencil are defined by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkSubpassDescriptionDepthStencilResolveKHR'::@depthResolveMode@
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkSubpassDescriptionDepthStencilResolveKHR'::@stencilResolveMode@,
-- respectively. At the end of each subpass, multisample resolve operations
-- read the subpass’s depth\/stencil attachment, and resolve the samples
-- for each pixel to the same pixel location in the corresponding resolve
-- attachment. If
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkSubpassDescriptionDepthStencilResolveKHR'::@depthResolveMode@
-- is
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_NONE_KHR',
-- then the depth component of the resolve attachment is not written to and
-- its contents are preserved. Similarly, if
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkSubpassDescriptionDepthStencilResolveKHR'::@stencilResolveMode@
-- is
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_NONE_KHR',
-- then the stencil component of the resolve attachment is not written to
-- and its contents are preserved.
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkSubpassDescriptionDepthStencilResolveKHR'::@depthResolveMode@
-- is ignored if the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of the
-- @pDepthStencilResolveAttachment@ does not have a depth component.
-- Similarly,
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkSubpassDescriptionDepthStencilResolveKHR'::@stencilResolveMode@
-- is ignored if the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of the
-- @pDepthStencilResolveAttachment@ does not have a stencil component.
--
-- If the image subresource range referenced by the depth\/stencil
-- attachment is created with
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT',
-- then the multisample resolve operation uses the sample locations state
-- specified in the @sampleLocationsInfo@ member of the element of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkRenderPassSampleLocationsBeginInfoEXT'::@pPostSubpassSampleLocations@
-- for the subpass.
--
-- If @pDepthStencilAttachment@ is @NULL@, or if its attachment index is
-- 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', it indicates
-- that no depth\/stencil attachment will be used in the subpass.
--
-- The contents of an attachment within the render area become undefined at
-- the start of a subpass __S__ if all of the following conditions are
-- true:
--
-- -   The attachment is used as a color, depth\/stencil, or resolve
--     attachment in any subpass in the render pass.
--
-- -   There is a subpass __S1__ that uses or preserves the attachment, and
--     a subpass dependency from __S1__ to __S__.
--
-- -   The attachment is not used or preserved in subpass __S__.
--
-- Once the contents of an attachment become undefined in subpass __S__,
-- they remain undefined for subpasses in subpass dependency chains
-- starting with subpass __S__ until they are written again. However, they
-- remain valid for subpasses in other subpass dependency chains starting
-- with subpass __S1__ if those subpasses use or preserve the attachment.
--
-- == Valid Usage
--
-- -   @pipelineBindPoint@ /must/ be
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   @colorAttachmentCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxColorAttachments@
--
-- -   If the first use of an attachment in this render pass is as an input
--     attachment, and the attachment is not also used as a color or
--     depth\/stencil attachment in the same subpass, then @loadOp@ /must/
--     not be 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR'
--
-- -   If @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', the
--     corresponding color attachment /must/ not be
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--
-- -   If @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', the
--     corresponding color attachment /must/ not have a sample count of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   If @pResolveAttachments@ is not @NULL@, each resolve attachment that
--     is not 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--     /must/ have a sample count of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   If @pResolveAttachments@ is not @NULL@, each resolve attachment that
--     is not 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--     /must/ have the same 'Graphics.Vulkan.C.Core10.Core.VkFormat' as its
--     corresponding color attachment
--
-- -   All attachments in @pColorAttachments@ that are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' /must/
--     have the same sample count
--
-- -   All attachments in @pInputAttachments@ that are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' /must/
--     have formats whose features contain at least one of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'.
--
-- -   All attachments in @pColorAttachments@ that are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' /must/
--     have formats whose features contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   All attachments in @pResolveAttachments@ that are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' /must/
--     have formats whose features contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   If @pDepthStencilAttachment@ is not @NULL@ and the attachment is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' then it
--     /must/ have a format whose features contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If the @VK_AMD_mixed_attachment_samples@ extension is enabled, and
--     all attachments in @pColorAttachments@ that are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' /must/
--     have a sample count that is smaller than or equal to the sample
--     count of @pDepthStencilAttachment@ if it is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--
-- -   If neither the @VK_AMD_mixed_attachment_samples@ nor the
--     @VK_NV_framebuffer_mixed_samples@ extensions are enabled, and if
--     @pDepthStencilAttachment@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' and any
--     attachments in @pColorAttachments@ are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', they
--     /must/ have the same sample count
--
-- -   The @attachment@ member of each element of @pPreserveAttachments@
--     /must/ not be
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--
-- -   Each element of @pPreserveAttachments@ /must/ not also be an element
--     of any other member of the subpass description
--
-- -   If any attachment is used by more than one
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' member, then
--     each use /must/ use the same @layout@
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes.VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX',
--     it /must/ also include
--     'Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes.VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'.
--
-- Unresolved directive in VkSubpassDescription.txt -
-- include::{generated}\/validity\/structs\/VkSubpassDescription.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference',
-- 'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint',
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescriptionFlags'
data SubpassDescription = SubpassDescription
  { -- No documentation found for Nested "SubpassDescription" "flags"
  flags :: SubpassDescriptionFlags
  , -- No documentation found for Nested "SubpassDescription" "pipelineBindPoint"
  pipelineBindPoint :: PipelineBindPoint
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription" "pInputAttachments"
  inputAttachments :: Vector AttachmentReference
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription" "pColorAttachments"
  colorAttachments :: Vector AttachmentReference
  , -- No documentation found for Nested "SubpassDescription" "pResolveAttachments"
  resolveAttachments :: Maybe (Vector AttachmentReference)
  , -- No documentation found for Nested "SubpassDescription" "pDepthStencilAttachment"
  depthStencilAttachment :: Maybe AttachmentReference
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription" "pPreserveAttachments"
  preserveAttachments :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubpassDescription' and
-- marshal a 'SubpassDescription' into it. The 'VkSubpassDescription' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubpassDescription :: SubpassDescription -> (VkSubpassDescription -> IO a) -> IO a
withCStructSubpassDescription marshalled cont = withVec (&) (preserveAttachments (marshalled :: SubpassDescription)) (\pPPreserveAttachments -> maybeWith (\a -> withCStructAttachmentReference a . flip with) (depthStencilAttachment (marshalled :: SubpassDescription)) (\pPDepthStencilAttachment -> maybeWith (withVec withCStructAttachmentReference) (resolveAttachments (marshalled :: SubpassDescription)) (\pPResolveAttachments -> withVec withCStructAttachmentReference (colorAttachments (marshalled :: SubpassDescription)) (\pPColorAttachments -> withVec withCStructAttachmentReference (inputAttachments (marshalled :: SubpassDescription)) (\pPInputAttachments -> cont (VkSubpassDescription (flags (marshalled :: SubpassDescription)) (pipelineBindPoint (marshalled :: SubpassDescription)) (fromIntegral (Data.Vector.length (inputAttachments (marshalled :: SubpassDescription)))) pPInputAttachments (fromIntegral (minimum ([Data.Vector.length (colorAttachments (marshalled :: SubpassDescription))] ++ [Data.Vector.length v | Just v <- [(resolveAttachments (marshalled :: SubpassDescription))]]))) pPColorAttachments pPResolveAttachments pPDepthStencilAttachment (fromIntegral (Data.Vector.length (preserveAttachments (marshalled :: SubpassDescription)))) pPPreserveAttachments))))))

-- | A function to read a 'VkSubpassDescription' and all additional
-- structures in the pointer chain into a 'SubpassDescription'.
fromCStructSubpassDescription :: VkSubpassDescription -> IO SubpassDescription
fromCStructSubpassDescription c = SubpassDescription <$> pure (vkFlags (c :: VkSubpassDescription))
                                                     <*> pure (vkPipelineBindPoint (c :: VkSubpassDescription))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkInputAttachmentCount (c :: VkSubpassDescription))) (((fromCStructAttachmentReference <=<) . peekElemOff) (vkPInputAttachments (c :: VkSubpassDescription))))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkColorAttachmentCount (c :: VkSubpassDescription))) (((fromCStructAttachmentReference <=<) . peekElemOff) (vkPColorAttachments (c :: VkSubpassDescription))))
                                                     <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkColorAttachmentCount (c :: VkSubpassDescription))) (((fromCStructAttachmentReference <=<) . peekElemOff) p)) (vkPResolveAttachments (c :: VkSubpassDescription))
                                                     <*> maybePeek (fromCStructAttachmentReference <=< peek) (vkPDepthStencilAttachment (c :: VkSubpassDescription))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkPreserveAttachmentCount (c :: VkSubpassDescription))) (peekElemOff (vkPPreserveAttachments (c :: VkSubpassDescription))))

instance Zero SubpassDescription where
  zero = SubpassDescription zero
                            zero
                            Data.Vector.empty
                            Data.Vector.empty
                            Nothing
                            Nothing
                            Data.Vector.empty


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


-- | vkCreateFramebuffer - Create a new framebuffer object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the framebuffer.
--
-- -   @pCreateInfo@ points to a
--     'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo' structure
--     which describes additional information about framebuffer creation.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pFramebuffer@ points to a
--     'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer' handle in which the
--     resulting framebuffer object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateFramebuffer.txt -
-- include::{generated}\/validity\/protos\/vkCreateFramebuffer.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo'
createFramebuffer :: Device ->  FramebufferCreateInfo ->  Maybe AllocationCallbacks ->  IO (Framebuffer)
createFramebuffer = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pFramebuffer' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructFramebufferCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateFramebuffer commandTable device' pCreateInfo' pAllocator pFramebuffer' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pFramebuffer')))))


-- | vkCreateRenderPass - Create a new render pass object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the render pass.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo' structure
--     that describes the parameters of the render pass.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pRenderPass@ points to a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass' handle in which the
--     resulting render pass object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateRenderPass.txt -
-- include::{generated}\/validity\/protos\/vkCreateRenderPass.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo'
createRenderPass :: Device ->  RenderPassCreateInfo ->  Maybe AllocationCallbacks ->  IO (RenderPass)
createRenderPass = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pRenderPass' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructRenderPassCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateRenderPass commandTable device' pCreateInfo' pAllocator pRenderPass' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pRenderPass')))))


-- | vkDestroyFramebuffer - Destroy a framebuffer object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the framebuffer.
--
-- -   @framebuffer@ is the handle of the framebuffer to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @framebuffer@ /must/ have
--     completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @framebuffer@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @framebuffer@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyFramebuffer.txt -
-- include::{generated}\/validity\/protos\/vkDestroyFramebuffer.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer'
destroyFramebuffer :: Device ->  Framebuffer ->  Maybe AllocationCallbacks ->  IO ()
destroyFramebuffer = \(Device device' commandTable) -> \framebuffer' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyFramebuffer commandTable device' framebuffer' pAllocator *> (pure ()))


-- | vkDestroyRenderPass - Destroy a render pass object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the render pass.
--
-- -   @renderPass@ is the handle of the render pass to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @renderPass@ /must/ have
--     completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @renderPass@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @renderPass@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyRenderPass.txt -
-- include::{generated}\/validity\/protos\/vkDestroyRenderPass.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass'
destroyRenderPass :: Device ->  RenderPass ->  Maybe AllocationCallbacks ->  IO ()
destroyRenderPass = \(Device device' commandTable) -> \renderPass' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyRenderPass commandTable device' renderPass' pAllocator *> (pure ()))


-- | vkGetRenderAreaGranularity - Returns the granularity for optimal render
-- area
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the render pass.
--
-- -   @renderPass@ is a handle to a render pass.
--
-- -   @pGranularity@ points to a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D' structure in which
--     the granularity is returned.
--
-- = Description
--
-- The conditions leading to an optimal @renderArea@ are:
--
-- -   the @offset.x@ member in @renderArea@ is a multiple of the @width@
--     member of the returned
--     'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D' (the horizontal
--     granularity).
--
-- -   the @offset.y@ member in @renderArea@ is a multiple of the @height@
--     of the returned 'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D' (the
--     vertical granularity).
--
-- -   either the @offset.width@ member in @renderArea@ is a multiple of
--     the horizontal granularity or @offset.x@+@offset.width@ is equal to
--     the @width@ of the @framebuffer@ in the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'.
--
-- -   either the @offset.height@ member in @renderArea@ is a multiple of
--     the vertical granularity or @offset.y@+@offset.height@ is equal to
--     the @height@ of the @framebuffer@ in the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'.
--
-- Subpass dependencies are not affected by the render area, and apply to
-- the entire image subresources attached to the framebuffer as specified
-- in the description of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-layout-transitions automatic layout transitions>.
-- Similarly, pipeline barriers are valid even if their effect extends
-- outside the render area.
--
-- Unresolved directive in vkGetRenderAreaGranularity.txt -
-- include::{generated}\/validity\/protos\/vkGetRenderAreaGranularity.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass'
getRenderAreaGranularity :: Device ->  RenderPass ->  IO (Extent2D)
getRenderAreaGranularity = \(Device device' commandTable) -> \renderPass' -> alloca (\pGranularity' -> vkGetRenderAreaGranularity commandTable device' renderPass' pGranularity' *> ((fromCStructExtent2D <=< peek) pGranularity'))

-- | A safe wrapper for 'createFramebuffer' and 'destroyFramebuffer' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withFramebuffer
  :: Device -> FramebufferCreateInfo -> Maybe (AllocationCallbacks) -> (Framebuffer -> IO a) -> IO a
withFramebuffer device framebufferCreateInfo allocationCallbacks = bracket
  (createFramebuffer device framebufferCreateInfo allocationCallbacks)
  (\o -> destroyFramebuffer device o allocationCallbacks)

-- | A safe wrapper for 'createRenderPass' and 'destroyRenderPass' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withRenderPass
  :: Device -> RenderPassCreateInfo -> Maybe (AllocationCallbacks) -> (RenderPass -> IO a) -> IO a
withRenderPass device renderPassCreateInfo allocationCallbacks = bracket
  (createRenderPass device renderPassCreateInfo allocationCallbacks)
  (\o -> destroyRenderPass device o allocationCallbacks)
