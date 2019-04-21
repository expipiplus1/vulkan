{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  , pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT
  , pattern VK_ACCESS_INDEX_READ_BIT
  , pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT
  , pattern VK_ACCESS_UNIFORM_READ_BIT
  , pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
  , pattern VK_ACCESS_SHADER_READ_BIT
  , pattern VK_ACCESS_SHADER_WRITE_BIT
  , pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
  , pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
  , pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
  , pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
  , pattern VK_ACCESS_TRANSFER_READ_BIT
  , pattern VK_ACCESS_TRANSFER_WRITE_BIT
  , pattern VK_ACCESS_HOST_READ_BIT
  , pattern VK_ACCESS_HOST_WRITE_BIT
  , pattern VK_ACCESS_MEMORY_READ_BIT
  , pattern VK_ACCESS_MEMORY_WRITE_BIT
  , VkAccessFlags
  , VkAttachmentDescription(..)
  , VkAttachmentDescriptionFlagBits(..)
  , pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
  , VkAttachmentDescriptionFlags
  , VkAttachmentLoadOp(..)
  , pattern VK_ATTACHMENT_LOAD_OP_LOAD
  , pattern VK_ATTACHMENT_LOAD_OP_CLEAR
  , pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE
  , VkAttachmentReference(..)
  , VkAttachmentStoreOp(..)
  , pattern VK_ATTACHMENT_STORE_OP_STORE
  , pattern VK_ATTACHMENT_STORE_OP_DONT_CARE
  , VkDependencyFlagBits(..)
  , pattern VK_DEPENDENCY_BY_REGION_BIT
  , VkDependencyFlags
  , VkFramebuffer
  , VkFramebufferCreateFlags(..)
  , VkFramebufferCreateInfo(..)
  , VkPipelineBindPoint(..)
  , pattern VK_PIPELINE_BIND_POINT_GRAPHICS
  , pattern VK_PIPELINE_BIND_POINT_COMPUTE
  , VkRenderPassCreateFlags(..)
  , VkRenderPassCreateInfo(..)
  , VkSubpassDependency(..)
  , VkSubpassDescription(..)
  , VkSubpassDescriptionFlagBits(..)
  , VkSubpassDescriptionFlags
  , FN_vkCreateFramebuffer
  , PFN_vkCreateFramebuffer
  , vkCreateFramebuffer
  , FN_vkCreateRenderPass
  , PFN_vkCreateRenderPass
  , vkCreateRenderPass
  , FN_vkDestroyFramebuffer
  , PFN_vkDestroyFramebuffer
  , vkDestroyFramebuffer
  , FN_vkDestroyRenderPass
  , PFN_vkDestroyRenderPass
  , vkDestroyRenderPass
  , FN_vkGetRenderAreaGranularity
  , PFN_vkGetRenderAreaGranularity
  , vkGetRenderAreaGranularity
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
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
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkSampleCountFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageView
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D(..)
  , VkRenderPass
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlags
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkAccessFlagBits

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
-- > | 'VK_ACCESS_INDIRECT_COMMAND_READ_ | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | BIT'                              | K_PIPELINE_STAGE_DRAW_INDIRECT_BI |
-- > |                                   | T'                                |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_INDEX_READ_BIT'        | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_VERTEX_INPUT_BIT |
-- > |                                   | '                                 |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_VERTEX_ATTRIBUTE_READ_ | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | BIT'                              | K_PIPELINE_STAGE_VERTEX_INPUT_BIT |
-- > |                                   | '                                 |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_UNIFORM_READ_BIT'      | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_mesh_shader.VK_PIPELINE_STAGE_ |
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
-- > | 'VK_ACCESS_SHADER_READ_BIT'       | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_mesh_shader.VK_PIPELINE_STAGE_ |
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
-- > | 'VK_ACCESS_SHADER_WRITE_BIT'      | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > |                                   | NV_mesh_shader.VK_PIPELINE_STAGE_ |
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
-- > | 'VK_ACCESS_INPUT_ATTACHMENT_READ_ | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | BIT'                              | K_PIPELINE_STAGE_FRAGMENT_SHADER_ |
-- > |                                   | BIT'                              |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_COLOR_ATTACHMENT_READ_ | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | BIT'                              | K_PIPELINE_STAGE_COLOR_ATTACHMENT |
-- > |                                   | _OUTPUT_BIT'                      |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_COLOR_ATTACHMENT_WRITE | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | _BIT'                             | K_PIPELINE_STAGE_COLOR_ATTACHMENT |
-- > |                                   | _OUTPUT_BIT'                      |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_DEPTH_STENCIL_ATTACHME | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | NT_READ_BIT'                      | K_PIPELINE_STAGE_EARLY_FRAGMENT_T |
-- > |                                   | ESTS_BIT',                        |
-- > |                                   | or                                |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_LATE_FRAGMENT_TE |
-- > |                                   | STS_BIT'                          |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_DEPTH_STENCIL_ATTACHME | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > | NT_WRITE_BIT'                     | K_PIPELINE_STAGE_EARLY_FRAGMENT_T |
-- > |                                   | ESTS_BIT',                        |
-- > |                                   | or                                |
-- > |                                   | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_LATE_FRAGMENT_TE |
-- > |                                   | STS_BIT'                          |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_TRANSFER_READ_BIT'     | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_TRANSFER_BIT'    |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_TRANSFER_WRITE_BIT'    | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_TRANSFER_BIT'    |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_HOST_READ_BIT'         | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_HOST_BIT'        |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_HOST_WRITE_BIT'        | 'Graphics.Vulkan.C.Core10.Queue.V |
-- > |                                   | K_PIPELINE_STAGE_HOST_BIT'        |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_MEMORY_READ_BIT'       | N\/A                              |
-- > +-----------------------------------+-----------------------------------+
-- > | 'VK_ACCESS_MEMORY_WRITE_BIT'      | N\/A                              |
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
-- 'VkAccessFlags'
newtype VkAccessFlagBits = VkAccessFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkAccessFlagBits where
  showsPrec _ VK_ACCESS_INDIRECT_COMMAND_READ_BIT = showString "VK_ACCESS_INDIRECT_COMMAND_READ_BIT"
  showsPrec _ VK_ACCESS_INDEX_READ_BIT = showString "VK_ACCESS_INDEX_READ_BIT"
  showsPrec _ VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = showString "VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT"
  showsPrec _ VK_ACCESS_UNIFORM_READ_BIT = showString "VK_ACCESS_UNIFORM_READ_BIT"
  showsPrec _ VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = showString "VK_ACCESS_INPUT_ATTACHMENT_READ_BIT"
  showsPrec _ VK_ACCESS_SHADER_READ_BIT = showString "VK_ACCESS_SHADER_READ_BIT"
  showsPrec _ VK_ACCESS_SHADER_WRITE_BIT = showString "VK_ACCESS_SHADER_WRITE_BIT"
  showsPrec _ VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = showString "VK_ACCESS_COLOR_ATTACHMENT_READ_BIT"
  showsPrec _ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = showString "VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT"
  showsPrec _ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = showString "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT"
  showsPrec _ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = showString "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT"
  showsPrec _ VK_ACCESS_TRANSFER_READ_BIT = showString "VK_ACCESS_TRANSFER_READ_BIT"
  showsPrec _ VK_ACCESS_TRANSFER_WRITE_BIT = showString "VK_ACCESS_TRANSFER_WRITE_BIT"
  showsPrec _ VK_ACCESS_HOST_READ_BIT = showString "VK_ACCESS_HOST_READ_BIT"
  showsPrec _ VK_ACCESS_HOST_WRITE_BIT = showString "VK_ACCESS_HOST_WRITE_BIT"
  showsPrec _ VK_ACCESS_MEMORY_READ_BIT = showString "VK_ACCESS_MEMORY_READ_BIT"
  showsPrec _ VK_ACCESS_MEMORY_WRITE_BIT = showString "VK_ACCESS_MEMORY_WRITE_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkAccessFlagBits 0x40000000) = showString "VK_ACCESS_RESERVED_30_BIT_KHR"
  showsPrec _ (VkAccessFlagBits 0x80000000) = showString "VK_ACCESS_RESERVED_31_BIT_KHR"
  showsPrec _ (VkAccessFlagBits 0x10000000) = showString "VK_ACCESS_RESERVED_28_BIT_KHR"
  showsPrec _ (VkAccessFlagBits 0x20000000) = showString "VK_ACCESS_RESERVED_29_BIT_KHR"
  showsPrec _ (VkAccessFlagBits 0x02000000) = showString "VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x04000000) = showString "VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x08000000) = showString "VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x00100000) = showString "VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x00020000) = showString "VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX"
  showsPrec _ (VkAccessFlagBits 0x00040000) = showString "VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX"
  showsPrec _ (VkAccessFlagBits 0x00080000) = showString "VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x00800000) = showString "VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV"
  showsPrec _ (VkAccessFlagBits 0x00200000) = showString "VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV"
  showsPrec _ (VkAccessFlagBits 0x00400000) = showString "VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV"
  showsPrec _ (VkAccessFlagBits 0x01000000) = showString "VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT"
  showsPrec p (VkAccessFlagBits x) = showParen (p >= 11) (showString "VkAccessFlagBits " . showsPrec 11 x)

instance Read VkAccessFlagBits where
  readPrec = parens ( choose [ ("VK_ACCESS_INDIRECT_COMMAND_READ_BIT",          pure VK_ACCESS_INDIRECT_COMMAND_READ_BIT)
                             , ("VK_ACCESS_INDEX_READ_BIT",                     pure VK_ACCESS_INDEX_READ_BIT)
                             , ("VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT",          pure VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT)
                             , ("VK_ACCESS_UNIFORM_READ_BIT",                   pure VK_ACCESS_UNIFORM_READ_BIT)
                             , ("VK_ACCESS_INPUT_ATTACHMENT_READ_BIT",          pure VK_ACCESS_INPUT_ATTACHMENT_READ_BIT)
                             , ("VK_ACCESS_SHADER_READ_BIT",                    pure VK_ACCESS_SHADER_READ_BIT)
                             , ("VK_ACCESS_SHADER_WRITE_BIT",                   pure VK_ACCESS_SHADER_WRITE_BIT)
                             , ("VK_ACCESS_COLOR_ATTACHMENT_READ_BIT",          pure VK_ACCESS_COLOR_ATTACHMENT_READ_BIT)
                             , ("VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT",         pure VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                             , ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT",  pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT)
                             , ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT", pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)
                             , ("VK_ACCESS_TRANSFER_READ_BIT",                  pure VK_ACCESS_TRANSFER_READ_BIT)
                             , ("VK_ACCESS_TRANSFER_WRITE_BIT",                 pure VK_ACCESS_TRANSFER_WRITE_BIT)
                             , ("VK_ACCESS_HOST_READ_BIT",                      pure VK_ACCESS_HOST_READ_BIT)
                             , ("VK_ACCESS_HOST_WRITE_BIT",                     pure VK_ACCESS_HOST_WRITE_BIT)
                             , ("VK_ACCESS_MEMORY_READ_BIT",                    pure VK_ACCESS_MEMORY_READ_BIT)
                             , ("VK_ACCESS_MEMORY_WRITE_BIT",                   pure VK_ACCESS_MEMORY_WRITE_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_ACCESS_RESERVED_30_BIT_KHR",                       pure (VkAccessFlagBits 0x40000000))
                             , ("VK_ACCESS_RESERVED_31_BIT_KHR",                       pure (VkAccessFlagBits 0x80000000))
                             , ("VK_ACCESS_RESERVED_28_BIT_KHR",                       pure (VkAccessFlagBits 0x10000000))
                             , ("VK_ACCESS_RESERVED_29_BIT_KHR",                       pure (VkAccessFlagBits 0x20000000))
                             , ("VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT",          pure (VkAccessFlagBits 0x02000000))
                             , ("VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT",   pure (VkAccessFlagBits 0x04000000))
                             , ("VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT",  pure (VkAccessFlagBits 0x08000000))
                             , ("VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT",        pure (VkAccessFlagBits 0x00100000))
                             , ("VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX",              pure (VkAccessFlagBits 0x00020000))
                             , ("VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX",             pure (VkAccessFlagBits 0x00040000))
                             , ("VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT", pure (VkAccessFlagBits 0x00080000))
                             , ("VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV",            pure (VkAccessFlagBits 0x00800000))
                             , ("VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV",        pure (VkAccessFlagBits 0x00200000))
                             , ("VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV",       pure (VkAccessFlagBits 0x00400000))
                             , ("VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT",         pure (VkAccessFlagBits 0x01000000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAccessFlagBits")
                        v <- step readPrec
                        pure (VkAccessFlagBits v)
                        )
                    )

-- | 'VK_ACCESS_INDIRECT_COMMAND_READ_BIT' specifies read access to indirect
-- command data read as part of an indirect drawing or dispatch command.
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessFlagBits 0x00000001

-- | 'VK_ACCESS_INDEX_READ_BIT' specifies read access to an index buffer as
-- part of an indexed drawing command, bound by
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'.
pattern VK_ACCESS_INDEX_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_INDEX_READ_BIT = VkAccessFlagBits 0x00000002

-- | 'VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT' specifies read access to a vertex
-- buffer as part of a drawing command, bound by
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers'.
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessFlagBits 0x00000004

-- | 'VK_ACCESS_UNIFORM_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer>.
pattern VK_ACCESS_UNIFORM_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessFlagBits 0x00000008

-- | 'VK_ACCESS_INPUT_ATTACHMENT_READ_BIT' specifies read access to an
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass input attachment>
-- within a render pass during fragment shading.
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x00000010

-- | 'VK_ACCESS_SHADER_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage image>.
pattern VK_ACCESS_SHADER_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_SHADER_READ_BIT = VkAccessFlagBits 0x00000020

-- | 'VK_ACCESS_SHADER_WRITE_BIT' specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage image>.
pattern VK_ACCESS_SHADER_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessFlagBits 0x00000040

-- | 'VK_ACCESS_COLOR_ATTACHMENT_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass color attachment>,
-- such as via
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blending blending>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-logicop logic operations>,
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
-- It does not include
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>.
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x00000080

-- | 'VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT' specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass color, resolve, or depth\/stencil resolve attachment>
-- during a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass render pass>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x00000100

-- | 'VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x00000200

-- | 'VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT' specifies write access to
-- a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x00000400

-- | 'VK_ACCESS_TRANSFER_READ_BIT' specifies read access to an image or
-- buffer in a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies copy>
-- operation.
pattern VK_ACCESS_TRANSFER_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessFlagBits 0x00000800

-- | 'VK_ACCESS_TRANSFER_WRITE_BIT' specifies write access to an image or
-- buffer in a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#clears clear>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies copy>
-- operation.
pattern VK_ACCESS_TRANSFER_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessFlagBits 0x00001000

-- | 'VK_ACCESS_HOST_READ_BIT' specifies read access by a host operation.
-- Accesses of this type are not performed through a resource, but directly
-- on memory.
pattern VK_ACCESS_HOST_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_HOST_READ_BIT = VkAccessFlagBits 0x00002000

-- | 'VK_ACCESS_HOST_WRITE_BIT' specifies write access by a host operation.
-- Accesses of this type are not performed through a resource, but directly
-- on memory.
pattern VK_ACCESS_HOST_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessFlagBits 0x00004000

-- | 'VK_ACCESS_MEMORY_READ_BIT' specifies read access via non-specific
-- entities. These entities include the Vulkan device and host, but /may/
-- also include entities external to the Vulkan device or otherwise not
-- part of the core Vulkan pipeline. When included in a destination access
-- mask, makes all available writes visible to all future read accesses on
-- entities known to the Vulkan device.
pattern VK_ACCESS_MEMORY_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessFlagBits 0x00008000

-- | 'VK_ACCESS_MEMORY_WRITE_BIT' specifies write access via non-specific
-- entities. These entities include the Vulkan device and host, but /may/
-- also include entities external to the Vulkan device or otherwise not
-- part of the core Vulkan pipeline. When included in a source access mask,
-- all writes that are performed by entities known to the Vulkan device are
-- made available. When included in a destination access mask, makes all
-- available writes visible to all future write accesses on entities known
-- to the Vulkan device.
pattern VK_ACCESS_MEMORY_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessFlagBits 0x00010000

-- | VkAccessFlags - Bitmask of VkAccessFlagBits
--
-- = Description
--
-- 'VkAccessFlags' is a bitmask type for setting a mask of zero or more
-- 'VkAccessFlagBits'.
--
-- = See Also
--
-- 'VkAccessFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier',
-- 'VkSubpassDependency'
type VkAccessFlags = VkAccessFlagBits

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
-- If @flags@ includes 'VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT', then the
-- attachment is treated as if it shares physical memory with another
-- attachment in the same render pass. This information limits the ability
-- of the implementation to reorder certain operations (like layout
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
-- 'VkAttachmentDescriptionFlags', 'VkAttachmentLoadOp',
-- 'VkAttachmentStoreOp', 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'VkRenderPassCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'
data VkAttachmentDescription = VkAttachmentDescription
  { -- | @flags@ is a bitmask of 'VkAttachmentDescriptionFlagBits' specifying
  -- additional properties of the attachment.
  vkFlags :: VkAttachmentDescriptionFlags
  , -- | @format@ is a 'Graphics.Vulkan.C.Core10.Core.VkFormat' value specifying
  -- the format of the image view that will be used for the attachment.
  vkFormat :: VkFormat
  , -- | @samples@ is the number of samples of the image as defined in
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'.
  vkSamples :: VkSampleCountFlagBits
  , -- | @loadOp@ is a 'VkAttachmentLoadOp' value specifying how the contents of
  -- color and depth components of the attachment are treated at the
  -- beginning of the subpass where it is first used.
  vkLoadOp :: VkAttachmentLoadOp
  , -- | @storeOp@ is a 'VkAttachmentStoreOp' value specifying how the contents
  -- of color and depth components of the attachment are treated at the end
  -- of the subpass where it is last used.
  vkStoreOp :: VkAttachmentStoreOp
  , -- | @stencilLoadOp@ is a 'VkAttachmentLoadOp' value specifying how the
  -- contents of stencil components of the attachment are treated at the
  -- beginning of the subpass where it is first used.
  vkStencilLoadOp :: VkAttachmentLoadOp
  , -- | @stencilStoreOp@ is a 'VkAttachmentStoreOp' value specifying how the
  -- contents of stencil components of the attachment are treated at the end
  -- of the last subpass where it is used.
  vkStencilStoreOp :: VkAttachmentStoreOp
  , -- | @initialLayout@ is the layout the attachment image subresource will be
  -- in when a render pass instance begins.
  vkInitialLayout :: VkImageLayout
  , -- | @finalLayout@ /must/ not be
  -- 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' or
  -- 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED'
  vkFinalLayout :: VkImageLayout
  }
  deriving (Eq, Show)

instance Storable VkAttachmentDescription where
  sizeOf ~_ = 36
  alignment ~_ = 4
  peek ptr = VkAttachmentDescription <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 28)
                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFlags (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 4) (vkFormat (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 8) (vkSamples (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 12) (vkLoadOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 16) (vkStoreOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 20) (vkStencilLoadOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 24) (vkStencilStoreOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 28) (vkInitialLayout (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 32) (vkFinalLayout (poked :: VkAttachmentDescription))

instance Zero VkAttachmentDescription where
  zero = VkAttachmentDescription zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

-- ** VkAttachmentDescriptionFlagBits

-- | VkAttachmentDescriptionFlagBits - Bitmask specifying additional
-- properties of an attachment
--
-- = See Also
--
-- 'VkAttachmentDescriptionFlags'
newtype VkAttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkAttachmentDescriptionFlagBits where
  showsPrec _ VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = showString "VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT"
  showsPrec p (VkAttachmentDescriptionFlagBits x) = showParen (p >= 11) (showString "VkAttachmentDescriptionFlagBits " . showsPrec 11 x)

instance Read VkAttachmentDescriptionFlagBits where
  readPrec = parens ( choose [ ("VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT", pure VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAttachmentDescriptionFlagBits")
                        v <- step readPrec
                        pure (VkAttachmentDescriptionFlagBits v)
                        )
                    )

-- | 'VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT' specifies that the attachment
-- aliases the same device memory as other attachments.
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT :: VkAttachmentDescriptionFlagBits
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VkAttachmentDescriptionFlagBits 0x00000001

-- | VkAttachmentDescriptionFlags - Bitmask of
-- VkAttachmentDescriptionFlagBits
--
-- = Description
--
-- 'VkAttachmentDescriptionFlags' is a bitmask type for setting a mask of
-- zero or more 'VkAttachmentDescriptionFlagBits'.
--
-- = See Also
--
-- 'VkAttachmentDescription', 'VkAttachmentDescriptionFlagBits'
type VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlagBits

-- ** VkAttachmentLoadOp

-- | VkAttachmentLoadOp - Specify how contents of an attachment are treated
-- at the beginning of a subpass
--
-- = See Also
--
-- 'VkAttachmentDescription'
newtype VkAttachmentLoadOp = VkAttachmentLoadOp Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkAttachmentLoadOp where
  showsPrec _ VK_ATTACHMENT_LOAD_OP_LOAD = showString "VK_ATTACHMENT_LOAD_OP_LOAD"
  showsPrec _ VK_ATTACHMENT_LOAD_OP_CLEAR = showString "VK_ATTACHMENT_LOAD_OP_CLEAR"
  showsPrec _ VK_ATTACHMENT_LOAD_OP_DONT_CARE = showString "VK_ATTACHMENT_LOAD_OP_DONT_CARE"
  showsPrec p (VkAttachmentLoadOp x) = showParen (p >= 11) (showString "VkAttachmentLoadOp " . showsPrec 11 x)

instance Read VkAttachmentLoadOp where
  readPrec = parens ( choose [ ("VK_ATTACHMENT_LOAD_OP_LOAD",      pure VK_ATTACHMENT_LOAD_OP_LOAD)
                             , ("VK_ATTACHMENT_LOAD_OP_CLEAR",     pure VK_ATTACHMENT_LOAD_OP_CLEAR)
                             , ("VK_ATTACHMENT_LOAD_OP_DONT_CARE", pure VK_ATTACHMENT_LOAD_OP_DONT_CARE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAttachmentLoadOp")
                        v <- step readPrec
                        pure (VkAttachmentLoadOp v)
                        )
                    )

-- | 'VK_ATTACHMENT_LOAD_OP_LOAD' specifies that the previous contents of the
-- image within the render area will be preserved. For attachments with a
-- depth\/stencil format, this uses the access type
-- 'VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT'. For attachments with a
-- color format, this uses the access type
-- 'VK_ACCESS_COLOR_ATTACHMENT_READ_BIT'.
pattern VK_ATTACHMENT_LOAD_OP_LOAD :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0

-- | 'VK_ATTACHMENT_LOAD_OP_CLEAR' specifies that the contents within the
-- render area will be cleared to a uniform value, which is specified when
-- a render pass instance is begun. For attachments with a depth\/stencil
-- format, this uses the access type
-- 'VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'. For attachments with a
-- color format, this uses the access type
-- 'VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern VK_ATTACHMENT_LOAD_OP_CLEAR :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1

-- | 'VK_ATTACHMENT_LOAD_OP_DONT_CARE' specifies that the previous contents
-- within the area need not be preserved; the contents of the attachment
-- will be undefined inside the render area. For attachments with a
-- depth\/stencil format, this uses the access type
-- 'VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'. For attachments with a
-- color format, this uses the access type
-- 'VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2

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
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout', 'VkSubpassDescription'
data VkAttachmentReference = VkAttachmentReference
  { -- | @attachment@ is either an integer value identifying an attachment at the
  -- corresponding index in 'VkRenderPassCreateInfo'::@pAttachments@, or
  -- 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' to signify
  -- that this attachment is not used.
  vkAttachment :: Word32
  , -- | @layout@ is a 'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
  -- specifying the layout the attachment uses during the subpass.
  vkLayout :: VkImageLayout
  }
  deriving (Eq, Show)

instance Storable VkAttachmentReference where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkAttachmentReference <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAttachment (poked :: VkAttachmentReference))
                *> poke (ptr `plusPtr` 4) (vkLayout (poked :: VkAttachmentReference))

instance Zero VkAttachmentReference where
  zero = VkAttachmentReference zero
                               zero

-- ** VkAttachmentStoreOp

-- | VkAttachmentStoreOp - Specify how contents of an attachment are treated
-- at the end of a subpass
--
-- = See Also
--
-- 'VkAttachmentDescription'
newtype VkAttachmentStoreOp = VkAttachmentStoreOp Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkAttachmentStoreOp where
  showsPrec _ VK_ATTACHMENT_STORE_OP_STORE = showString "VK_ATTACHMENT_STORE_OP_STORE"
  showsPrec _ VK_ATTACHMENT_STORE_OP_DONT_CARE = showString "VK_ATTACHMENT_STORE_OP_DONT_CARE"
  showsPrec p (VkAttachmentStoreOp x) = showParen (p >= 11) (showString "VkAttachmentStoreOp " . showsPrec 11 x)

instance Read VkAttachmentStoreOp where
  readPrec = parens ( choose [ ("VK_ATTACHMENT_STORE_OP_STORE",     pure VK_ATTACHMENT_STORE_OP_STORE)
                             , ("VK_ATTACHMENT_STORE_OP_DONT_CARE", pure VK_ATTACHMENT_STORE_OP_DONT_CARE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAttachmentStoreOp")
                        v <- step readPrec
                        pure (VkAttachmentStoreOp v)
                        )
                    )

-- | 'VK_ATTACHMENT_STORE_OP_STORE' specifies the contents generated during
-- the render pass and within the render area are written to memory. For
-- attachments with a depth\/stencil format, this uses the access type
-- 'VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'. For attachments with a
-- color format, this uses the access type
-- 'VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern VK_ATTACHMENT_STORE_OP_STORE :: VkAttachmentStoreOp
pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0

-- | 'VK_ATTACHMENT_STORE_OP_DONT_CARE' specifies the contents within the
-- render area are not needed after rendering, and /may/ be discarded; the
-- contents of the attachment will be undefined inside the render area. For
-- attachments with a depth\/stencil format, this uses the access type
-- 'VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'. For attachments with a
-- color format, this uses the access type
-- 'VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE :: VkAttachmentStoreOp
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1

-- ** VkDependencyFlagBits

-- | VkDependencyFlagBits - Bitmask specifying how execution and memory
-- dependencies are formed
--
-- = See Also
--
-- 'VkDependencyFlags'
newtype VkDependencyFlagBits = VkDependencyFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDependencyFlagBits where
  showsPrec _ VK_DEPENDENCY_BY_REGION_BIT = showString "VK_DEPENDENCY_BY_REGION_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDependencyFlagBits 0x00000004) = showString "VK_DEPENDENCY_DEVICE_GROUP_BIT"
  showsPrec _ (VkDependencyFlagBits 0x00000002) = showString "VK_DEPENDENCY_VIEW_LOCAL_BIT"
  showsPrec p (VkDependencyFlagBits x) = showParen (p >= 11) (showString "VkDependencyFlagBits " . showsPrec 11 x)

instance Read VkDependencyFlagBits where
  readPrec = parens ( choose [ ("VK_DEPENDENCY_BY_REGION_BIT", pure VK_DEPENDENCY_BY_REGION_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DEPENDENCY_DEVICE_GROUP_BIT", pure (VkDependencyFlagBits 0x00000004))
                             , ("VK_DEPENDENCY_VIEW_LOCAL_BIT",   pure (VkDependencyFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDependencyFlagBits")
                        v <- step readPrec
                        pure (VkDependencyFlagBits v)
                        )
                    )

-- | 'VK_DEPENDENCY_BY_REGION_BIT' specifies that dependencies will be
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local>.
pattern VK_DEPENDENCY_BY_REGION_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlagBits 0x00000001

-- | VkDependencyFlags - Bitmask of VkDependencyFlagBits
--
-- = Description
--
-- 'VkDependencyFlags' is a bitmask type for setting a mask of zero or more
-- 'VkDependencyFlagBits'.
--
-- = See Also
--
-- 'VkDependencyFlagBits', 'VkSubpassDependency',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
type VkDependencyFlags = VkDependencyFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkFramebuffer_T
-- | VkFramebuffer - Opaque handle to a framebuffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'vkCreateFramebuffer', 'vkDestroyFramebuffer'
type VkFramebuffer = Ptr VkFramebuffer_T

-- ** VkFramebufferCreateFlags

-- | VkFramebufferCreateFlags - Reserved for future use
--
-- = Description
--
-- 'VkFramebufferCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkFramebufferCreateInfo'
newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkFramebufferCreateFlags where
  
  showsPrec p (VkFramebufferCreateFlags x) = showParen (p >= 11) (showString "VkFramebufferCreateFlags " . showsPrec 11 x)

instance Read VkFramebufferCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFramebufferCreateFlags")
                        v <- step readPrec
                        pure (VkFramebufferCreateFlags v)
                        )
                    )



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
--     corresponding 'VkAttachmentDescription' in @renderPass@
--
-- -   Each element of @pAttachments@ /must/ have been created with a
--     @samples@ value that matches the @samples@ value specified by the
--     corresponding 'VkAttachmentDescription' in @renderPass@
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
-- 'VkFramebufferCreateFlags',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkCreateFramebuffer'
data VkFramebufferCreateInfo = VkFramebufferCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkFramebufferCreateFlags
  , -- | @renderPass@ is a render pass that defines what render passes the
  -- framebuffer will be compatible with. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-compatibility Render Pass Compatibility>
  -- for details.
  vkRenderPass :: VkRenderPass
  , -- | @attachmentCount@ is the number of attachments.
  vkAttachmentCount :: Word32
  , -- | @pAttachments@ is an array of
  -- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' handles, each of which
  -- will be used as the corresponding attachment in a render pass instance.
  vkPAttachments :: Ptr VkImageView
  , -- | @width@, @height@ and @layers@ define the dimensions of the framebuffer.
  -- If the render pass uses multiview, then @layers@ /must/ be one and each
  -- attachment requires a number of layers that is greater than the maximum
  -- bit index set in the view mask in the subpasses in which it is used.
  vkWidth :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "height"
  vkHeight :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "layers"
  vkLayers :: Word32
  }
  deriving (Eq, Show)

instance Storable VkFramebufferCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkFramebufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 40)
                                     <*> peek (ptr `plusPtr` 48)
                                     <*> peek (ptr `plusPtr` 52)
                                     <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkRenderPass (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkAttachmentCount (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPAttachments (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkWidth (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkHeight (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkLayers (poked :: VkFramebufferCreateInfo))

instance Zero VkFramebufferCreateInfo where
  zero = VkFramebufferCreateInfo VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

-- ** VkPipelineBindPoint

-- | VkPipelineBindPoint - Specify the bind point of a pipeline object to a
-- command buffer
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'VkSubpassDescription',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindPipeline'
newtype VkPipelineBindPoint = VkPipelineBindPoint Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkPipelineBindPoint where
  showsPrec _ VK_PIPELINE_BIND_POINT_GRAPHICS = showString "VK_PIPELINE_BIND_POINT_GRAPHICS"
  showsPrec _ VK_PIPELINE_BIND_POINT_COMPUTE = showString "VK_PIPELINE_BIND_POINT_COMPUTE"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkPipelineBindPoint 1000165000) = showString "VK_PIPELINE_BIND_POINT_RAY_TRACING_NV"
  showsPrec p (VkPipelineBindPoint x) = showParen (p >= 11) (showString "VkPipelineBindPoint " . showsPrec 11 x)

instance Read VkPipelineBindPoint where
  readPrec = parens ( choose [ ("VK_PIPELINE_BIND_POINT_GRAPHICS", pure VK_PIPELINE_BIND_POINT_GRAPHICS)
                             , ("VK_PIPELINE_BIND_POINT_COMPUTE",  pure VK_PIPELINE_BIND_POINT_COMPUTE)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_PIPELINE_BIND_POINT_RAY_TRACING_NV", pure (VkPipelineBindPoint 1000165000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineBindPoint")
                        v <- step readPrec
                        pure (VkPipelineBindPoint v)
                        )
                    )

-- | 'VK_PIPELINE_BIND_POINT_GRAPHICS' specifies binding as a graphics
-- pipeline.
pattern VK_PIPELINE_BIND_POINT_GRAPHICS :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

-- | 'VK_PIPELINE_BIND_POINT_COMPUTE' specifies binding as a compute
-- pipeline.
pattern VK_PIPELINE_BIND_POINT_COMPUTE :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1

-- ** VkRenderPassCreateFlags

-- | VkRenderPassCreateFlags - Reserved for future use
--
-- = Description
--
-- 'VkRenderPassCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkRenderPassCreateInfo'
newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkRenderPassCreateFlags where
  
  showsPrec p (VkRenderPassCreateFlags x) = showParen (p >= 11) (showString "VkRenderPassCreateFlags " . showsPrec 11 x)

instance Read VkRenderPassCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkRenderPassCreateFlags")
                        v <- step readPrec
                        pure (VkRenderPassCreateFlags v)
                        )
                    )



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
--     'VK_ATTACHMENT_LOAD_OP_CLEAR', the first use of that attachment
--     /must/ not specify a @layout@ equal to
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'.
--
-- -   For any member of @pAttachments@ with a @stencilLoadOp@ equal to
--     'VK_ATTACHMENT_LOAD_OP_CLEAR', the first use of that attachment
--     /must/ not specify a @layout@ equal to
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'.
--
-- -   For any member of @pAttachments@ with a @loadOp@ equal to
--     'VK_ATTACHMENT_LOAD_OP_CLEAR', the first use of that attachment
--     /must/ not specify a @layout@ equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'.
--
-- -   For any member of @pAttachments@ with a @stencilLoadOp@ equal to
--     'VK_ATTACHMENT_LOAD_OP_CLEAR', the first use of that attachment
--     /must/ not specify a @layout@ equal to
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
-- 'VkAttachmentDescription', 'VkRenderPassCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'VkSubpassDependency',
-- 'VkSubpassDescription', 'vkCreateRenderPass'
data VkRenderPassCreateInfo = VkRenderPassCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkRenderPassCreateFlags
  , -- | @attachmentCount@ is the number of attachments used by this render pass.
  vkAttachmentCount :: Word32
  , -- | @pAttachments@ points to an array of @attachmentCount@
  -- 'VkAttachmentDescription' structures describing the attachments used by
  -- the render pass.
  vkPAttachments :: Ptr VkAttachmentDescription
  , -- | @subpassCount@ is the number of subpasses to create.
  vkSubpassCount :: Word32
  , -- | @pSubpasses@ points to an array of @subpassCount@ 'VkSubpassDescription'
  -- structures describing each subpass.
  vkPSubpasses :: Ptr VkSubpassDescription
  , -- | @dependencyCount@ is the number of memory dependencies between pairs of
  -- subpasses.
  vkDependencyCount :: Word32
  , -- | @pDependencies@ points to an array of @dependencyCount@
  -- 'VkSubpassDependency' structures describing dependencies between pairs
  -- of subpasses.
  vkPDependencies :: Ptr VkSubpassDependency
  }
  deriving (Eq, Show)

instance Storable VkRenderPassCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkRenderPassCreateInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 20)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
                                    <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkAttachmentCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPAttachments (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkSubpassCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPSubpasses (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkDependencyCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPDependencies (poked :: VkRenderPassCreateInfo))

instance Zero VkRenderPassCreateInfo where
  zero = VkRenderPassCreateInfo VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero

-- | VkSubpassDependency - Structure specifying a subpass dependency
--
-- = Description
--
-- If @srcSubpass@ is equal to @dstSubpass@ then the 'VkSubpassDependency'
-- describes a
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
--     then @dependencyFlags@ /must/ include 'VK_DEPENDENCY_BY_REGION_BIT'
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
-- 'VkAccessFlags', 'VkDependencyFlags',
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags',
-- 'VkRenderPassCreateInfo'
data VkSubpassDependency = VkSubpassDependency
  { -- | @srcSubpass@ is the subpass index of the first subpass in the
  -- dependency, or 'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'.
  vkSrcSubpass :: Word32
  , -- | @dstSubpass@ is the subpass index of the second subpass in the
  -- dependency, or 'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'.
  vkDstSubpass :: Word32
  , -- | @srcStageMask@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' specifying the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>.
  vkSrcStageMask :: VkPipelineStageFlags
  , -- | @dstStageMask@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' specifying the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
  vkDstStageMask :: VkPipelineStageFlags
  , -- | @srcAccessMask@ is a bitmask of 'VkAccessFlagBits' specifying a
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
  vkSrcAccessMask :: VkAccessFlags
  , -- | @dstAccessMask@ is a bitmask of 'VkAccessFlagBits' specifying a
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
  vkDstAccessMask :: VkAccessFlags
  , -- | @dependencyFlags@ is a bitmask of 'VkDependencyFlagBits'.
  vkDependencyFlags :: VkDependencyFlags
  }
  deriving (Eq, Show)

instance Storable VkSubpassDependency where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek ptr = VkSubpassDependency <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 12)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 20)
                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubpass (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 4) (vkDstSubpass (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 8) (vkSrcStageMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 12) (vkDstStageMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 24) (vkDependencyFlags (poked :: VkSubpassDependency))

instance Zero VkSubpassDependency where
  zero = VkSubpassDependency zero
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
-- -   @pipelineBindPoint@ /must/ be 'VK_PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   @colorAttachmentCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxColorAttachments@
--
-- -   If the first use of an attachment in this render pass is as an input
--     attachment, and the attachment is not also used as a color or
--     depth\/stencil attachment in the same subpass, then @loadOp@ /must/
--     not be 'VK_ATTACHMENT_LOAD_OP_CLEAR'
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
-- -   If any attachment is used by more than one 'VkAttachmentReference'
--     member, then each use /must/ use the same @layout@
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
-- 'VkAttachmentReference', 'VkPipelineBindPoint',
-- 'VkRenderPassCreateInfo', 'VkSubpassDescriptionFlags'
data VkSubpassDescription = VkSubpassDescription
  { -- | @flags@ is a bitmask of 'VkSubpassDescriptionFlagBits' specifying usage
  -- of the subpass.
  vkFlags :: VkSubpassDescriptionFlags
  , -- | @pipelineBindPoint@ is a 'VkPipelineBindPoint' value specifying the
  -- pipeline type supported for this subpass.
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- | @inputAttachmentCount@ is the number of input attachments.
  vkInputAttachmentCount :: Word32
  , -- | @pInputAttachments@ is an array of 'VkAttachmentReference' structures
  -- defining the input attachments for this subpass and their layouts.
  vkPInputAttachments :: Ptr VkAttachmentReference
  , -- | @colorAttachmentCount@ is the number of color attachments.
  vkColorAttachmentCount :: Word32
  , -- | @pColorAttachments@ is an array of 'VkAttachmentReference' structures
  -- defining the color attachments for this subpass and their layouts.
  vkPColorAttachments :: Ptr VkAttachmentReference
  , -- | @pResolveAttachments@ is an optional array of @colorAttachmentCount@
  -- 'VkAttachmentReference' structures defining the resolve attachments for
  -- this subpass and their layouts.
  vkPResolveAttachments :: Ptr VkAttachmentReference
  , -- | @pDepthStencilAttachment@ is a pointer to a 'VkAttachmentReference'
  -- specifying the depth\/stencil attachment for this subpass and its
  -- layout.
  vkPDepthStencilAttachment :: Ptr VkAttachmentReference
  , -- | @preserveAttachmentCount@ is the number of preserved attachments.
  vkPreserveAttachmentCount :: Word32
  , -- | @pPreserveAttachments@ is an array of @preserveAttachmentCount@ render
  -- pass attachment indices identifying attachments that are not used by
  -- this subpass, but whose contents /must/ be preserved throughout the
  -- subpass.
  vkPPreserveAttachments :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkSubpassDescription where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkSubpassDescription <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
                                  <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFlags (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 4) (vkPipelineBindPoint (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 8) (vkInputAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 16) (vkPInputAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 24) (vkColorAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 32) (vkPColorAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 40) (vkPResolveAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 48) (vkPDepthStencilAttachment (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 56) (vkPreserveAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 64) (vkPPreserveAttachments (poked :: VkSubpassDescription))

instance Zero VkSubpassDescription where
  zero = VkSubpassDescription zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero

-- ** VkSubpassDescriptionFlagBits

-- | VkSubpassDescriptionFlagBits - Bitmask specifying usage of a subpass
--
-- = See Also
--
-- 'VkSubpassDescriptionFlags'
newtype VkSubpassDescriptionFlagBits = VkSubpassDescriptionFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkSubpassDescriptionFlagBits where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000001) = showString "VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX"
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000002) = showString "VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX"
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000004) = showString "VK_SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM"
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000008) = showString "VK_SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM"
  showsPrec p (VkSubpassDescriptionFlagBits x) = showParen (p >= 11) (showString "VkSubpassDescriptionFlagBits " . showsPrec 11 x)

instance Read VkSubpassDescriptionFlagBits where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX",      pure (VkSubpassDescriptionFlagBits 0x00000001))
                             , ("VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX", pure (VkSubpassDescriptionFlagBits 0x00000002))
                             , ("VK_SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM",              pure (VkSubpassDescriptionFlagBits 0x00000004))
                             , ("VK_SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM",              pure (VkSubpassDescriptionFlagBits 0x00000008))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSubpassDescriptionFlagBits")
                        v <- step readPrec
                        pure (VkSubpassDescriptionFlagBits v)
                        )
                    )



-- | VkSubpassDescriptionFlags - Bitmask of VkSubpassDescriptionFlagBits
--
-- = Description
--
-- 'VkSubpassDescriptionFlags' is a bitmask type for setting a mask of zero
-- or more 'VkSubpassDescriptionFlagBits'.
--
-- = See Also
--
-- 'VkSubpassDescription', 'VkSubpassDescriptionFlagBits'
type VkSubpassDescriptionFlags = VkSubpassDescriptionFlagBits

-- | vkCreateFramebuffer - Create a new framebuffer object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the framebuffer.
--
-- -   @pCreateInfo@ points to a 'VkFramebufferCreateInfo' structure which
--     describes additional information about framebuffer creation.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pFramebuffer@ points to a 'VkFramebuffer' handle in which the
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
-- 'VkFramebuffer', 'VkFramebufferCreateInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateFramebuffer" vkCreateFramebuffer :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult
#else
vkCreateFramebuffer :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult
vkCreateFramebuffer deviceCmds = mkVkCreateFramebuffer (pVkCreateFramebuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateFramebuffer
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult)
#endif

type FN_vkCreateFramebuffer = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult
type PFN_vkCreateFramebuffer = FunPtr FN_vkCreateFramebuffer

-- | vkCreateRenderPass - Create a new render pass object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the render pass.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkRenderPassCreateInfo' structure that describes the parameters of
--     the render pass.
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
-- 'VkRenderPassCreateInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateRenderPass" vkCreateRenderPass :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
#else
vkCreateRenderPass :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
vkCreateRenderPass deviceCmds = mkVkCreateRenderPass (pVkCreateRenderPass deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRenderPass
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult)
#endif

type FN_vkCreateRenderPass = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
type PFN_vkCreateRenderPass = FunPtr FN_vkCreateRenderPass

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
-- 'VkFramebuffer'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyFramebuffer" vkDestroyFramebuffer :: ("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyFramebuffer :: DeviceCmds -> ("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyFramebuffer deviceCmds = mkVkDestroyFramebuffer (pVkDestroyFramebuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyFramebuffer
  :: FunPtr (("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyFramebuffer = ("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyFramebuffer = FunPtr FN_vkDestroyFramebuffer

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
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyRenderPass" vkDestroyRenderPass :: ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyRenderPass :: DeviceCmds -> ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyRenderPass deviceCmds = mkVkDestroyRenderPass (pVkDestroyRenderPass deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyRenderPass
  :: FunPtr (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyRenderPass = ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyRenderPass = FunPtr FN_vkDestroyRenderPass

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
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetRenderAreaGranularity" vkGetRenderAreaGranularity :: ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()
#else
vkGetRenderAreaGranularity :: DeviceCmds -> ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()
vkGetRenderAreaGranularity deviceCmds = mkVkGetRenderAreaGranularity (pVkGetRenderAreaGranularity deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRenderAreaGranularity
  :: FunPtr (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()) -> (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ())
#endif

type FN_vkGetRenderAreaGranularity = ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()
type PFN_vkGetRenderAreaGranularity = FunPtr FN_vkGetRenderAreaGranularity
