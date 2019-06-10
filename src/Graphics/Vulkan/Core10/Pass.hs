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
  , pattern ACCESS_RESERVED_30_BIT_KHR
  , pattern ACCESS_RESERVED_31_BIT_KHR
  , pattern ACCESS_RESERVED_28_BIT_KHR
  , pattern ACCESS_RESERVED_29_BIT_KHR
  , pattern ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
  , pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
  , pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
  , pattern ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
  , pattern ACCESS_COMMAND_PROCESS_READ_BIT_NVX
  , pattern ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
  , pattern ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
  , pattern ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
  , pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
  , pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
  , pattern ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
  , AccessFlags
  , AttachmentDescription(..)
  , AttachmentDescriptionFlagBits
  , pattern ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
  , AttachmentDescriptionFlags
  , AttachmentLoadOp
  , pattern ATTACHMENT_LOAD_OP_LOAD
  , pattern ATTACHMENT_LOAD_OP_CLEAR
  , pattern ATTACHMENT_LOAD_OP_DONT_CARE
  , AttachmentReference(..)
  , AttachmentStoreOp
  , pattern ATTACHMENT_STORE_OP_STORE
  , pattern ATTACHMENT_STORE_OP_DONT_CARE
  , DependencyFlagBits
  , pattern DEPENDENCY_BY_REGION_BIT
  , pattern DEPENDENCY_DEVICE_GROUP_BIT
  , pattern DEPENDENCY_VIEW_LOCAL_BIT
  , DependencyFlags
  , Framebuffer
  , FramebufferCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , FramebufferCreateInfo(..)
#endif
  , PipelineBindPoint
  , pattern PIPELINE_BIND_POINT_GRAPHICS
  , pattern PIPELINE_BIND_POINT_COMPUTE
  , pattern PIPELINE_BIND_POINT_RAY_TRACING_NV
  , RenderPassCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , RenderPassCreateInfo(..)
#endif
  , SubpassDependency(..)
  , SubpassDescription(..)
  , SubpassDescriptionFlagBits
  , pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
  , pattern SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM
  , pattern SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM
  , SubpassDescriptionFlags
  , createFramebuffer
  , createRenderPass
  , destroyFramebuffer
  , destroyRenderPass
#if defined(VK_USE_PLATFORM_GGP)
  , getRenderAreaGranularity
#endif
  , withFramebuffer
  , withRenderPass
  , pattern VK_ACCESS_RESERVED_28_BIT_KHR
  , pattern VK_ACCESS_RESERVED_29_BIT_KHR
  , pattern VK_ACCESS_RESERVED_30_BIT_KHR
  , pattern VK_ACCESS_RESERVED_31_BIT_KHR
  , pattern VK_SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM
  , pattern VK_SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.Vector
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  , VkAttachmentDescriptionFlagBits(..)
  , VkAttachmentLoadOp(..)
  , VkAttachmentStoreOp(..)
  , VkDependencyFlagBits(..)
  , VkFramebufferCreateFlags(..)
  , VkPipelineBindPoint(..)
  , VkRenderPassCreateFlags(..)
  , VkSubpassDescriptionFlagBits(..)
  , VkFramebuffer
  , vkCreateFramebuffer
  , vkCreateRenderPass
  , vkDestroyFramebuffer
  , vkDestroyRenderPass
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

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Pass
  ( vkGetRenderAreaGranularity
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX
  , pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes
  ( pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
  , pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
  , pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( pattern VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , SampleCountFlagBits
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.ImageView
  ( ImageView
  )
#endif
import Graphics.Vulkan.Core10.Pipeline
  ( RenderPass
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  )
#endif
import Graphics.Vulkan.Core10.Queue
  ( PipelineStageFlags
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "AccessFlagBits"
type AccessFlagBits = VkAccessFlagBits


{-# complete ACCESS_INDIRECT_COMMAND_READ_BIT, ACCESS_INDEX_READ_BIT, ACCESS_VERTEX_ATTRIBUTE_READ_BIT, ACCESS_UNIFORM_READ_BIT, ACCESS_INPUT_ATTACHMENT_READ_BIT, ACCESS_SHADER_READ_BIT, ACCESS_SHADER_WRITE_BIT, ACCESS_COLOR_ATTACHMENT_READ_BIT, ACCESS_COLOR_ATTACHMENT_WRITE_BIT, ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT, ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT, ACCESS_TRANSFER_READ_BIT, ACCESS_TRANSFER_WRITE_BIT, ACCESS_HOST_READ_BIT, ACCESS_HOST_WRITE_BIT, ACCESS_MEMORY_READ_BIT, ACCESS_MEMORY_WRITE_BIT, ACCESS_RESERVED_30_BIT_KHR, ACCESS_RESERVED_31_BIT_KHR, ACCESS_RESERVED_28_BIT_KHR, ACCESS_RESERVED_29_BIT_KHR, ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT, ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT, ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT, ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT, ACCESS_COMMAND_PROCESS_READ_BIT_NVX, ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX, ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT, ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV, ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV, ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV, ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT :: AccessFlagBits #-}


-- No documentation found for Nested "AccessFlagBits" "ACCESS_INDIRECT_COMMAND_READ_BIT"
pattern ACCESS_INDIRECT_COMMAND_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_INDIRECT_COMMAND_READ_BIT = VK_ACCESS_INDIRECT_COMMAND_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_INDEX_READ_BIT"
pattern ACCESS_INDEX_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_INDEX_READ_BIT = VK_ACCESS_INDEX_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_VERTEX_ATTRIBUTE_READ_BIT"
pattern ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_UNIFORM_READ_BIT"
pattern ACCESS_UNIFORM_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_UNIFORM_READ_BIT = VK_ACCESS_UNIFORM_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_INPUT_ATTACHMENT_READ_BIT"
pattern ACCESS_INPUT_ATTACHMENT_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_INPUT_ATTACHMENT_READ_BIT = VK_ACCESS_INPUT_ATTACHMENT_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_SHADER_READ_BIT"
pattern ACCESS_SHADER_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_SHADER_READ_BIT = VK_ACCESS_SHADER_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_SHADER_WRITE_BIT"
pattern ACCESS_SHADER_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_SHADER_WRITE_BIT = VK_ACCESS_SHADER_WRITE_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_COLOR_ATTACHMENT_READ_BIT"
pattern ACCESS_COLOR_ATTACHMENT_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_COLOR_ATTACHMENT_READ_BIT = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_COLOR_ATTACHMENT_WRITE_BIT"
pattern ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT"
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT"
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_TRANSFER_READ_BIT"
pattern ACCESS_TRANSFER_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_TRANSFER_READ_BIT = VK_ACCESS_TRANSFER_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_TRANSFER_WRITE_BIT"
pattern ACCESS_TRANSFER_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_TRANSFER_WRITE_BIT = VK_ACCESS_TRANSFER_WRITE_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_HOST_READ_BIT"
pattern ACCESS_HOST_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_HOST_READ_BIT = VK_ACCESS_HOST_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_HOST_WRITE_BIT"
pattern ACCESS_HOST_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_HOST_WRITE_BIT = VK_ACCESS_HOST_WRITE_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_MEMORY_READ_BIT"
pattern ACCESS_MEMORY_READ_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_MEMORY_READ_BIT = VK_ACCESS_MEMORY_READ_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_MEMORY_WRITE_BIT"
pattern ACCESS_MEMORY_WRITE_BIT :: (a ~ AccessFlagBits) => a
pattern ACCESS_MEMORY_WRITE_BIT = VK_ACCESS_MEMORY_WRITE_BIT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_RESERVED_30_BIT_KHR"
pattern ACCESS_RESERVED_30_BIT_KHR :: (a ~ AccessFlagBits) => a
pattern ACCESS_RESERVED_30_BIT_KHR = VK_ACCESS_RESERVED_30_BIT_KHR


-- No documentation found for Nested "AccessFlagBits" "ACCESS_RESERVED_31_BIT_KHR"
pattern ACCESS_RESERVED_31_BIT_KHR :: (a ~ AccessFlagBits) => a
pattern ACCESS_RESERVED_31_BIT_KHR = VK_ACCESS_RESERVED_31_BIT_KHR


-- No documentation found for Nested "AccessFlagBits" "ACCESS_RESERVED_28_BIT_KHR"
pattern ACCESS_RESERVED_28_BIT_KHR :: (a ~ AccessFlagBits) => a
pattern ACCESS_RESERVED_28_BIT_KHR = VK_ACCESS_RESERVED_28_BIT_KHR


-- No documentation found for Nested "AccessFlagBits" "ACCESS_RESERVED_29_BIT_KHR"
pattern ACCESS_RESERVED_29_BIT_KHR :: (a ~ AccessFlagBits) => a
pattern ACCESS_RESERVED_29_BIT_KHR = VK_ACCESS_RESERVED_29_BIT_KHR


-- No documentation found for Nested "AccessFlagBits" "ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT"
pattern ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT :: (a ~ AccessFlagBits) => a
pattern ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT = VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT"
pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT :: (a ~ AccessFlagBits) => a
pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT = VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT"
pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT :: (a ~ AccessFlagBits) => a
pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT = VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT"
pattern ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT :: (a ~ AccessFlagBits) => a
pattern ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT = VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_COMMAND_PROCESS_READ_BIT_NVX"
pattern ACCESS_COMMAND_PROCESS_READ_BIT_NVX :: (a ~ AccessFlagBits) => a
pattern ACCESS_COMMAND_PROCESS_READ_BIT_NVX = VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX


-- No documentation found for Nested "AccessFlagBits" "ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX"
pattern ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX :: (a ~ AccessFlagBits) => a
pattern ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX = VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX


-- No documentation found for Nested "AccessFlagBits" "ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT"
pattern ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT :: (a ~ AccessFlagBits) => a
pattern ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT = VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT


-- No documentation found for Nested "AccessFlagBits" "ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV"
pattern ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV :: (a ~ AccessFlagBits) => a
pattern ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV = VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV


-- No documentation found for Nested "AccessFlagBits" "ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV"
pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV :: (a ~ AccessFlagBits) => a
pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV = VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV


-- No documentation found for Nested "AccessFlagBits" "ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV"
pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV :: (a ~ AccessFlagBits) => a
pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV = VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV


-- No documentation found for Nested "AccessFlagBits" "ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT"
pattern ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT :: (a ~ AccessFlagBits) => a
pattern ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT = VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT

-- No documentation found for TopLevel "AccessFlags"
type AccessFlags = AccessFlagBits


-- No documentation found for TopLevel "VkAttachmentDescription"
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


-- No documentation found for TopLevel "AttachmentDescriptionFlagBits"
type AttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits


{-# complete ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT :: AttachmentDescriptionFlagBits #-}


-- No documentation found for Nested "AttachmentDescriptionFlagBits" "ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT"
pattern ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT :: (a ~ AttachmentDescriptionFlagBits) => a
pattern ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT

-- No documentation found for TopLevel "AttachmentDescriptionFlags"
type AttachmentDescriptionFlags = AttachmentDescriptionFlagBits

-- No documentation found for TopLevel "AttachmentLoadOp"
type AttachmentLoadOp = VkAttachmentLoadOp


{-# complete ATTACHMENT_LOAD_OP_LOAD, ATTACHMENT_LOAD_OP_CLEAR, ATTACHMENT_LOAD_OP_DONT_CARE :: AttachmentLoadOp #-}


-- No documentation found for Nested "AttachmentLoadOp" "ATTACHMENT_LOAD_OP_LOAD"
pattern ATTACHMENT_LOAD_OP_LOAD :: (a ~ AttachmentLoadOp) => a
pattern ATTACHMENT_LOAD_OP_LOAD = VK_ATTACHMENT_LOAD_OP_LOAD


-- No documentation found for Nested "AttachmentLoadOp" "ATTACHMENT_LOAD_OP_CLEAR"
pattern ATTACHMENT_LOAD_OP_CLEAR :: (a ~ AttachmentLoadOp) => a
pattern ATTACHMENT_LOAD_OP_CLEAR = VK_ATTACHMENT_LOAD_OP_CLEAR


-- No documentation found for Nested "AttachmentLoadOp" "ATTACHMENT_LOAD_OP_DONT_CARE"
pattern ATTACHMENT_LOAD_OP_DONT_CARE :: (a ~ AttachmentLoadOp) => a
pattern ATTACHMENT_LOAD_OP_DONT_CARE = VK_ATTACHMENT_LOAD_OP_DONT_CARE


-- No documentation found for TopLevel "VkAttachmentReference"
data AttachmentReference = AttachmentReference
  { -- No documentation found for Nested "AttachmentReference" "attachment"
  attachment :: Word32
  , -- No documentation found for Nested "AttachmentReference" "layout"
  layout :: ImageLayout
  }
  deriving (Show, Eq)

instance Zero AttachmentReference where
  zero = AttachmentReference zero
                             zero


-- No documentation found for TopLevel "AttachmentStoreOp"
type AttachmentStoreOp = VkAttachmentStoreOp


{-# complete ATTACHMENT_STORE_OP_STORE, ATTACHMENT_STORE_OP_DONT_CARE :: AttachmentStoreOp #-}


-- No documentation found for Nested "AttachmentStoreOp" "ATTACHMENT_STORE_OP_STORE"
pattern ATTACHMENT_STORE_OP_STORE :: (a ~ AttachmentStoreOp) => a
pattern ATTACHMENT_STORE_OP_STORE = VK_ATTACHMENT_STORE_OP_STORE


-- No documentation found for Nested "AttachmentStoreOp" "ATTACHMENT_STORE_OP_DONT_CARE"
pattern ATTACHMENT_STORE_OP_DONT_CARE :: (a ~ AttachmentStoreOp) => a
pattern ATTACHMENT_STORE_OP_DONT_CARE = VK_ATTACHMENT_STORE_OP_DONT_CARE

-- No documentation found for TopLevel "DependencyFlagBits"
type DependencyFlagBits = VkDependencyFlagBits


{-# complete DEPENDENCY_BY_REGION_BIT, DEPENDENCY_DEVICE_GROUP_BIT, DEPENDENCY_VIEW_LOCAL_BIT :: DependencyFlagBits #-}


-- No documentation found for Nested "DependencyFlagBits" "DEPENDENCY_BY_REGION_BIT"
pattern DEPENDENCY_BY_REGION_BIT :: (a ~ DependencyFlagBits) => a
pattern DEPENDENCY_BY_REGION_BIT = VK_DEPENDENCY_BY_REGION_BIT


-- No documentation found for Nested "DependencyFlagBits" "DEPENDENCY_DEVICE_GROUP_BIT"
pattern DEPENDENCY_DEVICE_GROUP_BIT :: (a ~ DependencyFlagBits) => a
pattern DEPENDENCY_DEVICE_GROUP_BIT = VK_DEPENDENCY_DEVICE_GROUP_BIT


-- No documentation found for Nested "DependencyFlagBits" "DEPENDENCY_VIEW_LOCAL_BIT"
pattern DEPENDENCY_VIEW_LOCAL_BIT :: (a ~ DependencyFlagBits) => a
pattern DEPENDENCY_VIEW_LOCAL_BIT = VK_DEPENDENCY_VIEW_LOCAL_BIT

-- No documentation found for TopLevel "DependencyFlags"
type DependencyFlags = DependencyFlagBits

-- No documentation found for TopLevel "Framebuffer"
type Framebuffer = VkFramebuffer

-- No documentation found for TopLevel "FramebufferCreateFlags"
type FramebufferCreateFlags = VkFramebufferCreateFlags


-- No complete pragma for FramebufferCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkFramebufferCreateInfo"
data FramebufferCreateInfo = FramebufferCreateInfo
  { -- No documentation found for Nested "FramebufferCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FramebufferCreateInfo" "flags"
  flags :: FramebufferCreateFlags
  , -- No documentation found for Nested "FramebufferCreateInfo" "renderPass"
  renderPass :: RenderPass
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

instance Zero FramebufferCreateInfo where
  zero = FramebufferCreateInfo Nothing
                               zero
                               zero
                               mempty
                               zero
                               zero
                               zero

#endif

-- No documentation found for TopLevel "PipelineBindPoint"
type PipelineBindPoint = VkPipelineBindPoint


{-# complete PIPELINE_BIND_POINT_GRAPHICS, PIPELINE_BIND_POINT_COMPUTE, PIPELINE_BIND_POINT_RAY_TRACING_NV :: PipelineBindPoint #-}


-- No documentation found for Nested "PipelineBindPoint" "PIPELINE_BIND_POINT_GRAPHICS"
pattern PIPELINE_BIND_POINT_GRAPHICS :: (a ~ PipelineBindPoint) => a
pattern PIPELINE_BIND_POINT_GRAPHICS = VK_PIPELINE_BIND_POINT_GRAPHICS


-- No documentation found for Nested "PipelineBindPoint" "PIPELINE_BIND_POINT_COMPUTE"
pattern PIPELINE_BIND_POINT_COMPUTE :: (a ~ PipelineBindPoint) => a
pattern PIPELINE_BIND_POINT_COMPUTE = VK_PIPELINE_BIND_POINT_COMPUTE


-- No documentation found for Nested "PipelineBindPoint" "PIPELINE_BIND_POINT_RAY_TRACING_NV"
pattern PIPELINE_BIND_POINT_RAY_TRACING_NV :: (a ~ PipelineBindPoint) => a
pattern PIPELINE_BIND_POINT_RAY_TRACING_NV = VK_PIPELINE_BIND_POINT_RAY_TRACING_NV

-- No documentation found for TopLevel "RenderPassCreateFlags"
type RenderPassCreateFlags = VkRenderPassCreateFlags


-- No complete pragma for RenderPassCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkRenderPassCreateInfo"
data RenderPassCreateInfo = RenderPassCreateInfo
  { -- No documentation found for Nested "RenderPassCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassCreateInfo" "flags"
  flags :: RenderPassCreateFlags
  , -- No documentation found for Nested "RenderPassCreateInfo" "pAttachments"
  attachments :: Vector AttachmentDescription
  , -- No documentation found for Nested "RenderPassCreateInfo" "pSubpasses"
  subpasses :: Vector SubpassDescription
  , -- No documentation found for Nested "RenderPassCreateInfo" "pDependencies"
  dependencies :: Vector SubpassDependency
  }
  deriving (Show, Eq)

instance Zero RenderPassCreateInfo where
  zero = RenderPassCreateInfo Nothing
                              zero
                              mempty
                              mempty
                              mempty

#endif


-- No documentation found for TopLevel "VkSubpassDependency"
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

instance Zero SubpassDependency where
  zero = SubpassDependency zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero



-- No documentation found for TopLevel "VkSubpassDescription"
data SubpassDescription = SubpassDescription
  { -- No documentation found for Nested "SubpassDescription" "flags"
  flags :: SubpassDescriptionFlags
  , -- No documentation found for Nested "SubpassDescription" "pipelineBindPoint"
  pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "SubpassDescription" "pInputAttachments"
  inputAttachments :: Vector AttachmentReference
  , -- No documentation found for Nested "SubpassDescription" "pColorAttachments"
  colorAttachments :: Vector AttachmentReference
  , -- No documentation found for Nested "SubpassDescription" "pResolveAttachments"
  resolveAttachments :: Either Word32 (Vector AttachmentReference)
  , -- No documentation found for Nested "SubpassDescription" "pDepthStencilAttachment"
  depthStencilAttachment :: Maybe AttachmentReference
  , -- No documentation found for Nested "SubpassDescription" "pPreserveAttachments"
  preserveAttachments :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero SubpassDescription where
  zero = SubpassDescription zero
                            zero
                            mempty
                            mempty
                            (Left 0)
                            Nothing
                            mempty


-- No documentation found for TopLevel "SubpassDescriptionFlagBits"
type SubpassDescriptionFlagBits = VkSubpassDescriptionFlagBits


{-# complete SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX, SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX, SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM, SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM :: SubpassDescriptionFlagBits #-}


-- No documentation found for Nested "SubpassDescriptionFlagBits" "SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX"
pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX :: (a ~ SubpassDescriptionFlagBits) => a
pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX = VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX


-- No documentation found for Nested "SubpassDescriptionFlagBits" "SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX"
pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX :: (a ~ SubpassDescriptionFlagBits) => a
pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX = VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX


-- No documentation found for Nested "SubpassDescriptionFlagBits" "SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM"
pattern SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM :: (a ~ SubpassDescriptionFlagBits) => a
pattern SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM = VK_SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM


-- No documentation found for Nested "SubpassDescriptionFlagBits" "SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM"
pattern SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM :: (a ~ SubpassDescriptionFlagBits) => a
pattern SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM = VK_SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM

-- No documentation found for TopLevel "SubpassDescriptionFlags"
type SubpassDescriptionFlags = SubpassDescriptionFlagBits


-- No documentation found for TopLevel "vkCreateFramebuffer"
createFramebuffer :: Device ->  FramebufferCreateInfo ->  Maybe AllocationCallbacks ->  IO (Framebuffer)
createFramebuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateRenderPass"
createRenderPass :: Device ->  RenderPassCreateInfo ->  Maybe AllocationCallbacks ->  IO (RenderPass)
createRenderPass = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyFramebuffer"
destroyFramebuffer :: Device ->  Framebuffer ->  Maybe AllocationCallbacks ->  IO ()
destroyFramebuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyRenderPass"
destroyRenderPass :: Device ->  RenderPass ->  Maybe AllocationCallbacks ->  IO ()
destroyRenderPass = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetRenderAreaGranularity"
getRenderAreaGranularity :: Device ->  RenderPass ->  IO (Extent2D)
getRenderAreaGranularity = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- | A safe wrapper for 'createFramebuffer' and 'destroyFramebuffer' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withFramebuffer
  :: Device -> FramebufferCreateInfo -> Maybe AllocationCallbacks -> (Framebuffer -> IO a) -> IO a
withFramebuffer device framebufferCreateInfo allocationCallbacks = bracket
  (createFramebuffer device framebufferCreateInfo allocationCallbacks)
  (\o -> destroyFramebuffer device o allocationCallbacks)

-- | A safe wrapper for 'createRenderPass' and 'destroyRenderPass' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withRenderPass
  :: Device -> RenderPassCreateInfo -> Maybe AllocationCallbacks -> (RenderPass -> IO a) -> IO a
withRenderPass device renderPassCreateInfo allocationCallbacks = bracket
  (createRenderPass device renderPassCreateInfo allocationCallbacks)
  (\o -> destroyRenderPass device o allocationCallbacks)

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_RESERVED_28_BIT_KHR"
pattern VK_ACCESS_RESERVED_28_BIT_KHR :: VkAccessFlagBits
pattern VK_ACCESS_RESERVED_28_BIT_KHR = VkAccessFlagBits 0x10000000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_RESERVED_29_BIT_KHR"
pattern VK_ACCESS_RESERVED_29_BIT_KHR :: VkAccessFlagBits
pattern VK_ACCESS_RESERVED_29_BIT_KHR = VkAccessFlagBits 0x20000000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_RESERVED_30_BIT_KHR"
pattern VK_ACCESS_RESERVED_30_BIT_KHR :: VkAccessFlagBits
pattern VK_ACCESS_RESERVED_30_BIT_KHR = VkAccessFlagBits 0x40000000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_RESERVED_31_BIT_KHR"
pattern VK_ACCESS_RESERVED_31_BIT_KHR :: VkAccessFlagBits
pattern VK_ACCESS_RESERVED_31_BIT_KHR = VkAccessFlagBits 0x80000000

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM"
pattern VK_SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM :: VkSubpassDescriptionFlagBits
pattern VK_SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM = VkSubpassDescriptionFlagBits 0x00000004

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM"
pattern VK_SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM :: VkSubpassDescriptionFlagBits
pattern VK_SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM = VkSubpassDescriptionFlagBits 0x00000008
