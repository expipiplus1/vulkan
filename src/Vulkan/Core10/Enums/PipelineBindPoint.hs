{-# language CPP #-}
-- No documentation found for Chapter "PipelineBindPoint"
module Vulkan.Core10.Enums.PipelineBindPoint  (PipelineBindPoint( PIPELINE_BIND_POINT_GRAPHICS
                                                                , PIPELINE_BIND_POINT_COMPUTE
                                                                , PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI
                                                                , PIPELINE_BIND_POINT_RAY_TRACING_KHR
                                                                , PIPELINE_BIND_POINT_EXECUTION_GRAPH_AMDX
                                                                , ..
                                                                )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkPipelineBindPoint - Specify the bind point of a pipeline object to a
-- command buffer
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsMemoryRequirementsInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands_compute.PipelineIndirectDeviceAddressInfoNV',
-- 'Vulkan.Core10.Pass.SubpassDescription',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdBindDescriptorBufferEmbeddedSamplersEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdBindPipelineShaderGroupNV',
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetKHR',
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands_compute.cmdUpdatePipelineIndirectBufferNV'
newtype PipelineBindPoint = PipelineBindPoint Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PIPELINE_BIND_POINT_GRAPHICS' specifies binding as a graphics pipeline.
pattern PIPELINE_BIND_POINT_GRAPHICS = PipelineBindPoint 0

-- | 'PIPELINE_BIND_POINT_COMPUTE' specifies binding as a compute pipeline.
pattern PIPELINE_BIND_POINT_COMPUTE = PipelineBindPoint 1

-- | 'PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI' specifies binding as a
-- subpass shading pipeline.
pattern PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI = PipelineBindPoint 1000369003

-- | 'PIPELINE_BIND_POINT_RAY_TRACING_KHR' specifies binding as a ray tracing
-- pipeline.
pattern PIPELINE_BIND_POINT_RAY_TRACING_KHR = PipelineBindPoint 1000165000

-- | 'PIPELINE_BIND_POINT_EXECUTION_GRAPH_AMDX' specifies binding as an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#executiongraphs execution graph pipeline>.
pattern PIPELINE_BIND_POINT_EXECUTION_GRAPH_AMDX = PipelineBindPoint 1000134000

{-# COMPLETE
  PIPELINE_BIND_POINT_GRAPHICS
  , PIPELINE_BIND_POINT_COMPUTE
  , PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI
  , PIPELINE_BIND_POINT_RAY_TRACING_KHR
  , PIPELINE_BIND_POINT_EXECUTION_GRAPH_AMDX ::
    PipelineBindPoint
  #-}

conNamePipelineBindPoint :: String
conNamePipelineBindPoint = "PipelineBindPoint"

enumPrefixPipelineBindPoint :: String
enumPrefixPipelineBindPoint = "PIPELINE_BIND_POINT_"

showTablePipelineBindPoint :: [(PipelineBindPoint, String)]
showTablePipelineBindPoint =
  [ (PIPELINE_BIND_POINT_GRAPHICS, "GRAPHICS")
  , (PIPELINE_BIND_POINT_COMPUTE, "COMPUTE")
  ,
    ( PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI
    , "SUBPASS_SHADING_HUAWEI"
    )
  ,
    ( PIPELINE_BIND_POINT_RAY_TRACING_KHR
    , "RAY_TRACING_KHR"
    )
  ,
    ( PIPELINE_BIND_POINT_EXECUTION_GRAPH_AMDX
    , "EXECUTION_GRAPH_AMDX"
    )
  ]

instance Show PipelineBindPoint where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineBindPoint
      showTablePipelineBindPoint
      conNamePipelineBindPoint
      (\(PipelineBindPoint x) -> x)
      (showsPrec 11)

instance Read PipelineBindPoint where
  readPrec =
    enumReadPrec
      enumPrefixPipelineBindPoint
      showTablePipelineBindPoint
      conNamePipelineBindPoint
      PipelineBindPoint
