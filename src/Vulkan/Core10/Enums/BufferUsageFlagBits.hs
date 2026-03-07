{-# language CPP #-}
-- No documentation found for Chapter "BufferUsageFlagBits"
module Vulkan.Core10.Enums.BufferUsageFlagBits  ( BufferUsageFlags
                                                , BufferUsageFlagBits( BUFFER_USAGE_TRANSFER_SRC_BIT
                                                                     , BUFFER_USAGE_TRANSFER_DST_BIT
                                                                     , BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
                                                                     , BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
                                                                     , BUFFER_USAGE_UNIFORM_BUFFER_BIT
                                                                     , BUFFER_USAGE_STORAGE_BUFFER_BIT
                                                                     , BUFFER_USAGE_INDEX_BUFFER_BIT
                                                                     , BUFFER_USAGE_VERTEX_BUFFER_BIT
                                                                     , BUFFER_USAGE_INDIRECT_BUFFER_BIT
                                                                     , BUFFER_USAGE_TILE_MEMORY_BIT_QCOM
                                                                     , BUFFER_USAGE_MICROMAP_STORAGE_BIT_EXT
                                                                     , BUFFER_USAGE_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT
                                                                     , BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT
                                                                     , BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT
                                                                     , BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT
                                                                     , BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR
                                                                     , BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
                                                                     , BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
                                                                     , BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT
                                                                     , BUFFER_USAGE_EXECUTION_GRAPH_SCRATCH_BIT_AMDX
                                                                     , BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
                                                                     , BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
                                                                     , BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
                                                                     , BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
                                                                     , ..
                                                                     )
                                                ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type BufferUsageFlags = BufferUsageFlagBits

-- | VkBufferUsageFlagBits - Bitmask specifying allowed usage of a buffer
--
-- = Description
--
-- -   'BUFFER_USAGE_TRANSFER_SRC_BIT' specifies that the buffer /can/ be
--     used as the source of a /transfer command/ (see the definition of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-pipeline-stages-transfer >).
--
-- -   'BUFFER_USAGE_TRANSFER_DST_BIT' specifies that the buffer /can/ be
--     used as the destination of a transfer command.
--
-- -   'BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT' specifies that the buffer
--     /can/ be used to create a 'Vulkan.Core10.Handles.BufferView'
--     suitable for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot
--     of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
--
-- -   'BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT' specifies that the buffer
--     /can/ be used to create a 'Vulkan.Core10.Handles.BufferView'
--     suitable for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot
--     of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
--
-- -   'BUFFER_USAGE_UNIFORM_BUFFER_BIT' specifies that the buffer /can/ be
--     used in a 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo'
--     suitable for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot
--     either of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'.
--
-- -   'BUFFER_USAGE_STORAGE_BUFFER_BIT' specifies that the buffer /can/ be
--     used in a 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo'
--     suitable for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot
--     either of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'.
--
-- -   'BUFFER_USAGE_INDEX_BUFFER_BIT' specifies that the buffer is
--     suitable for passing as the @buffer@ parameter to
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2'
--     and 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
--
-- -   'BUFFER_USAGE_VERTEX_BUFFER_BIT' specifies that the buffer is
--     suitable for passing as an element of the @pBuffers@ array to
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'.
--
-- -   'BUFFER_USAGE_INDIRECT_BUFFER_BIT' specifies that the buffer is
--     suitable for passing as the @buffer@ parameter to
--     'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
--     'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
--     'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
--     'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
--     'Vulkan.Extensions.VK_EXT_mesh_shader.cmdDrawMeshTasksIndirectEXT',
--     'Vulkan.Extensions.VK_EXT_mesh_shader.cmdDrawMeshTasksIndirectCountEXT',
--     'Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader.cmdDrawClusterIndirectHUAWEI',
--     or 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'. It is
--     also suitable for passing as the @buffer@ member of
--     'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
--     or @sequencesCountBuffer@ or @sequencesIndexBuffer@ or
--     @preprocessedBuffer@ member of
--     'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV'.
--     It is also suitable for passing as the underlying buffer of either
--     the @preprocessAddress@ or @sequenceCountAddress@ members of
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.GeneratedCommandsInfoEXT'.
--
-- -   'BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT' specifies that the
--     buffer is suitable for passing as the @buffer@ parameter to
--     'Vulkan.Extensions.VK_EXT_conditional_rendering.cmdBeginConditionalRenderingEXT'.
--
-- -   'BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT' specifies that the
--     buffer is suitable for using for binding as a transform feedback
--     buffer with
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT'.
--
-- -   'BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT' specifies
--     that the buffer is suitable for using as a counter buffer with
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT'.
--
-- -   'BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT' specifies that the
--     buffer is suitable to contain sampler and combined image sampler
--     descriptors when bound as a descriptor buffer. Buffers containing
--     combined image sampler descriptors /must/ also specify
--     'BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'.
--
-- -   'BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT' specifies that the
--     buffer is suitable to contain resource descriptors when bound as a
--     descriptor buffer.
--
-- -   'BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT' specifies
--     that the buffer, when bound, /can/ be used by the implementation to
--     support push descriptors when using descriptor buffers.
--
-- -   'BUFFER_USAGE_TILE_MEMORY_BIT_QCOM' specifies that the buffer /can/
--     be bound to 'Vulkan.Core10.Handles.DeviceMemory' allocated from a
--     'Vulkan.Core10.DeviceInitialization.MemoryHeap' with the
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
--     property.
--
-- -   'Vulkan.Extensions.VK_NV_ray_tracing.BUFFER_USAGE_RAY_TRACING_BIT_NV'
--     specifies that the buffer is suitable for use in
--     'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV'.
--
-- -   'BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR' specifies that the
--     buffer is suitable for use as a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shader-binding-table Shader Binding Table>.
--
-- -   'BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR'
--     specifies that the buffer is suitable for use as a read-only input
--     to an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#acceleration-structure-building acceleration structure build>.
--
-- -   'BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR' specifies that
--     the buffer is suitable for storage space for a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR'.
--
-- -   'BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT' specifies that the buffer
--     /can/ be used to retrieve a buffer device address via
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--     and use that address to access the buffer’s memory from a shader.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkBufferUsageFlagBits VK_BUFFER_USAGE_VIDEO_DECODE_SRC_BIT_KHR>
--     specifies that the buffer /can/ be used as the source video
--     bitstream buffer in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-decode-operations video decode operation>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkBufferUsageFlagBits VK_BUFFER_USAGE_VIDEO_DECODE_DST_BIT_KHR>
--     is reserved for future use.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkBufferUsageFlagBits VK_BUFFER_USAGE_VIDEO_ENCODE_DST_BIT_KHR>
--     specifies that the buffer /can/ be used as the destination video
--     bitstream buffer in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-encode-operations video encode operation>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkBufferUsageFlagBits VK_BUFFER_USAGE_VIDEO_ENCODE_SRC_BIT_KHR>
--     is reserved for future use.
--
-- -   'BUFFER_USAGE_EXECUTION_GRAPH_SCRATCH_BIT_AMDX' specifies that the
--     buffer /can/ be used for as scratch memory for
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#executiongraphs execution graph dispatch>.
--
-- -   'BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT' specifies that the buffer
--     /can/ be used as a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps descriptor heap>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'BufferUsageFlags'
newtype BufferUsageFlagBits = BufferUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFER_SRC_BIT"
pattern BUFFER_USAGE_TRANSFER_SRC_BIT = BufferUsageFlagBits 0x00000001

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFER_DST_BIT"
pattern BUFFER_USAGE_TRANSFER_DST_BIT = BufferUsageFlagBits 0x00000002

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = BufferUsageFlagBits 0x00000004

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = BufferUsageFlagBits 0x00000008

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT"
pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT = BufferUsageFlagBits 0x00000010

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_STORAGE_BUFFER_BIT"
pattern BUFFER_USAGE_STORAGE_BUFFER_BIT = BufferUsageFlagBits 0x00000020

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_INDEX_BUFFER_BIT"
pattern BUFFER_USAGE_INDEX_BUFFER_BIT = BufferUsageFlagBits 0x00000040

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_VERTEX_BUFFER_BIT"
pattern BUFFER_USAGE_VERTEX_BUFFER_BIT = BufferUsageFlagBits 0x00000080

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT"
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT = BufferUsageFlagBits 0x00000100

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TILE_MEMORY_BIT_QCOM"
pattern BUFFER_USAGE_TILE_MEMORY_BIT_QCOM = BufferUsageFlagBits 0x08000000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_MICROMAP_STORAGE_BIT_EXT"
pattern BUFFER_USAGE_MICROMAP_STORAGE_BIT_EXT = BufferUsageFlagBits 0x01000000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT"
pattern BUFFER_USAGE_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT = BufferUsageFlagBits 0x00800000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT = BufferUsageFlagBits 0x04000000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT = BufferUsageFlagBits 0x00400000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT = BufferUsageFlagBits 0x00200000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR"
pattern BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR = BufferUsageFlagBits 0x00000400

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR"
pattern BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR = BufferUsageFlagBits 0x00100000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR"
pattern BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR = BufferUsageFlagBits 0x00080000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT"
pattern BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT = BufferUsageFlagBits 0x10000000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_EXECUTION_GRAPH_SCRATCH_BIT_AMDX"
pattern BUFFER_USAGE_EXECUTION_GRAPH_SCRATCH_BIT_AMDX = BufferUsageFlagBits 0x02000000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT = BufferUsageFlagBits 0x00000200

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = BufferUsageFlagBits 0x00001000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT = BufferUsageFlagBits 0x00000800

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT"
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT = BufferUsageFlagBits 0x00020000

conNameBufferUsageFlagBits :: String
conNameBufferUsageFlagBits = "BufferUsageFlagBits"

enumPrefixBufferUsageFlagBits :: String
enumPrefixBufferUsageFlagBits = "BUFFER_USAGE_"

showTableBufferUsageFlagBits :: [(BufferUsageFlagBits, String)]
showTableBufferUsageFlagBits =
  [
    ( BUFFER_USAGE_TRANSFER_SRC_BIT
    , "TRANSFER_SRC_BIT"
    )
  ,
    ( BUFFER_USAGE_TRANSFER_DST_BIT
    , "TRANSFER_DST_BIT"
    )
  ,
    ( BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
    , "UNIFORM_TEXEL_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
    , "STORAGE_TEXEL_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_UNIFORM_BUFFER_BIT
    , "UNIFORM_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_STORAGE_BUFFER_BIT
    , "STORAGE_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_INDEX_BUFFER_BIT
    , "INDEX_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_VERTEX_BUFFER_BIT
    , "VERTEX_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_INDIRECT_BUFFER_BIT
    , "INDIRECT_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_TILE_MEMORY_BIT_QCOM
    , "TILE_MEMORY_BIT_QCOM"
    )
  ,
    ( BUFFER_USAGE_MICROMAP_STORAGE_BIT_EXT
    , "MICROMAP_STORAGE_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT
    , "MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT
    , "PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT
    , "RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT
    , "SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR
    , "SHADER_BINDING_TABLE_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
    , "ACCELERATION_STRUCTURE_STORAGE_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
    , "ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT
    , "DESCRIPTOR_HEAP_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_EXECUTION_GRAPH_SCRATCH_BIT_AMDX
    , "EXECUTION_GRAPH_SCRATCH_BIT_AMDX"
    )
  ,
    ( BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
    , "CONDITIONAL_RENDERING_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
    , "TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
    , "TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
    , "SHADER_DEVICE_ADDRESS_BIT"
    )
  ]

instance Show BufferUsageFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixBufferUsageFlagBits
      showTableBufferUsageFlagBits
      conNameBufferUsageFlagBits
      (\(BufferUsageFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read BufferUsageFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixBufferUsageFlagBits
      showTableBufferUsageFlagBits
      conNameBufferUsageFlagBits
      BufferUsageFlagBits
