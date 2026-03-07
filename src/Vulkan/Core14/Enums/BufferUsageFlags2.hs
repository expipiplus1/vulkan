{-# language CPP #-}
-- No documentation found for Chapter "BufferUsageFlags2"
module Vulkan.Core14.Enums.BufferUsageFlags2  ( BufferUsageFlags2
                                              , BufferUsageFlagBits2( BUFFER_USAGE_2_TRANSFER_SRC_BIT
                                                                    , BUFFER_USAGE_2_TRANSFER_DST_BIT
                                                                    , BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT
                                                                    , BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT
                                                                    , BUFFER_USAGE_2_UNIFORM_BUFFER_BIT
                                                                    , BUFFER_USAGE_2_STORAGE_BUFFER_BIT
                                                                    , BUFFER_USAGE_2_INDEX_BUFFER_BIT
                                                                    , BUFFER_USAGE_2_VERTEX_BUFFER_BIT
                                                                    , BUFFER_USAGE_2_INDIRECT_BUFFER_BIT
                                                                    , BUFFER_USAGE_2_PREPROCESS_BUFFER_BIT_EXT
                                                                    , BUFFER_USAGE_2_MEMORY_DECOMPRESSION_BIT_EXT
                                                                    , BUFFER_USAGE_2_TILE_MEMORY_BIT_QCOM
                                                                    , BUFFER_USAGE_2_DATA_GRAPH_FOREIGN_DESCRIPTOR_BIT_ARM
                                                                    , BUFFER_USAGE_2_COMPRESSED_DATA_DGF1_BIT_AMDX
                                                                    , BUFFER_USAGE_2_MICROMAP_STORAGE_BIT_EXT
                                                                    , BUFFER_USAGE_2_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT
                                                                    , BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT
                                                                    , BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT
                                                                    , BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT
                                                                    , BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
                                                                    , BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
                                                                    , BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR
                                                                    , BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR
                                                                    , BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR
                                                                    , BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR
                                                                    , BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
                                                                    , BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
                                                                    , BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR
                                                                    , BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT
                                                                    , BUFFER_USAGE_2_DESCRIPTOR_HEAP_BIT_EXT
                                                                    , BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX
                                                                    , BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT
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
import Vulkan.Core10.FundamentalTypes (Flags64)
type BufferUsageFlags2 = BufferUsageFlagBits2

-- | VkBufferUsageFlagBits2 - Bitmask controlling how a pipeline is created
--
-- = Description
--
-- -   'BUFFER_USAGE_2_TRANSFER_SRC_BIT' specifies that the buffer /can/ be
--     used as the source of a /transfer command/ (see the definition of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-pipeline-stages-transfer >).
--
-- -   'BUFFER_USAGE_2_TRANSFER_DST_BIT' specifies that the buffer /can/ be
--     used as the destination of a transfer command.
--
-- -   'BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT' specifies that the buffer
--     /can/ be used to create a 'Vulkan.Core10.Handles.BufferView'
--     suitable for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot
--     of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
--
-- -   'BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT' specifies that the buffer
--     /can/ be used to create a 'Vulkan.Core10.Handles.BufferView'
--     suitable for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot
--     of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
--
-- -   'BUFFER_USAGE_2_UNIFORM_BUFFER_BIT' specifies that the buffer /can/
--     be used in a 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo'
--     suitable for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot
--     either of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'.
--
-- -   'BUFFER_USAGE_2_STORAGE_BUFFER_BIT' specifies that the buffer /can/
--     be used in a 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo'
--     suitable for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot
--     either of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'.
--
-- -   'BUFFER_USAGE_2_INDEX_BUFFER_BIT' specifies that the buffer is
--     suitable for passing as the @buffer@ parameter to
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2'
--     and 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
--
-- -   'BUFFER_USAGE_2_VERTEX_BUFFER_BIT' specifies that the buffer is
--     suitable for passing as an element of the @pBuffers@ array to
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'.
--
-- -   'BUFFER_USAGE_2_INDIRECT_BUFFER_BIT' specifies that the buffer is
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
-- -   'BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT' specifies that the
--     buffer is suitable for passing as the @buffer@ parameter to
--     'Vulkan.Extensions.VK_EXT_conditional_rendering.cmdBeginConditionalRenderingEXT'.
--
-- -   'BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT' specifies that
--     the buffer is suitable for using for binding as a transform feedback
--     buffer with
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT'.
--
-- -   'BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT' specifies
--     that the buffer is suitable for using as a counter buffer with
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT'.
--
-- -   'BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT' specifies that
--     the buffer is suitable to contain sampler and combined image sampler
--     descriptors when bound as a descriptor buffer. Buffers containing
--     combined image sampler descriptors /must/ also specify
--     'BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'.
--
-- -   'BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT' specifies that
--     the buffer is suitable to contain resource descriptors when bound as
--     a descriptor buffer.
--
-- -   'BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT'
--     specifies that the buffer, when bound, /can/ be used by the
--     implementation to support push descriptors when using descriptor
--     buffers.
--
-- -   'BUFFER_USAGE_2_TILE_MEMORY_BIT_QCOM' specifies that the buffer
--     /can/ be bound to 'Vulkan.Core10.Handles.DeviceMemory' allocated
--     from a 'Vulkan.Core10.DeviceInitialization.MemoryHeap' with the
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
--     property.
--
-- -   'Vulkan.Extensions.VK_KHR_maintenance5.BUFFER_USAGE_2_RAY_TRACING_BIT_NV'
--     specifies that the buffer is suitable for use in
--     'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV'.
--
-- -   'BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR' specifies that the
--     buffer is suitable for use as a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shader-binding-table Shader Binding Table>.
--
-- -   'BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR'
--     specifies that the buffer is suitable for use as a read-only input
--     to an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#acceleration-structure-building acceleration structure build>.
--
-- -   'BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR' specifies
--     that the buffer is suitable for storage space for a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR'.
--
-- -   'BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT' specifies that the buffer
--     /can/ be used to retrieve a buffer device address via
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--     and use that address to access the buffer’s memory from a shader.
--
-- -   'BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR' specifies that the buffer
--     /can/ be used as the source video bitstream buffer in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-decode-operations video decode operation>.
--
-- -   'BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR' is reserved for future
--     use.
--
-- -   'BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR' specifies that the buffer
--     /can/ be used as the destination video bitstream buffer in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-encode-operations video encode operation>.
--
-- -   'BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR' is reserved for future
--     use.
--
-- -   'BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX' specifies that the
--     buffer /can/ be used for as scratch memory for
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#executiongraphs execution graph dispatch>.
--
-- -   'BUFFER_USAGE_2_PREPROCESS_BUFFER_BIT_EXT' specifies that the buffer
--     /can/ be used as a preprocess buffer for
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#device-generated-commands Device-Generated Commands>.
--
-- -   'BUFFER_USAGE_2_COMPRESSED_DATA_DGF1_BIT_AMDX' specifies that the
--     buffer is suitable as storage space for
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#dense-geometry-format Dense Geometry Format>
--     data.
--
-- -   'BUFFER_USAGE_2_DATA_GRAPH_FOREIGN_DESCRIPTOR_BIT_ARM' specifies
--     that the buffer is suitable to contain resource descriptors when
--     bound as a descriptor buffer in command buffers allocated from a
--     command pool that /can/ target foreign
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#graphs-processing-engines data graph processing engines>.
--
-- -   'BUFFER_USAGE_2_MEMORY_DECOMPRESSION_BIT_EXT' specifies that the
--     buffer /can/ be used as a destination buffer in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-decompression memory decompression>.
--
-- -   'BUFFER_USAGE_2_DESCRIPTOR_HEAP_BIT_EXT' specifies that the buffer
--     /can/ be used as a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps descriptor heap>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'BufferUsageFlags2'
newtype BufferUsageFlagBits2 = BufferUsageFlagBits2 Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_TRANSFER_SRC_BIT"
pattern BUFFER_USAGE_2_TRANSFER_SRC_BIT = BufferUsageFlagBits2 0x0000000000000001

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_TRANSFER_DST_BIT"
pattern BUFFER_USAGE_2_TRANSFER_DST_BIT = BufferUsageFlagBits2 0x0000000000000002

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT"
pattern BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT = BufferUsageFlagBits2 0x0000000000000004

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT"
pattern BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT = BufferUsageFlagBits2 0x0000000000000008

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_UNIFORM_BUFFER_BIT"
pattern BUFFER_USAGE_2_UNIFORM_BUFFER_BIT = BufferUsageFlagBits2 0x0000000000000010

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_STORAGE_BUFFER_BIT"
pattern BUFFER_USAGE_2_STORAGE_BUFFER_BIT = BufferUsageFlagBits2 0x0000000000000020

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_INDEX_BUFFER_BIT"
pattern BUFFER_USAGE_2_INDEX_BUFFER_BIT = BufferUsageFlagBits2 0x0000000000000040

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_VERTEX_BUFFER_BIT"
pattern BUFFER_USAGE_2_VERTEX_BUFFER_BIT = BufferUsageFlagBits2 0x0000000000000080

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_INDIRECT_BUFFER_BIT"
pattern BUFFER_USAGE_2_INDIRECT_BUFFER_BIT = BufferUsageFlagBits2 0x0000000000000100

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_PREPROCESS_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_2_PREPROCESS_BUFFER_BIT_EXT = BufferUsageFlagBits2 0x0000000080000000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_MEMORY_DECOMPRESSION_BIT_EXT"
pattern BUFFER_USAGE_2_MEMORY_DECOMPRESSION_BIT_EXT = BufferUsageFlagBits2 0x0000000100000000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_TILE_MEMORY_BIT_QCOM"
pattern BUFFER_USAGE_2_TILE_MEMORY_BIT_QCOM = BufferUsageFlagBits2 0x0000000008000000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_DATA_GRAPH_FOREIGN_DESCRIPTOR_BIT_ARM"
pattern BUFFER_USAGE_2_DATA_GRAPH_FOREIGN_DESCRIPTOR_BIT_ARM = BufferUsageFlagBits2 0x0000000020000000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_COMPRESSED_DATA_DGF1_BIT_AMDX"
pattern BUFFER_USAGE_2_COMPRESSED_DATA_DGF1_BIT_AMDX = BufferUsageFlagBits2 0x0000000200000000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_MICROMAP_STORAGE_BIT_EXT"
pattern BUFFER_USAGE_2_MICROMAP_STORAGE_BIT_EXT = BufferUsageFlagBits2 0x0000000001000000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT"
pattern BUFFER_USAGE_2_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT = BufferUsageFlagBits2 0x0000000000800000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT = BufferUsageFlagBits2 0x0000000004000000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT = BufferUsageFlagBits2 0x0000000000400000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT = BufferUsageFlagBits2 0x0000000000200000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR"
pattern BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR = BufferUsageFlagBits2 0x0000000000100000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR"
pattern BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR = BufferUsageFlagBits2 0x0000000000080000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR"
pattern BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR = BufferUsageFlagBits2 0x0000000000010000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR"
pattern BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR = BufferUsageFlagBits2 0x0000000000008000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR"
pattern BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR = BufferUsageFlagBits2 0x0000000000004000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR"
pattern BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR = BufferUsageFlagBits2 0x0000000000002000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = BufferUsageFlagBits2 0x0000000000001000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT = BufferUsageFlagBits2 0x0000000000000800

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR"
pattern BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR = BufferUsageFlagBits2 0x0000000000000400

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT"
pattern BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT = BufferUsageFlagBits2 0x0000000000000200

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_DESCRIPTOR_HEAP_BIT_EXT"
pattern BUFFER_USAGE_2_DESCRIPTOR_HEAP_BIT_EXT = BufferUsageFlagBits2 0x0000000010000000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX"
pattern BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX = BufferUsageFlagBits2 0x0000000002000000

-- No documentation found for Nested "VkBufferUsageFlagBits2" "VK_BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT"
pattern BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT = BufferUsageFlagBits2 0x0000000000020000

conNameBufferUsageFlagBits2 :: String
conNameBufferUsageFlagBits2 = "BufferUsageFlagBits2"

enumPrefixBufferUsageFlagBits2 :: String
enumPrefixBufferUsageFlagBits2 = "BUFFER_USAGE_2_"

showTableBufferUsageFlagBits2 :: [(BufferUsageFlagBits2, String)]
showTableBufferUsageFlagBits2 =
  [
    ( BUFFER_USAGE_2_TRANSFER_SRC_BIT
    , "TRANSFER_SRC_BIT"
    )
  ,
    ( BUFFER_USAGE_2_TRANSFER_DST_BIT
    , "TRANSFER_DST_BIT"
    )
  ,
    ( BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT
    , "UNIFORM_TEXEL_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT
    , "STORAGE_TEXEL_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_2_UNIFORM_BUFFER_BIT
    , "UNIFORM_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_2_STORAGE_BUFFER_BIT
    , "STORAGE_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_2_INDEX_BUFFER_BIT
    , "INDEX_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_2_VERTEX_BUFFER_BIT
    , "VERTEX_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_2_INDIRECT_BUFFER_BIT
    , "INDIRECT_BUFFER_BIT"
    )
  ,
    ( BUFFER_USAGE_2_PREPROCESS_BUFFER_BIT_EXT
    , "PREPROCESS_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_MEMORY_DECOMPRESSION_BIT_EXT
    , "MEMORY_DECOMPRESSION_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_TILE_MEMORY_BIT_QCOM
    , "TILE_MEMORY_BIT_QCOM"
    )
  ,
    ( BUFFER_USAGE_2_DATA_GRAPH_FOREIGN_DESCRIPTOR_BIT_ARM
    , "DATA_GRAPH_FOREIGN_DESCRIPTOR_BIT_ARM"
    )
  ,
    ( BUFFER_USAGE_2_COMPRESSED_DATA_DGF1_BIT_AMDX
    , "COMPRESSED_DATA_DGF1_BIT_AMDX"
    )
  ,
    ( BUFFER_USAGE_2_MICROMAP_STORAGE_BIT_EXT
    , "MICROMAP_STORAGE_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT
    , "MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT
    , "PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT
    , "RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT
    , "SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
    , "ACCELERATION_STRUCTURE_STORAGE_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
    , "ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR
    , "VIDEO_ENCODE_SRC_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR
    , "VIDEO_ENCODE_DST_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR
    , "VIDEO_DECODE_DST_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR
    , "VIDEO_DECODE_SRC_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
    , "TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
    , "TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR
    , "SHADER_BINDING_TABLE_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT
    , "CONDITIONAL_RENDERING_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_DESCRIPTOR_HEAP_BIT_EXT
    , "DESCRIPTOR_HEAP_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX
    , "EXECUTION_GRAPH_SCRATCH_BIT_AMDX"
    )
  ,
    ( BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT
    , "SHADER_DEVICE_ADDRESS_BIT"
    )
  ]

instance Show BufferUsageFlagBits2 where
  showsPrec =
    enumShowsPrec
      enumPrefixBufferUsageFlagBits2
      showTableBufferUsageFlagBits2
      conNameBufferUsageFlagBits2
      (\(BufferUsageFlagBits2 x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read BufferUsageFlagBits2 where
  readPrec =
    enumReadPrec
      enumPrefixBufferUsageFlagBits2
      showTableBufferUsageFlagBits2
      conNameBufferUsageFlagBits2
      BufferUsageFlagBits2
