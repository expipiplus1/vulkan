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
                                                                     , BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR
                                                                     , BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
                                                                     , BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
                                                                     , BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
                                                                     , BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
                                                                     , BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
                                                                     , BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
                                                                     , ..
                                                                     )
                                                ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type BufferUsageFlags = BufferUsageFlagBits

-- | VkBufferUsageFlagBits - Bitmask specifying allowed usage of a buffer
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'BufferUsageFlags'
newtype BufferUsageFlagBits = BufferUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'BUFFER_USAGE_TRANSFER_SRC_BIT' specifies that the buffer /can/ be used
-- as the source of a /transfer command/ (see the definition of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer >).
pattern BUFFER_USAGE_TRANSFER_SRC_BIT                          = BufferUsageFlagBits 0x00000001
-- | 'BUFFER_USAGE_TRANSFER_DST_BIT' specifies that the buffer /can/ be used
-- as the destination of a transfer command.
pattern BUFFER_USAGE_TRANSFER_DST_BIT                          = BufferUsageFlagBits 0x00000002
-- | 'BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT' specifies that the buffer /can/
-- be used to create a 'Vulkan.Core10.Handles.BufferView' suitable for
-- occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT                  = BufferUsageFlagBits 0x00000004
-- | 'BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT' specifies that the buffer /can/
-- be used to create a 'Vulkan.Core10.Handles.BufferView' suitable for
-- occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT                  = BufferUsageFlagBits 0x00000008
-- | 'BUFFER_USAGE_UNIFORM_BUFFER_BIT' specifies that the buffer /can/ be
-- used in a 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo' suitable
-- for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot either of
-- type 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
-- or
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'.
pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT                        = BufferUsageFlagBits 0x00000010
-- | 'BUFFER_USAGE_STORAGE_BUFFER_BIT' specifies that the buffer /can/ be
-- used in a 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo' suitable
-- for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot either of
-- type 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
-- or
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'.
pattern BUFFER_USAGE_STORAGE_BUFFER_BIT                        = BufferUsageFlagBits 0x00000020
-- | 'BUFFER_USAGE_INDEX_BUFFER_BIT' specifies that the buffer is suitable
-- for passing as the @buffer@ parameter to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
pattern BUFFER_USAGE_INDEX_BUFFER_BIT                          = BufferUsageFlagBits 0x00000040
-- | 'BUFFER_USAGE_VERTEX_BUFFER_BIT' specifies that the buffer is suitable
-- for passing as an element of the @pBuffers@ array to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'.
pattern BUFFER_USAGE_VERTEX_BUFFER_BIT                         = BufferUsageFlagBits 0x00000080
-- | 'BUFFER_USAGE_INDIRECT_BUFFER_BIT' specifies that the buffer is suitable
-- for passing as the @buffer@ parameter to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- or 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'. It is also
-- suitable for passing as the @buffer@ member of
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
-- or @sequencesCountBuffer@ or @sequencesIndexBuffer@ or
-- @preprocessedBuffer@ member of
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV'
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT                       = BufferUsageFlagBits 0x00000100
-- | 'BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR' specifies that the buffer is
-- suitable for use as a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shader-binding-table Shader Binding Table>.
pattern BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR              = BufferUsageFlagBits 0x00000400
-- | 'BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR' specifies that the
-- buffer is suitable for storage space for a
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR'.
pattern BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR    = BufferUsageFlagBits 0x00100000
-- | 'BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR'
-- specifies that the buffer is suitable for use as a read-only input to an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-building acceleration structure build>.
pattern BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR = BufferUsageFlagBits 0x00080000
-- | 'BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT' specifies that the buffer
-- is suitable for passing as the @buffer@ parameter to
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.cmdBeginConditionalRenderingEXT'.
pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT             = BufferUsageFlagBits 0x00000200
-- | 'BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT' specifies that
-- the buffer is suitable for using as a counter buffer with
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT'
-- and
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT'.
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = BufferUsageFlagBits 0x00001000
-- | 'BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT' specifies that the
-- buffer is suitable for using for binding as a transform feedback buffer
-- with
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT'.
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT         = BufferUsageFlagBits 0x00000800
-- | 'BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT' specifies that the buffer /can/
-- be used to retrieve a buffer device address via
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
-- and use that address to access the buffer’s memory from a shader.
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT                 = BufferUsageFlagBits 0x00020000

conNameBufferUsageFlagBits :: String
conNameBufferUsageFlagBits = "BufferUsageFlagBits"

enumPrefixBufferUsageFlagBits :: String
enumPrefixBufferUsageFlagBits = "BUFFER_USAGE_"

showTableBufferUsageFlagBits :: [(BufferUsageFlagBits, String)]
showTableBufferUsageFlagBits =
  [ (BUFFER_USAGE_TRANSFER_SRC_BIT                      , "TRANSFER_SRC_BIT")
  , (BUFFER_USAGE_TRANSFER_DST_BIT                      , "TRANSFER_DST_BIT")
  , (BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT              , "UNIFORM_TEXEL_BUFFER_BIT")
  , (BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT              , "STORAGE_TEXEL_BUFFER_BIT")
  , (BUFFER_USAGE_UNIFORM_BUFFER_BIT                    , "UNIFORM_BUFFER_BIT")
  , (BUFFER_USAGE_STORAGE_BUFFER_BIT                    , "STORAGE_BUFFER_BIT")
  , (BUFFER_USAGE_INDEX_BUFFER_BIT                      , "INDEX_BUFFER_BIT")
  , (BUFFER_USAGE_VERTEX_BUFFER_BIT                     , "VERTEX_BUFFER_BIT")
  , (BUFFER_USAGE_INDIRECT_BUFFER_BIT                   , "INDIRECT_BUFFER_BIT")
  , (BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR          , "SHADER_BINDING_TABLE_BIT_KHR")
  , (BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR, "ACCELERATION_STRUCTURE_STORAGE_BIT_KHR")
  , ( BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
    , "ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR"
    )
  , (BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT            , "CONDITIONAL_RENDERING_BIT_EXT")
  , (BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT, "TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT")
  , (BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT        , "TRANSFORM_FEEDBACK_BUFFER_BIT_EXT")
  , (BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT                , "SHADER_DEVICE_ADDRESS_BIT")
  ]

instance Show BufferUsageFlagBits where
  showsPrec = enumShowsPrec enumPrefixBufferUsageFlagBits
                            showTableBufferUsageFlagBits
                            conNameBufferUsageFlagBits
                            (\(BufferUsageFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read BufferUsageFlagBits where
  readPrec = enumReadPrec enumPrefixBufferUsageFlagBits
                          showTableBufferUsageFlagBits
                          conNameBufferUsageFlagBits
                          BufferUsageFlagBits

