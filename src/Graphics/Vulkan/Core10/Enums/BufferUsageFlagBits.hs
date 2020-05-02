{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits  ( BufferUsageFlagBits( BUFFER_USAGE_TRANSFER_SRC_BIT
                                                                              , BUFFER_USAGE_TRANSFER_DST_BIT
                                                                              , BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
                                                                              , BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
                                                                              , BUFFER_USAGE_UNIFORM_BUFFER_BIT
                                                                              , BUFFER_USAGE_STORAGE_BUFFER_BIT
                                                                              , BUFFER_USAGE_INDEX_BUFFER_BIT
                                                                              , BUFFER_USAGE_VERTEX_BUFFER_BIT
                                                                              , BUFFER_USAGE_INDIRECT_BUFFER_BIT
                                                                              , BUFFER_USAGE_RAY_TRACING_BIT_KHR
                                                                              , BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
                                                                              , BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
                                                                              , BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
                                                                              , BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
                                                                              , ..
                                                                              )
                                                         , BufferUsageFlags
                                                         ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkBufferUsageFlagBits - Bitmask specifying allowed usage of a buffer
--
-- = See Also
--
-- 'BufferUsageFlags'
newtype BufferUsageFlagBits = BufferUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'BUFFER_USAGE_TRANSFER_SRC_BIT' specifies that the buffer /can/ be used
-- as the source of a /transfer command/ (see the definition of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer >).
pattern BUFFER_USAGE_TRANSFER_SRC_BIT = BufferUsageFlagBits 0x00000001
-- | 'BUFFER_USAGE_TRANSFER_DST_BIT' specifies that the buffer /can/ be used
-- as the destination of a transfer command.
pattern BUFFER_USAGE_TRANSFER_DST_BIT = BufferUsageFlagBits 0x00000002
-- | 'BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT' specifies that the buffer /can/
-- be used to create a 'Graphics.Vulkan.Core10.Handles.BufferView' suitable
-- for occupying a 'Graphics.Vulkan.Core10.Handles.DescriptorSet' slot of
-- type
-- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = BufferUsageFlagBits 0x00000004
-- | 'BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT' specifies that the buffer /can/
-- be used to create a 'Graphics.Vulkan.Core10.Handles.BufferView' suitable
-- for occupying a 'Graphics.Vulkan.Core10.Handles.DescriptorSet' slot of
-- type
-- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = BufferUsageFlagBits 0x00000008
-- | 'BUFFER_USAGE_UNIFORM_BUFFER_BIT' specifies that the buffer /can/ be
-- used in a 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorBufferInfo'
-- suitable for occupying a 'Graphics.Vulkan.Core10.Handles.DescriptorSet'
-- slot either of type
-- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
-- or
-- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'.
pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT = BufferUsageFlagBits 0x00000010
-- | 'BUFFER_USAGE_STORAGE_BUFFER_BIT' specifies that the buffer /can/ be
-- used in a 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorBufferInfo'
-- suitable for occupying a 'Graphics.Vulkan.Core10.Handles.DescriptorSet'
-- slot either of type
-- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
-- or
-- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'.
pattern BUFFER_USAGE_STORAGE_BUFFER_BIT = BufferUsageFlagBits 0x00000020
-- | 'BUFFER_USAGE_INDEX_BUFFER_BIT' specifies that the buffer is suitable
-- for passing as the @buffer@ parameter to
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
pattern BUFFER_USAGE_INDEX_BUFFER_BIT = BufferUsageFlagBits 0x00000040
-- | 'BUFFER_USAGE_VERTEX_BUFFER_BIT' specifies that the buffer is suitable
-- for passing as an element of the @pBuffers@ array to
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'.
pattern BUFFER_USAGE_VERTEX_BUFFER_BIT = BufferUsageFlagBits 0x00000080
-- | 'BUFFER_USAGE_INDIRECT_BUFFER_BIT' specifies that the buffer is suitable
-- for passing as the @buffer@ parameter to
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- or 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'.
-- It is also suitable for passing as the @buffer@ member of
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
-- or @sequencesCountBuffer@ or @sequencesIndexBuffer@ or
-- @preprocessedBuffer@ member of
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV'
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT = BufferUsageFlagBits 0x00000100
-- | 'BUFFER_USAGE_RAY_TRACING_BIT_KHR' specifies that the buffer is suitable
-- for use in
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.cmdTraceRaysKHR' and
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.cmdBuildAccelerationStructureKHR'.
pattern BUFFER_USAGE_RAY_TRACING_BIT_KHR = BufferUsageFlagBits 0x00000400
-- | 'BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT' specifies that the buffer
-- is suitable for passing as the @buffer@ parameter to
-- 'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.cmdBeginConditionalRenderingEXT'.
pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT = BufferUsageFlagBits 0x00000200
-- | 'BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT' specifies that
-- the buffer is suitable for using as a counter buffer with
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT'
-- and
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT'.
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = BufferUsageFlagBits 0x00001000
-- | 'BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT' specifies that the
-- buffer is suitable for using for binding as a transform feedback buffer
-- with
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT'.
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT = BufferUsageFlagBits 0x00000800
-- | 'BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT' specifies that the buffer /can/
-- be used to retrieve a buffer device address via
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
-- and use that address to access the buffer’s memory from a shader.
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT = BufferUsageFlagBits 0x00020000

type BufferUsageFlags = BufferUsageFlagBits

instance Show BufferUsageFlagBits where
  showsPrec p = \case
    BUFFER_USAGE_TRANSFER_SRC_BIT -> showString "BUFFER_USAGE_TRANSFER_SRC_BIT"
    BUFFER_USAGE_TRANSFER_DST_BIT -> showString "BUFFER_USAGE_TRANSFER_DST_BIT"
    BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT -> showString "BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
    BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT -> showString "BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
    BUFFER_USAGE_UNIFORM_BUFFER_BIT -> showString "BUFFER_USAGE_UNIFORM_BUFFER_BIT"
    BUFFER_USAGE_STORAGE_BUFFER_BIT -> showString "BUFFER_USAGE_STORAGE_BUFFER_BIT"
    BUFFER_USAGE_INDEX_BUFFER_BIT -> showString "BUFFER_USAGE_INDEX_BUFFER_BIT"
    BUFFER_USAGE_VERTEX_BUFFER_BIT -> showString "BUFFER_USAGE_VERTEX_BUFFER_BIT"
    BUFFER_USAGE_INDIRECT_BUFFER_BIT -> showString "BUFFER_USAGE_INDIRECT_BUFFER_BIT"
    BUFFER_USAGE_RAY_TRACING_BIT_KHR -> showString "BUFFER_USAGE_RAY_TRACING_BIT_KHR"
    BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT -> showString "BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT"
    BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT -> showString "BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
    BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT -> showString "BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
    BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT -> showString "BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT"
    BufferUsageFlagBits x -> showParen (p >= 11) (showString "BufferUsageFlagBits 0x" . showHex x)

instance Read BufferUsageFlagBits where
  readPrec = parens (choose [("BUFFER_USAGE_TRANSFER_SRC_BIT", pure BUFFER_USAGE_TRANSFER_SRC_BIT)
                            , ("BUFFER_USAGE_TRANSFER_DST_BIT", pure BUFFER_USAGE_TRANSFER_DST_BIT)
                            , ("BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT", pure BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT)
                            , ("BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT", pure BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT)
                            , ("BUFFER_USAGE_UNIFORM_BUFFER_BIT", pure BUFFER_USAGE_UNIFORM_BUFFER_BIT)
                            , ("BUFFER_USAGE_STORAGE_BUFFER_BIT", pure BUFFER_USAGE_STORAGE_BUFFER_BIT)
                            , ("BUFFER_USAGE_INDEX_BUFFER_BIT", pure BUFFER_USAGE_INDEX_BUFFER_BIT)
                            , ("BUFFER_USAGE_VERTEX_BUFFER_BIT", pure BUFFER_USAGE_VERTEX_BUFFER_BIT)
                            , ("BUFFER_USAGE_INDIRECT_BUFFER_BIT", pure BUFFER_USAGE_INDIRECT_BUFFER_BIT)
                            , ("BUFFER_USAGE_RAY_TRACING_BIT_KHR", pure BUFFER_USAGE_RAY_TRACING_BIT_KHR)
                            , ("BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT", pure BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT)
                            , ("BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT", pure BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT)
                            , ("BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT", pure BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT)
                            , ("BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT", pure BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "BufferUsageFlagBits")
                       v <- step readPrec
                       pure (BufferUsageFlagBits v)))

