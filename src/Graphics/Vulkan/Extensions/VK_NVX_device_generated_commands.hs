{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( VkIndirectCommandsTokenTypeNVX(..)
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX
  , VkObjectEntryTypeNVX(..)
  , pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
  , VkIndirectCommandsLayoutUsageFlagBitsNVX(..)
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
  , VkObjectEntryUsageFlagBitsNVX(..)
  , pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
  , pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
  , pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX
  , pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX
  , pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX
  , pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
  , pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  , VkObjectTableNVX
  , VkIndirectCommandsLayoutNVX
  , vkCmdProcessCommandsNVX
  , vkCmdReserveSpaceForCommandsNVX
  , vkCreateIndirectCommandsLayoutNVX
  , vkDestroyIndirectCommandsLayoutNVX
  , vkCreateObjectTableNVX
  , vkDestroyObjectTableNVX
  , vkRegisterObjectsNVX
  , vkUnregisterObjectsNVX
  , vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsTokenNVX(..)
  , VkIndirectCommandsLayoutTokenNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkCmdProcessCommandsInfoNVX(..)
  , VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkObjectTableCreateInfoNVX(..)
  , VkObjectTableEntryNVX(..)
  , VkObjectTablePipelineEntryNVX(..)
  , VkObjectTableDescriptorSetEntryNVX(..)
  , VkObjectTableVertexBufferEntryNVX(..)
  , VkObjectTableIndexBufferEntryNVX(..)
  , VkObjectTablePushConstantEntryNVX(..)
  , VkIndirectCommandsLayoutUsageFlagsNVX
  , VkObjectEntryUsageFlagsNVX
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
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
import Graphics.Vulkan.NamedType
  ( (:::)
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


import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( VkIndexType(..)
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.Core10.Pass
  ( VkAccessFlagBits(..)
  , VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkPipeline
  , VkPipelineLayout
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )


-- ** VkIndirectCommandsTokenTypeNVX

-- | VkIndirectCommandsTokenTypeNVX - Enum specifying
--
-- = Description
--
-- \'
--
-- +------------------------------------------------------+----------------------------+
-- | Token type                                           | Equivalent command         |
-- +======================================================+============================+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX@       | @vkCmdBindPipeline@        |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX@ | @vkCmdBindDescriptorSets@  |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX@   | @vkCmdBindIndexBuffer@     |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX@  | @vkCmdBindVertexBuffers@   |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX@  | @vkCmdPushConstants@       |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX@   | @vkCmdDrawIndexedIndirect@ |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX@           | @vkCmdDrawIndirect@        |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX@       | @vkCmdDispatchIndirect@    |
-- +------------------------------------------------------+----------------------------+
--
-- Supported indirect command tokens
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutTokenNVX', 'VkIndirectCommandsTokenNVX'
newtype VkIndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX Int32
  deriving (Eq, Ord, Storable)

instance Show VkIndirectCommandsTokenTypeNVX where
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
  showsPrec p (VkIndirectCommandsTokenTypeNVX x) = showParen (p >= 11) (showString "VkIndirectCommandsTokenTypeNVX " . showsPrec 11 x)

instance Read VkIndirectCommandsTokenTypeNVX where
  readPrec = parens ( choose [ ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX",       pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX",   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX",  pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX",  pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX",   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX",           pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX",       pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIndirectCommandsTokenTypeNVX")
                        v <- step readPrec
                        pure (VkIndirectCommandsTokenTypeNVX v)
                        )
                    )

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX = VkIndirectCommandsTokenTypeNVX 0

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX = VkIndirectCommandsTokenTypeNVX 1

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX = VkIndirectCommandsTokenTypeNVX 2

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX = VkIndirectCommandsTokenTypeNVX 3

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX = VkIndirectCommandsTokenTypeNVX 4

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX = VkIndirectCommandsTokenTypeNVX 5

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX = VkIndirectCommandsTokenTypeNVX 6

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX = VkIndirectCommandsTokenTypeNVX 7
-- ** VkObjectEntryTypeNVX

-- | VkObjectEntryTypeNVX - Enum specifying object table entry type
--
-- = See Also
--
-- 'VkObjectTableCreateInfoNVX', 'VkObjectTableDescriptorSetEntryNVX',
-- 'VkObjectTableEntryNVX', 'VkObjectTableIndexBufferEntryNVX',
-- 'VkObjectTablePipelineEntryNVX', 'VkObjectTablePushConstantEntryNVX',
-- 'VkObjectTableVertexBufferEntryNVX', 'vkUnregisterObjectsNVX'
newtype VkObjectEntryTypeNVX = VkObjectEntryTypeNVX Int32
  deriving (Eq, Ord, Storable)

instance Show VkObjectEntryTypeNVX where
  showsPrec _ VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = showString "VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = showString "VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = showString "VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = showString "VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = showString "VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX"
  showsPrec p (VkObjectEntryTypeNVX x) = showParen (p >= 11) (showString "VkObjectEntryTypeNVX " . showsPrec 11 x)

instance Read VkObjectEntryTypeNVX where
  readPrec = parens ( choose [ ("VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX", pure VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX",       pure VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX",   pure VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX",  pure VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX",  pure VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkObjectEntryTypeNVX")
                        v <- step readPrec
                        pure (VkObjectEntryTypeNVX v)
                        )
                    )

-- | @VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX@ specifies a @VkDescriptorSet@
-- resource entry that is registered via
-- @VkObjectTableDescriptorSetEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = VkObjectEntryTypeNVX 0

-- | @VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX@ specifies a @VkPipeline@ resource
-- entry that is registered via @VkObjectTablePipelineEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = VkObjectEntryTypeNVX 1

-- | @VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX@ specifies a @VkBuffer@ resource
-- entry that is registered via @VkObjectTableIndexBufferEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = VkObjectEntryTypeNVX 2

-- | @VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX@ specifies a @VkBuffer@ resource
-- entry that is registered via @VkObjectTableVertexBufferEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = VkObjectEntryTypeNVX 3

-- | @VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX@ specifies the resource entry is
-- registered via @VkObjectTablePushConstantEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = VkObjectEntryTypeNVX 4
-- ** VkIndirectCommandsLayoutUsageFlagBitsNVX

-- | VkIndirectCommandsLayoutUsageFlagBitsNVX - Bitmask specifying allowed
-- usage of a indirect commands layout
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutUsageFlagsNVX'
newtype VkIndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkIndirectCommandsLayoutUsageFlagBitsNVX where
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX"
  showsPrec p (VkIndirectCommandsLayoutUsageFlagBitsNVX x) = showParen (p >= 11) (showString "VkIndirectCommandsLayoutUsageFlagBitsNVX " . showsPrec 11 x)

instance Read VkIndirectCommandsLayoutUsageFlagBitsNVX where
  readPrec = parens ( choose [ ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX", pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX)
                             , ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX",    pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX)
                             , ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX",    pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX)
                             , ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX",   pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIndirectCommandsLayoutUsageFlagBitsNVX")
                        v <- step readPrec
                        pure (VkIndirectCommandsLayoutUsageFlagBitsNVX v)
                        )
                    )

-- | @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX@
-- specifies that the processing of sequences /can/ happen at an
-- implementation-dependent order, which is not guaranteed to be coherent
-- across multiple invocations.
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000001

-- | @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX@ specifies
-- that there is likely a high difference between allocated number of
-- sequences and actually used.
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000002

-- | @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX@ specifies
-- that there are likely many draw or dispatch calls that are zero-sized
-- (zero grid dimension, no primitives to render).
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000004

-- | @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX@ specifies
-- that the input data for the sequences is not implicitly indexed from
-- 0..sequencesUsed but a user provided @VkBuffer@ encoding the index is
-- provided.
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000008
-- ** VkObjectEntryUsageFlagBitsNVX

-- | VkObjectEntryUsageFlagBitsNVX - Bitmask specifying allowed usage of an
-- object entry
--
-- = See Also
--
-- 'VkObjectEntryUsageFlagsNVX'
newtype VkObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkObjectEntryUsageFlagBitsNVX where
  showsPrec _ VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = showString "VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX"
  showsPrec _ VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = showString "VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX"
  showsPrec p (VkObjectEntryUsageFlagBitsNVX x) = showParen (p >= 11) (showString "VkObjectEntryUsageFlagBitsNVX " . showsPrec 11 x)

instance Read VkObjectEntryUsageFlagBitsNVX where
  readPrec = parens ( choose [ ("VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX", pure VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX)
                             , ("VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX",  pure VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkObjectEntryUsageFlagBitsNVX")
                        v <- step readPrec
                        pure (VkObjectEntryUsageFlagBitsNVX v)
                        )
                    )

-- | @VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX@ specifies that the resource is
-- bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000001

-- | @VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX@ specifies that the resource is
-- bound to @VK_PIPELINE_BIND_POINT_COMPUTE@
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX"
pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX = VkStructureType 1000086000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX"
pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX = VkStructureType 1000086001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX"
pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX = VkStructureType 1000086002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX"
pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX = VkStructureType 1000086003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX"
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX = VkStructureType 1000086004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX"
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX = VkStructureType 1000086005
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_OBJECT_TABLE_NVX"
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX :: VkObjectType
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX = VkObjectType 1000086000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX"
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX :: VkObjectType
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX = VkObjectType 1000086001
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX"
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX :: VkAccessFlagBits
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX = VkAccessFlagBits 0x00020000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX"
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX :: VkAccessFlagBits
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX = VkAccessFlagBits 0x00040000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX"
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX = VkPipelineStageFlagBits 0x00020000
-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION"
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: Integral a => a
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3
-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME"
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NVX_device_generated_commands"
-- | Dummy data to tag the 'Ptr' with
data VkObjectTableNVX_T
-- | VkObjectTableNVX - Opaque handle to an object table
--
-- = See Also
--
-- 'VkCmdProcessCommandsInfoNVX', 'VkCmdReserveSpaceForCommandsInfoNVX',
-- 'vkCreateObjectTableNVX', 'vkDestroyObjectTableNVX',
-- 'vkRegisterObjectsNVX', 'vkUnregisterObjectsNVX'
type VkObjectTableNVX = Ptr VkObjectTableNVX_T
-- | Dummy data to tag the 'Ptr' with
data VkIndirectCommandsLayoutNVX_T
-- | VkIndirectCommandsLayoutNVX - Opaque handle to an indirect commands
-- layout object
--
-- = See Also
--
-- 'VkCmdProcessCommandsInfoNVX', 'VkCmdReserveSpaceForCommandsInfoNVX',
-- 'vkCreateIndirectCommandsLayoutNVX',
-- 'vkDestroyIndirectCommandsLayoutNVX'
type VkIndirectCommandsLayoutNVX = Ptr VkIndirectCommandsLayoutNVX_T
-- | vkCmdProcessCommandsNVX - Performs the generation of commands on the
-- device
--
-- = Parameters
--
-- -   @commandBuffer@ is the primary command buffer in which the
--     generation process takes space.
--
-- -   @pProcessCommandsInfo@ is a pointer to an instance of the
--     'VkCmdProcessCommandsInfoNVX' structure containing parameters
--     affecting the processing of commands.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pProcessCommandsInfo@ /must/ be a valid pointer to a valid
--     @VkCmdProcessCommandsInfoNVX@ structure
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Inside                                                                                                     | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdProcessCommandsNVX" vkCmdProcessCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()
-- | vkCmdReserveSpaceForCommandsNVX - Perform a reservation of command
-- buffer space
--
-- = Parameters
--
-- -   @commandBuffer@ is the secondary command buffer in which the space
--     for device-generated commands is reserved.
--
-- -   @pProcessCommandsInfo@ is a pointer to an instance of the
--     'vkCmdReserveSpaceForCommandsNVX' structure containing parameters
--     affecting the reservation of command buffer space.
--
-- == Valid Usage
--
-- -   The provided @commandBuffer@ /must/ not have had a prior space
--     reservation since its creation or the last reset.
--
-- -   The state of the @commandBuffer@ /must/ be legal to execute all
--     commands within the sequence provided by the
--     @indirectCommandsLayout@ member of @pProcessCommandsInfo@.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pReserveSpaceInfo@ /must/ be a valid pointer to a valid
--     @VkCmdReserveSpaceForCommandsInfoNVX@ structure
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   @commandBuffer@ /must/ be a secondary @VkCommandBuffer@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Secondary                                                                                                   | Inside                                                                                                     | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'VkCmdReserveSpaceForCommandsInfoNVX',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdReserveSpaceForCommandsNVX" vkCmdReserveSpaceForCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()
-- | vkCreateIndirectCommandsLayoutNVX - Create an indirect command layout
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the indirect command
--     layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkIndirectCommandsLayoutCreateInfoNVX@ structure containing
--     parameters affecting creation of the indirect command layout.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pIndirectCommandsLayout@ points to a @VkIndirectCommandsLayoutNVX@
--     handle in which the resulting indirect command layout is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkIndirectCommandsLayoutCreateInfoNVX@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pIndirectCommandsLayout@ /must/ be a valid pointer to a
--     @VkIndirectCommandsLayoutNVX@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkIndirectCommandsLayoutCreateInfoNVX', 'VkIndirectCommandsLayoutNVX'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateIndirectCommandsLayoutNVX" vkCreateIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult
-- | vkDestroyIndirectCommandsLayoutNVX - Destroy a object table
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the layout.
--
-- -   @indirectCommandsLayout@ is the table to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @indirectCommandsLayout@ /must/
--     have completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @objectTable@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @objectTable@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     @VkIndirectCommandsLayoutNVX@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @indirectCommandsLayout@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkIndirectCommandsLayoutNVX'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyIndirectCommandsLayoutNVX" vkDestroyIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkCreateObjectTableNVX - Create an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkObjectTableCreateInfoNVX@ structure containing parameters
--     affecting creation of the table.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pObjectTable@ points to a @VkObjectTableNVX@ handle in which the
--     resulting object table is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkObjectTableCreateInfoNVX@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pObjectTable@ /must/ be a valid pointer to a @VkObjectTableNVX@
--     handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkObjectTableCreateInfoNVX', 'VkObjectTableNVX'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateObjectTableNVX" vkCreateObjectTableNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult
-- | vkDestroyObjectTableNVX - Destroy a object table
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the table.
--
-- -   @objectTable@ is the table to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @objectTable@ /must/ have
--     completed execution.
--
-- -   If @VkAllocationCallbacks@ were provided when @objectTable@ was
--     created, a compatible set of callbacks /must/ be provided here.
--
-- -   If no @VkAllocationCallbacks@ were provided when @objectTable@ was
--     created, @pAllocator@ /must/ be @NULL@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @objectTable@ /must/ be a valid @VkObjectTableNVX@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @objectTable@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkObjectTableNVX'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyObjectTableNVX" vkDestroyObjectTableNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkRegisterObjectsNVX - Register resource bindings in an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @objectTable@ is the table for which the resources are registered.
--
-- -   @objectCount@ is the number of resources to register.
--
-- -   @ppObjectTableEntries@ provides an array for detailed binding
--     informations, each array element is a pointer to a struct of type
--     @VkObjectTablePipelineEntryNVX@,
--     @VkObjectTableDescriptorSetEntryNVX@,
--     @VkObjectTableVertexBufferEntryNVX@,
--     @VkObjectTableIndexBufferEntryNVX@ or
--     @VkObjectTablePushConstantEntryNVX@ (see below for details).
--
-- -   @pObjectIndices@ are the indices at which each resource is
--     registered.
--
-- == Valid Usage
--
-- -   The contents of @pObjectTableEntry@ /must/ yield plausible bindings
--     supported by the device.
--
-- -   At any @pObjectIndices@ there /must/ not be a registered resource
--     already.
--
-- -   Any value inside @pObjectIndices@ /must/ be below the appropriate
--     @VkObjectTableCreateInfoNVX@::@pObjectEntryCounts@ limits provided
--     at @objectTable@ creation time.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @objectTable@ /must/ be a valid @VkObjectTableNVX@ handle
--
-- -   @ppObjectTableEntries@ /must/ be a valid pointer to an array of
--     @objectCount@ valid @VkObjectTableEntryNVX@ structures
--
-- -   @pObjectIndices@ /must/ be a valid pointer to an array of
--     @objectCount@ @uint32_t@ values
--
-- -   @objectCount@ /must/ be greater than @0@
--
-- -   @objectTable@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkObjectTableEntryNVX', 'VkObjectTableNVX'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkRegisterObjectsNVX" vkRegisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
-- | vkUnregisterObjectsNVX - Unregister resource bindings in an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @objectTable@ is the table from which the resources are
--     unregistered.
--
-- -   @objectCount@ is the number of resources being removed from the
--     object table.
--
-- -   @pObjectEntryType@ provides an array of @VkObjectEntryTypeNVX@ for
--     the resources being removed.
--
-- -   @pObjectIndices@ provides the array of object indices to be removed.
--
-- == Valid Usage
--
-- -   At any @pObjectIndices@ there /must/ be a registered resource
--     already.
--
-- -   The @pObjectEntryTypes@ of the resource at @pObjectIndices@ /must/
--     match.
--
-- -   All operations on the device using the registered resource /must/
--     have been completed.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @objectTable@ /must/ be a valid @VkObjectTableNVX@ handle
--
-- -   @pObjectEntryTypes@ /must/ be a valid pointer to an array of
--     @objectCount@ valid 'VkObjectEntryTypeNVX' values
--
-- -   @pObjectIndices@ /must/ be a valid pointer to an array of
--     @objectCount@ @uint32_t@ values
--
-- -   @objectCount@ /must/ be greater than @0@
--
-- -   @objectTable@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkObjectEntryTypeNVX', 'VkObjectTableNVX'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUnregisterObjectsNVX" vkUnregisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
-- | vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX - Returns
-- device-generated commands related properties of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pFeatures@ points to an instance of the
--     'VkDeviceGeneratedCommandsFeaturesNVX' structure, that will be
--     filled with returned information.
--
-- -   @pLimits@ points to an instance of the
--     'VkDeviceGeneratedCommandsLimitsNVX' structure, that will be filled
--     with returned information.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pFeatures@ /must/ be a valid pointer to a
--     @VkDeviceGeneratedCommandsFeaturesNVX@ structure
--
-- -   @pLimits@ /must/ be a valid pointer to a
--     @VkDeviceGeneratedCommandsLimitsNVX@ structure
--
-- = See Also
--
-- 'VkDeviceGeneratedCommandsFeaturesNVX',
-- 'VkDeviceGeneratedCommandsLimitsNVX',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX" vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()
-- | VkDeviceGeneratedCommandsFeaturesNVX - Structure specifying physical
-- device support
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX@
--
-- -   @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX'
data VkDeviceGeneratedCommandsFeaturesNVX = VkDeviceGeneratedCommandsFeaturesNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @computeBindingPointSupport@ specifies whether the @VkObjectTableNVX@
  -- supports entries with @VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX@ bit set
  -- and @VkIndirectCommandsLayoutNVX@ supports
  -- @VK_PIPELINE_BIND_POINT_COMPUTE@.
  vkComputeBindingPointSupport :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGeneratedCommandsFeaturesNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGeneratedCommandsFeaturesNVX <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGeneratedCommandsFeaturesNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGeneratedCommandsFeaturesNVX))
                *> poke (ptr `plusPtr` 16) (vkComputeBindingPointSupport (poked :: VkDeviceGeneratedCommandsFeaturesNVX))
-- | VkDeviceGeneratedCommandsLimitsNVX - Structure specifying physical
-- device limits
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX@
--
-- -   @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX'
data VkDeviceGeneratedCommandsLimitsNVX = VkDeviceGeneratedCommandsLimitsNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxIndirectCommandsLayoutTokenCount@ the maximum number of tokens in
  -- @VkIndirectCommandsLayoutNVX@.
  vkMaxIndirectCommandsLayoutTokenCount :: Word32
  , -- | @maxObjectEntryCounts@ the maximum number of entries per resource type
  -- in @VkObjectTableNVX@.
  vkMaxObjectEntryCounts :: Word32
  , -- | @minSequenceCountBufferOffsetAlignment@ the minimum alignment for memory
  -- addresses optionally used in @vkCmdProcessCommandsNVX@.
  vkMinSequenceCountBufferOffsetAlignment :: Word32
  , -- | @minSequenceIndexBufferOffsetAlignment@ the minimum alignment for memory
  -- addresses optionally used in @vkCmdProcessCommandsNVX@.
  vkMinSequenceIndexBufferOffsetAlignment :: Word32
  , -- | @minCommandsTokenBufferOffsetAlignment@ the minimum alignment for memory
  -- addresses optionally used in @vkCmdProcessCommandsNVX@.
  vkMinCommandsTokenBufferOffsetAlignment :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGeneratedCommandsLimitsNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDeviceGeneratedCommandsLimitsNVX <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 28)
                                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 16) (vkMaxIndirectCommandsLayoutTokenCount (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 20) (vkMaxObjectEntryCounts (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 24) (vkMinSequenceCountBufferOffsetAlignment (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 28) (vkMinSequenceIndexBufferOffsetAlignment (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 32) (vkMinCommandsTokenBufferOffsetAlignment (poked :: VkDeviceGeneratedCommandsLimitsNVX))
-- | VkIndirectCommandsTokenNVX - Structure specifying parameters for the
-- reservation of command buffer space
--
-- == Valid Usage
--
-- -   The @buffer@’s usage flag /must/ have the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set.
--
-- -   The @offset@ /must/ be aligned to
--     @VkDeviceGeneratedCommandsLimitsNVX@::@minCommandsTokenBufferOffsetAlignment@.
--
-- == Valid Usage (Implicit)
--
-- -   @tokenType@ /must/ be a valid 'VkIndirectCommandsTokenTypeNVX' value
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'VkCmdProcessCommandsInfoNVX', @VkDeviceSize@,
-- 'VkIndirectCommandsTokenTypeNVX'
data VkIndirectCommandsTokenNVX = VkIndirectCommandsTokenNVX
  { -- | @tokenType@ specifies the token command type.
  vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , -- | @buffer@ specifies the @VkBuffer@ storing the functional arguments for
  -- each squence. These argumetns can be written by the device.
  vkBuffer :: VkBuffer
  , -- | @offset@ specified an offset into @buffer@ where the arguments start.
  vkOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkIndirectCommandsTokenNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkIndirectCommandsTokenNVX <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkTokenType (poked :: VkIndirectCommandsTokenNVX))
                *> poke (ptr `plusPtr` 8) (vkBuffer (poked :: VkIndirectCommandsTokenNVX))
                *> poke (ptr `plusPtr` 16) (vkOffset (poked :: VkIndirectCommandsTokenNVX))
-- | VkIndirectCommandsLayoutTokenNVX - Struct specifying the details of an
-- indirect command layout token
--
-- == Valid Usage
--
-- -   @bindingUnit@ /must/ stay within device supported limits for the
--     appropriate commands.
--
-- -   @dynamicCount@ /must/ stay within device supported limits for the
--     appropriate commands.
--
-- -   @divisor@ /must/ be greater than @0@ and a power of two.
--
-- == Valid Usage (Implicit)
--
-- -   @tokenType@ /must/ be a valid 'VkIndirectCommandsTokenTypeNVX' value
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutCreateInfoNVX',
-- 'VkIndirectCommandsTokenTypeNVX'
data VkIndirectCommandsLayoutTokenNVX = VkIndirectCommandsLayoutTokenNVX
  { -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNVX" "tokenType"
  vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , -- | @bindingUnit@ has a different meaning depending on the type, please
  -- refer pseudo code further down for details.
  vkBindingUnit :: Word32
  , -- | @dynamicCount@ has a different meaning depending on the type, please
  -- refer pseudo code further down for details.
  vkDynamicCount :: Word32
  , -- | @divisor@ defines the rate at which the input data buffers are accessed.
  vkDivisor :: Word32
  }
  deriving (Eq, Show)

instance Storable VkIndirectCommandsLayoutTokenNVX where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkIndirectCommandsLayoutTokenNVX <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkTokenType (poked :: VkIndirectCommandsLayoutTokenNVX))
                *> poke (ptr `plusPtr` 4) (vkBindingUnit (poked :: VkIndirectCommandsLayoutTokenNVX))
                *> poke (ptr `plusPtr` 8) (vkDynamicCount (poked :: VkIndirectCommandsLayoutTokenNVX))
                *> poke (ptr `plusPtr` 12) (vkDivisor (poked :: VkIndirectCommandsLayoutTokenNVX))
-- | VkIndirectCommandsLayoutCreateInfoNVX - Structure specifying the
-- parameters of a newly created indirect commands layout object
--
-- = Description
--
-- The following code illustrates some of the key flags:
--
-- > void cmdProcessAllSequences(cmd, objectTable, indirectCommandsLayout, pIndirectCommandsTokens, sequencesCount, indexbuffer, indexbufferoffset)
-- > {
-- >   for (s = 0; s < sequencesCount; s++)
-- >   {
-- >     sequence = s;
-- >
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX) {
-- >       sequence = incoherent_implementation_dependent_permutation[ sequence ];
-- >     }
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX) {
-- >       sequence = indexbuffer.load_uint32( sequence * sizeof(uint32_t) + indexbufferoffset);
-- >     }
-- >
-- >     cmdProcessSequence( cmd, objectTable, indirectCommandsLayout, pIndirectCommandsTokens, sequence );
-- >   }
-- > }
--
-- == Valid Usage
--
-- -   @tokenCount@ /must/ be greater than @0@ and below
--     @VkDeviceGeneratedCommandsLimitsNVX@::@maxIndirectCommandsLayoutTokenCount@
--
-- -   If the
--     @VkDeviceGeneratedCommandsFeaturesNVX@::@computeBindingPointSupport@
--     feature is not enabled, then @pipelineBindPoint@ /must/ not be
--     @VK_PIPELINE_BIND_POINT_COMPUTE@
--
-- -   If @pTokens@ contains an entry of
--     @VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX@ it /must/ be the
--     first element of the array and there /must/ be only a single element
--     of such token type.
--
-- -   All state binding tokens in @pTokens@ /must/ occur prior work
--     provoking tokens (@VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX@,
--     @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX@,
--     @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX@).
--
-- -   The content of @pTokens@ /must/ include one single work provoking
--     token that is compatible with the @pipelineBindPoint@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' value
--
-- -   @flags@ /must/ be a valid combination of
--     'VkIndirectCommandsLayoutUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @pTokens@ /must/ be a valid pointer to an array of @tokenCount@
--     valid @VkIndirectCommandsLayoutTokenNVX@ structures
--
-- -   @tokenCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutTokenNVX',
-- 'VkIndirectCommandsLayoutUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateIndirectCommandsLayoutNVX'
data VkIndirectCommandsLayoutCreateInfoNVX = VkIndirectCommandsLayoutCreateInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pipelineBindPoint@ is the @VkPipelineBindPoint@ that this layout
  -- targets.
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- | @flags@ is a bitmask of 'VkIndirectCommandsLayoutUsageFlagBitsNVX'
  -- specifying usage hints of this layout.
  vkFlags :: VkIndirectCommandsLayoutUsageFlagsNVX
  , -- | @tokenCount@ is the length of the individual command sequnce.
  vkTokenCount :: Word32
  , -- | @pTokens@ is an array describing each command token in detail. See
  -- 'VkIndirectCommandsTokenTypeNVX' and 'VkIndirectCommandsLayoutTokenNVX'
  -- below for details.
  vkPTokens :: Ptr VkIndirectCommandsLayoutTokenNVX
  }
  deriving (Eq, Show)

instance Storable VkIndirectCommandsLayoutCreateInfoNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkIndirectCommandsLayoutCreateInfoNVX <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
                                                   <*> peek (ptr `plusPtr` 24)
                                                   <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkPipelineBindPoint (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 20) (vkFlags (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkTokenCount (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkPTokens (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
-- | VkCmdProcessCommandsInfoNVX - Structure specifying parameters for the
-- generation of commands
--
-- == Valid Usage
--
-- -   The provided @objectTable@ /must/ include all objects referenced by
--     the generation process.
--
-- -   @indirectCommandsTokenCount@ /must/ match the
--     @indirectCommandsLayout@’s @tokenCount@.
--
-- -   The @tokenType@ member of each entry in the
--     @pIndirectCommandsTokens@ array /must/ match the values used at
--     creation time of @indirectCommandsLayout@
--
-- -   If @targetCommandBuffer@ is provided, it /must/ have reserved
--     command space.
--
-- -   If @targetCommandBuffer@ is provided, the @objectTable@ /must/ match
--     the reservation’s objectTable and /must/ have had all referenced
--     objects registered at reservation time.
--
-- -   If @targetCommandBuffer@ is provided, the @indirectCommandsLayout@
--     /must/ match the reservation’s indirectCommandsLayout.
--
-- -   If @targetCommandBuffer@ is provided, the @maxSequencesCount@ /must/
--     not exceed the reservation’s maxSequencesCount.
--
-- -   If @sequencesCountBuffer@ is used, its usage flag /must/ have
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set.
--
-- -   If @sequencesCountBuffer@ is used, @sequencesCountOffset@ /must/ be
--     aligned to
--     @VkDeviceGeneratedCommandsLimitsNVX@::@minSequenceCountBufferOffsetAlignment@.
--
-- -   If @sequencesIndexBuffer@ is used, its usage flag /must/ have
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set.
--
-- -   If @sequencesIndexBuffer@ is used, @sequencesIndexOffset@ /must/ be
--     aligned to
--     @VkDeviceGeneratedCommandsLimitsNVX@::@minSequenceIndexBufferOffsetAlignment@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectTable@ /must/ be a valid @VkObjectTableNVX@ handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     @VkIndirectCommandsLayoutNVX@ handle
--
-- -   @pIndirectCommandsTokens@ /must/ be a valid pointer to an array of
--     @indirectCommandsTokenCount@ valid @VkIndirectCommandsTokenNVX@
--     structures
--
-- -   If @targetCommandBuffer@ is not @NULL@, @targetCommandBuffer@ /must/
--     be a valid @VkCommandBuffer@ handle
--
-- -   If @sequencesCountBuffer@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @sequencesCountBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   If @sequencesIndexBuffer@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @sequencesIndexBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @indirectCommandsTokenCount@ /must/ be greater than @0@
--
-- -   Each of @indirectCommandsLayout@, @objectTable@,
--     @sequencesCountBuffer@, @sequencesIndexBuffer@, and
--     @targetCommandBuffer@ that are valid handles /must/ have been
--     created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- -   Host access to @targetCommandBuffer@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@,
-- 'VkIndirectCommandsLayoutNVX', 'VkIndirectCommandsTokenNVX',
-- 'VkObjectTableNVX', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCmdProcessCommandsNVX'
data VkCmdProcessCommandsInfoNVX = VkCmdProcessCommandsInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @objectTable@ is the @VkObjectTableNVX@ to be used for the generation
  -- process. Only registered objects at the time
  -- 'vkCmdReserveSpaceForCommandsNVX' is called, will be taken into account
  -- for the reservation.
  vkObjectTable :: VkObjectTableNVX
  , -- | @indirectCommandsLayout@ is the @VkIndirectCommandsLayoutNVX@ that
  -- provides the command sequence to generate.
  vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX
  , -- | @indirectCommandsTokenCount@ defines the number of input tokens used.
  vkIndirectCommandsTokenCount :: Word32
  , -- | @pIndirectCommandsTokens@ provides an array of
  -- 'VkIndirectCommandsTokenNVX' that reference the input data for each
  -- token command.
  vkPIndirectCommandsTokens :: Ptr VkIndirectCommandsTokenNVX
  , -- | @maxSequencesCount@ is the maximum number of sequences for which command
  -- buffer space will be reserved. If @sequencesCountBuffer@ is
  -- 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', this is also the
  -- actual number of sequences generated.
  vkMaxSequencesCount :: Word32
  , -- | @targetCommandBuffer@ /can/ be the secondary @VkCommandBuffer@ in which
  -- the commands should be recorded. If @targetCommandBuffer@ is @NULL@ an
  -- implicit reservation as well as execution takes place on the processing
  -- @VkCommandBuffer@.
  vkTargetCommandBuffer :: VkCommandBuffer
  , -- | @sequencesCountBuffer@ /can/ be @VkBuffer@ from which the actual amount
  -- of sequences is sourced from as @uint32_t@ value.
  vkSequencesCountBuffer :: VkBuffer
  , -- | @sequencesCountOffset@ is the byte offset into @sequencesCountBuffer@
  -- where the count value is stored.
  vkSequencesCountOffset :: VkDeviceSize
  , -- | @sequencesIndexBuffer@ /must/ be set if @indirectCommandsLayout@’s
  -- @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT@ is set and
  -- provides the used sequence indices as @uint32_t@ array. Otherwise it
  -- /must/ be 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'.
  vkSequencesIndexBuffer :: VkBuffer
  , -- | @sequencesIndexOffset@ is the byte offset into @sequencesIndexBuffer@
  -- where the index values start.
  vkSequencesIndexOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkCmdProcessCommandsInfoNVX where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkCmdProcessCommandsInfoNVX <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
                                         <*> peek (ptr `plusPtr` 40)
                                         <*> peek (ptr `plusPtr` 48)
                                         <*> peek (ptr `plusPtr` 56)
                                         <*> peek (ptr `plusPtr` 64)
                                         <*> peek (ptr `plusPtr` 72)
                                         <*> peek (ptr `plusPtr` 80)
                                         <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkObjectTable (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkIndirectCommandsLayout (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkIndirectCommandsTokenCount (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 40) (vkPIndirectCommandsTokens (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 48) (vkMaxSequencesCount (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 56) (vkTargetCommandBuffer (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 64) (vkSequencesCountBuffer (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 72) (vkSequencesCountOffset (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 80) (vkSequencesIndexBuffer (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 88) (vkSequencesIndexOffset (poked :: VkCmdProcessCommandsInfoNVX))
-- | VkCmdReserveSpaceForCommandsInfoNVX - Structure specifying parameters
-- for the reservation of command buffer space
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectTable@ /must/ be a valid @VkObjectTableNVX@ handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     @VkIndirectCommandsLayoutNVX@ handle
--
-- -   Both of @indirectCommandsLayout@, and @objectTable@ /must/ have been
--     created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutNVX', 'VkObjectTableNVX',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCmdReserveSpaceForCommandsNVX'
data VkCmdReserveSpaceForCommandsInfoNVX = VkCmdReserveSpaceForCommandsInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @objectTable@ is the @VkObjectTableNVX@ to be used for the generation
  -- process. Only registered objects at the time
  -- 'vkCmdReserveSpaceForCommandsNVX' is called, will be taken into account
  -- for the reservation.
  vkObjectTable :: VkObjectTableNVX
  , -- | @indirectCommandsLayout@ is the @VkIndirectCommandsLayoutNVX@ that
  -- /must/ also be used at generation time.
  vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX
  , -- | @maxSequencesCount@ is the maximum number of sequences for which command
  -- buffer space will be reserved.
  vkMaxSequencesCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkCmdReserveSpaceForCommandsInfoNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkCmdReserveSpaceForCommandsInfoNVX <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkObjectTable (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkIndirectCommandsLayout (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkMaxSequencesCount (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
-- | VkObjectTableCreateInfoNVX - Structure specifying the parameters of a
-- newly created object table
--
-- == Valid Usage
--
-- -   If the
--     @VkDeviceGeneratedCommandsFeaturesNVX@::@computeBindingPointSupport@
--     feature is not enabled, @pObjectEntryUsageFlags@ /must/ not contain
--     @VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX@
--
-- -   Any value within @pObjectEntryCounts@ /must/ not exceed
--     @VkDeviceGeneratedCommandsLimitsNVX@::@maxObjectEntryCounts@
--
-- -   @maxUniformBuffersPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- -   @maxStorageBuffersPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- -   @maxStorageImagesPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- -   @maxSampledImagesPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @pObjectEntryTypes@ /must/ be a valid pointer to an array of
--     @objectCount@ valid 'VkObjectEntryTypeNVX' values
--
-- -   @pObjectEntryCounts@ /must/ be a valid pointer to an array of
--     @objectCount@ @uint32_t@ values
--
-- -   @pObjectEntryUsageFlags@ /must/ be a valid pointer to an array of
--     @objectCount@ valid combinations of 'VkObjectEntryUsageFlagBitsNVX'
--     values
--
-- -   Each element of @pObjectEntryUsageFlags@ /must/ not be @0@
--
-- -   @objectCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateObjectTableNVX'
data VkObjectTableCreateInfoNVX = VkObjectTableCreateInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @objectCount@ is the number of entry configurations that the object
  -- table supports.
  vkObjectCount :: Word32
  , -- | @pObjectEntryTypes@ is an array of 'VkObjectEntryTypeNVX' values
  -- providing the entry type of a given configuration.
  vkPObjectEntryTypes :: Ptr VkObjectEntryTypeNVX
  , -- | @pObjectEntryCounts@ is an array of counts of how many objects can be
  -- registered in the table.
  vkPObjectEntryCounts :: Ptr Word32
  , -- | @pObjectEntryUsageFlags@ is an array of bitmasks of
  -- 'VkObjectEntryUsageFlagBitsNVX' specifying the binding usage of the
  -- entry.
  vkPObjectEntryUsageFlags :: Ptr VkObjectEntryUsageFlagsNVX
  , -- | @maxUniformBuffersPerDescriptor@ is the maximum number of
  -- @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
  -- @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ used by any single
  -- registered @VkDescriptorSet@ in this table.
  vkMaxUniformBuffersPerDescriptor :: Word32
  , -- | @maxStorageBuffersPerDescriptor@ is the maximum number of
  -- @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
  -- @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ used by any single
  -- registered @VkDescriptorSet@ in this table.
  vkMaxStorageBuffersPerDescriptor :: Word32
  , -- | @maxStorageImagesPerDescriptor@ is the maximum number of
  -- @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@ or
  -- @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ used by any single registered
  -- @VkDescriptorSet@ in this table.
  vkMaxStorageImagesPerDescriptor :: Word32
  , -- | @maxSampledImagesPerDescriptor@ is the maximum number of
  -- @VK_DESCRIPTOR_TYPE_SAMPLER@,
  -- @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@,
  -- @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ or
  -- @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@ used by any single registered
  -- @VkDescriptorSet@ in this table.
  vkMaxSampledImagesPerDescriptor :: Word32
  , -- | @maxPipelineLayouts@ is the maximum number of unique @VkPipelineLayout@
  -- used by any registered @VkDescriptorSet@ or @VkPipeline@ in this table.
  vkMaxPipelineLayouts :: Word32
  }
  deriving (Eq, Show)

instance Storable VkObjectTableCreateInfoNVX where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkObjectTableCreateInfoNVX <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
                                        <*> peek (ptr `plusPtr` 40)
                                        <*> peek (ptr `plusPtr` 48)
                                        <*> peek (ptr `plusPtr` 52)
                                        <*> peek (ptr `plusPtr` 56)
                                        <*> peek (ptr `plusPtr` 60)
                                        <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkObjectCount (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkPObjectEntryTypes (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkPObjectEntryCounts (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 40) (vkPObjectEntryUsageFlags (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 48) (vkMaxUniformBuffersPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 52) (vkMaxStorageBuffersPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 56) (vkMaxStorageImagesPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 60) (vkMaxSampledImagesPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 64) (vkMaxPipelineLayouts (poked :: VkObjectTableCreateInfoNVX))
-- | VkObjectTableEntryNVX - Common parameters of an object table resource
-- entry
--
-- == Valid Usage
--
-- -   If the
--     @VkDeviceGeneratedCommandsFeaturesNVX@::@computeBindingPointSupport@
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX@
--
-- == Valid Usage (Implicit)
--
-- -   @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
--
-- -   @flags@ /must/ be a valid combination of
--     'VkObjectEntryUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- = See Also
--
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX',
-- 'vkRegisterObjectsNVX'
data VkObjectTableEntryNVX = VkObjectTableEntryNVX
  { -- | @type@ defines the entry type
  vkType :: VkObjectEntryTypeNVX
  , -- | @flags@ defines which @VkPipelineBindPoint@ the resource can be used
  -- with. Some entry types allow only a single flag to be set.
  vkFlags :: VkObjectEntryUsageFlagsNVX
  }
  deriving (Eq, Show)

instance Storable VkObjectTableEntryNVX where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkObjectTableEntryNVX <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableEntryNVX))
-- | VkObjectTablePipelineEntryNVX - Parameters of an object table pipeline
-- entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX@
--
-- == Valid Usage (Implicit)
--
-- -   @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
--
-- -   @flags@ /must/ be a valid combination of
--     'VkObjectEntryUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @pipeline@ /must/ be a valid @VkPipeline@ handle
--
-- = See Also
--
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipeline'
data VkObjectTablePipelineEntryNVX = VkObjectTablePipelineEntryNVX
  { -- No documentation found for Nested "VkObjectTablePipelineEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTablePipelineEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @pipeline@ specifies the @VkPipeline@ that this resource entry
  -- references.
  vkPipeline :: VkPipeline
  }
  deriving (Eq, Show)

instance Storable VkObjectTablePipelineEntryNVX where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkObjectTablePipelineEntryNVX <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTablePipelineEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTablePipelineEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkPipeline (poked :: VkObjectTablePipelineEntryNVX))
-- | VkObjectTableDescriptorSetEntryNVX - Parameters of an object table
-- descriptor set entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX@
--
-- == Valid Usage (Implicit)
--
-- -   @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
--
-- -   @flags@ /must/ be a valid combination of
--     'VkObjectEntryUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @pipelineLayout@ /must/ be a valid @VkPipelineLayout@ handle
--
-- -   @descriptorSet@ /must/ be a valid @VkDescriptorSet@ handle
--
-- -   Both of @descriptorSet@, and @pipelineLayout@ /must/ have been
--     created, allocated, or retrieved from the same @VkDevice@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSet',
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout'
data VkObjectTableDescriptorSetEntryNVX = VkObjectTableDescriptorSetEntryNVX
  { -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @pipelineLayout@ specifies the @VkPipelineLayout@ that the
  -- @descriptorSet@ is used with.
  vkPipelineLayout :: VkPipelineLayout
  , -- | @descriptorSet@ specifies the @VkDescriptorSet@ that can be bound with
  -- this entry.
  vkDescriptorSet :: VkDescriptorSet
  }
  deriving (Eq, Show)

instance Storable VkObjectTableDescriptorSetEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkObjectTableDescriptorSetEntryNVX <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 4)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableDescriptorSetEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableDescriptorSetEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkPipelineLayout (poked :: VkObjectTableDescriptorSetEntryNVX))
                *> poke (ptr `plusPtr` 16) (vkDescriptorSet (poked :: VkObjectTableDescriptorSetEntryNVX))
-- | VkObjectTableVertexBufferEntryNVX - Parameters of an object table vertex
-- buffer entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX@
--
-- == Valid Usage (Implicit)
--
-- -   @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
--
-- -   @flags@ /must/ be a valid combination of
--     'VkObjectEntryUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX'
data VkObjectTableVertexBufferEntryNVX = VkObjectTableVertexBufferEntryNVX
  { -- No documentation found for Nested "VkObjectTableVertexBufferEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableVertexBufferEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @buffer@ specifies the @VkBuffer@ that can be bound as vertex bufer
  vkBuffer :: VkBuffer
  }
  deriving (Eq, Show)

instance Storable VkObjectTableVertexBufferEntryNVX where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkObjectTableVertexBufferEntryNVX <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableVertexBufferEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableVertexBufferEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkBuffer (poked :: VkObjectTableVertexBufferEntryNVX))
-- | VkObjectTableIndexBufferEntryNVX - Parameters of an object table index
-- buffer entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX@
--
-- == Valid Usage (Implicit)
--
-- -   @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
--
-- -   @flags@ /must/ be a valid combination of
--     'VkObjectEntryUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @indexType@ /must/ be a valid
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.VkIndexType' value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkIndexType',
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX'
data VkObjectTableIndexBufferEntryNVX = VkObjectTableIndexBufferEntryNVX
  { -- No documentation found for Nested "VkObjectTableIndexBufferEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableIndexBufferEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @buffer@ specifies the @VkBuffer@ that can be bound as index buffer
  vkBuffer :: VkBuffer
  , -- | @indexType@ specifies the @VkIndexType@ used with this index buffer
  vkIndexType :: VkIndexType
  }
  deriving (Eq, Show)

instance Storable VkObjectTableIndexBufferEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkObjectTableIndexBufferEntryNVX <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableIndexBufferEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableIndexBufferEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkBuffer (poked :: VkObjectTableIndexBufferEntryNVX))
                *> poke (ptr `plusPtr` 16) (vkIndexType (poked :: VkObjectTableIndexBufferEntryNVX))
-- | VkObjectTablePushConstantEntryNVX - Parameters of an object table push
-- constant entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX@
--
-- == Valid Usage (Implicit)
--
-- -   @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
--
-- -   @flags@ /must/ be a valid combination of
--     'VkObjectEntryUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @pipelineLayout@ /must/ be a valid @VkPipelineLayout@ handle
--
-- -   @stageFlags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Pipeline.VkShaderStageFlagBits' values
--
-- -   @stageFlags@ /must/ not be @0@
--
-- = See Also
--
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkShaderStageFlags'
data VkObjectTablePushConstantEntryNVX = VkObjectTablePushConstantEntryNVX
  { -- No documentation found for Nested "VkObjectTablePushConstantEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTablePushConstantEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @pipelineLayout@ specifies the @VkPipelineLayout@ that the pushconstants
  -- are used with
  vkPipelineLayout :: VkPipelineLayout
  , -- | @stageFlags@ specifies the @VkShaderStageFlags@ that the pushconstants
  -- are used with
  vkStageFlags :: VkShaderStageFlags
  }
  deriving (Eq, Show)

instance Storable VkObjectTablePushConstantEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkObjectTablePushConstantEntryNVX <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTablePushConstantEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTablePushConstantEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkPipelineLayout (poked :: VkObjectTablePushConstantEntryNVX))
                *> poke (ptr `plusPtr` 16) (vkStageFlags (poked :: VkObjectTablePushConstantEntryNVX))
-- | VkIndirectCommandsLayoutUsageFlagsNVX - Bitmask of
-- VkIndirectCommandsLayoutUsageFlagBitsNVX
--
-- = Description
--
-- @VkIndirectCommandsLayoutUsageFlagsNVX@ is a bitmask type for setting a
-- mask of zero or more 'VkIndirectCommandsLayoutUsageFlagBitsNVX'.
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutCreateInfoNVX',
-- 'VkIndirectCommandsLayoutUsageFlagBitsNVX'
type VkIndirectCommandsLayoutUsageFlagsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX
-- | VkObjectEntryUsageFlagsNVX - Bitmask of VkObjectEntryUsageFlagBitsNVX
--
-- = Description
--
-- @VkObjectEntryUsageFlagsNVX@ is a bitmask type for setting a mask of
-- zero or more 'VkObjectEntryUsageFlagBitsNVX'.
--
-- = See Also
--
-- 'VkObjectEntryUsageFlagBitsNVX', 'VkObjectTableCreateInfoNVX',
-- 'VkObjectTableDescriptorSetEntryNVX', 'VkObjectTableEntryNVX',
-- 'VkObjectTableIndexBufferEntryNVX', 'VkObjectTablePipelineEntryNVX',
-- 'VkObjectTablePushConstantEntryNVX', 'VkObjectTableVertexBufferEntryNVX'
type VkObjectEntryUsageFlagsNVX = VkObjectEntryUsageFlagBitsNVX
