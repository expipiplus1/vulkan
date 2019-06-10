{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  CmdProcessCommandsInfoNVX(..)
  , 
  CmdReserveSpaceForCommandsInfoNVX(..)
  , DeviceGeneratedCommandsFeaturesNVX(..)
  , DeviceGeneratedCommandsLimitsNVX(..)
  , IndirectCommandsLayoutCreateInfoNVX(..)
#endif
  , IndirectCommandsLayoutNVX
  , IndirectCommandsLayoutTokenNVX(..)
  , IndirectCommandsLayoutUsageFlagBitsNVX
  , pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
  , pattern INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
  , pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
  , pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
  , IndirectCommandsLayoutUsageFlagsNVX
  , IndirectCommandsTokenNVX(..)
  , IndirectCommandsTokenTypeNVX
  , pattern INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX
  , pattern INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX
  , pattern INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX
  , pattern INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX
  , pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX
  , pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX
  , pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX
  , pattern INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX
  , ObjectEntryTypeNVX
  , pattern OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
  , pattern OBJECT_ENTRY_TYPE_PIPELINE_NVX
  , pattern OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
  , pattern OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
  , pattern OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
  , ObjectEntryUsageFlagBitsNVX
  , pattern OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
  , pattern OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
  , ObjectEntryUsageFlagsNVX
#if defined(VK_USE_PLATFORM_GGP)
  , ObjectTableCreateInfoNVX(..)
#endif
  , ObjectTableDescriptorSetEntryNVX(..)
  , ObjectTableEntryNVX(..)
  , ObjectTableIndexBufferEntryNVX(..)
  , ObjectTableNVX
  , ObjectTablePipelineEntryNVX(..)
  , ObjectTablePushConstantEntryNVX(..)
  , ObjectTableVertexBufferEntryNVX(..)
  , cmdProcessCommandsNVX
  , cmdReserveSpaceForCommandsNVX
  , createIndirectCommandsLayoutNVX
  , createObjectTableNVX
  , destroyIndirectCommandsLayoutNVX
  , destroyObjectTableNVX
  , getPhysicalDeviceGeneratedCommandsPropertiesNVX
  , registerObjectsNVX
  , unregisterObjectsNVX
  , withIndirectCommandsLayoutNVX
  , withObjectTableNVX
  , withRegisteredObjectsNVX
  , pattern NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  , pattern NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
  , pattern STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
  , pattern ACCESS_COMMAND_PROCESS_READ_BIT_NVX
  , pattern ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
  , pattern OBJECT_TYPE_OBJECT_TABLE_NVX
  , pattern OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX
  ) where

import Control.Exception
  ( bracket
  , bracket_
  )
import Control.Monad
  ( (<=<)
  )
import Data.Function
  ( (&)
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
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
import Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkIndirectCommandsLayoutUsageFlagBitsNVX(..)
  , VkIndirectCommandsTokenTypeNVX(..)
  , VkObjectEntryTypeNVX(..)
  , VkObjectEntryUsageFlagBitsNVX(..)
  , VkIndirectCommandsLayoutNVX
  , VkObjectTableNVX
  , vkCmdProcessCommandsNVX
  , vkCmdReserveSpaceForCommandsNVX
  , vkCreateIndirectCommandsLayoutNVX
  , vkCreateObjectTableNVX
  , vkDestroyIndirectCommandsLayoutNVX
  , vkDestroyObjectTableNVX
  , vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , vkRegisterObjectsNVX
  , vkUnregisterObjectsNVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
  , pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
  , pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
  , pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
  )
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( IndexType
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , PhysicalDevice(..)
  , DeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pass
  ( PipelineBindPoint
  )
#endif
import Graphics.Vulkan.Core10.Pipeline
  ( Pipeline
  , PipelineLayout
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( ShaderStageFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX
  , pattern OBJECT_TYPE_OBJECT_TABLE_NVX
  , pattern STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern ACCESS_COMMAND_PROCESS_READ_BIT_NVX
  , pattern ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
  )
import Graphics.Vulkan.Core10.Queue
  ( pattern PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCmdProcessCommandsInfoNVX"
data CmdProcessCommandsInfoNVX = CmdProcessCommandsInfoNVX
  { -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "objectTable"
  objectTable :: ObjectTableNVX
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "indirectCommandsLayout"
  indirectCommandsLayout :: IndirectCommandsLayoutNVX
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "pIndirectCommandsTokens"
  indirectCommandsTokens :: Vector IndirectCommandsTokenNVX
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "maxSequencesCount"
  maxSequencesCount :: Word32
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "targetCommandBuffer"
  targetCommandBuffer :: Maybe CommandBuffer
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "sequencesCountBuffer"
  sequencesCountBuffer :: Buffer
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "sequencesCountOffset"
  sequencesCountOffset :: DeviceSize
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "sequencesIndexBuffer"
  sequencesIndexBuffer :: Buffer
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "sequencesIndexOffset"
  sequencesIndexOffset :: DeviceSize
  }
  deriving (Show, Eq)



#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCmdReserveSpaceForCommandsInfoNVX"
data CmdReserveSpaceForCommandsInfoNVX = CmdReserveSpaceForCommandsInfoNVX
  { -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "objectTable"
  objectTable :: ObjectTableNVX
  , -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "indirectCommandsLayout"
  indirectCommandsLayout :: IndirectCommandsLayoutNVX
  , -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "maxSequencesCount"
  maxSequencesCount :: Word32
  }
  deriving (Show, Eq)

instance Zero CmdReserveSpaceForCommandsInfoNVX where
  zero = CmdReserveSpaceForCommandsInfoNVX Nothing
                                           zero
                                           zero
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGeneratedCommandsFeaturesNVX"
data DeviceGeneratedCommandsFeaturesNVX = DeviceGeneratedCommandsFeaturesNVX
  { -- No documentation found for Nested "DeviceGeneratedCommandsFeaturesNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGeneratedCommandsFeaturesNVX" "computeBindingPointSupport"
  computeBindingPointSupport :: Bool
  }
  deriving (Show, Eq)

instance Zero DeviceGeneratedCommandsFeaturesNVX where
  zero = DeviceGeneratedCommandsFeaturesNVX Nothing
                                            False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGeneratedCommandsLimitsNVX"
data DeviceGeneratedCommandsLimitsNVX = DeviceGeneratedCommandsLimitsNVX
  { -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "maxIndirectCommandsLayoutTokenCount"
  maxIndirectCommandsLayoutTokenCount :: Word32
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "maxObjectEntryCounts"
  maxObjectEntryCounts :: Word32
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "minSequenceCountBufferOffsetAlignment"
  minSequenceCountBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "minSequenceIndexBufferOffsetAlignment"
  minSequenceIndexBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "minCommandsTokenBufferOffsetAlignment"
  minCommandsTokenBufferOffsetAlignment :: Word32
  }
  deriving (Show, Eq)

instance Zero DeviceGeneratedCommandsLimitsNVX where
  zero = DeviceGeneratedCommandsLimitsNVX Nothing
                                          zero
                                          zero
                                          zero
                                          zero
                                          zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkIndirectCommandsLayoutCreateInfoNVX"
data IndirectCommandsLayoutCreateInfoNVX = IndirectCommandsLayoutCreateInfoNVX
  { -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "pipelineBindPoint"
  pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "flags"
  flags :: IndirectCommandsLayoutUsageFlagsNVX
  , -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "pTokens"
  tokens :: Vector IndirectCommandsLayoutTokenNVX
  }
  deriving (Show, Eq)

instance Zero IndirectCommandsLayoutCreateInfoNVX where
  zero = IndirectCommandsLayoutCreateInfoNVX Nothing
                                             zero
                                             zero
                                             mempty

#endif

-- No documentation found for TopLevel "IndirectCommandsLayoutNVX"
type IndirectCommandsLayoutNVX = VkIndirectCommandsLayoutNVX


-- No documentation found for TopLevel "VkIndirectCommandsLayoutTokenNVX"
data IndirectCommandsLayoutTokenNVX = IndirectCommandsLayoutTokenNVX
  { -- No documentation found for Nested "IndirectCommandsLayoutTokenNVX" "tokenType"
  tokenType :: IndirectCommandsTokenTypeNVX
  , -- No documentation found for Nested "IndirectCommandsLayoutTokenNVX" "bindingUnit"
  bindingUnit :: Word32
  , -- No documentation found for Nested "IndirectCommandsLayoutTokenNVX" "dynamicCount"
  dynamicCount :: Word32
  , -- No documentation found for Nested "IndirectCommandsLayoutTokenNVX" "divisor"
  divisor :: Word32
  }
  deriving (Show, Eq)

instance Zero IndirectCommandsLayoutTokenNVX where
  zero = IndirectCommandsLayoutTokenNVX zero
                                        zero
                                        zero
                                        zero


-- No documentation found for TopLevel "IndirectCommandsLayoutUsageFlagBitsNVX"
type IndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX


{-# complete INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX, INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX, INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX, INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX :: IndirectCommandsLayoutUsageFlagBitsNVX #-}


-- No documentation found for Nested "IndirectCommandsLayoutUsageFlagBitsNVX" "INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX :: (a ~ IndirectCommandsLayoutUsageFlagBitsNVX) => a
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX


-- No documentation found for Nested "IndirectCommandsLayoutUsageFlagBitsNVX" "INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX :: (a ~ IndirectCommandsLayoutUsageFlagBitsNVX) => a
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX


-- No documentation found for Nested "IndirectCommandsLayoutUsageFlagBitsNVX" "INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX :: (a ~ IndirectCommandsLayoutUsageFlagBitsNVX) => a
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX


-- No documentation found for Nested "IndirectCommandsLayoutUsageFlagBitsNVX" "INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX :: (a ~ IndirectCommandsLayoutUsageFlagBitsNVX) => a
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX

-- No documentation found for TopLevel "IndirectCommandsLayoutUsageFlagsNVX"
type IndirectCommandsLayoutUsageFlagsNVX = IndirectCommandsLayoutUsageFlagBitsNVX


-- No documentation found for TopLevel "VkIndirectCommandsTokenNVX"
data IndirectCommandsTokenNVX = IndirectCommandsTokenNVX
  { -- No documentation found for Nested "IndirectCommandsTokenNVX" "tokenType"
  tokenType :: IndirectCommandsTokenTypeNVX
  , -- No documentation found for Nested "IndirectCommandsTokenNVX" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "IndirectCommandsTokenNVX" "offset"
  offset :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero IndirectCommandsTokenNVX where
  zero = IndirectCommandsTokenNVX zero
                                  zero
                                  zero


-- No documentation found for TopLevel "IndirectCommandsTokenTypeNVX"
type IndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX


{-# complete INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX, INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX, INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX, INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX, INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX, INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX, INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX, INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX :: IndirectCommandsTokenTypeNVX #-}


-- No documentation found for Nested "IndirectCommandsTokenTypeNVX" "INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX :: (a ~ IndirectCommandsTokenTypeNVX) => a
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX = VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX


-- No documentation found for Nested "IndirectCommandsTokenTypeNVX" "INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX :: (a ~ IndirectCommandsTokenTypeNVX) => a
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX = VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX


-- No documentation found for Nested "IndirectCommandsTokenTypeNVX" "INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX :: (a ~ IndirectCommandsTokenTypeNVX) => a
pattern INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX = VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX


-- No documentation found for Nested "IndirectCommandsTokenTypeNVX" "INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX :: (a ~ IndirectCommandsTokenTypeNVX) => a
pattern INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX = VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX


-- No documentation found for Nested "IndirectCommandsTokenTypeNVX" "INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX :: (a ~ IndirectCommandsTokenTypeNVX) => a
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX = VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX


-- No documentation found for Nested "IndirectCommandsTokenTypeNVX" "INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX :: (a ~ IndirectCommandsTokenTypeNVX) => a
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX = VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX


-- No documentation found for Nested "IndirectCommandsTokenTypeNVX" "INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX :: (a ~ IndirectCommandsTokenTypeNVX) => a
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX = VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX


-- No documentation found for Nested "IndirectCommandsTokenTypeNVX" "INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX :: (a ~ IndirectCommandsTokenTypeNVX) => a
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX = VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX

-- No documentation found for TopLevel "ObjectEntryTypeNVX"
type ObjectEntryTypeNVX = VkObjectEntryTypeNVX


{-# complete OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX, OBJECT_ENTRY_TYPE_PIPELINE_NVX, OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX, OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX, OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX :: ObjectEntryTypeNVX #-}


-- No documentation found for Nested "ObjectEntryTypeNVX" "OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX"
pattern OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX


-- No documentation found for Nested "ObjectEntryTypeNVX" "OBJECT_ENTRY_TYPE_PIPELINE_NVX"
pattern OBJECT_ENTRY_TYPE_PIPELINE_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_PIPELINE_NVX = VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX


-- No documentation found for Nested "ObjectEntryTypeNVX" "OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX"
pattern OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX


-- No documentation found for Nested "ObjectEntryTypeNVX" "OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX"
pattern OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX


-- No documentation found for Nested "ObjectEntryTypeNVX" "OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX"
pattern OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX

-- No documentation found for TopLevel "ObjectEntryUsageFlagBitsNVX"
type ObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX


{-# complete OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX, OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX :: ObjectEntryUsageFlagBitsNVX #-}


-- No documentation found for Nested "ObjectEntryUsageFlagBitsNVX" "OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX"
pattern OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX :: (a ~ ObjectEntryUsageFlagBitsNVX) => a
pattern OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX


-- No documentation found for Nested "ObjectEntryUsageFlagBitsNVX" "OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX"
pattern OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX :: (a ~ ObjectEntryUsageFlagBitsNVX) => a
pattern OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX

-- No documentation found for TopLevel "ObjectEntryUsageFlagsNVX"
type ObjectEntryUsageFlagsNVX = ObjectEntryUsageFlagBitsNVX


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkObjectTableCreateInfoNVX"
data ObjectTableCreateInfoNVX = ObjectTableCreateInfoNVX
  { -- No documentation found for Nested "ObjectTableCreateInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "pObjectEntryTypes"
  objectEntryTypes :: Vector ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "pObjectEntryCounts"
  objectEntryCounts :: Vector Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "pObjectEntryUsageFlags"
  objectEntryUsageFlags :: Vector ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxUniformBuffersPerDescriptor"
  maxUniformBuffersPerDescriptor :: Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxStorageBuffersPerDescriptor"
  maxStorageBuffersPerDescriptor :: Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxStorageImagesPerDescriptor"
  maxStorageImagesPerDescriptor :: Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxSampledImagesPerDescriptor"
  maxSampledImagesPerDescriptor :: Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxPipelineLayouts"
  maxPipelineLayouts :: Word32
  }
  deriving (Show, Eq)

instance Zero ObjectTableCreateInfoNVX where
  zero = ObjectTableCreateInfoNVX Nothing
                                  mempty
                                  mempty
                                  mempty
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero

#endif


-- No documentation found for TopLevel "VkObjectTableDescriptorSetEntryNVX"
data ObjectTableDescriptorSetEntryNVX = ObjectTableDescriptorSetEntryNVX
  { -- No documentation found for Nested "ObjectTableDescriptorSetEntryNVX" "type"
  type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableDescriptorSetEntryNVX" "flags"
  flags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTableDescriptorSetEntryNVX" "pipelineLayout"
  pipelineLayout :: PipelineLayout
  , -- No documentation found for Nested "ObjectTableDescriptorSetEntryNVX" "descriptorSet"
  descriptorSet :: DescriptorSet
  }
  deriving (Show, Eq)

instance Zero ObjectTableDescriptorSetEntryNVX where
  zero = ObjectTableDescriptorSetEntryNVX zero
                                          zero
                                          zero
                                          zero



-- No documentation found for TopLevel "VkObjectTableEntryNVX"
data ObjectTableEntryNVX = ObjectTableEntryNVX
  { -- No documentation found for Nested "ObjectTableEntryNVX" "type"
  type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableEntryNVX" "flags"
  flags :: ObjectEntryUsageFlagsNVX
  }
  deriving (Show, Eq)

instance Zero ObjectTableEntryNVX where
  zero = ObjectTableEntryNVX zero
                             zero



-- No documentation found for TopLevel "VkObjectTableIndexBufferEntryNVX"
data ObjectTableIndexBufferEntryNVX = ObjectTableIndexBufferEntryNVX
  { -- No documentation found for Nested "ObjectTableIndexBufferEntryNVX" "type"
  type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableIndexBufferEntryNVX" "flags"
  flags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTableIndexBufferEntryNVX" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "ObjectTableIndexBufferEntryNVX" "indexType"
  indexType :: IndexType
  }
  deriving (Show, Eq)

instance Zero ObjectTableIndexBufferEntryNVX where
  zero = ObjectTableIndexBufferEntryNVX zero
                                        zero
                                        zero
                                        zero


-- No documentation found for TopLevel "ObjectTableNVX"
type ObjectTableNVX = VkObjectTableNVX


-- No documentation found for TopLevel "VkObjectTablePipelineEntryNVX"
data ObjectTablePipelineEntryNVX = ObjectTablePipelineEntryNVX
  { -- No documentation found for Nested "ObjectTablePipelineEntryNVX" "type"
  type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTablePipelineEntryNVX" "flags"
  flags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTablePipelineEntryNVX" "pipeline"
  pipeline :: Pipeline
  }
  deriving (Show, Eq)

instance Zero ObjectTablePipelineEntryNVX where
  zero = ObjectTablePipelineEntryNVX zero
                                     zero
                                     zero



-- No documentation found for TopLevel "VkObjectTablePushConstantEntryNVX"
data ObjectTablePushConstantEntryNVX = ObjectTablePushConstantEntryNVX
  { -- No documentation found for Nested "ObjectTablePushConstantEntryNVX" "type"
  type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTablePushConstantEntryNVX" "flags"
  flags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTablePushConstantEntryNVX" "pipelineLayout"
  pipelineLayout :: PipelineLayout
  , -- No documentation found for Nested "ObjectTablePushConstantEntryNVX" "stageFlags"
  stageFlags :: ShaderStageFlags
  }
  deriving (Show, Eq)

instance Zero ObjectTablePushConstantEntryNVX where
  zero = ObjectTablePushConstantEntryNVX zero
                                         zero
                                         zero
                                         zero



-- No documentation found for TopLevel "VkObjectTableVertexBufferEntryNVX"
data ObjectTableVertexBufferEntryNVX = ObjectTableVertexBufferEntryNVX
  { -- No documentation found for Nested "ObjectTableVertexBufferEntryNVX" "type"
  type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableVertexBufferEntryNVX" "flags"
  flags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTableVertexBufferEntryNVX" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

instance Zero ObjectTableVertexBufferEntryNVX where
  zero = ObjectTableVertexBufferEntryNVX zero
                                         zero
                                         zero



-- No documentation found for TopLevel "vkCmdProcessCommandsNVX"
cmdProcessCommandsNVX :: CommandBuffer ->  CmdProcessCommandsInfoNVX ->  IO ()
cmdProcessCommandsNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdReserveSpaceForCommandsNVX"
cmdReserveSpaceForCommandsNVX :: CommandBuffer ->  CmdReserveSpaceForCommandsInfoNVX ->  IO ()
cmdReserveSpaceForCommandsNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateIndirectCommandsLayoutNVX"
createIndirectCommandsLayoutNVX :: Device ->  IndirectCommandsLayoutCreateInfoNVX ->  Maybe AllocationCallbacks ->  IO (IndirectCommandsLayoutNVX)
createIndirectCommandsLayoutNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateObjectTableNVX"
createObjectTableNVX :: Device ->  ObjectTableCreateInfoNVX ->  Maybe AllocationCallbacks ->  IO (ObjectTableNVX)
createObjectTableNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyIndirectCommandsLayoutNVX"
destroyIndirectCommandsLayoutNVX :: Device ->  IndirectCommandsLayoutNVX ->  Maybe AllocationCallbacks ->  IO ()
destroyIndirectCommandsLayoutNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyObjectTableNVX"
destroyObjectTableNVX :: Device ->  ObjectTableNVX ->  Maybe AllocationCallbacks ->  IO ()
destroyObjectTableNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"
getPhysicalDeviceGeneratedCommandsPropertiesNVX :: PhysicalDevice ->  IO (DeviceGeneratedCommandsFeaturesNVX, DeviceGeneratedCommandsLimitsNVX)
getPhysicalDeviceGeneratedCommandsPropertiesNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkRegisterObjectsNVX"
registerObjectsNVX :: Device ->  ObjectTableNVX ->  Vector ObjectTableEntryNVX ->  Vector Word32 ->  IO ()
registerObjectsNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkUnregisterObjectsNVX"
unregisterObjectsNVX :: Device ->  ObjectTableNVX ->  Vector ObjectEntryTypeNVX ->  Vector Word32 ->  IO ()
unregisterObjectsNVX = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createIndirectCommandsLayoutNVX' and 'destroyIndirectCommandsLayoutNVX' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withIndirectCommandsLayoutNVX
  :: Device -> IndirectCommandsLayoutCreateInfoNVX -> Maybe AllocationCallbacks -> (IndirectCommandsLayoutNVX -> IO a) -> IO a
withIndirectCommandsLayoutNVX device indirectCommandsLayoutCreateInfoNVX allocationCallbacks = bracket
  (createIndirectCommandsLayoutNVX device indirectCommandsLayoutCreateInfoNVX allocationCallbacks)
  (\o -> destroyIndirectCommandsLayoutNVX device o allocationCallbacks)

-- | A safe wrapper for 'createObjectTableNVX' and 'destroyObjectTableNVX' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withObjectTableNVX
  :: Device -> ObjectTableCreateInfoNVX -> Maybe AllocationCallbacks -> (ObjectTableNVX -> IO a) -> IO a
withObjectTableNVX device objectTableCreateInfoNVX allocationCallbacks = bracket
  (createObjectTableNVX device objectTableCreateInfoNVX allocationCallbacks)
  (\o -> destroyObjectTableNVX device o allocationCallbacks)

-- | A safe wrapper for 'registerObjectsNVX' and 'unregisterObjectsNVX' using 'bracket_'
--
-- The allocated value must not be returned from the provided computation
withRegisteredObjectsNVX
  :: Device -> ObjectTableNVX -> Vector ObjectTableEntryNVX -> Vector Word32 -> Vector ObjectEntryTypeNVX -> IO a -> IO a
withRegisteredObjectsNVX device objectTableNVX objectTableEntryNVX objectIndices objectEntryTypeNVX = bracket_
  (registerObjectsNVX device objectTableNVX objectTableEntryNVX objectIndices)
  ( unregisterObjectsNVX device objectTableNVX objectEntryTypeNVX objectIndices)

-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME"
pattern NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION"
pattern NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: Integral a => a
pattern NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
