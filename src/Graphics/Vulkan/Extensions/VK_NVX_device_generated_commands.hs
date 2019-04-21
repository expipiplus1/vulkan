{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( withCStructCmdProcessCommandsInfoNVX
  , fromCStructCmdProcessCommandsInfoNVX
  , CmdProcessCommandsInfoNVX(..)
  , withCStructCmdReserveSpaceForCommandsInfoNVX
  , fromCStructCmdReserveSpaceForCommandsInfoNVX
  , CmdReserveSpaceForCommandsInfoNVX(..)
  , withCStructDeviceGeneratedCommandsFeaturesNVX
  , fromCStructDeviceGeneratedCommandsFeaturesNVX
  , DeviceGeneratedCommandsFeaturesNVX(..)
  , withCStructDeviceGeneratedCommandsLimitsNVX
  , fromCStructDeviceGeneratedCommandsLimitsNVX
  , DeviceGeneratedCommandsLimitsNVX(..)
  , withCStructIndirectCommandsLayoutCreateInfoNVX
  , fromCStructIndirectCommandsLayoutCreateInfoNVX
  , IndirectCommandsLayoutCreateInfoNVX(..)
  , IndirectCommandsLayoutNVX
  , withCStructIndirectCommandsLayoutTokenNVX
  , fromCStructIndirectCommandsLayoutTokenNVX
  , IndirectCommandsLayoutTokenNVX(..)
  , IndirectCommandsLayoutUsageFlagBitsNVX
  , pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
  , pattern INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
  , pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
  , pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
  , IndirectCommandsLayoutUsageFlagsNVX
  , withCStructIndirectCommandsTokenNVX
  , fromCStructIndirectCommandsTokenNVX
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
  , withCStructObjectTableCreateInfoNVX
  , fromCStructObjectTableCreateInfoNVX
  , ObjectTableCreateInfoNVX(..)
  , withCStructObjectTableDescriptorSetEntryNVX
  , fromCStructObjectTableDescriptorSetEntryNVX
  , ObjectTableDescriptorSetEntryNVX(..)
  , withCStructObjectTableEntryNVX
  , fromCStructObjectTableEntryNVX
  , ObjectTableEntryNVX(..)
  , withCStructObjectTableIndexBufferEntryNVX
  , fromCStructObjectTableIndexBufferEntryNVX
  , ObjectTableIndexBufferEntryNVX(..)
  , ObjectTableNVX
  , withCStructObjectTablePipelineEntryNVX
  , fromCStructObjectTablePipelineEntryNVX
  , ObjectTablePipelineEntryNVX(..)
  , withCStructObjectTablePushConstantEntryNVX
  , fromCStructObjectTablePushConstantEntryNVX
  , ObjectTablePushConstantEntryNVX(..)
  , withCStructObjectTableVertexBufferEntryNVX
  , fromCStructObjectTableVertexBufferEntryNVX
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
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
  , pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX
  , pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
  , pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX
  , pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX
  ) where

import Control.Exception
  ( bracket
  , bracket_
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
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkCmdProcessCommandsInfoNVX(..)
  , VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkIndirectCommandsLayoutTokenNVX(..)
  , VkIndirectCommandsLayoutUsageFlagBitsNVX(..)
  , VkIndirectCommandsTokenNVX(..)
  , VkIndirectCommandsTokenTypeNVX(..)
  , VkObjectEntryTypeNVX(..)
  , VkObjectEntryUsageFlagBitsNVX(..)
  , VkObjectTableCreateInfoNVX(..)
  , VkObjectTableDescriptorSetEntryNVX(..)
  , VkObjectTableEntryNVX(..)
  , VkObjectTableIndexBufferEntryNVX(..)
  , VkObjectTablePipelineEntryNVX(..)
  , VkObjectTablePushConstantEntryNVX(..)
  , VkObjectTableVertexBufferEntryNVX(..)
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
  , pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
  , pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
  , pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
  )
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( IndexType
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , PhysicalDevice(..)
  , DeviceSize
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Pass
  ( PipelineBindPoint
  )
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
import Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX
  , pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX
  , pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX
  , pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
  )



-- | VkCmdProcessCommandsInfoNVX - Structure specifying parameters for the
-- generation of commands
--
-- == Valid Usage
--
-- -   The provided @objectTable@ /must/ include all objects referenced by
--     the generation process
--
-- -   @indirectCommandsTokenCount@ /must/ match the
--     @indirectCommandsLayout@’s @tokenCount@
--
-- -   The @tokenType@ member of each entry in the
--     @pIndirectCommandsTokens@ array /must/ match the values used at
--     creation time of @indirectCommandsLayout@
--
-- -   If @targetCommandBuffer@ is provided, it /must/ have reserved
--     command space
--
-- -   If @targetCommandBuffer@ is provided, the @objectTable@ /must/ match
--     the reservation’s @objectTable@ and /must/ have had all referenced
--     objects registered at reservation time
--
-- -   If @targetCommandBuffer@ is provided, the @indirectCommandsLayout@
--     /must/ match the reservation’s @indirectCommandsLayout@
--
-- -   If @targetCommandBuffer@ is provided, the @maxSequencesCount@ /must/
--     not exceed the reservation’s @maxSequencesCount@
--
-- -   If @sequencesCountBuffer@ is used, its usage flag /must/ have the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   If @sequencesCountBuffer@ is used, @sequencesCountOffset@ /must/ be
--     aligned to
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsLimitsNVX'::@minSequenceCountBufferOffsetAlignment@
--
-- -   If @sequencesIndexBuffer@ is used, its usage flag /must/ have the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   If @sequencesIndexBuffer@ is used, @sequencesIndexOffset@ /must/ be
--     aligned to
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsLimitsNVX'::@minSequenceIndexBufferOffsetAlignment@
--
-- Unresolved directive in VkCmdProcessCommandsInfoNVX.txt -
-- include::{generated}\/validity\/structs\/VkCmdProcessCommandsInfoNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data CmdProcessCommandsInfoNVX = CmdProcessCommandsInfoNVX
  { -- Univalued member elided
  -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "objectTable"
  objectTable :: ObjectTableNVX
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "indirectCommandsLayout"
  indirectCommandsLayout :: IndirectCommandsLayoutNVX
  -- Length valued member elided
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "pIndirectCommandsTokens"
  indirectCommandsTokens :: Vector IndirectCommandsTokenNVX
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "maxSequencesCount"
  maxSequencesCount :: Word32
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "targetCommandBuffer"
  targetCommandBuffer :: CommandBuffer
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

-- | A function to temporarily allocate memory for a 'VkCmdProcessCommandsInfoNVX' and
-- marshal a 'CmdProcessCommandsInfoNVX' into it. The 'VkCmdProcessCommandsInfoNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCmdProcessCommandsInfoNVX :: CmdProcessCommandsInfoNVX -> (VkCmdProcessCommandsInfoNVX -> IO a) -> IO a
withCStructCmdProcessCommandsInfoNVX marshalled cont = withVec withCStructIndirectCommandsTokenNVX (indirectCommandsTokens (marshalled :: CmdProcessCommandsInfoNVX)) (\pPIndirectCommandsTokens -> maybeWith withSomeVkStruct (next (marshalled :: CmdProcessCommandsInfoNVX)) (\pPNext -> cont (VkCmdProcessCommandsInfoNVX VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX pPNext (objectTable (marshalled :: CmdProcessCommandsInfoNVX)) (indirectCommandsLayout (marshalled :: CmdProcessCommandsInfoNVX)) (fromIntegral (Data.Vector.length (indirectCommandsTokens (marshalled :: CmdProcessCommandsInfoNVX)))) pPIndirectCommandsTokens (maxSequencesCount (marshalled :: CmdProcessCommandsInfoNVX)) (commandBufferHandle (targetCommandBuffer (marshalled :: CmdProcessCommandsInfoNVX))) (sequencesCountBuffer (marshalled :: CmdProcessCommandsInfoNVX)) (sequencesCountOffset (marshalled :: CmdProcessCommandsInfoNVX)) (sequencesIndexBuffer (marshalled :: CmdProcessCommandsInfoNVX)) (sequencesIndexOffset (marshalled :: CmdProcessCommandsInfoNVX)))))

-- | A function to read a 'VkCmdProcessCommandsInfoNVX' and all additional
-- structures in the pointer chain into a 'CmdProcessCommandsInfoNVX'.
fromCStructCmdProcessCommandsInfoNVX :: DeviceCmds -> VkCmdProcessCommandsInfoNVX -> IO CmdProcessCommandsInfoNVX
fromCStructCmdProcessCommandsInfoNVX commandTable c = CmdProcessCommandsInfoNVX <$> -- Univalued Member elided
                                                                                maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCmdProcessCommandsInfoNVX)))
                                                                                <*> pure (vkObjectTable (c :: VkCmdProcessCommandsInfoNVX))
                                                                                <*> pure (vkIndirectCommandsLayout (c :: VkCmdProcessCommandsInfoNVX))
                                                                                -- Length valued member elided
                                                                                <*> (Data.Vector.generateM (fromIntegral (vkIndirectCommandsTokenCount (c :: VkCmdProcessCommandsInfoNVX))) (((fromCStructIndirectCommandsTokenNVX <=<) . peekElemOff) (vkPIndirectCommandsTokens (c :: VkCmdProcessCommandsInfoNVX))))
                                                                                <*> pure (vkMaxSequencesCount (c :: VkCmdProcessCommandsInfoNVX))
                                                                                <*> pure (CommandBuffer (vkTargetCommandBuffer (c :: VkCmdProcessCommandsInfoNVX)) commandTable)
                                                                                <*> pure (vkSequencesCountBuffer (c :: VkCmdProcessCommandsInfoNVX))
                                                                                <*> pure (vkSequencesCountOffset (c :: VkCmdProcessCommandsInfoNVX))
                                                                                <*> pure (vkSequencesIndexBuffer (c :: VkCmdProcessCommandsInfoNVX))
                                                                                <*> pure (vkSequencesIndexOffset (c :: VkCmdProcessCommandsInfoNVX))





-- | VkCmdReserveSpaceForCommandsInfoNVX - Structure specifying parameters
-- for the reservation of command buffer space
--
-- = Description
--
-- Unresolved directive in VkCmdReserveSpaceForCommandsInfoNVX.txt -
-- include::{generated}\/validity\/structs\/VkCmdReserveSpaceForCommandsInfoNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data CmdReserveSpaceForCommandsInfoNVX = CmdReserveSpaceForCommandsInfoNVX
  { -- Univalued member elided
  -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "objectTable"
  objectTable :: ObjectTableNVX
  , -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "indirectCommandsLayout"
  indirectCommandsLayout :: IndirectCommandsLayoutNVX
  , -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "maxSequencesCount"
  maxSequencesCount :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCmdReserveSpaceForCommandsInfoNVX' and
-- marshal a 'CmdReserveSpaceForCommandsInfoNVX' into it. The 'VkCmdReserveSpaceForCommandsInfoNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCmdReserveSpaceForCommandsInfoNVX :: CmdReserveSpaceForCommandsInfoNVX -> (VkCmdReserveSpaceForCommandsInfoNVX -> IO a) -> IO a
withCStructCmdReserveSpaceForCommandsInfoNVX marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: CmdReserveSpaceForCommandsInfoNVX)) (\pPNext -> cont (VkCmdReserveSpaceForCommandsInfoNVX VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX pPNext (objectTable (marshalled :: CmdReserveSpaceForCommandsInfoNVX)) (indirectCommandsLayout (marshalled :: CmdReserveSpaceForCommandsInfoNVX)) (maxSequencesCount (marshalled :: CmdReserveSpaceForCommandsInfoNVX))))

-- | A function to read a 'VkCmdReserveSpaceForCommandsInfoNVX' and all additional
-- structures in the pointer chain into a 'CmdReserveSpaceForCommandsInfoNVX'.
fromCStructCmdReserveSpaceForCommandsInfoNVX :: VkCmdReserveSpaceForCommandsInfoNVX -> IO CmdReserveSpaceForCommandsInfoNVX
fromCStructCmdReserveSpaceForCommandsInfoNVX c = CmdReserveSpaceForCommandsInfoNVX <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCmdReserveSpaceForCommandsInfoNVX)))
                                                                                   <*> pure (vkObjectTable (c :: VkCmdReserveSpaceForCommandsInfoNVX))
                                                                                   <*> pure (vkIndirectCommandsLayout (c :: VkCmdReserveSpaceForCommandsInfoNVX))
                                                                                   <*> pure (vkMaxSequencesCount (c :: VkCmdReserveSpaceForCommandsInfoNVX))

instance Zero CmdReserveSpaceForCommandsInfoNVX where
  zero = CmdReserveSpaceForCommandsInfoNVX Nothing
                                           zero
                                           zero
                                           zero



-- | VkDeviceGeneratedCommandsFeaturesNVX - Structure specifying physical
-- device support
--
-- = Description
--
-- Unresolved directive in VkDeviceGeneratedCommandsFeaturesNVX.txt -
-- include::{generated}\/validity\/structs\/VkDeviceGeneratedCommandsFeaturesNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data DeviceGeneratedCommandsFeaturesNVX = DeviceGeneratedCommandsFeaturesNVX
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGeneratedCommandsFeaturesNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGeneratedCommandsFeaturesNVX" "computeBindingPointSupport"
  computeBindingPointSupport :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceGeneratedCommandsFeaturesNVX' and
-- marshal a 'DeviceGeneratedCommandsFeaturesNVX' into it. The 'VkDeviceGeneratedCommandsFeaturesNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGeneratedCommandsFeaturesNVX :: DeviceGeneratedCommandsFeaturesNVX -> (VkDeviceGeneratedCommandsFeaturesNVX -> IO a) -> IO a
withCStructDeviceGeneratedCommandsFeaturesNVX marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DeviceGeneratedCommandsFeaturesNVX)) (\pPNext -> cont (VkDeviceGeneratedCommandsFeaturesNVX VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX pPNext (boolToBool32 (computeBindingPointSupport (marshalled :: DeviceGeneratedCommandsFeaturesNVX)))))

-- | A function to read a 'VkDeviceGeneratedCommandsFeaturesNVX' and all additional
-- structures in the pointer chain into a 'DeviceGeneratedCommandsFeaturesNVX'.
fromCStructDeviceGeneratedCommandsFeaturesNVX :: VkDeviceGeneratedCommandsFeaturesNVX -> IO DeviceGeneratedCommandsFeaturesNVX
fromCStructDeviceGeneratedCommandsFeaturesNVX c = DeviceGeneratedCommandsFeaturesNVX <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGeneratedCommandsFeaturesNVX)))
                                                                                     <*> pure (bool32ToBool (vkComputeBindingPointSupport (c :: VkDeviceGeneratedCommandsFeaturesNVX)))

instance Zero DeviceGeneratedCommandsFeaturesNVX where
  zero = DeviceGeneratedCommandsFeaturesNVX Nothing
                                            False



-- | VkDeviceGeneratedCommandsLimitsNVX - Structure specifying physical
-- device limits
--
-- = Description
--
-- Unresolved directive in VkDeviceGeneratedCommandsLimitsNVX.txt -
-- include::{generated}\/validity\/structs\/VkDeviceGeneratedCommandsLimitsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data DeviceGeneratedCommandsLimitsNVX = DeviceGeneratedCommandsLimitsNVX
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkDeviceGeneratedCommandsLimitsNVX' and
-- marshal a 'DeviceGeneratedCommandsLimitsNVX' into it. The 'VkDeviceGeneratedCommandsLimitsNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGeneratedCommandsLimitsNVX :: DeviceGeneratedCommandsLimitsNVX -> (VkDeviceGeneratedCommandsLimitsNVX -> IO a) -> IO a
withCStructDeviceGeneratedCommandsLimitsNVX marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DeviceGeneratedCommandsLimitsNVX)) (\pPNext -> cont (VkDeviceGeneratedCommandsLimitsNVX VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX pPNext (maxIndirectCommandsLayoutTokenCount (marshalled :: DeviceGeneratedCommandsLimitsNVX)) (maxObjectEntryCounts (marshalled :: DeviceGeneratedCommandsLimitsNVX)) (minSequenceCountBufferOffsetAlignment (marshalled :: DeviceGeneratedCommandsLimitsNVX)) (minSequenceIndexBufferOffsetAlignment (marshalled :: DeviceGeneratedCommandsLimitsNVX)) (minCommandsTokenBufferOffsetAlignment (marshalled :: DeviceGeneratedCommandsLimitsNVX))))

-- | A function to read a 'VkDeviceGeneratedCommandsLimitsNVX' and all additional
-- structures in the pointer chain into a 'DeviceGeneratedCommandsLimitsNVX'.
fromCStructDeviceGeneratedCommandsLimitsNVX :: VkDeviceGeneratedCommandsLimitsNVX -> IO DeviceGeneratedCommandsLimitsNVX
fromCStructDeviceGeneratedCommandsLimitsNVX c = DeviceGeneratedCommandsLimitsNVX <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGeneratedCommandsLimitsNVX)))
                                                                                 <*> pure (vkMaxIndirectCommandsLayoutTokenCount (c :: VkDeviceGeneratedCommandsLimitsNVX))
                                                                                 <*> pure (vkMaxObjectEntryCounts (c :: VkDeviceGeneratedCommandsLimitsNVX))
                                                                                 <*> pure (vkMinSequenceCountBufferOffsetAlignment (c :: VkDeviceGeneratedCommandsLimitsNVX))
                                                                                 <*> pure (vkMinSequenceIndexBufferOffsetAlignment (c :: VkDeviceGeneratedCommandsLimitsNVX))
                                                                                 <*> pure (vkMinCommandsTokenBufferOffsetAlignment (c :: VkDeviceGeneratedCommandsLimitsNVX))

instance Zero DeviceGeneratedCommandsLimitsNVX where
  zero = DeviceGeneratedCommandsLimitsNVX Nothing
                                          zero
                                          zero
                                          zero
                                          zero
                                          zero



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
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsLimitsNVX'::@maxIndirectCommandsLayoutTokenCount@
--
-- -   If the
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsFeaturesNVX'::@computeBindingPointSupport@
--     feature is not enabled, then @pipelineBindPoint@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_COMPUTE'
--
-- -   If @pTokens@ contains an entry of
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX'
--     it /must/ be the first element of the array and there /must/ be only
--     a single element of such token type.
--
-- -   All state binding tokens in @pTokens@ /must/ occur prior work
--     provoking tokens
--     ('Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX',
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX',
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX').
--
-- -   The content of @pTokens@ /must/ include one single work provoking
--     token that is compatible with the @pipelineBindPoint@.
--
-- Unresolved directive in VkIndirectCommandsLayoutCreateInfoNVX.txt -
-- include::{generated}\/validity\/structs\/VkIndirectCommandsLayoutCreateInfoNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data IndirectCommandsLayoutCreateInfoNVX = IndirectCommandsLayoutCreateInfoNVX
  { -- Univalued member elided
  -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "pipelineBindPoint"
  pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "flags"
  flags :: IndirectCommandsLayoutUsageFlagsNVX
  -- Length valued member elided
  , -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "pTokens"
  tokens :: Vector IndirectCommandsLayoutTokenNVX
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkIndirectCommandsLayoutCreateInfoNVX' and
-- marshal a 'IndirectCommandsLayoutCreateInfoNVX' into it. The 'VkIndirectCommandsLayoutCreateInfoNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructIndirectCommandsLayoutCreateInfoNVX :: IndirectCommandsLayoutCreateInfoNVX -> (VkIndirectCommandsLayoutCreateInfoNVX -> IO a) -> IO a
withCStructIndirectCommandsLayoutCreateInfoNVX marshalled cont = withVec withCStructIndirectCommandsLayoutTokenNVX (tokens (marshalled :: IndirectCommandsLayoutCreateInfoNVX)) (\pPTokens -> maybeWith withSomeVkStruct (next (marshalled :: IndirectCommandsLayoutCreateInfoNVX)) (\pPNext -> cont (VkIndirectCommandsLayoutCreateInfoNVX VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX pPNext (pipelineBindPoint (marshalled :: IndirectCommandsLayoutCreateInfoNVX)) (flags (marshalled :: IndirectCommandsLayoutCreateInfoNVX)) (fromIntegral (Data.Vector.length (tokens (marshalled :: IndirectCommandsLayoutCreateInfoNVX)))) pPTokens)))

-- | A function to read a 'VkIndirectCommandsLayoutCreateInfoNVX' and all additional
-- structures in the pointer chain into a 'IndirectCommandsLayoutCreateInfoNVX'.
fromCStructIndirectCommandsLayoutCreateInfoNVX :: VkIndirectCommandsLayoutCreateInfoNVX -> IO IndirectCommandsLayoutCreateInfoNVX
fromCStructIndirectCommandsLayoutCreateInfoNVX c = IndirectCommandsLayoutCreateInfoNVX <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkIndirectCommandsLayoutCreateInfoNVX)))
                                                                                       <*> pure (vkPipelineBindPoint (c :: VkIndirectCommandsLayoutCreateInfoNVX))
                                                                                       <*> pure (vkFlags (c :: VkIndirectCommandsLayoutCreateInfoNVX))
                                                                                       -- Length valued member elided
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkTokenCount (c :: VkIndirectCommandsLayoutCreateInfoNVX))) (((fromCStructIndirectCommandsLayoutTokenNVX <=<) . peekElemOff) (vkPTokens (c :: VkIndirectCommandsLayoutCreateInfoNVX))))

instance Zero IndirectCommandsLayoutCreateInfoNVX where
  zero = IndirectCommandsLayoutCreateInfoNVX Nothing
                                             zero
                                             zero
                                             Data.Vector.empty


-- | VkIndirectCommandsLayoutNVX - Opaque handle to an indirect commands
-- layout object
--
-- = See Also
--
-- No cross-references are available
type IndirectCommandsLayoutNVX = VkIndirectCommandsLayoutNVX


-- | VkIndirectCommandsLayoutTokenNVX - Struct specifying the details of an
-- indirect command layout token
--
-- == Valid Usage
--
-- Unresolved directive in VkIndirectCommandsLayoutTokenNVX.txt -
-- include::{generated}\/validity\/structs\/VkIndirectCommandsLayoutTokenNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
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

-- | A function to temporarily allocate memory for a 'VkIndirectCommandsLayoutTokenNVX' and
-- marshal a 'IndirectCommandsLayoutTokenNVX' into it. The 'VkIndirectCommandsLayoutTokenNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructIndirectCommandsLayoutTokenNVX :: IndirectCommandsLayoutTokenNVX -> (VkIndirectCommandsLayoutTokenNVX -> IO a) -> IO a
withCStructIndirectCommandsLayoutTokenNVX marshalled cont = cont (VkIndirectCommandsLayoutTokenNVX (tokenType (marshalled :: IndirectCommandsLayoutTokenNVX)) (bindingUnit (marshalled :: IndirectCommandsLayoutTokenNVX)) (dynamicCount (marshalled :: IndirectCommandsLayoutTokenNVX)) (divisor (marshalled :: IndirectCommandsLayoutTokenNVX)))

-- | A function to read a 'VkIndirectCommandsLayoutTokenNVX' and all additional
-- structures in the pointer chain into a 'IndirectCommandsLayoutTokenNVX'.
fromCStructIndirectCommandsLayoutTokenNVX :: VkIndirectCommandsLayoutTokenNVX -> IO IndirectCommandsLayoutTokenNVX
fromCStructIndirectCommandsLayoutTokenNVX c = IndirectCommandsLayoutTokenNVX <$> pure (vkTokenType (c :: VkIndirectCommandsLayoutTokenNVX))
                                                                             <*> pure (vkBindingUnit (c :: VkIndirectCommandsLayoutTokenNVX))
                                                                             <*> pure (vkDynamicCount (c :: VkIndirectCommandsLayoutTokenNVX))
                                                                             <*> pure (vkDivisor (c :: VkIndirectCommandsLayoutTokenNVX))

instance Zero IndirectCommandsLayoutTokenNVX where
  zero = IndirectCommandsLayoutTokenNVX zero
                                        zero
                                        zero
                                        zero


-- | VkIndirectCommandsLayoutUsageFlagBitsNVX - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- No cross-references are available
type IndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX'
-- specifies that the processing of sequences /can/ happen at an
-- implementation-dependent order, which is not guaranteed to be coherent
-- across multiple invocations.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX :: (a ~ IndirectCommandsLayoutUsageFlagBitsNVX) => a
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX'
-- specifies that there is likely a high difference between allocated
-- number of sequences and actually used.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX :: (a ~ IndirectCommandsLayoutUsageFlagBitsNVX) => a
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX'
-- specifies that there are likely many draw or dispatch calls that are
-- zero-sized (zero grid dimension, no primitives to render).
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX :: (a ~ IndirectCommandsLayoutUsageFlagBitsNVX) => a
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX'
-- specifies that the input data for the sequences is not implicitly
-- indexed from 0..sequencesUsed but a user provided
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' encoding the index
-- is provided.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX :: (a ~ IndirectCommandsLayoutUsageFlagBitsNVX) => a
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX

-- | VkIndirectCommandsLayoutUsageFlagsNVX - Bitmask of
-- VkIndirectCommandsLayoutUsageFlagBitsNVX
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutUsageFlagsNVX'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutUsageFlagBitsNVX'.
--
-- = See Also
--
-- No cross-references are available
type IndirectCommandsLayoutUsageFlagsNVX = IndirectCommandsLayoutUsageFlagBitsNVX


-- | VkIndirectCommandsTokenNVX - Structure specifying parameters for the
-- reservation of command buffer space
--
-- == Valid Usage
--
-- -   The @buffer@’s usage flag /must/ have the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set.
--
-- -   The @offset@ /must/ be aligned to
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsLimitsNVX'::@minCommandsTokenBufferOffsetAlignment@.
--
-- Unresolved directive in VkIndirectCommandsTokenNVX.txt -
-- include::{generated}\/validity\/structs\/VkIndirectCommandsTokenNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data IndirectCommandsTokenNVX = IndirectCommandsTokenNVX
  { -- No documentation found for Nested "IndirectCommandsTokenNVX" "tokenType"
  tokenType :: IndirectCommandsTokenTypeNVX
  , -- No documentation found for Nested "IndirectCommandsTokenNVX" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "IndirectCommandsTokenNVX" "offset"
  offset :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkIndirectCommandsTokenNVX' and
-- marshal a 'IndirectCommandsTokenNVX' into it. The 'VkIndirectCommandsTokenNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructIndirectCommandsTokenNVX :: IndirectCommandsTokenNVX -> (VkIndirectCommandsTokenNVX -> IO a) -> IO a
withCStructIndirectCommandsTokenNVX marshalled cont = cont (VkIndirectCommandsTokenNVX (tokenType (marshalled :: IndirectCommandsTokenNVX)) (buffer (marshalled :: IndirectCommandsTokenNVX)) (offset (marshalled :: IndirectCommandsTokenNVX)))

-- | A function to read a 'VkIndirectCommandsTokenNVX' and all additional
-- structures in the pointer chain into a 'IndirectCommandsTokenNVX'.
fromCStructIndirectCommandsTokenNVX :: VkIndirectCommandsTokenNVX -> IO IndirectCommandsTokenNVX
fromCStructIndirectCommandsTokenNVX c = IndirectCommandsTokenNVX <$> pure (vkTokenType (c :: VkIndirectCommandsTokenNVX))
                                                                 <*> pure (vkBuffer (c :: VkIndirectCommandsTokenNVX))
                                                                 <*> pure (vkOffset (c :: VkIndirectCommandsTokenNVX))

instance Zero IndirectCommandsTokenNVX where
  zero = IndirectCommandsTokenNVX zero
                                  zero
                                  zero


-- | VkIndirectCommandsTokenTypeNVX - Enum specifying
--
-- = Description
--
-- \'
--
-- > +-----------------------------------------------+----------------------+
-- > | Token type                                    | Equivalent command   |
-- > +===============================================+======================+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_PIPELINE_NVX'                            | uilding.vkCmdBindPip |
-- > |                                               | eline'               |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_DESCRIPTOR_SET_NVX'                      | uilding.vkCmdBindDes |
-- > |                                               | criptorSets'         |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_INDEX_BUFFER_NVX'                        | uilding.vkCmdBindInd |
-- > |                                               | exBuffer'            |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_VERTEX_BUFFER_NVX'                       | uilding.vkCmdBindVer |
-- > |                                               | texBuffers'          |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_PUSH_CONSTANT_NVX'                       | uilding.vkCmdPushCon |
-- > |                                               | stants'              |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_DRAW_INDEXED_NVX'                        | uilding.vkCmdDrawInd |
-- > |                                               | exedIndirect'        |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_DRAW_NVX'                                | uilding.vkCmdDrawInd |
-- > |                                               | irect'               |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_DISPATCH_NVX'                            | uilding.vkCmdDispatc |
-- > |                                               | hIndirect'           |
-- > +-----------------------------------------------+----------------------+
-- >
-- > Supported indirect command tokens
--
-- = See Also
--
-- No cross-references are available
type IndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX


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

-- | VkObjectEntryTypeNVX - Enum specifying object table entry type
--
-- = See Also
--
-- No cross-references are available
type ObjectEntryTypeNVX = VkObjectEntryTypeNVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX'
-- specifies a 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet'
-- resource entry that is registered via
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableDescriptorSetEntryNVX'.
pattern OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX'
-- specifies a 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' resource
-- entry that is registered via
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePipelineEntryNVX'.
pattern OBJECT_ENTRY_TYPE_PIPELINE_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_PIPELINE_NVX = VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX'
-- specifies a 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer'
-- resource entry that is registered via
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableIndexBufferEntryNVX'.
pattern OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX'
-- specifies a 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer'
-- resource entry that is registered via
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableVertexBufferEntryNVX'.
pattern OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX'
-- specifies the resource entry is registered via
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePushConstantEntryNVX'.
pattern OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX :: (a ~ ObjectEntryTypeNVX) => a
pattern OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX

-- | VkObjectEntryUsageFlagBitsNVX - Bitmask specifying allowed usage of an
-- object entry
--
-- = See Also
--
-- No cross-references are available
type ObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX'
-- specifies that the resource is bound to
-- 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
pattern OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX :: (a ~ ObjectEntryUsageFlagBitsNVX) => a
pattern OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX


-- | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX'
-- specifies that the resource is bound to
-- 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_COMPUTE'
pattern OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX :: (a ~ ObjectEntryUsageFlagBitsNVX) => a
pattern OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX

-- | VkObjectEntryUsageFlagsNVX - Bitmask of VkObjectEntryUsageFlagBitsNVX
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectEntryUsageFlagsNVX'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectEntryUsageFlagBitsNVX'.
--
-- = See Also
--
-- No cross-references are available
type ObjectEntryUsageFlagsNVX = ObjectEntryUsageFlagBitsNVX


-- | VkObjectTableCreateInfoNVX - Structure specifying the parameters of a
-- newly created object table
--
-- == Valid Usage
--
-- -   If the
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsFeaturesNVX'::@computeBindingPointSupport@
--     feature is not enabled, @pObjectEntryUsageFlags@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX'
--
-- -   Any value within @pObjectEntryCounts@ /must/ not exceed
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsLimitsNVX'::@maxObjectEntryCounts@
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
-- Unresolved directive in VkObjectTableCreateInfoNVX.txt -
-- include::{generated}\/validity\/structs\/VkObjectTableCreateInfoNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data ObjectTableCreateInfoNVX = ObjectTableCreateInfoNVX
  { -- Univalued member elided
  -- No documentation found for Nested "ObjectTableCreateInfoNVX" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
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

-- | A function to temporarily allocate memory for a 'VkObjectTableCreateInfoNVX' and
-- marshal a 'ObjectTableCreateInfoNVX' into it. The 'VkObjectTableCreateInfoNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructObjectTableCreateInfoNVX :: ObjectTableCreateInfoNVX -> (VkObjectTableCreateInfoNVX -> IO a) -> IO a
withCStructObjectTableCreateInfoNVX marshalled cont = withVec (&) (objectEntryUsageFlags (marshalled :: ObjectTableCreateInfoNVX)) (\pPObjectEntryUsageFlags -> withVec (&) (objectEntryCounts (marshalled :: ObjectTableCreateInfoNVX)) (\pPObjectEntryCounts -> withVec (&) (objectEntryTypes (marshalled :: ObjectTableCreateInfoNVX)) (\pPObjectEntryTypes -> maybeWith withSomeVkStruct (next (marshalled :: ObjectTableCreateInfoNVX)) (\pPNext -> cont (VkObjectTableCreateInfoNVX VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX pPNext (fromIntegral (minimum ([Data.Vector.length (objectEntryTypes (marshalled :: ObjectTableCreateInfoNVX)), Data.Vector.length (objectEntryCounts (marshalled :: ObjectTableCreateInfoNVX)), Data.Vector.length (objectEntryUsageFlags (marshalled :: ObjectTableCreateInfoNVX))]))) pPObjectEntryTypes pPObjectEntryCounts pPObjectEntryUsageFlags (maxUniformBuffersPerDescriptor (marshalled :: ObjectTableCreateInfoNVX)) (maxStorageBuffersPerDescriptor (marshalled :: ObjectTableCreateInfoNVX)) (maxStorageImagesPerDescriptor (marshalled :: ObjectTableCreateInfoNVX)) (maxSampledImagesPerDescriptor (marshalled :: ObjectTableCreateInfoNVX)) (maxPipelineLayouts (marshalled :: ObjectTableCreateInfoNVX)))))))

-- | A function to read a 'VkObjectTableCreateInfoNVX' and all additional
-- structures in the pointer chain into a 'ObjectTableCreateInfoNVX'.
fromCStructObjectTableCreateInfoNVX :: VkObjectTableCreateInfoNVX -> IO ObjectTableCreateInfoNVX
fromCStructObjectTableCreateInfoNVX c = ObjectTableCreateInfoNVX <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkObjectTableCreateInfoNVX)))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkObjectCount (c :: VkObjectTableCreateInfoNVX))) (peekElemOff (vkPObjectEntryTypes (c :: VkObjectTableCreateInfoNVX))))
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkObjectCount (c :: VkObjectTableCreateInfoNVX))) (peekElemOff (vkPObjectEntryCounts (c :: VkObjectTableCreateInfoNVX))))
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkObjectCount (c :: VkObjectTableCreateInfoNVX))) (peekElemOff (vkPObjectEntryUsageFlags (c :: VkObjectTableCreateInfoNVX))))
                                                                 <*> pure (vkMaxUniformBuffersPerDescriptor (c :: VkObjectTableCreateInfoNVX))
                                                                 <*> pure (vkMaxStorageBuffersPerDescriptor (c :: VkObjectTableCreateInfoNVX))
                                                                 <*> pure (vkMaxStorageImagesPerDescriptor (c :: VkObjectTableCreateInfoNVX))
                                                                 <*> pure (vkMaxSampledImagesPerDescriptor (c :: VkObjectTableCreateInfoNVX))
                                                                 <*> pure (vkMaxPipelineLayouts (c :: VkObjectTableCreateInfoNVX))

instance Zero ObjectTableCreateInfoNVX where
  zero = ObjectTableCreateInfoNVX Nothing
                                  Data.Vector.empty
                                  Data.Vector.empty
                                  Data.Vector.empty
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero



-- | VkObjectTableDescriptorSetEntryNVX - Parameters of an object table
-- descriptor set entry
--
-- == Valid Usage
--
-- Unresolved directive in VkObjectTableDescriptorSetEntryNVX.txt -
-- include::{generated}\/validity\/structs\/VkObjectTableDescriptorSetEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
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

-- | A function to temporarily allocate memory for a 'VkObjectTableDescriptorSetEntryNVX' and
-- marshal a 'ObjectTableDescriptorSetEntryNVX' into it. The 'VkObjectTableDescriptorSetEntryNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructObjectTableDescriptorSetEntryNVX :: ObjectTableDescriptorSetEntryNVX -> (VkObjectTableDescriptorSetEntryNVX -> IO a) -> IO a
withCStructObjectTableDescriptorSetEntryNVX marshalled cont = cont (VkObjectTableDescriptorSetEntryNVX (type' (marshalled :: ObjectTableDescriptorSetEntryNVX)) (flags (marshalled :: ObjectTableDescriptorSetEntryNVX)) (pipelineLayout (marshalled :: ObjectTableDescriptorSetEntryNVX)) (descriptorSet (marshalled :: ObjectTableDescriptorSetEntryNVX)))

-- | A function to read a 'VkObjectTableDescriptorSetEntryNVX' and all additional
-- structures in the pointer chain into a 'ObjectTableDescriptorSetEntryNVX'.
fromCStructObjectTableDescriptorSetEntryNVX :: VkObjectTableDescriptorSetEntryNVX -> IO ObjectTableDescriptorSetEntryNVX
fromCStructObjectTableDescriptorSetEntryNVX c = ObjectTableDescriptorSetEntryNVX <$> pure (vkType (c :: VkObjectTableDescriptorSetEntryNVX))
                                                                                 <*> pure (vkFlags (c :: VkObjectTableDescriptorSetEntryNVX))
                                                                                 <*> pure (vkPipelineLayout (c :: VkObjectTableDescriptorSetEntryNVX))
                                                                                 <*> pure (vkDescriptorSet (c :: VkObjectTableDescriptorSetEntryNVX))

instance Zero ObjectTableDescriptorSetEntryNVX where
  zero = ObjectTableDescriptorSetEntryNVX zero
                                          zero
                                          zero
                                          zero



-- | VkObjectTableEntryNVX - Common parameters of an object table resource
-- entry
--
-- == Valid Usage
--
-- -   If the
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsFeaturesNVX'::@computeBindingPointSupport@
--     feature is not enabled, @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX'
--
-- Unresolved directive in VkObjectTableEntryNVX.txt -
-- include::{generated}\/validity\/structs\/VkObjectTableEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data ObjectTableEntryNVX = ObjectTableEntryNVX
  { -- No documentation found for Nested "ObjectTableEntryNVX" "type"
  type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableEntryNVX" "flags"
  flags :: ObjectEntryUsageFlagsNVX
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkObjectTableEntryNVX' and
-- marshal a 'ObjectTableEntryNVX' into it. The 'VkObjectTableEntryNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructObjectTableEntryNVX :: ObjectTableEntryNVX -> (VkObjectTableEntryNVX -> IO a) -> IO a
withCStructObjectTableEntryNVX marshalled cont = cont (VkObjectTableEntryNVX (type' (marshalled :: ObjectTableEntryNVX)) (flags (marshalled :: ObjectTableEntryNVX)))

-- | A function to read a 'VkObjectTableEntryNVX' and all additional
-- structures in the pointer chain into a 'ObjectTableEntryNVX'.
fromCStructObjectTableEntryNVX :: VkObjectTableEntryNVX -> IO ObjectTableEntryNVX
fromCStructObjectTableEntryNVX c = ObjectTableEntryNVX <$> pure (vkType (c :: VkObjectTableEntryNVX))
                                                       <*> pure (vkFlags (c :: VkObjectTableEntryNVX))

instance Zero ObjectTableEntryNVX where
  zero = ObjectTableEntryNVX zero
                             zero



-- | VkObjectTableIndexBufferEntryNVX - Parameters of an object table index
-- buffer entry
--
-- == Valid Usage
--
-- Unresolved directive in VkObjectTableIndexBufferEntryNVX.txt -
-- include::{generated}\/validity\/structs\/VkObjectTableIndexBufferEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
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

-- | A function to temporarily allocate memory for a 'VkObjectTableIndexBufferEntryNVX' and
-- marshal a 'ObjectTableIndexBufferEntryNVX' into it. The 'VkObjectTableIndexBufferEntryNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructObjectTableIndexBufferEntryNVX :: ObjectTableIndexBufferEntryNVX -> (VkObjectTableIndexBufferEntryNVX -> IO a) -> IO a
withCStructObjectTableIndexBufferEntryNVX marshalled cont = cont (VkObjectTableIndexBufferEntryNVX (type' (marshalled :: ObjectTableIndexBufferEntryNVX)) (flags (marshalled :: ObjectTableIndexBufferEntryNVX)) (buffer (marshalled :: ObjectTableIndexBufferEntryNVX)) (indexType (marshalled :: ObjectTableIndexBufferEntryNVX)))

-- | A function to read a 'VkObjectTableIndexBufferEntryNVX' and all additional
-- structures in the pointer chain into a 'ObjectTableIndexBufferEntryNVX'.
fromCStructObjectTableIndexBufferEntryNVX :: VkObjectTableIndexBufferEntryNVX -> IO ObjectTableIndexBufferEntryNVX
fromCStructObjectTableIndexBufferEntryNVX c = ObjectTableIndexBufferEntryNVX <$> pure (vkType (c :: VkObjectTableIndexBufferEntryNVX))
                                                                             <*> pure (vkFlags (c :: VkObjectTableIndexBufferEntryNVX))
                                                                             <*> pure (vkBuffer (c :: VkObjectTableIndexBufferEntryNVX))
                                                                             <*> pure (vkIndexType (c :: VkObjectTableIndexBufferEntryNVX))

instance Zero ObjectTableIndexBufferEntryNVX where
  zero = ObjectTableIndexBufferEntryNVX zero
                                        zero
                                        zero
                                        zero


-- | VkObjectTableNVX - Opaque handle to an object table
--
-- = See Also
--
-- No cross-references are available
type ObjectTableNVX = VkObjectTableNVX


-- | VkObjectTablePipelineEntryNVX - Parameters of an object table pipeline
-- entry
--
-- == Valid Usage
--
-- Unresolved directive in VkObjectTablePipelineEntryNVX.txt -
-- include::{generated}\/validity\/structs\/VkObjectTablePipelineEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data ObjectTablePipelineEntryNVX = ObjectTablePipelineEntryNVX
  { -- No documentation found for Nested "ObjectTablePipelineEntryNVX" "type"
  type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTablePipelineEntryNVX" "flags"
  flags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTablePipelineEntryNVX" "pipeline"
  pipeline :: Pipeline
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkObjectTablePipelineEntryNVX' and
-- marshal a 'ObjectTablePipelineEntryNVX' into it. The 'VkObjectTablePipelineEntryNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructObjectTablePipelineEntryNVX :: ObjectTablePipelineEntryNVX -> (VkObjectTablePipelineEntryNVX -> IO a) -> IO a
withCStructObjectTablePipelineEntryNVX marshalled cont = cont (VkObjectTablePipelineEntryNVX (type' (marshalled :: ObjectTablePipelineEntryNVX)) (flags (marshalled :: ObjectTablePipelineEntryNVX)) (pipeline (marshalled :: ObjectTablePipelineEntryNVX)))

-- | A function to read a 'VkObjectTablePipelineEntryNVX' and all additional
-- structures in the pointer chain into a 'ObjectTablePipelineEntryNVX'.
fromCStructObjectTablePipelineEntryNVX :: VkObjectTablePipelineEntryNVX -> IO ObjectTablePipelineEntryNVX
fromCStructObjectTablePipelineEntryNVX c = ObjectTablePipelineEntryNVX <$> pure (vkType (c :: VkObjectTablePipelineEntryNVX))
                                                                       <*> pure (vkFlags (c :: VkObjectTablePipelineEntryNVX))
                                                                       <*> pure (vkPipeline (c :: VkObjectTablePipelineEntryNVX))

instance Zero ObjectTablePipelineEntryNVX where
  zero = ObjectTablePipelineEntryNVX zero
                                     zero
                                     zero



-- | VkObjectTablePushConstantEntryNVX - Parameters of an object table push
-- constant entry
--
-- == Valid Usage
--
-- Unresolved directive in VkObjectTablePushConstantEntryNVX.txt -
-- include::{generated}\/validity\/structs\/VkObjectTablePushConstantEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
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

-- | A function to temporarily allocate memory for a 'VkObjectTablePushConstantEntryNVX' and
-- marshal a 'ObjectTablePushConstantEntryNVX' into it. The 'VkObjectTablePushConstantEntryNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructObjectTablePushConstantEntryNVX :: ObjectTablePushConstantEntryNVX -> (VkObjectTablePushConstantEntryNVX -> IO a) -> IO a
withCStructObjectTablePushConstantEntryNVX marshalled cont = cont (VkObjectTablePushConstantEntryNVX (type' (marshalled :: ObjectTablePushConstantEntryNVX)) (flags (marshalled :: ObjectTablePushConstantEntryNVX)) (pipelineLayout (marshalled :: ObjectTablePushConstantEntryNVX)) (stageFlags (marshalled :: ObjectTablePushConstantEntryNVX)))

-- | A function to read a 'VkObjectTablePushConstantEntryNVX' and all additional
-- structures in the pointer chain into a 'ObjectTablePushConstantEntryNVX'.
fromCStructObjectTablePushConstantEntryNVX :: VkObjectTablePushConstantEntryNVX -> IO ObjectTablePushConstantEntryNVX
fromCStructObjectTablePushConstantEntryNVX c = ObjectTablePushConstantEntryNVX <$> pure (vkType (c :: VkObjectTablePushConstantEntryNVX))
                                                                               <*> pure (vkFlags (c :: VkObjectTablePushConstantEntryNVX))
                                                                               <*> pure (vkPipelineLayout (c :: VkObjectTablePushConstantEntryNVX))
                                                                               <*> pure (vkStageFlags (c :: VkObjectTablePushConstantEntryNVX))

instance Zero ObjectTablePushConstantEntryNVX where
  zero = ObjectTablePushConstantEntryNVX zero
                                         zero
                                         zero
                                         zero



-- | VkObjectTableVertexBufferEntryNVX - Parameters of an object table vertex
-- buffer entry
--
-- == Valid Usage
--
-- Unresolved directive in VkObjectTableVertexBufferEntryNVX.txt -
-- include::{generated}\/validity\/structs\/VkObjectTableVertexBufferEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data ObjectTableVertexBufferEntryNVX = ObjectTableVertexBufferEntryNVX
  { -- No documentation found for Nested "ObjectTableVertexBufferEntryNVX" "type"
  type' :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableVertexBufferEntryNVX" "flags"
  flags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTableVertexBufferEntryNVX" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkObjectTableVertexBufferEntryNVX' and
-- marshal a 'ObjectTableVertexBufferEntryNVX' into it. The 'VkObjectTableVertexBufferEntryNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructObjectTableVertexBufferEntryNVX :: ObjectTableVertexBufferEntryNVX -> (VkObjectTableVertexBufferEntryNVX -> IO a) -> IO a
withCStructObjectTableVertexBufferEntryNVX marshalled cont = cont (VkObjectTableVertexBufferEntryNVX (type' (marshalled :: ObjectTableVertexBufferEntryNVX)) (flags (marshalled :: ObjectTableVertexBufferEntryNVX)) (buffer (marshalled :: ObjectTableVertexBufferEntryNVX)))

-- | A function to read a 'VkObjectTableVertexBufferEntryNVX' and all additional
-- structures in the pointer chain into a 'ObjectTableVertexBufferEntryNVX'.
fromCStructObjectTableVertexBufferEntryNVX :: VkObjectTableVertexBufferEntryNVX -> IO ObjectTableVertexBufferEntryNVX
fromCStructObjectTableVertexBufferEntryNVX c = ObjectTableVertexBufferEntryNVX <$> pure (vkType (c :: VkObjectTableVertexBufferEntryNVX))
                                                                               <*> pure (vkFlags (c :: VkObjectTableVertexBufferEntryNVX))
                                                                               <*> pure (vkBuffer (c :: VkObjectTableVertexBufferEntryNVX))

instance Zero ObjectTableVertexBufferEntryNVX where
  zero = ObjectTableVertexBufferEntryNVX zero
                                         zero
                                         zero



-- | vkCmdProcessCommandsNVX - Performs the generation of commands on the
-- device
--
-- = Parameters
--
-- -   @commandBuffer@ is the primary command buffer in which the
--     generation process takes space.
--
-- -   @pProcessCommandsInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX'
--     structure containing parameters affecting the processing of
--     commands.
--
-- = Description
--
-- Unresolved directive in vkCmdProcessCommandsNVX.txt -
-- include::{generated}\/validity\/protos\/vkCmdProcessCommandsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdProcessCommandsNVX :: CommandBuffer ->  CmdProcessCommandsInfoNVX ->  IO ()
cmdProcessCommandsNVX = \(CommandBuffer commandBuffer' commandTable) -> \processCommandsInfo' -> (\marshalled -> withCStructCmdProcessCommandsInfoNVX marshalled . flip with) processCommandsInfo' (\pProcessCommandsInfo' -> vkCmdProcessCommandsNVX commandTable commandBuffer' pProcessCommandsInfo' *> (pure ()))


-- | vkCmdReserveSpaceForCommandsNVX - Perform a reservation of command
-- buffer space
--
-- = Parameters
--
-- -   @commandBuffer@ is the secondary command buffer in which the space
--     for device-generated commands is reserved.
--
-- -   @pProcessCommandsInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdReserveSpaceForCommandsInfoNVX'
--     structure containing parameters affecting the reservation of command
--     buffer space.
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
-- Unresolved directive in vkCmdReserveSpaceForCommandsNVX.txt -
-- include::{generated}\/validity\/protos\/vkCmdReserveSpaceForCommandsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdReserveSpaceForCommandsNVX :: CommandBuffer ->  CmdReserveSpaceForCommandsInfoNVX ->  IO ()
cmdReserveSpaceForCommandsNVX = \(CommandBuffer commandBuffer' commandTable) -> \reserveSpaceInfo' -> (\marshalled -> withCStructCmdReserveSpaceForCommandsInfoNVX marshalled . flip with) reserveSpaceInfo' (\pReserveSpaceInfo' -> vkCmdReserveSpaceForCommandsNVX commandTable commandBuffer' pReserveSpaceInfo' *> (pure ()))


-- | vkCreateIndirectCommandsLayoutNVX - Create an indirect command layout
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the indirect command
--     layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutCreateInfoNVX'
--     structure containing parameters affecting creation of the indirect
--     command layout.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pIndirectCommandsLayout@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutNVX'
--     handle in which the resulting indirect command layout is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateIndirectCommandsLayoutNVX.txt -
-- include::{generated}\/validity\/protos\/vkCreateIndirectCommandsLayoutNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
createIndirectCommandsLayoutNVX :: Device ->  IndirectCommandsLayoutCreateInfoNVX ->  Maybe AllocationCallbacks ->  IO (IndirectCommandsLayoutNVX)
createIndirectCommandsLayoutNVX = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pIndirectCommandsLayout' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructIndirectCommandsLayoutCreateInfoNVX marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateIndirectCommandsLayoutNVX commandTable device' pCreateInfo' pAllocator pIndirectCommandsLayout' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pIndirectCommandsLayout')))))


-- | vkCreateObjectTableNVX - Create an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableCreateInfoNVX'
--     structure containing parameters affecting creation of the table.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pObjectTable@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableNVX'
--     handle in which the resulting object table is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateObjectTableNVX.txt -
-- include::{generated}\/validity\/protos\/vkCreateObjectTableNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
createObjectTableNVX :: Device ->  ObjectTableCreateInfoNVX ->  Maybe AllocationCallbacks ->  IO (ObjectTableNVX)
createObjectTableNVX = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pObjectTable' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructObjectTableCreateInfoNVX marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateObjectTableNVX commandTable device' pCreateInfo' pAllocator pObjectTable' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pObjectTable')))))


-- | vkDestroyIndirectCommandsLayoutNVX - Destroy an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the layout.
--
-- -   @indirectCommandsLayout@ is the table to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @indirectCommandsLayout@ /must/
--     have completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @objectTable@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @objectTable@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyIndirectCommandsLayoutNVX.txt -
-- include::{generated}\/validity\/protos\/vkDestroyIndirectCommandsLayoutNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
destroyIndirectCommandsLayoutNVX :: Device ->  IndirectCommandsLayoutNVX ->  Maybe AllocationCallbacks ->  IO ()
destroyIndirectCommandsLayoutNVX = \(Device device' commandTable) -> \indirectCommandsLayout' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyIndirectCommandsLayoutNVX commandTable device' indirectCommandsLayout' pAllocator *> (pure ()))


-- | vkDestroyObjectTableNVX - Destroy an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the table.
--
-- -   @objectTable@ is the table to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @objectTable@ /must/ have
--     completed execution.
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @objectTable@ was created, a compatible set of
--     callbacks /must/ be provided here.
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @objectTable@ was created, @pAllocator@ /must/ be
--     @NULL@.
--
-- Unresolved directive in vkDestroyObjectTableNVX.txt -
-- include::{generated}\/validity\/protos\/vkDestroyObjectTableNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
destroyObjectTableNVX :: Device ->  ObjectTableNVX ->  Maybe AllocationCallbacks ->  IO ()
destroyObjectTableNVX = \(Device device' commandTable) -> \objectTable' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyObjectTableNVX commandTable device' objectTable' pAllocator *> (pure ()))


-- | vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX - Returns
-- device-generated commands related properties of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pFeatures@ points to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsFeaturesNVX'
--     structure, that will be filled with returned information.
--
-- -   @pLimits@ points to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsLimitsNVX'
--     structure, that will be filled with returned information.
--
-- = Description
--
-- Unresolved directive in
-- vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceGeneratedCommandsPropertiesNVX :: PhysicalDevice ->  IO (DeviceGeneratedCommandsFeaturesNVX, DeviceGeneratedCommandsLimitsNVX)
getPhysicalDeviceGeneratedCommandsPropertiesNVX = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pLimits' -> alloca (\pFeatures' -> vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX commandTable physicalDevice' pFeatures' pLimits' *> ((,) <$> (fromCStructDeviceGeneratedCommandsFeaturesNVX <=< peek) pFeatures'<*>(fromCStructDeviceGeneratedCommandsLimitsNVX <=< peek) pLimits')))


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
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePipelineEntryNVX',
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableDescriptorSetEntryNVX',
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableVertexBufferEntryNVX',
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableIndexBufferEntryNVX'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePushConstantEntryNVX'
--     (see below for details).
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
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableCreateInfoNVX'::@pObjectEntryCounts@
--     limits provided at @objectTable@ creation time.
--
-- Unresolved directive in vkRegisterObjectsNVX.txt -
-- include::{generated}\/validity\/protos\/vkRegisterObjectsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
registerObjectsNVX :: Device ->  ObjectTableNVX ->  Vector ObjectTableEntryNVX ->  Vector Word32 ->  IO ()
registerObjectsNVX = \(Device device' commandTable) -> \objectTable' -> \objectTableEntries' -> \objectIndices' -> withVec (&) objectIndices' (\pObjectIndices' -> withVec (\marshalled -> withCStructObjectTableEntryNVX marshalled . flip with) objectTableEntries' (\pObjectTableEntries' -> vkRegisterObjectsNVX commandTable device' objectTable' (fromIntegral $ Data.Vector.length objectTableEntries' `min` Data.Vector.length objectIndices') pObjectTableEntries' pObjectIndices' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))))


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
-- -   @pObjectEntryType@ provides an array of
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectEntryTypeNVX'
--     for the resources being removed.
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
-- Unresolved directive in vkUnregisterObjectsNVX.txt -
-- include::{generated}\/validity\/protos\/vkUnregisterObjectsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
unregisterObjectsNVX :: Device ->  ObjectTableNVX ->  Vector ObjectEntryTypeNVX ->  Vector Word32 ->  IO ()
unregisterObjectsNVX = \(Device device' commandTable) -> \objectTable' -> \objectEntryTypes' -> \objectIndices' -> withVec (&) objectIndices' (\pObjectIndices' -> withVec (&) objectEntryTypes' (\pObjectEntryTypes' -> vkUnregisterObjectsNVX commandTable device' objectTable' (fromIntegral $ Data.Vector.length objectEntryTypes' `min` Data.Vector.length objectIndices') pObjectEntryTypes' pObjectIndices' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))))

-- | A safe wrapper for 'createIndirectCommandsLayoutNVX' and 'destroyIndirectCommandsLayoutNVX' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withIndirectCommandsLayoutNVX
  :: Device -> IndirectCommandsLayoutCreateInfoNVX -> Maybe (AllocationCallbacks) -> (IndirectCommandsLayoutNVX -> IO a) -> IO a
withIndirectCommandsLayoutNVX device indirectCommandsLayoutCreateInfoNVX allocationCallbacks = bracket
  (createIndirectCommandsLayoutNVX device indirectCommandsLayoutCreateInfoNVX allocationCallbacks)
  (\o -> destroyIndirectCommandsLayoutNVX device o allocationCallbacks)

-- | A safe wrapper for 'createObjectTableNVX' and 'destroyObjectTableNVX' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withObjectTableNVX
  :: Device -> ObjectTableCreateInfoNVX -> Maybe (AllocationCallbacks) -> (ObjectTableNVX -> IO a) -> IO a
withObjectTableNVX device objectTableCreateInfoNVX allocationCallbacks = bracket
  (createObjectTableNVX device objectTableCreateInfoNVX allocationCallbacks)
  (\o -> destroyObjectTableNVX device o allocationCallbacks)

-- | A safe wrapper for 'registerObjectsNVX' and 'unregisterObjectsNVX' using 'bracket_'
--
-- The allocated value must not be returned from the provided computation
withRegisteredObjectsNVX
  :: Device -> ObjectTableNVX -> Vector (ObjectTableEntryNVX) -> Vector (Word32) -> Vector (ObjectEntryTypeNVX) -> IO a -> IO a
withRegisteredObjectsNVX device objectTableNVX objectTableEntryNVX objectIndices objectEntryTypeNVX = bracket_
  (registerObjectsNVX device objectTableNVX objectTableEntryNVX objectIndices)
  ( unregisterObjectsNVX device objectTableNVX objectEntryTypeNVX objectIndices)
