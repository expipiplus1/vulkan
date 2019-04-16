{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

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
  , IndirectCommandsLayoutUsageFlagsNVX
  , withCStructIndirectCommandsTokenNVX
  , fromCStructIndirectCommandsTokenNVX
  , IndirectCommandsTokenNVX(..)
  , IndirectCommandsTokenTypeNVX
  , ObjectEntryTypeNVX
  , ObjectEntryUsageFlagBitsNVX
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
  ( throwIO
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
  ( generateM
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
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdProcessCommandsNVX
  , cmdReserveSpaceForCommandsNVX
  , createIndirectCommandsLayoutNVX
  , createObjectTableNVX
  , destroyIndirectCommandsLayoutNVX
  , destroyObjectTableNVX
  , getPhysicalDeviceGeneratedCommandsPropertiesNVX
  , registerObjectsNVX
  , unregisterObjectsNVX
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
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


-- No documentation found for TopLevel "CmdProcessCommandsInfoNVX"
data CmdProcessCommandsInfoNVX = CmdProcessCommandsInfoNVX
  { -- Univalued Member elided
  -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "objectTable"
  vkObjectTable :: ObjectTableNVX
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "indirectCommandsLayout"
  vkIndirectCommandsLayout :: IndirectCommandsLayoutNVX
  -- Length valued member elided
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "pIndirectCommandsTokens"
  vkPIndirectCommandsTokens :: Vector IndirectCommandsTokenNVX
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "maxSequencesCount"
  vkMaxSequencesCount :: Word32
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "targetCommandBuffer"
  vkTargetCommandBuffer :: CommandBuffer
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "sequencesCountBuffer"
  vkSequencesCountBuffer :: Buffer
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "sequencesCountOffset"
  vkSequencesCountOffset :: DeviceSize
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "sequencesIndexBuffer"
  vkSequencesIndexBuffer :: Buffer
  , -- No documentation found for Nested "CmdProcessCommandsInfoNVX" "sequencesIndexOffset"
  vkSequencesIndexOffset :: DeviceSize
  }
  deriving (Show, Eq)
withCStructCmdProcessCommandsInfoNVX :: CmdProcessCommandsInfoNVX -> (VkCmdProcessCommandsInfoNVX -> IO a) -> IO a
withCStructCmdProcessCommandsInfoNVX from cont = withVec withCStructIndirectCommandsTokenNVX (vkPIndirectCommandsTokens (from :: CmdProcessCommandsInfoNVX)) (\pIndirectCommandsTokens -> maybeWith withSomeVkStruct (vkPNext (from :: CmdProcessCommandsInfoNVX)) (\pPNext -> cont (VkCmdProcessCommandsInfoNVX VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX pPNext (vkObjectTable (from :: CmdProcessCommandsInfoNVX)) (vkIndirectCommandsLayout (from :: CmdProcessCommandsInfoNVX)) (fromIntegral (Data.Vector.length (vkPIndirectCommandsTokens (from :: CmdProcessCommandsInfoNVX)))) pIndirectCommandsTokens (vkMaxSequencesCount (from :: CmdProcessCommandsInfoNVX)) (commandBufferHandle (vkTargetCommandBuffer (from :: CmdProcessCommandsInfoNVX))) (vkSequencesCountBuffer (from :: CmdProcessCommandsInfoNVX)) (vkSequencesCountOffset (from :: CmdProcessCommandsInfoNVX)) (vkSequencesIndexBuffer (from :: CmdProcessCommandsInfoNVX)) (vkSequencesIndexOffset (from :: CmdProcessCommandsInfoNVX)))))
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
-- No documentation found for TopLevel "CmdReserveSpaceForCommandsInfoNVX"
data CmdReserveSpaceForCommandsInfoNVX = CmdReserveSpaceForCommandsInfoNVX
  { -- Univalued Member elided
  -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "objectTable"
  vkObjectTable :: ObjectTableNVX
  , -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "indirectCommandsLayout"
  vkIndirectCommandsLayout :: IndirectCommandsLayoutNVX
  , -- No documentation found for Nested "CmdReserveSpaceForCommandsInfoNVX" "maxSequencesCount"
  vkMaxSequencesCount :: Word32
  }
  deriving (Show, Eq)
withCStructCmdReserveSpaceForCommandsInfoNVX :: CmdReserveSpaceForCommandsInfoNVX -> (VkCmdReserveSpaceForCommandsInfoNVX -> IO a) -> IO a
withCStructCmdReserveSpaceForCommandsInfoNVX from cont = maybeWith withSomeVkStruct (vkPNext (from :: CmdReserveSpaceForCommandsInfoNVX)) (\pPNext -> cont (VkCmdReserveSpaceForCommandsInfoNVX VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX pPNext (vkObjectTable (from :: CmdReserveSpaceForCommandsInfoNVX)) (vkIndirectCommandsLayout (from :: CmdReserveSpaceForCommandsInfoNVX)) (vkMaxSequencesCount (from :: CmdReserveSpaceForCommandsInfoNVX))))
fromCStructCmdReserveSpaceForCommandsInfoNVX :: VkCmdReserveSpaceForCommandsInfoNVX -> IO CmdReserveSpaceForCommandsInfoNVX
fromCStructCmdReserveSpaceForCommandsInfoNVX c = CmdReserveSpaceForCommandsInfoNVX <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCmdReserveSpaceForCommandsInfoNVX)))
                                                                                   <*> pure (vkObjectTable (c :: VkCmdReserveSpaceForCommandsInfoNVX))
                                                                                   <*> pure (vkIndirectCommandsLayout (c :: VkCmdReserveSpaceForCommandsInfoNVX))
                                                                                   <*> pure (vkMaxSequencesCount (c :: VkCmdReserveSpaceForCommandsInfoNVX))
-- No documentation found for TopLevel "DeviceGeneratedCommandsFeaturesNVX"
data DeviceGeneratedCommandsFeaturesNVX = DeviceGeneratedCommandsFeaturesNVX
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGeneratedCommandsFeaturesNVX" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGeneratedCommandsFeaturesNVX" "computeBindingPointSupport"
  vkComputeBindingPointSupport :: Bool
  }
  deriving (Show, Eq)
withCStructDeviceGeneratedCommandsFeaturesNVX :: DeviceGeneratedCommandsFeaturesNVX -> (VkDeviceGeneratedCommandsFeaturesNVX -> IO a) -> IO a
withCStructDeviceGeneratedCommandsFeaturesNVX from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceGeneratedCommandsFeaturesNVX)) (\pPNext -> cont (VkDeviceGeneratedCommandsFeaturesNVX VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX pPNext (boolToBool32 (vkComputeBindingPointSupport (from :: DeviceGeneratedCommandsFeaturesNVX)))))
fromCStructDeviceGeneratedCommandsFeaturesNVX :: VkDeviceGeneratedCommandsFeaturesNVX -> IO DeviceGeneratedCommandsFeaturesNVX
fromCStructDeviceGeneratedCommandsFeaturesNVX c = DeviceGeneratedCommandsFeaturesNVX <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGeneratedCommandsFeaturesNVX)))
                                                                                     <*> pure (bool32ToBool (vkComputeBindingPointSupport (c :: VkDeviceGeneratedCommandsFeaturesNVX)))
-- No documentation found for TopLevel "DeviceGeneratedCommandsLimitsNVX"
data DeviceGeneratedCommandsLimitsNVX = DeviceGeneratedCommandsLimitsNVX
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "maxIndirectCommandsLayoutTokenCount"
  vkMaxIndirectCommandsLayoutTokenCount :: Word32
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "maxObjectEntryCounts"
  vkMaxObjectEntryCounts :: Word32
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "minSequenceCountBufferOffsetAlignment"
  vkMinSequenceCountBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "minSequenceIndexBufferOffsetAlignment"
  vkMinSequenceIndexBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "DeviceGeneratedCommandsLimitsNVX" "minCommandsTokenBufferOffsetAlignment"
  vkMinCommandsTokenBufferOffsetAlignment :: Word32
  }
  deriving (Show, Eq)
withCStructDeviceGeneratedCommandsLimitsNVX :: DeviceGeneratedCommandsLimitsNVX -> (VkDeviceGeneratedCommandsLimitsNVX -> IO a) -> IO a
withCStructDeviceGeneratedCommandsLimitsNVX from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceGeneratedCommandsLimitsNVX)) (\pPNext -> cont (VkDeviceGeneratedCommandsLimitsNVX VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX pPNext (vkMaxIndirectCommandsLayoutTokenCount (from :: DeviceGeneratedCommandsLimitsNVX)) (vkMaxObjectEntryCounts (from :: DeviceGeneratedCommandsLimitsNVX)) (vkMinSequenceCountBufferOffsetAlignment (from :: DeviceGeneratedCommandsLimitsNVX)) (vkMinSequenceIndexBufferOffsetAlignment (from :: DeviceGeneratedCommandsLimitsNVX)) (vkMinCommandsTokenBufferOffsetAlignment (from :: DeviceGeneratedCommandsLimitsNVX))))
fromCStructDeviceGeneratedCommandsLimitsNVX :: VkDeviceGeneratedCommandsLimitsNVX -> IO DeviceGeneratedCommandsLimitsNVX
fromCStructDeviceGeneratedCommandsLimitsNVX c = DeviceGeneratedCommandsLimitsNVX <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGeneratedCommandsLimitsNVX)))
                                                                                 <*> pure (vkMaxIndirectCommandsLayoutTokenCount (c :: VkDeviceGeneratedCommandsLimitsNVX))
                                                                                 <*> pure (vkMaxObjectEntryCounts (c :: VkDeviceGeneratedCommandsLimitsNVX))
                                                                                 <*> pure (vkMinSequenceCountBufferOffsetAlignment (c :: VkDeviceGeneratedCommandsLimitsNVX))
                                                                                 <*> pure (vkMinSequenceIndexBufferOffsetAlignment (c :: VkDeviceGeneratedCommandsLimitsNVX))
                                                                                 <*> pure (vkMinCommandsTokenBufferOffsetAlignment (c :: VkDeviceGeneratedCommandsLimitsNVX))
-- No documentation found for TopLevel "IndirectCommandsLayoutCreateInfoNVX"
data IndirectCommandsLayoutCreateInfoNVX = IndirectCommandsLayoutCreateInfoNVX
  { -- Univalued Member elided
  -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "pipelineBindPoint"
  vkPipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "flags"
  vkFlags :: IndirectCommandsLayoutUsageFlagsNVX
  -- Length valued member elided
  , -- No documentation found for Nested "IndirectCommandsLayoutCreateInfoNVX" "pTokens"
  vkPTokens :: Vector IndirectCommandsLayoutTokenNVX
  }
  deriving (Show, Eq)
withCStructIndirectCommandsLayoutCreateInfoNVX :: IndirectCommandsLayoutCreateInfoNVX -> (VkIndirectCommandsLayoutCreateInfoNVX -> IO a) -> IO a
withCStructIndirectCommandsLayoutCreateInfoNVX from cont = withVec withCStructIndirectCommandsLayoutTokenNVX (vkPTokens (from :: IndirectCommandsLayoutCreateInfoNVX)) (\pTokens -> maybeWith withSomeVkStruct (vkPNext (from :: IndirectCommandsLayoutCreateInfoNVX)) (\pPNext -> cont (VkIndirectCommandsLayoutCreateInfoNVX VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX pPNext (vkPipelineBindPoint (from :: IndirectCommandsLayoutCreateInfoNVX)) (vkFlags (from :: IndirectCommandsLayoutCreateInfoNVX)) (fromIntegral (Data.Vector.length (vkPTokens (from :: IndirectCommandsLayoutCreateInfoNVX)))) pTokens)))
fromCStructIndirectCommandsLayoutCreateInfoNVX :: VkIndirectCommandsLayoutCreateInfoNVX -> IO IndirectCommandsLayoutCreateInfoNVX
fromCStructIndirectCommandsLayoutCreateInfoNVX c = IndirectCommandsLayoutCreateInfoNVX <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkIndirectCommandsLayoutCreateInfoNVX)))
                                                                                       <*> pure (vkPipelineBindPoint (c :: VkIndirectCommandsLayoutCreateInfoNVX))
                                                                                       <*> pure (vkFlags (c :: VkIndirectCommandsLayoutCreateInfoNVX))
                                                                                       -- Length valued member elided
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkTokenCount (c :: VkIndirectCommandsLayoutCreateInfoNVX))) (((fromCStructIndirectCommandsLayoutTokenNVX <=<) . peekElemOff) (vkPTokens (c :: VkIndirectCommandsLayoutCreateInfoNVX))))
-- No documentation found for TopLevel "IndirectCommandsLayoutNVX"
type IndirectCommandsLayoutNVX = VkIndirectCommandsLayoutNVX
-- No documentation found for TopLevel "IndirectCommandsLayoutTokenNVX"
data IndirectCommandsLayoutTokenNVX = IndirectCommandsLayoutTokenNVX
  { -- No documentation found for Nested "IndirectCommandsLayoutTokenNVX" "tokenType"
  vkTokenType :: IndirectCommandsTokenTypeNVX
  , -- No documentation found for Nested "IndirectCommandsLayoutTokenNVX" "bindingUnit"
  vkBindingUnit :: Word32
  , -- No documentation found for Nested "IndirectCommandsLayoutTokenNVX" "dynamicCount"
  vkDynamicCount :: Word32
  , -- No documentation found for Nested "IndirectCommandsLayoutTokenNVX" "divisor"
  vkDivisor :: Word32
  }
  deriving (Show, Eq)
withCStructIndirectCommandsLayoutTokenNVX :: IndirectCommandsLayoutTokenNVX -> (VkIndirectCommandsLayoutTokenNVX -> IO a) -> IO a
withCStructIndirectCommandsLayoutTokenNVX from cont = cont (VkIndirectCommandsLayoutTokenNVX (vkTokenType (from :: IndirectCommandsLayoutTokenNVX)) (vkBindingUnit (from :: IndirectCommandsLayoutTokenNVX)) (vkDynamicCount (from :: IndirectCommandsLayoutTokenNVX)) (vkDivisor (from :: IndirectCommandsLayoutTokenNVX)))
fromCStructIndirectCommandsLayoutTokenNVX :: VkIndirectCommandsLayoutTokenNVX -> IO IndirectCommandsLayoutTokenNVX
fromCStructIndirectCommandsLayoutTokenNVX c = IndirectCommandsLayoutTokenNVX <$> pure (vkTokenType (c :: VkIndirectCommandsLayoutTokenNVX))
                                                                             <*> pure (vkBindingUnit (c :: VkIndirectCommandsLayoutTokenNVX))
                                                                             <*> pure (vkDynamicCount (c :: VkIndirectCommandsLayoutTokenNVX))
                                                                             <*> pure (vkDivisor (c :: VkIndirectCommandsLayoutTokenNVX))
-- No documentation found for TopLevel "IndirectCommandsLayoutUsageFlagBitsNVX"
type IndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX
-- No documentation found for TopLevel "IndirectCommandsLayoutUsageFlagsNVX"
type IndirectCommandsLayoutUsageFlagsNVX = IndirectCommandsLayoutUsageFlagBitsNVX
-- No documentation found for TopLevel "IndirectCommandsTokenNVX"
data IndirectCommandsTokenNVX = IndirectCommandsTokenNVX
  { -- No documentation found for Nested "IndirectCommandsTokenNVX" "tokenType"
  vkTokenType :: IndirectCommandsTokenTypeNVX
  , -- No documentation found for Nested "IndirectCommandsTokenNVX" "buffer"
  vkBuffer :: Buffer
  , -- No documentation found for Nested "IndirectCommandsTokenNVX" "offset"
  vkOffset :: DeviceSize
  }
  deriving (Show, Eq)
withCStructIndirectCommandsTokenNVX :: IndirectCommandsTokenNVX -> (VkIndirectCommandsTokenNVX -> IO a) -> IO a
withCStructIndirectCommandsTokenNVX from cont = cont (VkIndirectCommandsTokenNVX (vkTokenType (from :: IndirectCommandsTokenNVX)) (vkBuffer (from :: IndirectCommandsTokenNVX)) (vkOffset (from :: IndirectCommandsTokenNVX)))
fromCStructIndirectCommandsTokenNVX :: VkIndirectCommandsTokenNVX -> IO IndirectCommandsTokenNVX
fromCStructIndirectCommandsTokenNVX c = IndirectCommandsTokenNVX <$> pure (vkTokenType (c :: VkIndirectCommandsTokenNVX))
                                                                 <*> pure (vkBuffer (c :: VkIndirectCommandsTokenNVX))
                                                                 <*> pure (vkOffset (c :: VkIndirectCommandsTokenNVX))
-- No documentation found for TopLevel "IndirectCommandsTokenTypeNVX"
type IndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX
-- No documentation found for TopLevel "ObjectEntryTypeNVX"
type ObjectEntryTypeNVX = VkObjectEntryTypeNVX
-- No documentation found for TopLevel "ObjectEntryUsageFlagBitsNVX"
type ObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX
-- No documentation found for TopLevel "ObjectEntryUsageFlagsNVX"
type ObjectEntryUsageFlagsNVX = ObjectEntryUsageFlagBitsNVX
-- No documentation found for TopLevel "ObjectTableCreateInfoNVX"
data ObjectTableCreateInfoNVX = ObjectTableCreateInfoNVX
  { -- Univalued Member elided
  -- No documentation found for Nested "ObjectTableCreateInfoNVX" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "pObjectEntryTypes"
  vkPObjectEntryTypes :: Vector ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "pObjectEntryCounts"
  vkPObjectEntryCounts :: Vector Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "pObjectEntryUsageFlags"
  vkPObjectEntryUsageFlags :: Vector ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxUniformBuffersPerDescriptor"
  vkMaxUniformBuffersPerDescriptor :: Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxStorageBuffersPerDescriptor"
  vkMaxStorageBuffersPerDescriptor :: Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxStorageImagesPerDescriptor"
  vkMaxStorageImagesPerDescriptor :: Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxSampledImagesPerDescriptor"
  vkMaxSampledImagesPerDescriptor :: Word32
  , -- No documentation found for Nested "ObjectTableCreateInfoNVX" "maxPipelineLayouts"
  vkMaxPipelineLayouts :: Word32
  }
  deriving (Show, Eq)
withCStructObjectTableCreateInfoNVX :: ObjectTableCreateInfoNVX -> (VkObjectTableCreateInfoNVX -> IO a) -> IO a
withCStructObjectTableCreateInfoNVX from cont = withVec (&) (vkPObjectEntryUsageFlags (from :: ObjectTableCreateInfoNVX)) (\pObjectEntryUsageFlags -> withVec (&) (vkPObjectEntryCounts (from :: ObjectTableCreateInfoNVX)) (\pObjectEntryCounts -> withVec (&) (vkPObjectEntryTypes (from :: ObjectTableCreateInfoNVX)) (\pObjectEntryTypes -> maybeWith withSomeVkStruct (vkPNext (from :: ObjectTableCreateInfoNVX)) (\pPNext -> cont (VkObjectTableCreateInfoNVX VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX pPNext (fromIntegral (minimum ([ Data.Vector.length (vkPObjectEntryTypes (from :: ObjectTableCreateInfoNVX))
, Data.Vector.length (vkPObjectEntryCounts (from :: ObjectTableCreateInfoNVX))
, Data.Vector.length (vkPObjectEntryUsageFlags (from :: ObjectTableCreateInfoNVX)) ]))) pObjectEntryTypes pObjectEntryCounts pObjectEntryUsageFlags (vkMaxUniformBuffersPerDescriptor (from :: ObjectTableCreateInfoNVX)) (vkMaxStorageBuffersPerDescriptor (from :: ObjectTableCreateInfoNVX)) (vkMaxStorageImagesPerDescriptor (from :: ObjectTableCreateInfoNVX)) (vkMaxSampledImagesPerDescriptor (from :: ObjectTableCreateInfoNVX)) (vkMaxPipelineLayouts (from :: ObjectTableCreateInfoNVX)))))))
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
-- No documentation found for TopLevel "ObjectTableDescriptorSetEntryNVX"
data ObjectTableDescriptorSetEntryNVX = ObjectTableDescriptorSetEntryNVX
  { -- No documentation found for Nested "ObjectTableDescriptorSetEntryNVX" "type"
  vkType :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableDescriptorSetEntryNVX" "flags"
  vkFlags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTableDescriptorSetEntryNVX" "pipelineLayout"
  vkPipelineLayout :: PipelineLayout
  , -- No documentation found for Nested "ObjectTableDescriptorSetEntryNVX" "descriptorSet"
  vkDescriptorSet :: DescriptorSet
  }
  deriving (Show, Eq)
withCStructObjectTableDescriptorSetEntryNVX :: ObjectTableDescriptorSetEntryNVX -> (VkObjectTableDescriptorSetEntryNVX -> IO a) -> IO a
withCStructObjectTableDescriptorSetEntryNVX from cont = cont (VkObjectTableDescriptorSetEntryNVX (vkType (from :: ObjectTableDescriptorSetEntryNVX)) (vkFlags (from :: ObjectTableDescriptorSetEntryNVX)) (vkPipelineLayout (from :: ObjectTableDescriptorSetEntryNVX)) (vkDescriptorSet (from :: ObjectTableDescriptorSetEntryNVX)))
fromCStructObjectTableDescriptorSetEntryNVX :: VkObjectTableDescriptorSetEntryNVX -> IO ObjectTableDescriptorSetEntryNVX
fromCStructObjectTableDescriptorSetEntryNVX c = ObjectTableDescriptorSetEntryNVX <$> pure (vkType (c :: VkObjectTableDescriptorSetEntryNVX))
                                                                                 <*> pure (vkFlags (c :: VkObjectTableDescriptorSetEntryNVX))
                                                                                 <*> pure (vkPipelineLayout (c :: VkObjectTableDescriptorSetEntryNVX))
                                                                                 <*> pure (vkDescriptorSet (c :: VkObjectTableDescriptorSetEntryNVX))
-- No documentation found for TopLevel "ObjectTableEntryNVX"
data ObjectTableEntryNVX = ObjectTableEntryNVX
  { -- No documentation found for Nested "ObjectTableEntryNVX" "type"
  vkType :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableEntryNVX" "flags"
  vkFlags :: ObjectEntryUsageFlagsNVX
  }
  deriving (Show, Eq)
withCStructObjectTableEntryNVX :: ObjectTableEntryNVX -> (VkObjectTableEntryNVX -> IO a) -> IO a
withCStructObjectTableEntryNVX from cont = cont (VkObjectTableEntryNVX (vkType (from :: ObjectTableEntryNVX)) (vkFlags (from :: ObjectTableEntryNVX)))
fromCStructObjectTableEntryNVX :: VkObjectTableEntryNVX -> IO ObjectTableEntryNVX
fromCStructObjectTableEntryNVX c = ObjectTableEntryNVX <$> pure (vkType (c :: VkObjectTableEntryNVX))
                                                       <*> pure (vkFlags (c :: VkObjectTableEntryNVX))
-- No documentation found for TopLevel "ObjectTableIndexBufferEntryNVX"
data ObjectTableIndexBufferEntryNVX = ObjectTableIndexBufferEntryNVX
  { -- No documentation found for Nested "ObjectTableIndexBufferEntryNVX" "type"
  vkType :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableIndexBufferEntryNVX" "flags"
  vkFlags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTableIndexBufferEntryNVX" "buffer"
  vkBuffer :: Buffer
  , -- No documentation found for Nested "ObjectTableIndexBufferEntryNVX" "indexType"
  vkIndexType :: IndexType
  }
  deriving (Show, Eq)
withCStructObjectTableIndexBufferEntryNVX :: ObjectTableIndexBufferEntryNVX -> (VkObjectTableIndexBufferEntryNVX -> IO a) -> IO a
withCStructObjectTableIndexBufferEntryNVX from cont = cont (VkObjectTableIndexBufferEntryNVX (vkType (from :: ObjectTableIndexBufferEntryNVX)) (vkFlags (from :: ObjectTableIndexBufferEntryNVX)) (vkBuffer (from :: ObjectTableIndexBufferEntryNVX)) (vkIndexType (from :: ObjectTableIndexBufferEntryNVX)))
fromCStructObjectTableIndexBufferEntryNVX :: VkObjectTableIndexBufferEntryNVX -> IO ObjectTableIndexBufferEntryNVX
fromCStructObjectTableIndexBufferEntryNVX c = ObjectTableIndexBufferEntryNVX <$> pure (vkType (c :: VkObjectTableIndexBufferEntryNVX))
                                                                             <*> pure (vkFlags (c :: VkObjectTableIndexBufferEntryNVX))
                                                                             <*> pure (vkBuffer (c :: VkObjectTableIndexBufferEntryNVX))
                                                                             <*> pure (vkIndexType (c :: VkObjectTableIndexBufferEntryNVX))
-- No documentation found for TopLevel "ObjectTableNVX"
type ObjectTableNVX = VkObjectTableNVX
-- No documentation found for TopLevel "ObjectTablePipelineEntryNVX"
data ObjectTablePipelineEntryNVX = ObjectTablePipelineEntryNVX
  { -- No documentation found for Nested "ObjectTablePipelineEntryNVX" "type"
  vkType :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTablePipelineEntryNVX" "flags"
  vkFlags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTablePipelineEntryNVX" "pipeline"
  vkPipeline :: Pipeline
  }
  deriving (Show, Eq)
withCStructObjectTablePipelineEntryNVX :: ObjectTablePipelineEntryNVX -> (VkObjectTablePipelineEntryNVX -> IO a) -> IO a
withCStructObjectTablePipelineEntryNVX from cont = cont (VkObjectTablePipelineEntryNVX (vkType (from :: ObjectTablePipelineEntryNVX)) (vkFlags (from :: ObjectTablePipelineEntryNVX)) (vkPipeline (from :: ObjectTablePipelineEntryNVX)))
fromCStructObjectTablePipelineEntryNVX :: VkObjectTablePipelineEntryNVX -> IO ObjectTablePipelineEntryNVX
fromCStructObjectTablePipelineEntryNVX c = ObjectTablePipelineEntryNVX <$> pure (vkType (c :: VkObjectTablePipelineEntryNVX))
                                                                       <*> pure (vkFlags (c :: VkObjectTablePipelineEntryNVX))
                                                                       <*> pure (vkPipeline (c :: VkObjectTablePipelineEntryNVX))
-- No documentation found for TopLevel "ObjectTablePushConstantEntryNVX"
data ObjectTablePushConstantEntryNVX = ObjectTablePushConstantEntryNVX
  { -- No documentation found for Nested "ObjectTablePushConstantEntryNVX" "type"
  vkType :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTablePushConstantEntryNVX" "flags"
  vkFlags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTablePushConstantEntryNVX" "pipelineLayout"
  vkPipelineLayout :: PipelineLayout
  , -- No documentation found for Nested "ObjectTablePushConstantEntryNVX" "stageFlags"
  vkStageFlags :: ShaderStageFlags
  }
  deriving (Show, Eq)
withCStructObjectTablePushConstantEntryNVX :: ObjectTablePushConstantEntryNVX -> (VkObjectTablePushConstantEntryNVX -> IO a) -> IO a
withCStructObjectTablePushConstantEntryNVX from cont = cont (VkObjectTablePushConstantEntryNVX (vkType (from :: ObjectTablePushConstantEntryNVX)) (vkFlags (from :: ObjectTablePushConstantEntryNVX)) (vkPipelineLayout (from :: ObjectTablePushConstantEntryNVX)) (vkStageFlags (from :: ObjectTablePushConstantEntryNVX)))
fromCStructObjectTablePushConstantEntryNVX :: VkObjectTablePushConstantEntryNVX -> IO ObjectTablePushConstantEntryNVX
fromCStructObjectTablePushConstantEntryNVX c = ObjectTablePushConstantEntryNVX <$> pure (vkType (c :: VkObjectTablePushConstantEntryNVX))
                                                                               <*> pure (vkFlags (c :: VkObjectTablePushConstantEntryNVX))
                                                                               <*> pure (vkPipelineLayout (c :: VkObjectTablePushConstantEntryNVX))
                                                                               <*> pure (vkStageFlags (c :: VkObjectTablePushConstantEntryNVX))
-- No documentation found for TopLevel "ObjectTableVertexBufferEntryNVX"
data ObjectTableVertexBufferEntryNVX = ObjectTableVertexBufferEntryNVX
  { -- No documentation found for Nested "ObjectTableVertexBufferEntryNVX" "type"
  vkType :: ObjectEntryTypeNVX
  , -- No documentation found for Nested "ObjectTableVertexBufferEntryNVX" "flags"
  vkFlags :: ObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "ObjectTableVertexBufferEntryNVX" "buffer"
  vkBuffer :: Buffer
  }
  deriving (Show, Eq)
withCStructObjectTableVertexBufferEntryNVX :: ObjectTableVertexBufferEntryNVX -> (VkObjectTableVertexBufferEntryNVX -> IO a) -> IO a
withCStructObjectTableVertexBufferEntryNVX from cont = cont (VkObjectTableVertexBufferEntryNVX (vkType (from :: ObjectTableVertexBufferEntryNVX)) (vkFlags (from :: ObjectTableVertexBufferEntryNVX)) (vkBuffer (from :: ObjectTableVertexBufferEntryNVX)))
fromCStructObjectTableVertexBufferEntryNVX :: VkObjectTableVertexBufferEntryNVX -> IO ObjectTableVertexBufferEntryNVX
fromCStructObjectTableVertexBufferEntryNVX c = ObjectTableVertexBufferEntryNVX <$> pure (vkType (c :: VkObjectTableVertexBufferEntryNVX))
                                                                               <*> pure (vkFlags (c :: VkObjectTableVertexBufferEntryNVX))
                                                                               <*> pure (vkBuffer (c :: VkObjectTableVertexBufferEntryNVX))

-- | Wrapper for 'vkCmdProcessCommandsNVX'
cmdProcessCommandsNVX :: CommandBuffer ->  CmdProcessCommandsInfoNVX ->  IO ()
cmdProcessCommandsNVX = \(CommandBuffer commandBuffer commandTable) -> \processCommandsInfo -> (\a -> withCStructCmdProcessCommandsInfoNVX a . flip with) processCommandsInfo (\pProcessCommandsInfo -> Graphics.Vulkan.C.Dynamic.cmdProcessCommandsNVX commandTable commandBuffer pProcessCommandsInfo *> (pure ()))

-- | Wrapper for 'vkCmdReserveSpaceForCommandsNVX'
cmdReserveSpaceForCommandsNVX :: CommandBuffer ->  CmdReserveSpaceForCommandsInfoNVX ->  IO ()
cmdReserveSpaceForCommandsNVX = \(CommandBuffer commandBuffer commandTable) -> \reserveSpaceInfo -> (\a -> withCStructCmdReserveSpaceForCommandsInfoNVX a . flip with) reserveSpaceInfo (\pReserveSpaceInfo -> Graphics.Vulkan.C.Dynamic.cmdReserveSpaceForCommandsNVX commandTable commandBuffer pReserveSpaceInfo *> (pure ()))

-- | Wrapper for 'vkCreateIndirectCommandsLayoutNVX'
createIndirectCommandsLayoutNVX :: Device ->  IndirectCommandsLayoutCreateInfoNVX ->  Maybe AllocationCallbacks ->  IO ( IndirectCommandsLayoutNVX )
createIndirectCommandsLayoutNVX = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pIndirectCommandsLayout -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructIndirectCommandsLayoutCreateInfoNVX a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createIndirectCommandsLayoutNVX commandTable device pCreateInfo pAllocator pIndirectCommandsLayout >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pIndirectCommandsLayout)))))

-- | Wrapper for 'vkCreateObjectTableNVX'
createObjectTableNVX :: Device ->  ObjectTableCreateInfoNVX ->  Maybe AllocationCallbacks ->  IO ( ObjectTableNVX )
createObjectTableNVX = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pObjectTable -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructObjectTableCreateInfoNVX a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createObjectTableNVX commandTable device pCreateInfo pAllocator pObjectTable >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pObjectTable)))))

-- | Wrapper for 'vkDestroyIndirectCommandsLayoutNVX'
destroyIndirectCommandsLayoutNVX :: Device ->  IndirectCommandsLayoutNVX ->  Maybe AllocationCallbacks ->  IO ()
destroyIndirectCommandsLayoutNVX = \(Device device commandTable) -> \indirectCommandsLayout -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyIndirectCommandsLayoutNVX commandTable device indirectCommandsLayout pAllocator *> (pure ()))

-- | Wrapper for 'vkDestroyObjectTableNVX'
destroyObjectTableNVX :: Device ->  ObjectTableNVX ->  Maybe AllocationCallbacks ->  IO ()
destroyObjectTableNVX = \(Device device commandTable) -> \objectTable -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyObjectTableNVX commandTable device objectTable pAllocator *> (pure ()))

-- | Wrapper for 'vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX'
getPhysicalDeviceGeneratedCommandsPropertiesNVX :: PhysicalDevice ->  IO ( DeviceGeneratedCommandsFeaturesNVX
, DeviceGeneratedCommandsLimitsNVX )
getPhysicalDeviceGeneratedCommandsPropertiesNVX = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pLimits -> alloca (\pFeatures -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceGeneratedCommandsPropertiesNVX commandTable physicalDevice pFeatures pLimits *> ((,) <$> (fromCStructDeviceGeneratedCommandsFeaturesNVX <=< peek) pFeatures<*>(fromCStructDeviceGeneratedCommandsLimitsNVX <=< peek) pLimits)))

-- | Wrapper for 'vkRegisterObjectsNVX'
registerObjectsNVX :: Device ->  ObjectTableNVX ->  Vector ObjectTableEntryNVX ->  Vector Word32 ->  IO (  )
registerObjectsNVX = \(Device device commandTable) -> \objectTable -> \pObjectTableEntries -> \objectIndices -> withVec (&) objectIndices (\pObjectIndices -> withVec (\a -> withCStructObjectTableEntryNVX a . flip with) pObjectTableEntries (\pPObjectTableEntries -> Graphics.Vulkan.C.Dynamic.registerObjectsNVX commandTable device objectTable (fromIntegral $ Data.Vector.length pObjectTableEntries `min` Data.Vector.length objectIndices) pPObjectTableEntries pObjectIndices >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))))

-- | Wrapper for 'vkUnregisterObjectsNVX'
unregisterObjectsNVX :: Device ->  ObjectTableNVX ->  Vector ObjectEntryTypeNVX ->  Vector Word32 ->  IO (  )
unregisterObjectsNVX = \(Device device commandTable) -> \objectTable -> \objectEntryTypes -> \objectIndices -> withVec (&) objectIndices (\pObjectIndices -> withVec (&) objectEntryTypes (\pObjectEntryTypes -> Graphics.Vulkan.C.Dynamic.unregisterObjectsNVX commandTable device objectTable (fromIntegral $ Data.Vector.length objectEntryTypes `min` Data.Vector.length objectIndices) pObjectEntryTypes pObjectIndices >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))))
withIndirectCommandsLayoutNVX :: CreateInfo -> Maybe AllocationCallbacks -> (t -> IO a) -> IO a
withIndirectCommandsLayoutNVX createInfo allocationCallbacks =
  bracket
    (vkCreateIndirectCommandsLayoutNVX createInfo allocationCallbacks)
    (`vkDestroyIndirectCommandsLayoutNVX` allocationCallbacks)
withObjectTableNVX :: CreateInfo -> Maybe AllocationCallbacks -> (t -> IO a) -> IO a
withObjectTableNVX createInfo allocationCallbacks =
  bracket
    (vkCreateObjectTableNVX createInfo allocationCallbacks)
    (`vkDestroyObjectTableNVX` allocationCallbacks)
