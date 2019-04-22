{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkCmdProcessCommandsInfoNVX(..)
  , VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkIndirectCommandsLayoutNVX
  , VkIndirectCommandsLayoutTokenNVX(..)
  , VkIndirectCommandsLayoutUsageFlagBitsNVX(..)
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
  , VkIndirectCommandsLayoutUsageFlagsNVX
  , VkIndirectCommandsTokenNVX(..)
  , VkIndirectCommandsTokenTypeNVX(..)
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
  , VkObjectEntryUsageFlagBitsNVX(..)
  , pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
  , pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
  , VkObjectEntryUsageFlagsNVX
  , VkObjectTableCreateInfoNVX(..)
  , VkObjectTableDescriptorSetEntryNVX(..)
  , VkObjectTableEntryNVX(..)
  , VkObjectTableIndexBufferEntryNVX(..)
  , VkObjectTableNVX
  , VkObjectTablePipelineEntryNVX(..)
  , VkObjectTablePushConstantEntryNVX(..)
  , VkObjectTableVertexBufferEntryNVX(..)
  , FN_vkCmdProcessCommandsNVX
  , PFN_vkCmdProcessCommandsNVX
  , vkCmdProcessCommandsNVX
  , FN_vkCmdReserveSpaceForCommandsNVX
  , PFN_vkCmdReserveSpaceForCommandsNVX
  , vkCmdReserveSpaceForCommandsNVX
  , FN_vkCreateIndirectCommandsLayoutNVX
  , PFN_vkCreateIndirectCommandsLayoutNVX
  , vkCreateIndirectCommandsLayoutNVX
  , FN_vkCreateObjectTableNVX
  , PFN_vkCreateObjectTableNVX
  , vkCreateObjectTableNVX
  , FN_vkDestroyIndirectCommandsLayoutNVX
  , PFN_vkDestroyIndirectCommandsLayoutNVX
  , vkDestroyIndirectCommandsLayoutNVX
  , FN_vkDestroyObjectTableNVX
  , PFN_vkDestroyObjectTableNVX
  , vkDestroyObjectTableNVX
  , FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , PFN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , FN_vkRegisterObjectsNVX
  , PFN_vkRegisterObjectsNVX
  , vkRegisterObjectsNVX
  , FN_vkUnregisterObjectsNVX
  , PFN_vkUnregisterObjectsNVX
  , vkUnregisterObjectsNVX
  , pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX
  , pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX
  , pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX
  , pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
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
  ( FunPtr
  , Ptr
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


import Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkIndexType(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorSet
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  , VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipeline
  , VkPipelineLayout
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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
--     'VkDeviceGeneratedCommandsLimitsNVX'::@minSequenceCountBufferOffsetAlignment@
--
-- -   If @sequencesIndexBuffer@ is used, its usage flag /must/ have the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   If @sequencesIndexBuffer@ is used, @sequencesIndexOffset@ /must/ be
--     aligned to
--     'VkDeviceGeneratedCommandsLimitsNVX'::@minSequenceIndexBufferOffsetAlignment@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectTable@ /must/ be a valid 'VkObjectTableNVX' handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     'VkIndirectCommandsLayoutNVX' handle
--
-- -   @pIndirectCommandsTokens@ /must/ be a valid pointer to an array of
--     @indirectCommandsTokenCount@ valid 'VkIndirectCommandsTokenNVX'
--     structures
--
-- -   If @targetCommandBuffer@ is not @NULL@, @targetCommandBuffer@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   If @sequencesCountBuffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @sequencesCountBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   If @sequencesIndexBuffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @sequencesIndexBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @indirectCommandsTokenCount@ /must/ be greater than @0@
--
-- -   Each of @indirectCommandsLayout@, @objectTable@,
--     @sequencesCountBuffer@, @sequencesIndexBuffer@, and
--     @targetCommandBuffer@ that are valid handles /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
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
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'VkIndirectCommandsLayoutNVX', 'VkIndirectCommandsTokenNVX',
-- 'VkObjectTableNVX', 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCmdProcessCommandsNVX'
data VkCmdProcessCommandsInfoNVX = VkCmdProcessCommandsInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @objectTable@ is the 'VkObjectTableNVX' to be used for the generation
  -- process. Only registered objects at the time
  -- 'vkCmdReserveSpaceForCommandsNVX' is called, will be taken into account
  -- for the reservation.
  vkObjectTable :: VkObjectTableNVX
  , -- | @indirectCommandsLayout@ is the 'VkIndirectCommandsLayoutNVX' that
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
  -- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', this is also the
  -- actual number of sequences generated.
  vkMaxSequencesCount :: Word32
  , -- | @targetCommandBuffer@ /can/ be the secondary
  -- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' in which the commands
  -- should be recorded. If @targetCommandBuffer@ is @NULL@ an implicit
  -- reservation as well as execution takes place on the processing
  -- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'.
  vkTargetCommandBuffer :: VkCommandBuffer
  , -- | @sequencesCountBuffer@ /can/ be
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' from which the
  -- actual amount of sequences is sourced from as @uint32_t@ value.
  vkSequencesCountBuffer :: VkBuffer
  , -- | @sequencesCountOffset@ is the byte offset into @sequencesCountBuffer@
  -- where the count value is stored.
  vkSequencesCountOffset :: VkDeviceSize
  , -- | @sequencesIndexBuffer@ /must/ be set if @indirectCommandsLayout@’s
  -- 'VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX' is set and
  -- provides the used sequence indices as @uint32_t@ array. Otherwise it
  -- /must/ be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'.
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

instance Zero VkCmdProcessCommandsInfoNVX where
  zero = VkCmdProcessCommandsInfoNVX VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero

-- | VkCmdReserveSpaceForCommandsInfoNVX - Structure specifying parameters
-- for the reservation of command buffer space
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectTable@ /must/ be a valid 'VkObjectTableNVX' handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     'VkIndirectCommandsLayoutNVX' handle
--
-- -   Both of @indirectCommandsLayout@, and @objectTable@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @objectTable@ /must/ be externally synchronized
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutNVX', 'VkObjectTableNVX',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCmdReserveSpaceForCommandsNVX'
data VkCmdReserveSpaceForCommandsInfoNVX = VkCmdReserveSpaceForCommandsInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @objectTable@ is the 'VkObjectTableNVX' to be used for the generation
  -- process. Only registered objects at the time
  -- 'vkCmdReserveSpaceForCommandsNVX' is called, will be taken into account
  -- for the reservation.
  vkObjectTable :: VkObjectTableNVX
  , -- | @indirectCommandsLayout@ is the 'VkIndirectCommandsLayoutNVX' that
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

instance Zero VkCmdReserveSpaceForCommandsInfoNVX where
  zero = VkCmdReserveSpaceForCommandsInfoNVX VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
                                             zero
                                             zero
                                             zero
                                             zero

-- | VkDeviceGeneratedCommandsFeaturesNVX - Structure specifying physical
-- device support
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX'
data VkDeviceGeneratedCommandsFeaturesNVX = VkDeviceGeneratedCommandsFeaturesNVX
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @computeBindingPointSupport@ specifies whether the 'VkObjectTableNVX'
  -- supports entries with 'VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX' bit set
  -- and 'VkIndirectCommandsLayoutNVX' supports
  -- 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_COMPUTE'.
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

instance Zero VkDeviceGeneratedCommandsFeaturesNVX where
  zero = VkDeviceGeneratedCommandsFeaturesNVX VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
                                              zero
                                              zero

-- | VkDeviceGeneratedCommandsLimitsNVX - Structure specifying physical
-- device limits
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX'
data VkDeviceGeneratedCommandsLimitsNVX = VkDeviceGeneratedCommandsLimitsNVX
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @maxIndirectCommandsLayoutTokenCount@ the maximum number of tokens in
  -- 'VkIndirectCommandsLayoutNVX'.
  vkMaxIndirectCommandsLayoutTokenCount :: Word32
  , -- | @maxObjectEntryCounts@ the maximum number of entries per resource type
  -- in 'VkObjectTableNVX'.
  vkMaxObjectEntryCounts :: Word32
  , -- | @minSequenceCountBufferOffsetAlignment@ the minimum alignment for memory
  -- addresses optionally used in 'vkCmdProcessCommandsNVX'.
  vkMinSequenceCountBufferOffsetAlignment :: Word32
  , -- | @minSequenceIndexBufferOffsetAlignment@ the minimum alignment for memory
  -- addresses optionally used in 'vkCmdProcessCommandsNVX'.
  vkMinSequenceIndexBufferOffsetAlignment :: Word32
  , -- | @minCommandsTokenBufferOffsetAlignment@ the minimum alignment for memory
  -- addresses optionally used in 'vkCmdProcessCommandsNVX'.
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

instance Zero VkDeviceGeneratedCommandsLimitsNVX where
  zero = VkDeviceGeneratedCommandsLimitsNVX VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
                                            zero
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
--     'VkDeviceGeneratedCommandsLimitsNVX'::@maxIndirectCommandsLayoutTokenCount@
--
-- -   If the
--     'VkDeviceGeneratedCommandsFeaturesNVX'::@computeBindingPointSupport@
--     feature is not enabled, then @pipelineBindPoint@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_COMPUTE'
--
-- -   If @pTokens@ contains an entry of
--     'VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX' it /must/ be the
--     first element of the array and there /must/ be only a single element
--     of such token type.
--
-- -   All state binding tokens in @pTokens@ /must/ occur prior work
--     provoking tokens ('VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX',
--     'VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX',
--     'VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX').
--
-- -   The content of @pTokens@ /must/ include one single work provoking
--     token that is compatible with the @pipelineBindPoint@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' value
--
-- -   @flags@ /must/ be a valid combination of
--     'VkIndirectCommandsLayoutUsageFlagBitsNVX' values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @pTokens@ /must/ be a valid pointer to an array of @tokenCount@
--     valid 'VkIndirectCommandsLayoutTokenNVX' structures
--
-- -   @tokenCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutTokenNVX',
-- 'VkIndirectCommandsLayoutUsageFlagsNVX',
-- 'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateIndirectCommandsLayoutNVX'
data VkIndirectCommandsLayoutCreateInfoNVX = VkIndirectCommandsLayoutCreateInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pipelineBindPoint@ is the
  -- 'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' that this layout
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

instance Zero VkIndirectCommandsLayoutCreateInfoNVX where
  zero = VkIndirectCommandsLayoutCreateInfoNVX VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
                                               zero
                                               zero
                                               zero
                                               zero
                                               zero

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

-- | VkIndirectCommandsLayoutTokenNVX - Struct specifying the details of an
-- indirect command layout token
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutCreateInfoNVX',
-- 'VkIndirectCommandsTokenTypeNVX'
data VkIndirectCommandsLayoutTokenNVX = VkIndirectCommandsLayoutTokenNVX
  { -- | @tokenType@ /must/ be a valid 'VkIndirectCommandsTokenTypeNVX' value
  vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , -- | @bindingUnit@ /must/ stay within device supported limits for the
  -- appropriate commands.
  vkBindingUnit :: Word32
  , -- | @dynamicCount@ /must/ stay within device supported limits for the
  -- appropriate commands.
  vkDynamicCount :: Word32
  , -- | @divisor@ /must/ be greater than @0@ and a power of two.
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

instance Zero VkIndirectCommandsLayoutTokenNVX where
  zero = VkIndirectCommandsLayoutTokenNVX zero
                                          zero
                                          zero
                                          zero

-- ** VkIndirectCommandsLayoutUsageFlagBitsNVX

-- | VkIndirectCommandsLayoutUsageFlagBitsNVX - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutUsageFlagsNVX'
newtype VkIndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- | 'VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX'
-- specifies that the processing of sequences /can/ happen at an
-- implementation-dependent order, which is not guaranteed to be coherent
-- across multiple invocations.
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000001

-- | 'VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX' specifies
-- that there is likely a high difference between allocated number of
-- sequences and actually used.
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000002

-- | 'VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX' specifies
-- that there are likely many draw or dispatch calls that are zero-sized
-- (zero grid dimension, no primitives to render).
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000004

-- | 'VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX' specifies
-- that the input data for the sequences is not implicitly indexed from
-- 0..sequencesUsed but a user provided
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' encoding the index
-- is provided.
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000008

-- | VkIndirectCommandsLayoutUsageFlagsNVX - Bitmask of
-- VkIndirectCommandsLayoutUsageFlagBitsNVX
--
-- = Description
--
-- 'VkIndirectCommandsLayoutUsageFlagsNVX' is a bitmask type for setting a
-- mask of zero or more 'VkIndirectCommandsLayoutUsageFlagBitsNVX'.
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutCreateInfoNVX',
-- 'VkIndirectCommandsLayoutUsageFlagBitsNVX'
type VkIndirectCommandsLayoutUsageFlagsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX

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
--     'VkDeviceGeneratedCommandsLimitsNVX'::@minCommandsTokenBufferOffsetAlignment@.
--
-- == Valid Usage (Implicit)
--
-- -   @tokenType@ /must/ be a valid 'VkIndirectCommandsTokenTypeNVX' value
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'VkIndirectCommandsTokenTypeNVX'
data VkIndirectCommandsTokenNVX = VkIndirectCommandsTokenNVX
  { -- | @tokenType@ specifies the token command type.
  vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , -- | @buffer@ specifies the
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' storing the
  -- functional arguments for each squence. These argumetns can be written by
  -- the device.
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

instance Zero VkIndirectCommandsTokenNVX where
  zero = VkIndirectCommandsTokenNVX zero
                                    zero
                                    zero

-- ** VkIndirectCommandsTokenTypeNVX

-- | VkIndirectCommandsTokenTypeNVX - Enum specifying
--
-- = Description
--
-- \'
--
-- > +-----------------------------------------------+----------------------+
-- > | Token type                                    | Equivalent command   |
-- > +===============================================+======================+
-- > | 'VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX | 'Graphics.Vulkan.C.C |
-- > | '                                             | ore10.CommandBufferB |
-- > |                                               | uilding.vkCmdBindPip |
-- > |                                               | eline'               |
-- > +-----------------------------------------------+----------------------+
-- > | 'VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_S | 'Graphics.Vulkan.C.C |
-- > | ET_NVX'                                       | ore10.CommandBufferB |
-- > |                                               | uilding.vkCmdBindDes |
-- > |                                               | criptorSets'         |
-- > +-----------------------------------------------+----------------------+
-- > | 'VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER | 'Graphics.Vulkan.C.C |
-- > | _NVX'                                         | ore10.CommandBufferB |
-- > |                                               | uilding.vkCmdBindInd |
-- > |                                               | exBuffer'            |
-- > +-----------------------------------------------+----------------------+
-- > | 'VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFE | 'Graphics.Vulkan.C.C |
-- > | R_NVX'                                        | ore10.CommandBufferB |
-- > |                                               | uilding.vkCmdBindVer |
-- > |                                               | texBuffers'          |
-- > +-----------------------------------------------+----------------------+
-- > | 'VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTAN | 'Graphics.Vulkan.C.C |
-- > | T_NVX'                                        | ore10.CommandBufferB |
-- > |                                               | uilding.vkCmdPushCon |
-- > |                                               | stants'              |
-- > +-----------------------------------------------+----------------------+
-- > | 'VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED | 'Graphics.Vulkan.C.C |
-- > | _NVX'                                         | ore10.CommandBufferB |
-- > |                                               | uilding.vkCmdDrawInd |
-- > |                                               | exedIndirect'        |
-- > +-----------------------------------------------+----------------------+
-- > | 'VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX'    | 'Graphics.Vulkan.C.C |
-- > |                                               | ore10.CommandBufferB |
-- > |                                               | uilding.vkCmdDrawInd |
-- > |                                               | irect'               |
-- > +-----------------------------------------------+----------------------+
-- > | 'VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX | 'Graphics.Vulkan.C.C |
-- > | '                                             | ore10.CommandBufferB |
-- > |                                               | uilding.vkCmdDispatc |
-- > |                                               | hIndirect'           |
-- > +-----------------------------------------------+----------------------+
-- >
-- > Supported indirect command tokens
--
-- = See Also
--
-- 'VkIndirectCommandsLayoutTokenNVX', 'VkIndirectCommandsTokenNVX'
newtype VkIndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX Int32
  deriving (Eq, Ord, Storable, Zero)

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
  deriving (Eq, Ord, Storable, Zero)

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

-- | 'VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX' specifies a
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' resource entry
-- that is registered via 'VkObjectTableDescriptorSetEntryNVX'.
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = VkObjectEntryTypeNVX 0

-- | 'VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX' specifies a
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' resource entry that is
-- registered via 'VkObjectTablePipelineEntryNVX'.
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = VkObjectEntryTypeNVX 1

-- | 'VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX' specifies a
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' resource entry that
-- is registered via 'VkObjectTableIndexBufferEntryNVX'.
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = VkObjectEntryTypeNVX 2

-- | 'VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX' specifies a
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' resource entry that
-- is registered via 'VkObjectTableVertexBufferEntryNVX'.
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = VkObjectEntryTypeNVX 3

-- | 'VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX' specifies the resource entry is
-- registered via 'VkObjectTablePushConstantEntryNVX'.
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = VkObjectEntryTypeNVX 4

-- ** VkObjectEntryUsageFlagBitsNVX

-- | VkObjectEntryUsageFlagBitsNVX - Bitmask specifying allowed usage of an
-- object entry
--
-- = See Also
--
-- 'VkObjectEntryUsageFlagsNVX'
newtype VkObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- | 'VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX' specifies that the resource is
-- bound to 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000001

-- | 'VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX' specifies that the resource is
-- bound to 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_COMPUTE'
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000002

-- | VkObjectEntryUsageFlagsNVX - Bitmask of VkObjectEntryUsageFlagBitsNVX
--
-- = Description
--
-- 'VkObjectEntryUsageFlagsNVX' is a bitmask type for setting a mask of
-- zero or more 'VkObjectEntryUsageFlagBitsNVX'.
--
-- = See Also
--
-- 'VkObjectEntryUsageFlagBitsNVX', 'VkObjectTableCreateInfoNVX',
-- 'VkObjectTableDescriptorSetEntryNVX', 'VkObjectTableEntryNVX',
-- 'VkObjectTableIndexBufferEntryNVX', 'VkObjectTablePipelineEntryNVX',
-- 'VkObjectTablePushConstantEntryNVX', 'VkObjectTableVertexBufferEntryNVX'
type VkObjectEntryUsageFlagsNVX = VkObjectEntryUsageFlagBitsNVX

-- | VkObjectTableCreateInfoNVX - Structure specifying the parameters of a
-- newly created object table
--
-- == Valid Usage
--
-- -   If the
--     'VkDeviceGeneratedCommandsFeaturesNVX'::@computeBindingPointSupport@
--     feature is not enabled, @pObjectEntryUsageFlags@ /must/ not contain
--     'VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX'
--
-- -   Any value within @pObjectEntryCounts@ /must/ not exceed
--     'VkDeviceGeneratedCommandsLimitsNVX'::@maxObjectEntryCounts@
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
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX'
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
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateObjectTableNVX'
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
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
  -- used by any single registered
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' in this table.
  vkMaxUniformBuffersPerDescriptor :: Word32
  , -- | @maxStorageBuffersPerDescriptor@ is the maximum number of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
  -- used by any single registered
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' in this table.
  vkMaxStorageBuffersPerDescriptor :: Word32
  , -- | @maxStorageImagesPerDescriptor@ is the maximum number of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
  -- used by any single registered
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' in this table.
  vkMaxStorageImagesPerDescriptor :: Word32
  , -- | @maxSampledImagesPerDescriptor@ is the maximum number of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
  -- used by any single registered
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' in this table.
  vkMaxSampledImagesPerDescriptor :: Word32
  , -- | @maxPipelineLayouts@ is the maximum number of unique
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used by any
  -- registered 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' or
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' in this table.
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

instance Zero VkObjectTableCreateInfoNVX where
  zero = VkObjectTableCreateInfoNVX VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
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
-- -   @type@ /must/ be 'VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX'
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
-- -   @pipelineLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle
--
-- -   @descriptorSet@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' handle
--
-- -   Both of @descriptorSet@, and @pipelineLayout@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet',
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout'
data VkObjectTableDescriptorSetEntryNVX = VkObjectTableDescriptorSetEntryNVX
  { -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @pipelineLayout@ specifies the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that the
  -- @descriptorSet@ is used with.
  vkPipelineLayout :: VkPipelineLayout
  , -- | @descriptorSet@ specifies the
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' that can be
  -- bound with this entry.
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

instance Zero VkObjectTableDescriptorSetEntryNVX where
  zero = VkObjectTableDescriptorSetEntryNVX zero
                                            zero
                                            zero
                                            zero

-- | VkObjectTableEntryNVX - Common parameters of an object table resource
-- entry
--
-- == Valid Usage
--
-- -   If the
--     'VkDeviceGeneratedCommandsFeaturesNVX'::@computeBindingPointSupport@
--     feature is not enabled, @flags@ /must/ not contain
--     'VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX'
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
  , -- | @flags@ defines which
  -- 'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' the resource can be
  -- used with. Some entry types allow only a single flag to be set.
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

instance Zero VkObjectTableEntryNVX where
  zero = VkObjectTableEntryNVX zero
                               zero

-- | VkObjectTableIndexBufferEntryNVX - Parameters of an object table index
-- buffer entry
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType',
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX'
data VkObjectTableIndexBufferEntryNVX = VkObjectTableIndexBufferEntryNVX
  { -- | @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
  vkType :: VkObjectEntryTypeNVX
  , -- | @flags@ /must/ not be @0@
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @buffer@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
  vkBuffer :: VkBuffer
  , -- | @indexType@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType' value
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

instance Zero VkObjectTableIndexBufferEntryNVX where
  zero = VkObjectTableIndexBufferEntryNVX zero
                                          zero
                                          zero
                                          zero

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

-- | VkObjectTablePipelineEntryNVX - Parameters of an object table pipeline
-- entry
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
data VkObjectTablePipelineEntryNVX = VkObjectTablePipelineEntryNVX
  { -- | @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
  vkType :: VkObjectEntryTypeNVX
  , -- | @flags@ /must/ not be @0@
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @pipeline@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' handle
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

instance Zero VkObjectTablePipelineEntryNVX where
  zero = VkObjectTablePipelineEntryNVX zero
                                       zero
                                       zero

-- | VkObjectTablePushConstantEntryNVX - Parameters of an object table push
-- constant entry
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags'
data VkObjectTablePushConstantEntryNVX = VkObjectTablePushConstantEntryNVX
  { -- | @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
  vkType :: VkObjectEntryTypeNVX
  , -- | @flags@ /must/ not be @0@
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @pipelineLayout@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle
  vkPipelineLayout :: VkPipelineLayout
  , -- | @stageFlags@ /must/ not be @0@
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

instance Zero VkObjectTablePushConstantEntryNVX where
  zero = VkObjectTablePushConstantEntryNVX zero
                                           zero
                                           zero
                                           zero

-- | VkObjectTableVertexBufferEntryNVX - Parameters of an object table vertex
-- buffer entry
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'VkObjectEntryTypeNVX', 'VkObjectEntryUsageFlagsNVX'
data VkObjectTableVertexBufferEntryNVX = VkObjectTableVertexBufferEntryNVX
  { -- | @type@ /must/ be a valid 'VkObjectEntryTypeNVX' value
  vkType :: VkObjectEntryTypeNVX
  , -- | @flags@ /must/ not be @0@
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @buffer@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
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

instance Zero VkObjectTableVertexBufferEntryNVX where
  zero = VkObjectTableVertexBufferEntryNVX zero
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
--     'VkCmdProcessCommandsInfoNVX' structure containing parameters
--     affecting the processing of commands.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pProcessCommandsInfo@ /must/ be a valid pointer to a valid
--     'VkCmdProcessCommandsInfoNVX' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdProcessCommandsNVX" vkCmdProcessCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()
#else
vkCmdProcessCommandsNVX :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()
vkCmdProcessCommandsNVX deviceCmds = mkVkCmdProcessCommandsNVX (pVkCmdProcessCommandsNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdProcessCommandsNVX
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ())
#endif

type FN_vkCmdProcessCommandsNVX = ("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()
type PFN_vkCmdProcessCommandsNVX = FunPtr FN_vkCmdProcessCommandsNVX

-- | vkCmdReserveSpaceForCommandsNVX - Perform a reservation of command
-- buffer space
--
-- = Parameters
--
-- -   @commandBuffer@ is the secondary command buffer in which the space
--     for device-generated commands is reserved.
--
-- -   @pProcessCommandsInfo@ is a pointer to an instance of the
--     'VkCmdReserveSpaceForCommandsInfoNVX' structure containing
--     parameters affecting the reservation of command buffer space.
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
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pReserveSpaceInfo@ /must/ be a valid pointer to a valid
--     'VkCmdReserveSpaceForCommandsInfoNVX' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   @commandBuffer@ /must/ be a secondary
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Secondary       | Inside          | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'VkCmdReserveSpaceForCommandsInfoNVX',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdReserveSpaceForCommandsNVX" vkCmdReserveSpaceForCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()
#else
vkCmdReserveSpaceForCommandsNVX :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()
vkCmdReserveSpaceForCommandsNVX deviceCmds = mkVkCmdReserveSpaceForCommandsNVX (pVkCmdReserveSpaceForCommandsNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdReserveSpaceForCommandsNVX
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ())
#endif

type FN_vkCmdReserveSpaceForCommandsNVX = ("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()
type PFN_vkCmdReserveSpaceForCommandsNVX = FunPtr FN_vkCmdReserveSpaceForCommandsNVX

-- | vkCreateIndirectCommandsLayoutNVX - Create an indirect command layout
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the indirect command
--     layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkIndirectCommandsLayoutCreateInfoNVX' structure containing
--     parameters affecting creation of the indirect command layout.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pIndirectCommandsLayout@ points to a 'VkIndirectCommandsLayoutNVX'
--     handle in which the resulting indirect command layout is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'VkIndirectCommandsLayoutCreateInfoNVX' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pIndirectCommandsLayout@ /must/ be a valid pointer to a
--     'VkIndirectCommandsLayoutNVX' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkIndirectCommandsLayoutCreateInfoNVX', 'VkIndirectCommandsLayoutNVX'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateIndirectCommandsLayoutNVX" vkCreateIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult
#else
vkCreateIndirectCommandsLayoutNVX :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult
vkCreateIndirectCommandsLayoutNVX deviceCmds = mkVkCreateIndirectCommandsLayoutNVX (pVkCreateIndirectCommandsLayoutNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIndirectCommandsLayoutNVX
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult)
#endif

type FN_vkCreateIndirectCommandsLayoutNVX = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult
type PFN_vkCreateIndirectCommandsLayoutNVX = FunPtr FN_vkCreateIndirectCommandsLayoutNVX

-- | vkCreateObjectTableNVX - Create an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkObjectTableCreateInfoNVX' structure containing parameters
--     affecting creation of the table.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pObjectTable@ points to a 'VkObjectTableNVX' handle in which the
--     resulting object table is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'VkObjectTableCreateInfoNVX' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pObjectTable@ /must/ be a valid pointer to a 'VkObjectTableNVX'
--     handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkObjectTableCreateInfoNVX', 'VkObjectTableNVX'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateObjectTableNVX" vkCreateObjectTableNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult
#else
vkCreateObjectTableNVX :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult
vkCreateObjectTableNVX deviceCmds = mkVkCreateObjectTableNVX (pVkCreateObjectTableNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateObjectTableNVX
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult)
#endif

type FN_vkCreateObjectTableNVX = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult
type PFN_vkCreateObjectTableNVX = FunPtr FN_vkCreateObjectTableNVX

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
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @indirectCommandsLayout@ /must/ be a valid
--     'VkIndirectCommandsLayoutNVX' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @indirectCommandsLayout@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkIndirectCommandsLayoutNVX'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyIndirectCommandsLayoutNVX" vkDestroyIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyIndirectCommandsLayoutNVX :: DeviceCmds -> ("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyIndirectCommandsLayoutNVX deviceCmds = mkVkDestroyIndirectCommandsLayoutNVX (pVkDestroyIndirectCommandsLayoutNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyIndirectCommandsLayoutNVX
  :: FunPtr (("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyIndirectCommandsLayoutNVX = ("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyIndirectCommandsLayoutNVX = FunPtr FN_vkDestroyIndirectCommandsLayoutNVX

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
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @objectTable@ /must/ be a valid 'VkObjectTableNVX' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkObjectTableNVX'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyObjectTableNVX" vkDestroyObjectTableNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyObjectTableNVX :: DeviceCmds -> ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyObjectTableNVX deviceCmds = mkVkDestroyObjectTableNVX (pVkDestroyObjectTableNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyObjectTableNVX
  :: FunPtr (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyObjectTableNVX = ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyObjectTableNVX = FunPtr FN_vkDestroyObjectTableNVX

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
-- = See Also
--
-- 'VkDeviceGeneratedCommandsFeaturesNVX',
-- 'VkDeviceGeneratedCommandsLimitsNVX',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX" vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()
#else
vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()
vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX deviceCmds = mkVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX (pVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ())
#endif

type FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()
type PFN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX = FunPtr FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX

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
--     'VkObjectTablePipelineEntryNVX',
--     'VkObjectTableDescriptorSetEntryNVX',
--     'VkObjectTableVertexBufferEntryNVX',
--     'VkObjectTableIndexBufferEntryNVX' or
--     'VkObjectTablePushConstantEntryNVX' (see below for details).
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
--     'VkObjectTableCreateInfoNVX'::@pObjectEntryCounts@ limits provided
--     at @objectTable@ creation time.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @objectTable@ /must/ be a valid 'VkObjectTableNVX' handle
--
-- -   @ppObjectTableEntries@ /must/ be a valid pointer to an array of
--     @objectCount@ valid 'VkObjectTableEntryNVX' structures
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkObjectTableEntryNVX', 'VkObjectTableNVX'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkRegisterObjectsNVX" vkRegisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
#else
vkRegisterObjectsNVX :: DeviceCmds -> ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
vkRegisterObjectsNVX deviceCmds = mkVkRegisterObjectsNVX (pVkRegisterObjectsNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterObjectsNVX
  :: FunPtr (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult) -> (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult)
#endif

type FN_vkRegisterObjectsNVX = ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
type PFN_vkRegisterObjectsNVX = FunPtr FN_vkRegisterObjectsNVX

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
-- -   @pObjectEntryType@ provides an array of 'VkObjectEntryTypeNVX' for
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
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @objectTable@ /must/ be a valid 'VkObjectTableNVX' handle
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkObjectEntryTypeNVX', 'VkObjectTableNVX'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUnregisterObjectsNVX" vkUnregisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
#else
vkUnregisterObjectsNVX :: DeviceCmds -> ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
vkUnregisterObjectsNVX deviceCmds = mkVkUnregisterObjectsNVX (pVkUnregisterObjectsNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUnregisterObjectsNVX
  :: FunPtr (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult) -> (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult)
#endif

type FN_vkUnregisterObjectsNVX = ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
type PFN_vkUnregisterObjectsNVX = FunPtr FN_vkUnregisterObjectsNVX

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX"
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX :: VkAccessFlagBits
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX = VkAccessFlagBits 0x00020000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX"
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX :: VkAccessFlagBits
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX = VkAccessFlagBits 0x00040000

-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME"
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NVX_device_generated_commands"

-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION"
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: Integral a => a
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX"
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX :: VkObjectType
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX = VkObjectType 1000086001

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_OBJECT_TABLE_NVX"
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX :: VkObjectType
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX = VkObjectType 1000086000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX"
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX = VkPipelineStageFlagBits 0x00020000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX"
pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX = VkStructureType 1000086002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX"
pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX = VkStructureType 1000086003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX"
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX = VkStructureType 1000086005

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX"
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX = VkStructureType 1000086004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX"
pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX = VkStructureType 1000086001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX"
pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX = VkStructureType 1000086000
