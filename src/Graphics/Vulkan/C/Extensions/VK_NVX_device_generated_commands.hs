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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdProcessCommandsNVX
#endif
  , FN_vkCmdProcessCommandsNVX
  , PFN_vkCmdProcessCommandsNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdReserveSpaceForCommandsNVX
#endif
  , FN_vkCmdReserveSpaceForCommandsNVX
  , PFN_vkCmdReserveSpaceForCommandsNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateIndirectCommandsLayoutNVX
#endif
  , FN_vkCreateIndirectCommandsLayoutNVX
  , PFN_vkCreateIndirectCommandsLayoutNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateObjectTableNVX
#endif
  , FN_vkCreateObjectTableNVX
  , PFN_vkCreateObjectTableNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkDestroyIndirectCommandsLayoutNVX
#endif
  , FN_vkDestroyIndirectCommandsLayoutNVX
  , PFN_vkDestroyIndirectCommandsLayoutNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkDestroyObjectTableNVX
#endif
  , FN_vkDestroyObjectTableNVX
  , PFN_vkDestroyObjectTableNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
#endif
  , FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , PFN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkRegisterObjectsNVX
#endif
  , FN_vkRegisterObjectsNVX
  , PFN_vkRegisterObjectsNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkUnregisterObjectsNVX
#endif
  , FN_vkUnregisterObjectsNVX
  , PFN_vkUnregisterObjectsNVX
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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkCmdProcessCommandsInfoNVX"
data VkCmdProcessCommandsInfoNVX = VkCmdProcessCommandsInfoNVX
  { -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "objectTable"
  vkObjectTable :: VkObjectTableNVX
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "indirectCommandsLayout"
  vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "indirectCommandsTokenCount"
  vkIndirectCommandsTokenCount :: Word32
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "pIndirectCommandsTokens"
  vkPIndirectCommandsTokens :: Ptr VkIndirectCommandsTokenNVX
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "maxSequencesCount"
  vkMaxSequencesCount :: Word32
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "targetCommandBuffer"
  vkTargetCommandBuffer :: VkCommandBuffer
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "sequencesCountBuffer"
  vkSequencesCountBuffer :: VkBuffer
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "sequencesCountOffset"
  vkSequencesCountOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "sequencesIndexBuffer"
  vkSequencesIndexBuffer :: VkBuffer
  , -- No documentation found for Nested "VkCmdProcessCommandsInfoNVX" "sequencesIndexOffset"
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
  zero = VkCmdProcessCommandsInfoNVX zero
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
-- No documentation found for TopLevel "VkCmdReserveSpaceForCommandsInfoNVX"
data VkCmdReserveSpaceForCommandsInfoNVX = VkCmdReserveSpaceForCommandsInfoNVX
  { -- No documentation found for Nested "VkCmdReserveSpaceForCommandsInfoNVX" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCmdReserveSpaceForCommandsInfoNVX" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCmdReserveSpaceForCommandsInfoNVX" "objectTable"
  vkObjectTable :: VkObjectTableNVX
  , -- No documentation found for Nested "VkCmdReserveSpaceForCommandsInfoNVX" "indirectCommandsLayout"
  vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX
  , -- No documentation found for Nested "VkCmdReserveSpaceForCommandsInfoNVX" "maxSequencesCount"
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
  zero = VkCmdReserveSpaceForCommandsInfoNVX zero
                                             zero
                                             zero
                                             zero
                                             zero
-- No documentation found for TopLevel "VkDeviceGeneratedCommandsFeaturesNVX"
data VkDeviceGeneratedCommandsFeaturesNVX = VkDeviceGeneratedCommandsFeaturesNVX
  { -- No documentation found for Nested "VkDeviceGeneratedCommandsFeaturesNVX" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGeneratedCommandsFeaturesNVX" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGeneratedCommandsFeaturesNVX" "computeBindingPointSupport"
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
  zero = VkDeviceGeneratedCommandsFeaturesNVX zero
                                              zero
                                              zero
-- No documentation found for TopLevel "VkDeviceGeneratedCommandsLimitsNVX"
data VkDeviceGeneratedCommandsLimitsNVX = VkDeviceGeneratedCommandsLimitsNVX
  { -- No documentation found for Nested "VkDeviceGeneratedCommandsLimitsNVX" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGeneratedCommandsLimitsNVX" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGeneratedCommandsLimitsNVX" "maxIndirectCommandsLayoutTokenCount"
  vkMaxIndirectCommandsLayoutTokenCount :: Word32
  , -- No documentation found for Nested "VkDeviceGeneratedCommandsLimitsNVX" "maxObjectEntryCounts"
  vkMaxObjectEntryCounts :: Word32
  , -- No documentation found for Nested "VkDeviceGeneratedCommandsLimitsNVX" "minSequenceCountBufferOffsetAlignment"
  vkMinSequenceCountBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "VkDeviceGeneratedCommandsLimitsNVX" "minSequenceIndexBufferOffsetAlignment"
  vkMinSequenceIndexBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "VkDeviceGeneratedCommandsLimitsNVX" "minCommandsTokenBufferOffsetAlignment"
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
  zero = VkDeviceGeneratedCommandsLimitsNVX zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
-- No documentation found for TopLevel "VkIndirectCommandsLayoutCreateInfoNVX"
data VkIndirectCommandsLayoutCreateInfoNVX = VkIndirectCommandsLayoutCreateInfoNVX
  { -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNVX" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNVX" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNVX" "pipelineBindPoint"
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNVX" "flags"
  vkFlags :: VkIndirectCommandsLayoutUsageFlagsNVX
  , -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNVX" "tokenCount"
  vkTokenCount :: Word32
  , -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNVX" "pTokens"
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
  zero = VkIndirectCommandsLayoutCreateInfoNVX zero
                                               zero
                                               zero
                                               zero
                                               zero
                                               zero
-- | Dummy data to tag the 'Ptr' with
data VkIndirectCommandsLayoutNVX_T
-- No documentation found for TopLevel "VkIndirectCommandsLayoutNVX"
type VkIndirectCommandsLayoutNVX = Ptr VkIndirectCommandsLayoutNVX_T
-- No documentation found for TopLevel "VkIndirectCommandsLayoutTokenNVX"
data VkIndirectCommandsLayoutTokenNVX = VkIndirectCommandsLayoutTokenNVX
  { -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNVX" "tokenType"
  vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNVX" "bindingUnit"
  vkBindingUnit :: Word32
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNVX" "dynamicCount"
  vkDynamicCount :: Word32
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNVX" "divisor"
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

-- No documentation found for TopLevel "VkIndirectCommandsLayoutUsageFlagBitsNVX"
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

-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNVX" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX"
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000001

-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNVX" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX"
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000002

-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNVX" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX"
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000004

-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNVX" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX"
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000008
-- No documentation found for TopLevel "VkIndirectCommandsLayoutUsageFlagsNVX"
type VkIndirectCommandsLayoutUsageFlagsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX
-- No documentation found for TopLevel "VkIndirectCommandsTokenNVX"
data VkIndirectCommandsTokenNVX = VkIndirectCommandsTokenNVX
  { -- No documentation found for Nested "VkIndirectCommandsTokenNVX" "tokenType"
  vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , -- No documentation found for Nested "VkIndirectCommandsTokenNVX" "buffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkIndirectCommandsTokenNVX" "offset"
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

-- No documentation found for TopLevel "VkIndirectCommandsTokenTypeNVX"
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

-- No documentation found for TopLevel "VkObjectEntryTypeNVX"
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

-- No documentation found for Nested "VkObjectEntryTypeNVX" "VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX"
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = VkObjectEntryTypeNVX 0

-- No documentation found for Nested "VkObjectEntryTypeNVX" "VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX"
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = VkObjectEntryTypeNVX 1

-- No documentation found for Nested "VkObjectEntryTypeNVX" "VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX"
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = VkObjectEntryTypeNVX 2

-- No documentation found for Nested "VkObjectEntryTypeNVX" "VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX"
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = VkObjectEntryTypeNVX 3

-- No documentation found for Nested "VkObjectEntryTypeNVX" "VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX"
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = VkObjectEntryTypeNVX 4
-- ** VkObjectEntryUsageFlagBitsNVX

-- No documentation found for TopLevel "VkObjectEntryUsageFlagBitsNVX"
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

-- No documentation found for Nested "VkObjectEntryUsageFlagBitsNVX" "VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX"
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000001

-- No documentation found for Nested "VkObjectEntryUsageFlagBitsNVX" "VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX"
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000002
-- No documentation found for TopLevel "VkObjectEntryUsageFlagsNVX"
type VkObjectEntryUsageFlagsNVX = VkObjectEntryUsageFlagBitsNVX
-- No documentation found for TopLevel "VkObjectTableCreateInfoNVX"
data VkObjectTableCreateInfoNVX = VkObjectTableCreateInfoNVX
  { -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "objectCount"
  vkObjectCount :: Word32
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "pObjectEntryTypes"
  vkPObjectEntryTypes :: Ptr VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "pObjectEntryCounts"
  vkPObjectEntryCounts :: Ptr Word32
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "pObjectEntryUsageFlags"
  vkPObjectEntryUsageFlags :: Ptr VkObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "maxUniformBuffersPerDescriptor"
  vkMaxUniformBuffersPerDescriptor :: Word32
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "maxStorageBuffersPerDescriptor"
  vkMaxStorageBuffersPerDescriptor :: Word32
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "maxStorageImagesPerDescriptor"
  vkMaxStorageImagesPerDescriptor :: Word32
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "maxSampledImagesPerDescriptor"
  vkMaxSampledImagesPerDescriptor :: Word32
  , -- No documentation found for Nested "VkObjectTableCreateInfoNVX" "maxPipelineLayouts"
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
  zero = VkObjectTableCreateInfoNVX zero
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
-- No documentation found for TopLevel "VkObjectTableDescriptorSetEntryNVX"
data VkObjectTableDescriptorSetEntryNVX = VkObjectTableDescriptorSetEntryNVX
  { -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "pipelineLayout"
  vkPipelineLayout :: VkPipelineLayout
  , -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "descriptorSet"
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
-- No documentation found for TopLevel "VkObjectTableEntryNVX"
data VkObjectTableEntryNVX = VkObjectTableEntryNVX
  { -- No documentation found for Nested "VkObjectTableEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableEntryNVX" "flags"
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
-- No documentation found for TopLevel "VkObjectTableIndexBufferEntryNVX"
data VkObjectTableIndexBufferEntryNVX = VkObjectTableIndexBufferEntryNVX
  { -- No documentation found for Nested "VkObjectTableIndexBufferEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableIndexBufferEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "VkObjectTableIndexBufferEntryNVX" "buffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkObjectTableIndexBufferEntryNVX" "indexType"
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
-- No documentation found for TopLevel "VkObjectTableNVX"
type VkObjectTableNVX = Ptr VkObjectTableNVX_T
-- No documentation found for TopLevel "VkObjectTablePipelineEntryNVX"
data VkObjectTablePipelineEntryNVX = VkObjectTablePipelineEntryNVX
  { -- No documentation found for Nested "VkObjectTablePipelineEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTablePipelineEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "VkObjectTablePipelineEntryNVX" "pipeline"
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
-- No documentation found for TopLevel "VkObjectTablePushConstantEntryNVX"
data VkObjectTablePushConstantEntryNVX = VkObjectTablePushConstantEntryNVX
  { -- No documentation found for Nested "VkObjectTablePushConstantEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTablePushConstantEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "VkObjectTablePushConstantEntryNVX" "pipelineLayout"
  vkPipelineLayout :: VkPipelineLayout
  , -- No documentation found for Nested "VkObjectTablePushConstantEntryNVX" "stageFlags"
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
-- No documentation found for TopLevel "VkObjectTableVertexBufferEntryNVX"
data VkObjectTableVertexBufferEntryNVX = VkObjectTableVertexBufferEntryNVX
  { -- No documentation found for Nested "VkObjectTableVertexBufferEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableVertexBufferEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- No documentation found for Nested "VkObjectTableVertexBufferEntryNVX" "buffer"
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdProcessCommandsNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdProcessCommandsNVX" vkCmdProcessCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()

#endif
type FN_vkCmdProcessCommandsNVX = ("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()
type PFN_vkCmdProcessCommandsNVX = FunPtr FN_vkCmdProcessCommandsNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdReserveSpaceForCommandsNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdReserveSpaceForCommandsNVX" vkCmdReserveSpaceForCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()

#endif
type FN_vkCmdReserveSpaceForCommandsNVX = ("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()
type PFN_vkCmdReserveSpaceForCommandsNVX = FunPtr FN_vkCmdReserveSpaceForCommandsNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateIndirectCommandsLayoutNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateIndirectCommandsLayoutNVX" vkCreateIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult

#endif
type FN_vkCreateIndirectCommandsLayoutNVX = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult
type PFN_vkCreateIndirectCommandsLayoutNVX = FunPtr FN_vkCreateIndirectCommandsLayoutNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateObjectTableNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateObjectTableNVX" vkCreateObjectTableNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult

#endif
type FN_vkCreateObjectTableNVX = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult
type PFN_vkCreateObjectTableNVX = FunPtr FN_vkCreateObjectTableNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkDestroyIndirectCommandsLayoutNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyIndirectCommandsLayoutNVX" vkDestroyIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyIndirectCommandsLayoutNVX = ("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyIndirectCommandsLayoutNVX = FunPtr FN_vkDestroyIndirectCommandsLayoutNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkDestroyObjectTableNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyObjectTableNVX" vkDestroyObjectTableNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyObjectTableNVX = ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyObjectTableNVX = FunPtr FN_vkDestroyObjectTableNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX" vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()

#endif
type FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()
type PFN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX = FunPtr FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkRegisterObjectsNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkRegisterObjectsNVX" vkRegisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult

#endif
type FN_vkRegisterObjectsNVX = ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
type PFN_vkRegisterObjectsNVX = FunPtr FN_vkRegisterObjectsNVX
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkUnregisterObjectsNVX"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUnregisterObjectsNVX" vkUnregisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult

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
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: (Eq a ,IsString a) => a
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
