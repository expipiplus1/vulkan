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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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
  , VkResult(..)
  , VkObjectType(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkPhysicalDevice
  , VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.Core10.Pass
  ( VkPipelineBindPoint(..)
  , VkAccessFlagBits(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkPipelineLayout
  , VkPipeline
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  , VkPipelineStageFlagBits(..)
  )


-- ** VkIndirectCommandsTokenTypeNVX

-- | 
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

-- | 
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX = VkIndirectCommandsTokenTypeNVX 0

-- | 
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX = VkIndirectCommandsTokenTypeNVX 1

-- | 
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX = VkIndirectCommandsTokenTypeNVX 2

-- | 
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX = VkIndirectCommandsTokenTypeNVX 3

-- | 
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX = VkIndirectCommandsTokenTypeNVX 4

-- | 
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX = VkIndirectCommandsTokenTypeNVX 5

-- | 
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX = VkIndirectCommandsTokenTypeNVX 6

-- | 
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX = VkIndirectCommandsTokenTypeNVX 7
-- ** VkObjectEntryTypeNVX

-- | 
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

-- | 
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = VkObjectEntryTypeNVX 0

-- | 
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = VkObjectEntryTypeNVX 1

-- | 
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = VkObjectEntryTypeNVX 2

-- | 
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = VkObjectEntryTypeNVX 3

-- | 
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = VkObjectEntryTypeNVX 4
-- ** VkIndirectCommandsLayoutUsageFlagBitsNVX

-- | 
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

-- | 
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000001

-- | 
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000002

-- | 
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000004

-- | 
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000008
-- ** VkObjectEntryUsageFlagBitsNVX

-- | 
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

-- | 
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000001

-- | 
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000002
-- | Nothing
pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX = VkStructureType 1000086000
-- | Nothing
pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX = VkStructureType 1000086001
-- | Nothing
pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX = VkStructureType 1000086002
-- | Nothing
pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX = VkStructureType 1000086003
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX = VkStructureType 1000086004
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX = VkStructureType 1000086005
-- | Just "VkobjectTableNVX"
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX :: VkObjectType
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX = VkObjectType 1000086000
-- | Just "VkIndirectCommandsLayoutNVX"
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX :: VkObjectType
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX = VkObjectType 1000086001
-- | Nothing
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX :: VkAccessFlagBits
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX = VkAccessFlagBits 0x00020000
-- | Nothing
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX :: VkAccessFlagBits
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX = VkAccessFlagBits 0x00040000
-- | Nothing
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX = VkPipelineStageFlagBits 0x00020000
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: Integral a => a
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NVX_device_generated_commands"
-- |
data VkObjectTableNVX_T
type VkObjectTableNVX = Ptr VkObjectTableNVX_T
-- |
data VkIndirectCommandsLayoutNVX_T
type VkIndirectCommandsLayoutNVX = Ptr VkIndirectCommandsLayoutNVX_T
-- | 
foreign import ccall "vkCmdProcessCommandsNVX" vkCmdProcessCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()
-- | 
foreign import ccall "vkCmdReserveSpaceForCommandsNVX" vkCmdReserveSpaceForCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()
-- | 
foreign import ccall "vkCreateIndirectCommandsLayoutNVX" vkCreateIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult
-- | 
foreign import ccall "vkDestroyIndirectCommandsLayoutNVX" vkDestroyIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkCreateObjectTableNVX" vkCreateObjectTableNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult
-- | 
foreign import ccall "vkDestroyObjectTableNVX" vkDestroyObjectTableNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkRegisterObjectsNVX" vkRegisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
-- | 
foreign import ccall "vkUnregisterObjectsNVX" vkUnregisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
-- | 
foreign import ccall "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX" vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()
-- | TODO: Struct comments
data VkDeviceGeneratedCommandsFeaturesNVX = VkDeviceGeneratedCommandsFeaturesNVX
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkComputeBindingPointSupport :: VkBool32
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
-- | TODO: Struct comments
data VkDeviceGeneratedCommandsLimitsNVX = VkDeviceGeneratedCommandsLimitsNVX
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkMaxIndirectCommandsLayoutTokenCount :: Word32
  , vkMaxObjectEntryCounts :: Word32
  , vkMinSequenceCountBufferOffsetAlignment :: Word32
  , vkMinSequenceIndexBufferOffsetAlignment :: Word32
  , vkMinCommandsTokenBufferOffsetAlignment :: Word32
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
-- | TODO: Struct comments
data VkIndirectCommandsTokenNVX = VkIndirectCommandsTokenNVX
  { vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , vkBuffer :: VkBuffer
  , vkOffset :: VkDeviceSize
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
-- | TODO: Struct comments
data VkIndirectCommandsLayoutTokenNVX = VkIndirectCommandsLayoutTokenNVX
  { vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , vkBindingUnit :: Word32
  , vkDynamicCount :: Word32
  , vkDivisor :: Word32
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
-- | TODO: Struct comments
data VkIndirectCommandsLayoutCreateInfoNVX = VkIndirectCommandsLayoutCreateInfoNVX
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkPipelineBindPoint :: VkPipelineBindPoint
  , vkFlags :: VkIndirectCommandsLayoutUsageFlagsNVX
  , vkTokenCount :: Word32
  , vkPTokens :: Ptr VkIndirectCommandsLayoutTokenNVX
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
-- | TODO: Struct comments
data VkCmdProcessCommandsInfoNVX = VkCmdProcessCommandsInfoNVX
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkObjectTable :: VkObjectTableNVX
  , vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX
  , vkIndirectCommandsTokenCount :: Word32
  , vkPIndirectCommandsTokens :: Ptr VkIndirectCommandsTokenNVX
  , vkMaxSequencesCount :: Word32
  , vkTargetCommandBuffer :: VkCommandBuffer
  , vkSequencesCountBuffer :: VkBuffer
  , vkSequencesCountOffset :: VkDeviceSize
  , vkSequencesIndexBuffer :: VkBuffer
  , vkSequencesIndexOffset :: VkDeviceSize
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
-- | TODO: Struct comments
data VkCmdReserveSpaceForCommandsInfoNVX = VkCmdReserveSpaceForCommandsInfoNVX
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkObjectTable :: VkObjectTableNVX
  , vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX
  , vkMaxSequencesCount :: Word32
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
-- | TODO: Struct comments
data VkObjectTableCreateInfoNVX = VkObjectTableCreateInfoNVX
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkObjectCount :: Word32
  , vkPObjectEntryTypes :: Ptr VkObjectEntryTypeNVX
  , vkPObjectEntryCounts :: Ptr Word32
  , vkPObjectEntryUsageFlags :: Ptr VkObjectEntryUsageFlagsNVX
  , vkMaxUniformBuffersPerDescriptor :: Word32
  , vkMaxStorageBuffersPerDescriptor :: Word32
  , vkMaxStorageImagesPerDescriptor :: Word32
  , vkMaxSampledImagesPerDescriptor :: Word32
  , vkMaxPipelineLayouts :: Word32
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
-- | TODO: Struct comments
data VkObjectTableEntryNVX = VkObjectTableEntryNVX
  { vkType :: VkObjectEntryTypeNVX
  , vkFlags :: VkObjectEntryUsageFlagsNVX
  }
  deriving (Eq, Show)

instance Storable VkObjectTableEntryNVX where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkObjectTableEntryNVX <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableEntryNVX))
-- | TODO: Struct comments
data VkObjectTablePipelineEntryNVX = VkObjectTablePipelineEntryNVX
  { vkType :: VkObjectEntryTypeNVX
  , vkFlags :: VkObjectEntryUsageFlagsNVX
  , vkPipeline :: VkPipeline
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
-- | TODO: Struct comments
data VkObjectTableDescriptorSetEntryNVX = VkObjectTableDescriptorSetEntryNVX
  { vkType :: VkObjectEntryTypeNVX
  , vkFlags :: VkObjectEntryUsageFlagsNVX
  , vkPipelineLayout :: VkPipelineLayout
  , vkDescriptorSet :: VkDescriptorSet
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
-- | TODO: Struct comments
data VkObjectTableVertexBufferEntryNVX = VkObjectTableVertexBufferEntryNVX
  { vkType :: VkObjectEntryTypeNVX
  , vkFlags :: VkObjectEntryUsageFlagsNVX
  , vkBuffer :: VkBuffer
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
-- | TODO: Struct comments
data VkObjectTableIndexBufferEntryNVX = VkObjectTableIndexBufferEntryNVX
  { vkType :: VkObjectEntryTypeNVX
  , vkFlags :: VkObjectEntryUsageFlagsNVX
  , vkBuffer :: VkBuffer
  , vkIndexType :: VkIndexType
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
-- | TODO: Struct comments
data VkObjectTablePushConstantEntryNVX = VkObjectTablePushConstantEntryNVX
  { vkType :: VkObjectEntryTypeNVX
  , vkFlags :: VkObjectEntryUsageFlagsNVX
  , vkPipelineLayout :: VkPipelineLayout
  , vkStageFlags :: VkShaderStageFlags
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
type VkIndirectCommandsLayoutUsageFlagsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX
type VkObjectEntryUsageFlagsNVX = VkObjectEntryUsageFlagBitsNVX
