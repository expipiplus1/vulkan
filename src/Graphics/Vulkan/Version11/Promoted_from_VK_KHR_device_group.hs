{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version11.Promoted_from_VK_KHR_device_group
  ( VkPeerMemoryFeatureFlagBits(..)
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , VkMemoryAllocateFlagBits(..)
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  , vkGetDeviceGroupPeerMemoryFeatures
  , vkCmdSetDeviceMask
  , vkCmdDispatchBase
  , VkMemoryAllocateFlagsInfo(..)
  , VkDeviceGroupRenderPassBeginInfo(..)
  , VkDeviceGroupCommandBufferBeginInfo(..)
  , VkDeviceGroupSubmitInfo(..)
  , VkDeviceGroupBindSparseInfo(..)
  , VkPeerMemoryFeatureFlags
  , VkMemoryAllocateFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
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


import Graphics.Vulkan.Version10.Core
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Version10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Version10.Pass
  ( VkDependencyFlagBits(..)
  )
import Graphics.Vulkan.Version10.Pipeline
  ( VkRect2D(..)
  , VkPipelineCreateFlagBits(..)
  )
import Graphics.Vulkan.Version10.Queue
  ( VkCommandBuffer
  )


-- ** VkPeerMemoryFeatureFlagBits

-- | 
newtype VkPeerMemoryFeatureFlagBits = VkPeerMemoryFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPeerMemoryFeatureFlagBits where
  showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT = showString "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT"
  showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_DST_BIT = showString "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT"
  showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = showString "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT"
  showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT = showString "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT"
  showsPrec p (VkPeerMemoryFeatureFlagBits x) = showParen (p >= 11) (showString "VkPeerMemoryFeatureFlagBits " . showsPrec 11 x)

instance Read VkPeerMemoryFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT",    pure VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT)
                             , ("VK_PEER_MEMORY_FEATURE_COPY_DST_BIT",    pure VK_PEER_MEMORY_FEATURE_COPY_DST_BIT)
                             , ("VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT", pure VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT)
                             , ("VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT", pure VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPeerMemoryFeatureFlagBits")
                        v <- step readPrec
                        pure (VkPeerMemoryFeatureFlagBits v)
                        )
                    )

-- | Can read with vkCmdCopy commands
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT = VkPeerMemoryFeatureFlagBits 0x00000001

-- | Can write with vkCmdCopy commands
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT = VkPeerMemoryFeatureFlagBits 0x00000002

-- | Can read with any access type/command
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = VkPeerMemoryFeatureFlagBits 0x00000004

-- | Can write with and access type/command
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT = VkPeerMemoryFeatureFlagBits 0x00000008
-- ** VkMemoryAllocateFlagBits

-- | 
newtype VkMemoryAllocateFlagBits = VkMemoryAllocateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkMemoryAllocateFlagBits where
  showsPrec _ VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT = showString "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT"
  showsPrec p (VkMemoryAllocateFlagBits x) = showParen (p >= 11) (showString "VkMemoryAllocateFlagBits " . showsPrec 11 x)

instance Read VkMemoryAllocateFlagBits where
  readPrec = parens ( choose [ ("VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT", pure VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryAllocateFlagBits")
                        v <- step readPrec
                        pure (VkMemoryAllocateFlagBits v)
                        )
                    )

-- | Force allocation on specific devices
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT :: VkMemoryAllocateFlagBits
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT = VkMemoryAllocateFlagBits 0x00000001
-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO = VkStructureType 1000060000
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO = VkStructureType 1000060003
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO = VkStructureType 1000060004
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO = VkStructureType 1000060005
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO = VkStructureType 1000060006
-- | Nothing
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT = VkPipelineCreateFlagBits 0x00000008
-- | Nothing
pattern VK_PIPELINE_CREATE_DISPATCH_BASE :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_DISPATCH_BASE = VkPipelineCreateFlagBits 0x00000010
-- | Just "Dependency is across devices"
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT = VkDependencyFlagBits 0x00000004
-- | 
foreign import ccall "vkGetDeviceGroupPeerMemoryFeatures" vkGetDeviceGroupPeerMemoryFeatures :: ("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()
-- | 
foreign import ccall "vkCmdSetDeviceMask" vkCmdSetDeviceMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdDispatchBase" vkCmdDispatchBase :: ("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
-- | TODO: Struct comments
data VkMemoryAllocateFlagsInfo = VkMemoryAllocateFlagsInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkMemoryAllocateFlags
  , vkDeviceMask :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryAllocateFlagsInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryAllocateFlagsInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryAllocateFlagsInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMemoryAllocateFlagsInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMemoryAllocateFlagsInfo))
                *> poke (ptr `plusPtr` 20) (vkDeviceMask (poked :: VkMemoryAllocateFlagsInfo))
-- | TODO: Struct comments
data VkDeviceGroupRenderPassBeginInfo = VkDeviceGroupRenderPassBeginInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkDeviceMask :: Word32
  , vkDeviceRenderAreaCount :: Word32
  , vkDeviceRenderAreas :: Ptr VkRect2D
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupRenderPassBeginInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDeviceGroupRenderPassBeginInfo <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceMask (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 20) (vkDeviceRenderAreaCount (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkDeviceRenderAreas (poked :: VkDeviceGroupRenderPassBeginInfo))
-- | TODO: Struct comments
data VkDeviceGroupCommandBufferBeginInfo = VkDeviceGroupCommandBufferBeginInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkDeviceMask :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupCommandBufferBeginInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupCommandBufferBeginInfo <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDeviceGroupCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceMask (poked :: VkDeviceGroupCommandBufferBeginInfo))
-- | TODO: Struct comments
data VkDeviceGroupSubmitInfo = VkDeviceGroupSubmitInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkWaitSemaphoreCount :: Word32
  , vkWaitSemaphoreDeviceIndices :: Ptr Word32
  , vkCommandBufferCount :: Word32
  , vkCommandBufferDeviceMasks :: Ptr Word32
  , vkSignalSemaphoreCount :: Word32
  , vkSignalSemaphoreDeviceIndices :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupSubmitInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkDeviceGroupSubmitInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 40)
                                     <*> peek (ptr `plusPtr` 48)
                                     <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 24) (vkWaitSemaphoreDeviceIndices (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 32) (vkCommandBufferCount (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 40) (vkCommandBufferDeviceMasks (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 48) (vkSignalSemaphoreCount (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 56) (vkSignalSemaphoreDeviceIndices (poked :: VkDeviceGroupSubmitInfo))
-- | TODO: Struct comments
data VkDeviceGroupBindSparseInfo = VkDeviceGroupBindSparseInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkResourceDeviceIndex :: Word32
  , vkMemoryDeviceIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupBindSparseInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupBindSparseInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupBindSparseInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDeviceGroupBindSparseInfo))
                *> poke (ptr `plusPtr` 16) (vkResourceDeviceIndex (poked :: VkDeviceGroupBindSparseInfo))
                *> poke (ptr `plusPtr` 20) (vkMemoryDeviceIndex (poked :: VkDeviceGroupBindSparseInfo))
type VkPeerMemoryFeatureFlags = VkPeerMemoryFeatureFlagBits
type VkMemoryAllocateFlags = VkMemoryAllocateFlagBits
