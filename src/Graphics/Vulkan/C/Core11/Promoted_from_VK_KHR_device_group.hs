{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkDeviceGroupBindSparseInfo(..)
  , VkDeviceGroupCommandBufferBeginInfo(..)
  , VkDeviceGroupRenderPassBeginInfo(..)
  , VkDeviceGroupSubmitInfo(..)
  , VkMemoryAllocateFlagBits(..)
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , VkMemoryAllocateFlags
  , VkMemoryAllocateFlagsInfo(..)
  , VkPeerMemoryFeatureFlagBits(..)
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , VkPeerMemoryFeatureFlags
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkCmdDispatchBase
#endif
  , FN_vkCmdDispatchBase
  , PFN_vkCmdDispatchBase
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkCmdSetDeviceMask
#endif
  , FN_vkCmdSetDeviceMask
  , PFN_vkCmdSetDeviceMask
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetDeviceGroupPeerMemoryFeatures
#endif
  , FN_vkGetDeviceGroupPeerMemoryFeatures
  , PFN_vkGetDeviceGroupPeerMemoryFeatures
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
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


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkDependencyFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipelineCreateFlagBits(..)
  , VkRect2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDeviceGroupBindSparseInfo"
data VkDeviceGroupBindSparseInfo = VkDeviceGroupBindSparseInfo
  { -- No documentation found for Nested "VkDeviceGroupBindSparseInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGroupBindSparseInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGroupBindSparseInfo" "resourceDeviceIndex"
  vkResourceDeviceIndex :: Word32
  , -- No documentation found for Nested "VkDeviceGroupBindSparseInfo" "memoryDeviceIndex"
  vkMemoryDeviceIndex :: Word32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupBindSparseInfo))
                *> poke (ptr `plusPtr` 16) (vkResourceDeviceIndex (poked :: VkDeviceGroupBindSparseInfo))
                *> poke (ptr `plusPtr` 20) (vkMemoryDeviceIndex (poked :: VkDeviceGroupBindSparseInfo))
-- No documentation found for TopLevel "VkDeviceGroupCommandBufferBeginInfo"
data VkDeviceGroupCommandBufferBeginInfo = VkDeviceGroupCommandBufferBeginInfo
  { -- No documentation found for Nested "VkDeviceGroupCommandBufferBeginInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGroupCommandBufferBeginInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGroupCommandBufferBeginInfo" "deviceMask"
  vkDeviceMask :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupCommandBufferBeginInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupCommandBufferBeginInfo <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceMask (poked :: VkDeviceGroupCommandBufferBeginInfo))
-- No documentation found for TopLevel "VkDeviceGroupRenderPassBeginInfo"
data VkDeviceGroupRenderPassBeginInfo = VkDeviceGroupRenderPassBeginInfo
  { -- No documentation found for Nested "VkDeviceGroupRenderPassBeginInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGroupRenderPassBeginInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGroupRenderPassBeginInfo" "deviceMask"
  vkDeviceMask :: Word32
  , -- No documentation found for Nested "VkDeviceGroupRenderPassBeginInfo" "deviceRenderAreaCount"
  vkDeviceRenderAreaCount :: Word32
  , -- No documentation found for Nested "VkDeviceGroupRenderPassBeginInfo" "pDeviceRenderAreas"
  vkPDeviceRenderAreas :: Ptr VkRect2D
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceMask (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 20) (vkDeviceRenderAreaCount (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkPDeviceRenderAreas (poked :: VkDeviceGroupRenderPassBeginInfo))
-- No documentation found for TopLevel "VkDeviceGroupSubmitInfo"
data VkDeviceGroupSubmitInfo = VkDeviceGroupSubmitInfo
  { -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "waitSemaphoreCount"
  vkWaitSemaphoreCount :: Word32
  , -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "pWaitSemaphoreDeviceIndices"
  vkPWaitSemaphoreDeviceIndices :: Ptr Word32
  , -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "commandBufferCount"
  vkCommandBufferCount :: Word32
  , -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "pCommandBufferDeviceMasks"
  vkPCommandBufferDeviceMasks :: Ptr Word32
  , -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "signalSemaphoreCount"
  vkSignalSemaphoreCount :: Word32
  , -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "pSignalSemaphoreDeviceIndices"
  vkPSignalSemaphoreDeviceIndices :: Ptr Word32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphoreDeviceIndices (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 32) (vkCommandBufferCount (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 40) (vkPCommandBufferDeviceMasks (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 48) (vkSignalSemaphoreCount (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 56) (vkPSignalSemaphoreDeviceIndices (poked :: VkDeviceGroupSubmitInfo))
-- ** VkMemoryAllocateFlagBits

-- No documentation found for TopLevel "VkMemoryAllocateFlagBits"
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

-- No documentation found for Nested "VkMemoryAllocateFlagBits" "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT"
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT :: VkMemoryAllocateFlagBits
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT = VkMemoryAllocateFlagBits 0x00000001
-- No documentation found for TopLevel "VkMemoryAllocateFlags"
type VkMemoryAllocateFlags = VkMemoryAllocateFlagBits
-- No documentation found for TopLevel "VkMemoryAllocateFlagsInfo"
data VkMemoryAllocateFlagsInfo = VkMemoryAllocateFlagsInfo
  { -- No documentation found for Nested "VkMemoryAllocateFlagsInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryAllocateFlagsInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryAllocateFlagsInfo" "flags"
  vkFlags :: VkMemoryAllocateFlags
  , -- No documentation found for Nested "VkMemoryAllocateFlagsInfo" "deviceMask"
  vkDeviceMask :: Word32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryAllocateFlagsInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMemoryAllocateFlagsInfo))
                *> poke (ptr `plusPtr` 20) (vkDeviceMask (poked :: VkMemoryAllocateFlagsInfo))
-- ** VkPeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagBits"
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

-- No documentation found for Nested "VkPeerMemoryFeatureFlagBits" "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT"
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT = VkPeerMemoryFeatureFlagBits 0x00000001

-- No documentation found for Nested "VkPeerMemoryFeatureFlagBits" "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT"
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT = VkPeerMemoryFeatureFlagBits 0x00000002

-- No documentation found for Nested "VkPeerMemoryFeatureFlagBits" "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT"
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = VkPeerMemoryFeatureFlagBits 0x00000004

-- No documentation found for Nested "VkPeerMemoryFeatureFlagBits" "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT"
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT = VkPeerMemoryFeatureFlagBits 0x00000008
-- No documentation found for TopLevel "VkPeerMemoryFeatureFlags"
type VkPeerMemoryFeatureFlags = VkPeerMemoryFeatureFlagBits
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkCmdDispatchBase"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDispatchBase" vkCmdDispatchBase :: ("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()

#endif
type FN_vkCmdDispatchBase = ("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
type PFN_vkCmdDispatchBase = FunPtr FN_vkCmdDispatchBase
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetDeviceMask"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetDeviceMask" vkCmdSetDeviceMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()

#endif
type FN_vkCmdSetDeviceMask = ("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()
type PFN_vkCmdSetDeviceMask = FunPtr FN_vkCmdSetDeviceMask
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetDeviceGroupPeerMemoryFeatures"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceGroupPeerMemoryFeatures" vkGetDeviceGroupPeerMemoryFeatures :: ("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()

#endif
type FN_vkGetDeviceGroupPeerMemoryFeatures = ("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()
type PFN_vkGetDeviceGroupPeerMemoryFeatures = FunPtr FN_vkGetDeviceGroupPeerMemoryFeatures
-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_DEVICE_GROUP_BIT"
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT = VkDependencyFlagBits 0x00000004
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DISPATCH_BASE"
pattern VK_PIPELINE_CREATE_DISPATCH_BASE :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_DISPATCH_BASE = VkPipelineCreateFlagBits 0x00000010
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT = VkPipelineCreateFlagBits 0x00000008
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO = VkStructureType 1000060006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO = VkStructureType 1000060004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO = VkStructureType 1000060003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO = VkStructureType 1000060005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO"
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO = VkStructureType 1000060000
