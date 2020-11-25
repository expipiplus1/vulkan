{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_device_group"
module Vulkan.Core11.Promoted_From_VK_KHR_device_group  ( getDeviceGroupPeerMemoryFeatures
                                                        , cmdSetDeviceMask
                                                        , cmdDispatchBase
                                                        , pattern PIPELINE_CREATE_DISPATCH_BASE
                                                        , MemoryAllocateFlagsInfo(..)
                                                        , DeviceGroupRenderPassBeginInfo(..)
                                                        , DeviceGroupCommandBufferBeginInfo(..)
                                                        , DeviceGroupSubmitInfo(..)
                                                        , DeviceGroupBindSparseInfo(..)
                                                        , StructureType(..)
                                                        , PipelineCreateFlagBits(..)
                                                        , PipelineCreateFlags
                                                        , DependencyFlagBits(..)
                                                        , DependencyFlags
                                                        , PeerMemoryFeatureFlagBits(..)
                                                        , PeerMemoryFeatureFlags
                                                        , MemoryAllocateFlagBits(..)
                                                        , MemoryAllocateFlags
                                                        ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchBase))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDeviceMask))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceGroupPeerMemoryFeatures))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(..))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_DISPATCH_BASE_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(..))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits(..))
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(..))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupPeerMemoryFeatures
  :: FunPtr (Ptr Device_T -> Word32 -> Word32 -> Word32 -> Ptr PeerMemoryFeatureFlags -> IO ()) -> Ptr Device_T -> Word32 -> Word32 -> Word32 -> Ptr PeerMemoryFeatureFlags -> IO ()

-- No documentation found for TopLevel "vkGetDeviceGroupPeerMemoryFeatures"
getDeviceGroupPeerMemoryFeatures :: forall io
                                  . (MonadIO io)
                                 => -- No documentation found for Nested "vkGetDeviceGroupPeerMemoryFeatures" "device"
                                    Device
                                 -> -- No documentation found for Nested "vkGetDeviceGroupPeerMemoryFeatures" "heapIndex"
                                    ("heapIndex" ::: Word32)
                                 -> -- No documentation found for Nested "vkGetDeviceGroupPeerMemoryFeatures" "localDeviceIndex"
                                    ("localDeviceIndex" ::: Word32)
                                 -> -- No documentation found for Nested "vkGetDeviceGroupPeerMemoryFeatures" "remoteDeviceIndex"
                                    ("remoteDeviceIndex" ::: Word32)
                                 -> io (("peerMemoryFeatures" ::: PeerMemoryFeatureFlags))
getDeviceGroupPeerMemoryFeatures device heapIndex localDeviceIndex remoteDeviceIndex = liftIO . evalContT $ do
  let vkGetDeviceGroupPeerMemoryFeaturesPtr = pVkGetDeviceGroupPeerMemoryFeatures (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceGroupPeerMemoryFeaturesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceGroupPeerMemoryFeatures is null" Nothing Nothing
  let vkGetDeviceGroupPeerMemoryFeatures' = mkVkGetDeviceGroupPeerMemoryFeatures vkGetDeviceGroupPeerMemoryFeaturesPtr
  pPPeerMemoryFeatures <- ContT $ bracket (callocBytes @PeerMemoryFeatureFlags 4) free
  lift $ vkGetDeviceGroupPeerMemoryFeatures' (deviceHandle (device)) (heapIndex) (localDeviceIndex) (remoteDeviceIndex) (pPPeerMemoryFeatures)
  pPeerMemoryFeatures <- lift $ peek @PeerMemoryFeatureFlags pPPeerMemoryFeatures
  pure $ (pPeerMemoryFeatures)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDeviceMask
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdSetDeviceMask"
cmdSetDeviceMask :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkCmdSetDeviceMask" "commandBuffer"
                    CommandBuffer
                 -> -- No documentation found for Nested "vkCmdSetDeviceMask" "deviceMask"
                    ("deviceMask" ::: Word32)
                 -> io ()
cmdSetDeviceMask commandBuffer deviceMask = liftIO $ do
  let vkCmdSetDeviceMaskPtr = pVkCmdSetDeviceMask (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDeviceMaskPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDeviceMask is null" Nothing Nothing
  let vkCmdSetDeviceMask' = mkVkCmdSetDeviceMask vkCmdSetDeviceMaskPtr
  vkCmdSetDeviceMask' (commandBufferHandle (commandBuffer)) (deviceMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchBase
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDispatchBase"
cmdDispatchBase :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkCmdDispatchBase" "commandBuffer"
                   CommandBuffer
                -> -- No documentation found for Nested "vkCmdDispatchBase" "baseGroupX"
                   ("baseGroupX" ::: Word32)
                -> -- No documentation found for Nested "vkCmdDispatchBase" "baseGroupY"
                   ("baseGroupY" ::: Word32)
                -> -- No documentation found for Nested "vkCmdDispatchBase" "baseGroupZ"
                   ("baseGroupZ" ::: Word32)
                -> -- No documentation found for Nested "vkCmdDispatchBase" "groupCountX"
                   ("groupCountX" ::: Word32)
                -> -- No documentation found for Nested "vkCmdDispatchBase" "groupCountY"
                   ("groupCountY" ::: Word32)
                -> -- No documentation found for Nested "vkCmdDispatchBase" "groupCountZ"
                   ("groupCountZ" ::: Word32)
                -> io ()
cmdDispatchBase commandBuffer baseGroupX baseGroupY baseGroupZ groupCountX groupCountY groupCountZ = liftIO $ do
  let vkCmdDispatchBasePtr = pVkCmdDispatchBase (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDispatchBasePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchBase is null" Nothing Nothing
  let vkCmdDispatchBase' = mkVkCmdDispatchBase vkCmdDispatchBasePtr
  vkCmdDispatchBase' (commandBufferHandle (commandBuffer)) (baseGroupX) (baseGroupY) (baseGroupZ) (groupCountX) (groupCountY) (groupCountZ)
  pure $ ()


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_DISPATCH_BASE"
pattern PIPELINE_CREATE_DISPATCH_BASE = PIPELINE_CREATE_DISPATCH_BASE_BIT



-- No documentation found for TopLevel "VkMemoryAllocateFlagsInfo"
data MemoryAllocateFlagsInfo = MemoryAllocateFlagsInfo
  { -- No documentation found for Nested "VkMemoryAllocateFlagsInfo" "flags"
    flags :: MemoryAllocateFlags
  , -- No documentation found for Nested "VkMemoryAllocateFlagsInfo" "deviceMask"
    deviceMask :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryAllocateFlagsInfo)
#endif
deriving instance Show MemoryAllocateFlagsInfo

instance ToCStruct MemoryAllocateFlagsInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryAllocateFlagsInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryAllocateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (deviceMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryAllocateFlagsInfo where
  peekCStruct p = do
    flags <- peek @MemoryAllocateFlags ((p `plusPtr` 16 :: Ptr MemoryAllocateFlags))
    deviceMask <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ MemoryAllocateFlagsInfo
             flags deviceMask


instance Storable MemoryAllocateFlagsInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryAllocateFlagsInfo where
  zero = MemoryAllocateFlagsInfo
           zero
           zero



-- No documentation found for TopLevel "VkDeviceGroupRenderPassBeginInfo"
data DeviceGroupRenderPassBeginInfo = DeviceGroupRenderPassBeginInfo
  { -- No documentation found for Nested "VkDeviceGroupRenderPassBeginInfo" "deviceMask"
    deviceMask :: Word32
  , -- No documentation found for Nested "VkDeviceGroupRenderPassBeginInfo" "pDeviceRenderAreas"
    deviceRenderAreas :: Vector Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupRenderPassBeginInfo)
#endif
deriving instance Show DeviceGroupRenderPassBeginInfo

instance ToCStruct DeviceGroupRenderPassBeginInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupRenderPassBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (deviceMask)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceRenderAreas)) :: Word32))
    pPDeviceRenderAreas' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (deviceRenderAreas)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceRenderAreas' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (deviceRenderAreas)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPDeviceRenderAreas')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    pPDeviceRenderAreas' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (mempty)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceRenderAreas' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPDeviceRenderAreas')
    lift $ f

instance FromCStruct DeviceGroupRenderPassBeginInfo where
  peekCStruct p = do
    deviceMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    deviceRenderAreaCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pDeviceRenderAreas <- peek @(Ptr Rect2D) ((p `plusPtr` 24 :: Ptr (Ptr Rect2D)))
    pDeviceRenderAreas' <- generateM (fromIntegral deviceRenderAreaCount) (\i -> peekCStruct @Rect2D ((pDeviceRenderAreas `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ DeviceGroupRenderPassBeginInfo
             deviceMask pDeviceRenderAreas'

instance Zero DeviceGroupRenderPassBeginInfo where
  zero = DeviceGroupRenderPassBeginInfo
           zero
           mempty



-- No documentation found for TopLevel "VkDeviceGroupCommandBufferBeginInfo"
data DeviceGroupCommandBufferBeginInfo = DeviceGroupCommandBufferBeginInfo
  { -- No documentation found for Nested "VkDeviceGroupCommandBufferBeginInfo" "deviceMask"
    deviceMask :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupCommandBufferBeginInfo)
#endif
deriving instance Show DeviceGroupCommandBufferBeginInfo

instance ToCStruct DeviceGroupCommandBufferBeginInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupCommandBufferBeginInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (deviceMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceGroupCommandBufferBeginInfo where
  peekCStruct p = do
    deviceMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DeviceGroupCommandBufferBeginInfo
             deviceMask


instance Storable DeviceGroupCommandBufferBeginInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGroupCommandBufferBeginInfo where
  zero = DeviceGroupCommandBufferBeginInfo
           zero



-- No documentation found for TopLevel "VkDeviceGroupSubmitInfo"
data DeviceGroupSubmitInfo = DeviceGroupSubmitInfo
  { -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "pWaitSemaphoreDeviceIndices"
    waitSemaphoreDeviceIndices :: Vector Word32
  , -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "pCommandBufferDeviceMasks"
    commandBufferDeviceMasks :: Vector Word32
  , -- No documentation found for Nested "VkDeviceGroupSubmitInfo" "pSignalSemaphoreDeviceIndices"
    signalSemaphoreDeviceIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupSubmitInfo)
#endif
deriving instance Show DeviceGroupSubmitInfo

instance ToCStruct DeviceGroupSubmitInfo where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupSubmitInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (waitSemaphoreDeviceIndices)) :: Word32))
    pPWaitSemaphoreDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (waitSemaphoreDeviceIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphoreDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (waitSemaphoreDeviceIndices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPWaitSemaphoreDeviceIndices')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (commandBufferDeviceMasks)) :: Word32))
    pPCommandBufferDeviceMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (commandBufferDeviceMasks)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBufferDeviceMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (commandBufferDeviceMasks)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPCommandBufferDeviceMasks')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (signalSemaphoreDeviceIndices)) :: Word32))
    pPSignalSemaphoreDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (signalSemaphoreDeviceIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphoreDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (signalSemaphoreDeviceIndices)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Word32))) (pPSignalSemaphoreDeviceIndices')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPWaitSemaphoreDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphoreDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPWaitSemaphoreDeviceIndices')
    pPCommandBufferDeviceMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBufferDeviceMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPCommandBufferDeviceMasks')
    pPSignalSemaphoreDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphoreDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Word32))) (pPSignalSemaphoreDeviceIndices')
    lift $ f

instance FromCStruct DeviceGroupSubmitInfo where
  peekCStruct p = do
    waitSemaphoreCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pWaitSemaphoreDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pWaitSemaphoreDeviceIndices' <- generateM (fromIntegral waitSemaphoreCount) (\i -> peek @Word32 ((pWaitSemaphoreDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    commandBufferCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pCommandBufferDeviceMasks <- peek @(Ptr Word32) ((p `plusPtr` 40 :: Ptr (Ptr Word32)))
    pCommandBufferDeviceMasks' <- generateM (fromIntegral commandBufferCount) (\i -> peek @Word32 ((pCommandBufferDeviceMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    signalSemaphoreCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pSignalSemaphoreDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 56 :: Ptr (Ptr Word32)))
    pSignalSemaphoreDeviceIndices' <- generateM (fromIntegral signalSemaphoreCount) (\i -> peek @Word32 ((pSignalSemaphoreDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ DeviceGroupSubmitInfo
             pWaitSemaphoreDeviceIndices' pCommandBufferDeviceMasks' pSignalSemaphoreDeviceIndices'

instance Zero DeviceGroupSubmitInfo where
  zero = DeviceGroupSubmitInfo
           mempty
           mempty
           mempty



-- No documentation found for TopLevel "VkDeviceGroupBindSparseInfo"
data DeviceGroupBindSparseInfo = DeviceGroupBindSparseInfo
  { -- No documentation found for Nested "VkDeviceGroupBindSparseInfo" "resourceDeviceIndex"
    resourceDeviceIndex :: Word32
  , -- No documentation found for Nested "VkDeviceGroupBindSparseInfo" "memoryDeviceIndex"
    memoryDeviceIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupBindSparseInfo)
#endif
deriving instance Show DeviceGroupBindSparseInfo

instance ToCStruct DeviceGroupBindSparseInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupBindSparseInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (resourceDeviceIndex)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (memoryDeviceIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceGroupBindSparseInfo where
  peekCStruct p = do
    resourceDeviceIndex <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    memoryDeviceIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ DeviceGroupBindSparseInfo
             resourceDeviceIndex memoryDeviceIndex


instance Storable DeviceGroupBindSparseInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGroupBindSparseInfo where
  zero = DeviceGroupBindSparseInfo
           zero
           zero

