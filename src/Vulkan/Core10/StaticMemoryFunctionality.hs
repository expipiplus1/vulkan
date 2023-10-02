{-# language CPP #-}
-- No documentation found for Chapter "StaticMemoryFunctionality"
module Vulkan.Core10.StaticMemoryFunctionality  ( getCommandPoolMemoryConsumption
                                                , PipelinePoolSize(..)
                                                , DeviceObjectReservationCreateInfo(..)
                                                , CommandPoolMemoryReservationCreateInfo(..)
                                                , CommandPoolMemoryConsumption(..)
                                                , StructureType(..)
                                                ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
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
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (CommandPool)
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetCommandPoolMemoryConsumption))
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_sci_sync2 (DeviceSemaphoreSciSyncPoolReservationCreateInfoNV)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PerformanceQueryReservationInfoKHR)
import Vulkan.Core10.PipelineCache (PipelineCacheCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_POOL_MEMORY_CONSUMPTION))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_POOL_MEMORY_RESERVATION_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_OBJECT_RESERVATION_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_POOL_SIZE))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetCommandPoolMemoryConsumption
  :: FunPtr (Ptr Device_T -> CommandPool -> Ptr CommandBuffer_T -> Ptr CommandPoolMemoryConsumption -> IO ()) -> Ptr Device_T -> CommandPool -> Ptr CommandBuffer_T -> Ptr CommandPoolMemoryConsumption -> IO ()

-- No documentation found for TopLevel "vkGetCommandPoolMemoryConsumption"
getCommandPoolMemoryConsumption :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkGetCommandPoolMemoryConsumption" "device"
                                   Device
                                -> -- No documentation found for Nested "vkGetCommandPoolMemoryConsumption" "commandPool"
                                   CommandPool
                                -> -- No documentation found for Nested "vkGetCommandPoolMemoryConsumption" "commandBuffer"
                                   CommandBuffer
                                -> io (CommandPoolMemoryConsumption)
getCommandPoolMemoryConsumption device
                                  commandPool
                                  commandBuffer = liftIO . evalContT $ do
  let vkGetCommandPoolMemoryConsumptionPtr = pVkGetCommandPoolMemoryConsumption (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetCommandPoolMemoryConsumptionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetCommandPoolMemoryConsumption is null" Nothing Nothing
  let vkGetCommandPoolMemoryConsumption' = mkVkGetCommandPoolMemoryConsumption vkGetCommandPoolMemoryConsumptionPtr
  pPConsumption <- ContT (withZeroCStruct @CommandPoolMemoryConsumption)
  lift $ traceAroundEvent "vkGetCommandPoolMemoryConsumption" (vkGetCommandPoolMemoryConsumption'
                                                                 (deviceHandle (device))
                                                                 (commandPool)
                                                                 (commandBufferHandle (commandBuffer))
                                                                 (pPConsumption))
  pConsumption <- lift $ peekCStruct @CommandPoolMemoryConsumption pPConsumption
  pure $ (pConsumption)


-- No documentation found for TopLevel "VkPipelinePoolSize"
data PipelinePoolSize = PipelinePoolSize
  { -- No documentation found for Nested "VkPipelinePoolSize" "poolEntrySize"
    poolEntrySize :: DeviceSize
  , -- No documentation found for Nested "VkPipelinePoolSize" "poolEntryCount"
    poolEntryCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelinePoolSize)
#endif
deriving instance Show PipelinePoolSize

instance ToCStruct PipelinePoolSize where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelinePoolSize{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_POOL_SIZE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (poolEntrySize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (poolEntryCount)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_POOL_SIZE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelinePoolSize where
  peekCStruct p = do
    poolEntrySize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    poolEntryCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ PipelinePoolSize
             poolEntrySize poolEntryCount

instance Storable PipelinePoolSize where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelinePoolSize where
  zero = PipelinePoolSize
           zero
           zero


-- No documentation found for TopLevel "VkDeviceObjectReservationCreateInfo"
data DeviceObjectReservationCreateInfo (es :: [Type]) = DeviceObjectReservationCreateInfo
  { -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "pPipelineCacheCreateInfos"
    pipelineCacheCreateInfos :: Vector PipelineCacheCreateInfo
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "pPipelinePoolSizes"
    pipelinePoolSizes :: Vector PipelinePoolSize
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "semaphoreRequestCount"
    semaphoreRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "commandBufferRequestCount"
    commandBufferRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "fenceRequestCount"
    fenceRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "deviceMemoryRequestCount"
    deviceMemoryRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "bufferRequestCount"
    bufferRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "imageRequestCount"
    imageRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "eventRequestCount"
    eventRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "queryPoolRequestCount"
    queryPoolRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "bufferViewRequestCount"
    bufferViewRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "imageViewRequestCount"
    imageViewRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "layeredImageViewRequestCount"
    layeredImageViewRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "pipelineCacheRequestCount"
    pipelineCacheRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "pipelineLayoutRequestCount"
    pipelineLayoutRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "renderPassRequestCount"
    renderPassRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "graphicsPipelineRequestCount"
    graphicsPipelineRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "computePipelineRequestCount"
    computePipelineRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "descriptorSetLayoutRequestCount"
    descriptorSetLayoutRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "samplerRequestCount"
    samplerRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "descriptorPoolRequestCount"
    descriptorPoolRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "descriptorSetRequestCount"
    descriptorSetRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "framebufferRequestCount"
    framebufferRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "commandPoolRequestCount"
    commandPoolRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "samplerYcbcrConversionRequestCount"
    samplerYcbcrConversionRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "surfaceRequestCount"
    surfaceRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "swapchainRequestCount"
    swapchainRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "displayModeRequestCount"
    displayModeRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "subpassDescriptionRequestCount"
    subpassDescriptionRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "attachmentDescriptionRequestCount"
    attachmentDescriptionRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "descriptorSetLayoutBindingRequestCount"
    descriptorSetLayoutBindingRequestCount :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "descriptorSetLayoutBindingLimit"
    descriptorSetLayoutBindingLimit :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "maxImageViewMipLevels"
    maxImageViewMipLevels :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "maxImageViewArrayLayers"
    maxImageViewArrayLayers :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "maxLayeredImageViewMipLevels"
    maxLayeredImageViewMipLevels :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "maxOcclusionQueriesPerPool"
    maxOcclusionQueriesPerPool :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "maxPipelineStatisticsQueriesPerPool"
    maxPipelineStatisticsQueriesPerPool :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "maxTimestampQueriesPerPool"
    maxTimestampQueriesPerPool :: Word32
  , -- No documentation found for Nested "VkDeviceObjectReservationCreateInfo" "maxImmutableSamplersPerDescriptorSetLayout"
    maxImmutableSamplersPerDescriptorSetLayout :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceObjectReservationCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DeviceObjectReservationCreateInfo es)

instance Extensible DeviceObjectReservationCreateInfo where
  extensibleTypeName = "DeviceObjectReservationCreateInfo"
  setNext DeviceObjectReservationCreateInfo{..} next' = DeviceObjectReservationCreateInfo{next = next', ..}
  getNext DeviceObjectReservationCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DeviceObjectReservationCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PerformanceQueryReservationInfoKHR = Just f
    | Just Refl <- eqT @e @DeviceSemaphoreSciSyncPoolReservationCreateInfoNV = Just f
    | otherwise = Nothing

instance ( Extendss DeviceObjectReservationCreateInfo es
         , PokeChain es ) => ToCStruct (DeviceObjectReservationCreateInfo es) where
  withCStruct x f = allocaBytes 200 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceObjectReservationCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_OBJECT_RESERVATION_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pipelineCacheCreateInfos)) :: Word32))
    pPPipelineCacheCreateInfos' <- ContT $ allocaBytes @PipelineCacheCreateInfo ((Data.Vector.length (pipelineCacheCreateInfos)) * 40)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelineCacheCreateInfos' `plusPtr` (40 * (i)) :: Ptr PipelineCacheCreateInfo) (e)) (pipelineCacheCreateInfos)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PipelineCacheCreateInfo))) (pPPipelineCacheCreateInfos')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pipelinePoolSizes)) :: Word32))
    pPPipelinePoolSizes' <- ContT $ allocaBytes @PipelinePoolSize ((Data.Vector.length (pipelinePoolSizes)) * 32)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelinePoolSizes' `plusPtr` (32 * (i)) :: Ptr PipelinePoolSize) (e)) (pipelinePoolSizes)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr PipelinePoolSize))) (pPPipelinePoolSizes')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (semaphoreRequestCount)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (commandBufferRequestCount)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (fenceRequestCount)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (deviceMemoryRequestCount)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) (bufferRequestCount)
    lift $ poke ((p `plusPtr` 68 :: Ptr Word32)) (imageRequestCount)
    lift $ poke ((p `plusPtr` 72 :: Ptr Word32)) (eventRequestCount)
    lift $ poke ((p `plusPtr` 76 :: Ptr Word32)) (queryPoolRequestCount)
    lift $ poke ((p `plusPtr` 80 :: Ptr Word32)) (bufferViewRequestCount)
    lift $ poke ((p `plusPtr` 84 :: Ptr Word32)) (imageViewRequestCount)
    lift $ poke ((p `plusPtr` 88 :: Ptr Word32)) (layeredImageViewRequestCount)
    lift $ poke ((p `plusPtr` 92 :: Ptr Word32)) (pipelineCacheRequestCount)
    lift $ poke ((p `plusPtr` 96 :: Ptr Word32)) (pipelineLayoutRequestCount)
    lift $ poke ((p `plusPtr` 100 :: Ptr Word32)) (renderPassRequestCount)
    lift $ poke ((p `plusPtr` 104 :: Ptr Word32)) (graphicsPipelineRequestCount)
    lift $ poke ((p `plusPtr` 108 :: Ptr Word32)) (computePipelineRequestCount)
    lift $ poke ((p `plusPtr` 112 :: Ptr Word32)) (descriptorSetLayoutRequestCount)
    lift $ poke ((p `plusPtr` 116 :: Ptr Word32)) (samplerRequestCount)
    lift $ poke ((p `plusPtr` 120 :: Ptr Word32)) (descriptorPoolRequestCount)
    lift $ poke ((p `plusPtr` 124 :: Ptr Word32)) (descriptorSetRequestCount)
    lift $ poke ((p `plusPtr` 128 :: Ptr Word32)) (framebufferRequestCount)
    lift $ poke ((p `plusPtr` 132 :: Ptr Word32)) (commandPoolRequestCount)
    lift $ poke ((p `plusPtr` 136 :: Ptr Word32)) (samplerYcbcrConversionRequestCount)
    lift $ poke ((p `plusPtr` 140 :: Ptr Word32)) (surfaceRequestCount)
    lift $ poke ((p `plusPtr` 144 :: Ptr Word32)) (swapchainRequestCount)
    lift $ poke ((p `plusPtr` 148 :: Ptr Word32)) (displayModeRequestCount)
    lift $ poke ((p `plusPtr` 152 :: Ptr Word32)) (subpassDescriptionRequestCount)
    lift $ poke ((p `plusPtr` 156 :: Ptr Word32)) (attachmentDescriptionRequestCount)
    lift $ poke ((p `plusPtr` 160 :: Ptr Word32)) (descriptorSetLayoutBindingRequestCount)
    lift $ poke ((p `plusPtr` 164 :: Ptr Word32)) (descriptorSetLayoutBindingLimit)
    lift $ poke ((p `plusPtr` 168 :: Ptr Word32)) (maxImageViewMipLevels)
    lift $ poke ((p `plusPtr` 172 :: Ptr Word32)) (maxImageViewArrayLayers)
    lift $ poke ((p `plusPtr` 176 :: Ptr Word32)) (maxLayeredImageViewMipLevels)
    lift $ poke ((p `plusPtr` 180 :: Ptr Word32)) (maxOcclusionQueriesPerPool)
    lift $ poke ((p `plusPtr` 184 :: Ptr Word32)) (maxPipelineStatisticsQueriesPerPool)
    lift $ poke ((p `plusPtr` 188 :: Ptr Word32)) (maxTimestampQueriesPerPool)
    lift $ poke ((p `plusPtr` 192 :: Ptr Word32)) (maxImmutableSamplersPerDescriptorSetLayout)
    lift $ f
  cStructSize = 200
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_OBJECT_RESERVATION_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 164 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 168 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 172 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 176 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 180 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 184 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 188 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 192 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss DeviceObjectReservationCreateInfo es
         , PeekChain es ) => FromCStruct (DeviceObjectReservationCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    pipelineCacheCreateInfoCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPipelineCacheCreateInfos <- peek @(Ptr PipelineCacheCreateInfo) ((p `plusPtr` 24 :: Ptr (Ptr PipelineCacheCreateInfo)))
    pPipelineCacheCreateInfos' <- generateM (fromIntegral pipelineCacheCreateInfoCount) (\i -> peekCStruct @PipelineCacheCreateInfo ((pPipelineCacheCreateInfos `advancePtrBytes` (40 * (i)) :: Ptr PipelineCacheCreateInfo)))
    pipelinePoolSizeCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pPipelinePoolSizes <- peek @(Ptr PipelinePoolSize) ((p `plusPtr` 40 :: Ptr (Ptr PipelinePoolSize)))
    pPipelinePoolSizes' <- generateM (fromIntegral pipelinePoolSizeCount) (\i -> peekCStruct @PipelinePoolSize ((pPipelinePoolSizes `advancePtrBytes` (32 * (i)) :: Ptr PipelinePoolSize)))
    semaphoreRequestCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    commandBufferRequestCount <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    fenceRequestCount <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    deviceMemoryRequestCount <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    bufferRequestCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    imageRequestCount <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    eventRequestCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    queryPoolRequestCount <- peek @Word32 ((p `plusPtr` 76 :: Ptr Word32))
    bufferViewRequestCount <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    imageViewRequestCount <- peek @Word32 ((p `plusPtr` 84 :: Ptr Word32))
    layeredImageViewRequestCount <- peek @Word32 ((p `plusPtr` 88 :: Ptr Word32))
    pipelineCacheRequestCount <- peek @Word32 ((p `plusPtr` 92 :: Ptr Word32))
    pipelineLayoutRequestCount <- peek @Word32 ((p `plusPtr` 96 :: Ptr Word32))
    renderPassRequestCount <- peek @Word32 ((p `plusPtr` 100 :: Ptr Word32))
    graphicsPipelineRequestCount <- peek @Word32 ((p `plusPtr` 104 :: Ptr Word32))
    computePipelineRequestCount <- peek @Word32 ((p `plusPtr` 108 :: Ptr Word32))
    descriptorSetLayoutRequestCount <- peek @Word32 ((p `plusPtr` 112 :: Ptr Word32))
    samplerRequestCount <- peek @Word32 ((p `plusPtr` 116 :: Ptr Word32))
    descriptorPoolRequestCount <- peek @Word32 ((p `plusPtr` 120 :: Ptr Word32))
    descriptorSetRequestCount <- peek @Word32 ((p `plusPtr` 124 :: Ptr Word32))
    framebufferRequestCount <- peek @Word32 ((p `plusPtr` 128 :: Ptr Word32))
    commandPoolRequestCount <- peek @Word32 ((p `plusPtr` 132 :: Ptr Word32))
    samplerYcbcrConversionRequestCount <- peek @Word32 ((p `plusPtr` 136 :: Ptr Word32))
    surfaceRequestCount <- peek @Word32 ((p `plusPtr` 140 :: Ptr Word32))
    swapchainRequestCount <- peek @Word32 ((p `plusPtr` 144 :: Ptr Word32))
    displayModeRequestCount <- peek @Word32 ((p `plusPtr` 148 :: Ptr Word32))
    subpassDescriptionRequestCount <- peek @Word32 ((p `plusPtr` 152 :: Ptr Word32))
    attachmentDescriptionRequestCount <- peek @Word32 ((p `plusPtr` 156 :: Ptr Word32))
    descriptorSetLayoutBindingRequestCount <- peek @Word32 ((p `plusPtr` 160 :: Ptr Word32))
    descriptorSetLayoutBindingLimit <- peek @Word32 ((p `plusPtr` 164 :: Ptr Word32))
    maxImageViewMipLevels <- peek @Word32 ((p `plusPtr` 168 :: Ptr Word32))
    maxImageViewArrayLayers <- peek @Word32 ((p `plusPtr` 172 :: Ptr Word32))
    maxLayeredImageViewMipLevels <- peek @Word32 ((p `plusPtr` 176 :: Ptr Word32))
    maxOcclusionQueriesPerPool <- peek @Word32 ((p `plusPtr` 180 :: Ptr Word32))
    maxPipelineStatisticsQueriesPerPool <- peek @Word32 ((p `plusPtr` 184 :: Ptr Word32))
    maxTimestampQueriesPerPool <- peek @Word32 ((p `plusPtr` 188 :: Ptr Word32))
    maxImmutableSamplersPerDescriptorSetLayout <- peek @Word32 ((p `plusPtr` 192 :: Ptr Word32))
    pure $ DeviceObjectReservationCreateInfo
             next
             pPipelineCacheCreateInfos'
             pPipelinePoolSizes'
             semaphoreRequestCount
             commandBufferRequestCount
             fenceRequestCount
             deviceMemoryRequestCount
             bufferRequestCount
             imageRequestCount
             eventRequestCount
             queryPoolRequestCount
             bufferViewRequestCount
             imageViewRequestCount
             layeredImageViewRequestCount
             pipelineCacheRequestCount
             pipelineLayoutRequestCount
             renderPassRequestCount
             graphicsPipelineRequestCount
             computePipelineRequestCount
             descriptorSetLayoutRequestCount
             samplerRequestCount
             descriptorPoolRequestCount
             descriptorSetRequestCount
             framebufferRequestCount
             commandPoolRequestCount
             samplerYcbcrConversionRequestCount
             surfaceRequestCount
             swapchainRequestCount
             displayModeRequestCount
             subpassDescriptionRequestCount
             attachmentDescriptionRequestCount
             descriptorSetLayoutBindingRequestCount
             descriptorSetLayoutBindingLimit
             maxImageViewMipLevels
             maxImageViewArrayLayers
             maxLayeredImageViewMipLevels
             maxOcclusionQueriesPerPool
             maxPipelineStatisticsQueriesPerPool
             maxTimestampQueriesPerPool
             maxImmutableSamplersPerDescriptorSetLayout

instance es ~ '[] => Zero (DeviceObjectReservationCreateInfo es) where
  zero = DeviceObjectReservationCreateInfo
           ()
           mempty
           mempty
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
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkCommandPoolMemoryReservationCreateInfo"
data CommandPoolMemoryReservationCreateInfo = CommandPoolMemoryReservationCreateInfo
  { -- No documentation found for Nested "VkCommandPoolMemoryReservationCreateInfo" "commandPoolReservedSize"
    commandPoolReservedSize :: DeviceSize
  , -- No documentation found for Nested "VkCommandPoolMemoryReservationCreateInfo" "commandPoolMaxCommandBuffers"
    commandPoolMaxCommandBuffers :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandPoolMemoryReservationCreateInfo)
#endif
deriving instance Show CommandPoolMemoryReservationCreateInfo

instance ToCStruct CommandPoolMemoryReservationCreateInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandPoolMemoryReservationCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_MEMORY_RESERVATION_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (commandPoolReservedSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (commandPoolMaxCommandBuffers)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_MEMORY_RESERVATION_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct CommandPoolMemoryReservationCreateInfo where
  peekCStruct p = do
    commandPoolReservedSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    commandPoolMaxCommandBuffers <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ CommandPoolMemoryReservationCreateInfo
             commandPoolReservedSize commandPoolMaxCommandBuffers

instance Storable CommandPoolMemoryReservationCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandPoolMemoryReservationCreateInfo where
  zero = CommandPoolMemoryReservationCreateInfo
           zero
           zero


-- No documentation found for TopLevel "VkCommandPoolMemoryConsumption"
data CommandPoolMemoryConsumption = CommandPoolMemoryConsumption
  { -- No documentation found for Nested "VkCommandPoolMemoryConsumption" "commandPoolAllocated"
    commandPoolAllocated :: DeviceSize
  , -- No documentation found for Nested "VkCommandPoolMemoryConsumption" "commandPoolReservedSize"
    commandPoolReservedSize :: DeviceSize
  , -- No documentation found for Nested "VkCommandPoolMemoryConsumption" "commandBufferAllocated"
    commandBufferAllocated :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandPoolMemoryConsumption)
#endif
deriving instance Show CommandPoolMemoryConsumption

instance ToCStruct CommandPoolMemoryConsumption where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandPoolMemoryConsumption{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_MEMORY_CONSUMPTION)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (commandPoolAllocated)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (commandPoolReservedSize)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (commandBufferAllocated)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_MEMORY_CONSUMPTION)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct CommandPoolMemoryConsumption where
  peekCStruct p = do
    commandPoolAllocated <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    commandPoolReservedSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    commandBufferAllocated <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ CommandPoolMemoryConsumption
             commandPoolAllocated commandPoolReservedSize commandBufferAllocated

instance Storable CommandPoolMemoryConsumption where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandPoolMemoryConsumption where
  zero = CommandPoolMemoryConsumption
           zero
           zero
           zero

