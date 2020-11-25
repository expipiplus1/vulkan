{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_device_diagnostic_checkpoints"
module Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints  ( cmdSetCheckpointNV
                                                              , getQueueCheckpointDataNV
                                                              , QueueFamilyCheckpointPropertiesNV(..)
                                                              , CheckpointDataNV(..)
                                                              , NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
                                                              , pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
                                                              , NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
                                                              , pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
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
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCheckpointNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetQueueCheckpointDataNV))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CHECKPOINT_DATA_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCheckpointNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> Ptr () -> IO ()

-- No documentation found for TopLevel "vkCmdSetCheckpointNV"
cmdSetCheckpointNV :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkCmdSetCheckpointNV" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkCmdSetCheckpointNV" "pCheckpointMarker"
                      ("checkpointMarker" ::: Ptr ())
                   -> io ()
cmdSetCheckpointNV commandBuffer checkpointMarker = liftIO $ do
  let vkCmdSetCheckpointNVPtr = pVkCmdSetCheckpointNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetCheckpointNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCheckpointNV is null" Nothing Nothing
  let vkCmdSetCheckpointNV' = mkVkCmdSetCheckpointNV vkCmdSetCheckpointNVPtr
  vkCmdSetCheckpointNV' (commandBufferHandle (commandBuffer)) (checkpointMarker)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetQueueCheckpointDataNV
  :: FunPtr (Ptr Queue_T -> Ptr Word32 -> Ptr CheckpointDataNV -> IO ()) -> Ptr Queue_T -> Ptr Word32 -> Ptr CheckpointDataNV -> IO ()

-- No documentation found for TopLevel "vkGetQueueCheckpointDataNV"
getQueueCheckpointDataNV :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkGetQueueCheckpointDataNV" "queue"
                            Queue
                         -> io (("checkpointData" ::: Vector CheckpointDataNV))
getQueueCheckpointDataNV queue = liftIO . evalContT $ do
  let vkGetQueueCheckpointDataNVPtr = pVkGetQueueCheckpointDataNV (deviceCmds (queue :: Queue))
  lift $ unless (vkGetQueueCheckpointDataNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetQueueCheckpointDataNV is null" Nothing Nothing
  let vkGetQueueCheckpointDataNV' = mkVkGetQueueCheckpointDataNV vkGetQueueCheckpointDataNVPtr
  let queue' = queueHandle (queue)
  pPCheckpointDataCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ vkGetQueueCheckpointDataNV' queue' (pPCheckpointDataCount) (nullPtr)
  pCheckpointDataCount <- lift $ peek @Word32 pPCheckpointDataCount
  pPCheckpointData <- ContT $ bracket (callocBytes @CheckpointDataNV ((fromIntegral (pCheckpointDataCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCheckpointData `advancePtrBytes` (i * 32) :: Ptr CheckpointDataNV) . ($ ())) [0..(fromIntegral (pCheckpointDataCount)) - 1]
  lift $ vkGetQueueCheckpointDataNV' queue' (pPCheckpointDataCount) ((pPCheckpointData))
  pCheckpointDataCount' <- lift $ peek @Word32 pPCheckpointDataCount
  pCheckpointData' <- lift $ generateM (fromIntegral (pCheckpointDataCount')) (\i -> peekCStruct @CheckpointDataNV (((pPCheckpointData) `advancePtrBytes` (32 * (i)) :: Ptr CheckpointDataNV)))
  pure $ (pCheckpointData')



-- No documentation found for TopLevel "VkQueueFamilyCheckpointPropertiesNV"
data QueueFamilyCheckpointPropertiesNV = QueueFamilyCheckpointPropertiesNV
  { -- No documentation found for Nested "VkQueueFamilyCheckpointPropertiesNV" "checkpointExecutionStageMask"
    checkpointExecutionStageMask :: PipelineStageFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyCheckpointPropertiesNV)
#endif
deriving instance Show QueueFamilyCheckpointPropertiesNV

instance ToCStruct QueueFamilyCheckpointPropertiesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyCheckpointPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags)) (checkpointExecutionStageMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags)) (zero)
    f

instance FromCStruct QueueFamilyCheckpointPropertiesNV where
  peekCStruct p = do
    checkpointExecutionStageMask <- peek @PipelineStageFlags ((p `plusPtr` 16 :: Ptr PipelineStageFlags))
    pure $ QueueFamilyCheckpointPropertiesNV
             checkpointExecutionStageMask


instance Storable QueueFamilyCheckpointPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyCheckpointPropertiesNV where
  zero = QueueFamilyCheckpointPropertiesNV
           zero



-- No documentation found for TopLevel "VkCheckpointDataNV"
data CheckpointDataNV = CheckpointDataNV
  { -- No documentation found for Nested "VkCheckpointDataNV" "stage"
    stage :: PipelineStageFlagBits
  , -- No documentation found for Nested "VkCheckpointDataNV" "pCheckpointMarker"
    checkpointMarker :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CheckpointDataNV)
#endif
deriving instance Show CheckpointDataNV

instance ToCStruct CheckpointDataNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CheckpointDataNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CHECKPOINT_DATA_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlagBits)) (stage)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (checkpointMarker)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CHECKPOINT_DATA_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlagBits)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct CheckpointDataNV where
  peekCStruct p = do
    stage <- peek @PipelineStageFlagBits ((p `plusPtr` 16 :: Ptr PipelineStageFlagBits))
    pCheckpointMarker <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ CheckpointDataNV
             stage pCheckpointMarker


instance Storable CheckpointDataNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CheckpointDataNV where
  zero = CheckpointDataNV
           zero
           zero


type NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION"
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION = 2


type NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME = "VK_NV_device_diagnostic_checkpoints"

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME"
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME = "VK_NV_device_diagnostic_checkpoints"

