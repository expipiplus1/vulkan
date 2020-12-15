{-# language CPP #-}
-- | = Name
--
-- VK_NV_device_diagnostic_checkpoints - device extension
--
-- == VK_NV_device_diagnostic_checkpoints
--
-- [__Name String__]
--     @VK_NV_device_diagnostic_checkpoints@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     207
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Nuno Subtil
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_device_diagnostic_checkpoints:%20&body=@nsubtil%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-16
--
-- [__Contributors__]
--
--     -   Oleg Kuznetsov, NVIDIA
--
--     -   Alex Dunn, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension allows applications to insert markers in the command
-- stream and associate them with custom data.
--
-- If a device lost error occurs, the application /may/ then query the
-- implementation for the last markers to cross specific
-- implementation-defined pipeline stages, in order to narrow down which
-- commands were executing at the time and might have caused the failure.
--
-- == New Commands
--
-- -   'cmdSetCheckpointNV'
--
-- -   'getQueueCheckpointDataNV'
--
-- == New Structures
--
-- -   'CheckpointDataNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyCheckpointPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME'
--
-- -   'NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CHECKPOINT_DATA_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2018-07-16 (Nuno Subtil)
--
--     -   Internal revisions
--
-- -   Revision 2, 2018-07-16 (Nuno Subtil)
--
--     -   ???
--
-- = See Also
--
-- 'CheckpointDataNV', 'QueueFamilyCheckpointPropertiesNV',
-- 'cmdSetCheckpointNV', 'getQueueCheckpointDataNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostic_checkpoints Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints  ( cmdSetCheckpointNV
                                                              , getQueueCheckpointDataNV
                                                              , QueueFamilyCheckpointPropertiesNV(..)
                                                              , CheckpointDataNV(..)
                                                              , NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
                                                              , pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION
                                                              , NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
                                                              , pattern NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
                                                              ) where

import Vulkan.Internal.Utils (traceAroundEvent)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CHECKPOINT_DATA_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCheckpointNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> Ptr () -> IO ()

-- | vkCmdSetCheckpointNV - insert diagnostic checkpoint in command stream
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCheckpointNV-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCheckpointNV-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCheckpointNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, compute, or transfer
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Transfer                                                                                                              |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetCheckpointNV :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer that will receive the marker
                      CommandBuffer
                   -> -- | @pCheckpointMarker@ is an opaque application-provided value that will be
                      -- associated with the checkpoint.
                      ("checkpointMarker" ::: Ptr ())
                   -> io ()
cmdSetCheckpointNV commandBuffer checkpointMarker = liftIO $ do
  let vkCmdSetCheckpointNVPtr = pVkCmdSetCheckpointNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetCheckpointNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCheckpointNV is null" Nothing Nothing
  let vkCmdSetCheckpointNV' = mkVkCmdSetCheckpointNV vkCmdSetCheckpointNVPtr
  traceAroundEvent "vkCmdSetCheckpointNV" (vkCmdSetCheckpointNV' (commandBufferHandle (commandBuffer)) (checkpointMarker))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetQueueCheckpointDataNV
  :: FunPtr (Ptr Queue_T -> Ptr Word32 -> Ptr CheckpointDataNV -> IO ()) -> Ptr Queue_T -> Ptr Word32 -> Ptr CheckpointDataNV -> IO ()

-- | vkGetQueueCheckpointDataNV - retrieve diagnostic checkpoint data
--
-- = Description
--
-- If @pCheckpointData@ is @NULL@, then the number of checkpoint markers
-- available is returned in @pCheckpointDataCount@.
--
-- Otherwise, @pCheckpointDataCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pCheckpointData@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pCheckpointData@.
--
-- If @pCheckpointDataCount@ is less than the number of checkpoint markers
-- available, at most @pCheckpointDataCount@ structures will be written.
--
-- == Valid Usage
--
-- -   #VUID-vkGetQueueCheckpointDataNV-queue-02025# The device that
--     @queue@ belongs to /must/ be in the lost state
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetQueueCheckpointDataNV-queue-parameter# @queue@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Queue' handle
--
-- -   #VUID-vkGetQueueCheckpointDataNV-pCheckpointDataCount-parameter#
--     @pCheckpointDataCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   #VUID-vkGetQueueCheckpointDataNV-pCheckpointData-parameter# If the
--     value referenced by @pCheckpointDataCount@ is not @0@, and
--     @pCheckpointData@ is not @NULL@, @pCheckpointData@ /must/ be a valid
--     pointer to an array of @pCheckpointDataCount@ 'CheckpointDataNV'
--     structures
--
-- = See Also
--
-- 'CheckpointDataNV', 'Vulkan.Core10.Handles.Queue'
getQueueCheckpointDataNV :: forall io
                          . (MonadIO io)
                         => -- | @queue@ is the 'Vulkan.Core10.Handles.Queue' object the caller would
                            -- like to retrieve checkpoint data for
                            Queue
                         -> io (("checkpointData" ::: Vector CheckpointDataNV))
getQueueCheckpointDataNV queue = liftIO . evalContT $ do
  let vkGetQueueCheckpointDataNVPtr = pVkGetQueueCheckpointDataNV (deviceCmds (queue :: Queue))
  lift $ unless (vkGetQueueCheckpointDataNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetQueueCheckpointDataNV is null" Nothing Nothing
  let vkGetQueueCheckpointDataNV' = mkVkGetQueueCheckpointDataNV vkGetQueueCheckpointDataNVPtr
  let queue' = queueHandle (queue)
  pPCheckpointDataCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetQueueCheckpointDataNV" (vkGetQueueCheckpointDataNV' queue' (pPCheckpointDataCount) (nullPtr))
  pCheckpointDataCount <- lift $ peek @Word32 pPCheckpointDataCount
  pPCheckpointData <- ContT $ bracket (callocBytes @CheckpointDataNV ((fromIntegral (pCheckpointDataCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCheckpointData `advancePtrBytes` (i * 32) :: Ptr CheckpointDataNV) . ($ ())) [0..(fromIntegral (pCheckpointDataCount)) - 1]
  lift $ traceAroundEvent "vkGetQueueCheckpointDataNV" (vkGetQueueCheckpointDataNV' queue' (pPCheckpointDataCount) ((pPCheckpointData)))
  pCheckpointDataCount' <- lift $ peek @Word32 pPCheckpointDataCount
  pCheckpointData' <- lift $ generateM (fromIntegral (pCheckpointDataCount')) (\i -> peekCStruct @CheckpointDataNV (((pPCheckpointData) `advancePtrBytes` (32 * (i)) :: Ptr CheckpointDataNV)))
  pure $ (pCheckpointData')


-- | VkQueueFamilyCheckpointPropertiesNV - return structure for queue family
-- checkpoint info query
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyCheckpointPropertiesNV = QueueFamilyCheckpointPropertiesNV
  { -- | @checkpointExecutionStageMask@ is a mask indicating which pipeline
    -- stages the implementation can execute checkpoint markers in.
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


-- | VkCheckpointDataNV - return structure for command buffer checkpoint data
--
-- = Description
--
-- The stages at which a checkpoint marker /can/ be executed are
-- implementation-defined and /can/ be queried by calling
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getQueueCheckpointDataNV'
data CheckpointDataNV = CheckpointDataNV
  { -- | @stage@ indicates which pipeline stage the checkpoint marker data refers
    -- to.
    stage :: PipelineStageFlagBits
  , -- | @pCheckpointMarker@ contains the value of the last checkpoint marker
    -- executed in the stage that @stage@ refers to.
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

