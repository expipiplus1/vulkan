{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_transform_feedback  ( cmdBindTransformFeedbackBuffersEXT
                                                    , cmdBeginTransformFeedbackEXT
                                                    , cmdUseTransformFeedbackEXT
                                                    , cmdEndTransformFeedbackEXT
                                                    , cmdBeginQueryIndexedEXT
                                                    , cmdUseQueryIndexedEXT
                                                    , cmdEndQueryIndexedEXT
                                                    , cmdDrawIndirectByteCountEXT
                                                    , PhysicalDeviceTransformFeedbackFeaturesEXT(..)
                                                    , PhysicalDeviceTransformFeedbackPropertiesEXT(..)
                                                    , PipelineRasterizationStateStreamCreateInfoEXT(..)
                                                    , PipelineRasterizationStateStreamCreateFlagsEXT(..)
                                                    , EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
                                                    , pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
                                                    , EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
                                                    , pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
                                                    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginQueryIndexedEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginTransformFeedbackEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindTransformFeedbackBuffersEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndirectByteCountEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndQueryIndexedEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndTransformFeedbackEXT))
import Vulkan.Core10.BaseType (DeviceSize)
import Vulkan.Core10.BaseType (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlagBits(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindTransformFeedbackBuffersEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()

-- | vkCmdBindTransformFeedbackBuffersEXT - Bind transform feedback buffers
-- to a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstBinding@ is the index of the first transform feedback binding
--     whose state is updated by the command.
--
-- -   @bindingCount@ is the number of transform feedback bindings whose
--     state is updated by the command.
--
-- -   @pBuffers@ is a pointer to an array of buffer handles.
--
-- -   @pOffsets@ is a pointer to an array of buffer offsets.
--
-- -   @pSizes@ is an optional array of buffer sizes, specifying the
--     maximum number of bytes to capture to the corresponding transform
--     feedback buffer. If @pSizes@ is @NULL@, or the value of the @pSizes@
--     array element is 'Vulkan.Core10.APIConstants.WHOLE_SIZE', then the
--     maximum bytes captured will be the size of the corresponding buffer
--     minus the buffer offset.
--
-- = Description
--
-- The values taken from elements i of @pBuffers@, @pOffsets@ and @pSizes@
-- replace the current state for the transform feedback binding
-- @firstBinding@ + i, for i in [0, @bindingCount@). The transform feedback
-- binding is updated to start at the offset indicated by @pOffsets@[i]
-- from the start of the buffer @pBuffers@[i].
--
-- == Valid Usage
--
-- -   'PhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   @firstBinding@ /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   The sum of @firstBinding@ and @bindingCount@ /must/ be less than or
--     equal to
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   All elements of @pOffsets@ /must/ be less than the size of the
--     corresponding element in @pBuffers@
--
-- -   All elements of @pOffsets@ /must/ be a multiple of 4
--
-- -   All elements of @pBuffers@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT'
--     flag
--
-- -   If the optional @pSize@ array is specified, each element of @pSizes@
--     /must/ either be 'Vulkan.Core10.APIConstants.WHOLE_SIZE', or be less
--     than or equal to
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBufferSize@
--
-- -   All elements of @pSizes@ /must/ be less than or equal to the size of
--     the corresponding buffer in @pBuffers@
--
-- -   All elements of @pOffsets@ plus @pSizes@, where the @pSizes@,
--     element is not 'Vulkan.Core10.APIConstants.WHOLE_SIZE', /must/ be
--     less than or equal to the size of the corresponding element in
--     @pBuffers@
--
-- -   Each element of @pBuffers@ that is non-sparse /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   Transform feedback /must/ not be active when the
--     'cmdBindTransformFeedbackBuffersEXT' command is recorded
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pBuffers@ /must/ be a valid pointer to an array of @bindingCount@
--     valid 'Vulkan.Core10.Handles.Buffer' handles
--
-- -   @pOffsets@ /must/ be a valid pointer to an array of @bindingCount@
--     'Vulkan.Core10.BaseType.DeviceSize' values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   @bindingCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and the elements of @pBuffers@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.BaseType.DeviceSize'
cmdBindTransformFeedbackBuffersEXT :: forall io . MonadIO io => CommandBuffer -> ("firstBinding" ::: Word32) -> ("buffers" ::: Vector Buffer) -> ("offsets" ::: Vector DeviceSize) -> ("sizes" ::: Vector DeviceSize) -> io ()
cmdBindTransformFeedbackBuffersEXT commandBuffer firstBinding buffers offsets sizes = liftIO . evalContT $ do
  let vkCmdBindTransformFeedbackBuffersEXTPtr = pVkCmdBindTransformFeedbackBuffersEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBindTransformFeedbackBuffersEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindTransformFeedbackBuffersEXT is null" Nothing Nothing
  let vkCmdBindTransformFeedbackBuffersEXT' = mkVkCmdBindTransformFeedbackBuffersEXT vkCmdBindTransformFeedbackBuffersEXTPtr
  let pBuffersLength = Data.Vector.length $ (buffers)
  let pOffsetsLength = Data.Vector.length $ (offsets)
  lift $ unless (pOffsetsLength == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  let pSizesLength = Data.Vector.length $ (sizes)
  lift $ unless (fromIntegral pSizesLength == pBuffersLength || pSizesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pSizes and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (buffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytesAligned @DeviceSize ((Data.Vector.length (offsets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  pSizes <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pPSizes <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (sizes))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pPSizes
  lift $ vkCmdBindTransformFeedbackBuffersEXT' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets) pSizes
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginTransformFeedbackEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- | vkCmdBeginTransformFeedbackEXT - Make transform feedback active in the
-- command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstCounterBuffer@ is the index of the first transform feedback
--     buffer corresponding to @pCounterBuffers@[0] and
--     @pCounterBufferOffsets@[0].
--
-- -   @counterBufferCount@ is the size of the @pCounterBuffers@ and
--     @pCounterBufferOffsets@ arrays.
--
-- -   @pCounterBuffers@ is an optional array of buffer handles to the
--     counter buffers which contain a 4 byte integer value representing
--     the byte offset from the start of the corresponding transform
--     feedback buffer from where to start capturing vertex data. If the
--     byte offset stored to the counter buffer location was done using
--     'cmdEndTransformFeedbackEXT' it can be used to resume transform
--     feedback from the previous location. If @pCounterBuffers@ is @NULL@,
--     then transform feedback will start capturing vertex data to byte
--     offset zero in all bound transform feedback buffers. For each
--     element of @pCounterBuffers@ that is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', transform feedback will
--     start capturing vertex data to byte zero in the corresponding bound
--     transform feedback buffer.
--
-- -   @pCounterBufferOffsets@ is an optional array of offsets within each
--     of the @pCounterBuffers@ where the counter values were previously
--     written. The location in each counter buffer at these offsets /must/
--     be large enough to contain 4 bytes of data. This data is the number
--     of bytes captured by the previous transform feedback to this buffer.
--     If @pCounterBufferOffsets@ is @NULL@, then it is assumed the offsets
--     are zero.
--
-- = Description
--
-- The active transform feedback buffers will capture primitives emitted
-- from the corresponding @XfbBuffer@ in the bound graphics pipeline. Any
-- @XfbBuffer@ emitted that does not output to an active transform feedback
-- buffer will not be captured.
--
-- == Valid Usage
--
-- -   'PhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   Transform feedback /must/ not be active
--
-- -   @firstCounterBuffer@ /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   The sum of @firstCounterBuffer@ and @counterBufferCount@ /must/ be
--     less than or equal to
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBuffers@ is not
--     @NULL@, @pCounterBuffers@ /must/ be a valid pointer to an array of
--     @counterBufferCount@ 'Vulkan.Core10.Handles.Buffer' handles that are
--     either valid or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   For each buffer handle in the array, if it is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ reference a
--     buffer large enough to hold 4 bytes at the corresponding offset from
--     the @pCounterBufferOffsets@ array
--
-- -   If @pCounterBuffer@ is @NULL@, then @pCounterBufferOffsets@ /must/
--     also be @NULL@
--
-- -   For each buffer handle in the @pCounterBuffers@ array that is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ have been created
--     with a @usage@ value containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- -   Transform feedback /must/ not be made active in a render pass
--     instance with multiview enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBufferOffsets@ is
--     not @NULL@, @pCounterBufferOffsets@ /must/ be a valid pointer to an
--     array of @counterBufferCount@ 'Vulkan.Core10.BaseType.DeviceSize'
--     values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @commandBuffer@, and the elements of @pCounterBuffers@ that
--     are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.BaseType.DeviceSize'
cmdBeginTransformFeedbackEXT :: forall io . MonadIO io => CommandBuffer -> ("firstCounterBuffer" ::: Word32) -> ("counterBuffers" ::: Vector Buffer) -> ("counterBufferOffsets" ::: Vector DeviceSize) -> io ()
cmdBeginTransformFeedbackEXT commandBuffer firstCounterBuffer counterBuffers counterBufferOffsets = liftIO . evalContT $ do
  let vkCmdBeginTransformFeedbackEXTPtr = pVkCmdBeginTransformFeedbackEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginTransformFeedbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginTransformFeedbackEXT is null" Nothing Nothing
  let vkCmdBeginTransformFeedbackEXT' = mkVkCmdBeginTransformFeedbackEXT vkCmdBeginTransformFeedbackEXTPtr
  let pCounterBuffersLength = Data.Vector.length $ (counterBuffers)
  let pCounterBufferOffsetsLength = Data.Vector.length $ (counterBufferOffsets)
  lift $ unless (fromIntegral pCounterBufferOffsetsLength == pCounterBuffersLength || pCounterBufferOffsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pCounterBufferOffsets and pCounterBuffers must have the same length" Nothing Nothing
  pPCounterBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (counterBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (counterBuffers)
  pCounterBufferOffsets <- if Data.Vector.null (counterBufferOffsets)
    then pure nullPtr
    else do
      pPCounterBufferOffsets <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (counterBufferOffsets))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBufferOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((counterBufferOffsets))
      pure $ pPCounterBufferOffsets
  lift $ vkCmdBeginTransformFeedbackEXT' (commandBufferHandle (commandBuffer)) (firstCounterBuffer) ((fromIntegral pCounterBuffersLength :: Word32)) (pPCounterBuffers) pCounterBufferOffsets
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginTransformFeedbackEXT' and 'cmdEndTransformFeedbackEXT'
--
-- Note that 'cmdEndTransformFeedbackEXT' is *not* called if an exception
-- is thrown by the inner action.
cmdUseTransformFeedbackEXT :: forall io r . MonadIO io => CommandBuffer -> Word32 -> Vector Buffer -> Vector DeviceSize -> io r -> io r
cmdUseTransformFeedbackEXT commandBuffer firstCounterBuffer pCounterBuffers pCounterBufferOffsets a =
  (cmdBeginTransformFeedbackEXT commandBuffer firstCounterBuffer pCounterBuffers pCounterBufferOffsets) *> a <* (cmdEndTransformFeedbackEXT commandBuffer firstCounterBuffer pCounterBuffers pCounterBufferOffsets)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndTransformFeedbackEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- | vkCmdEndTransformFeedbackEXT - Make transform feedback inactive in the
-- command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstCounterBuffer@ is the index of the first transform feedback
--     buffer corresponding to @pCounterBuffers@[0] and
--     @pCounterBufferOffsets@[0].
--
-- -   @counterBufferCount@ is the size of the @pCounterBuffers@ and
--     @pCounterBufferOffsets@ arrays.
--
-- -   @pCounterBuffers@ is an optional array of buffer handles to the
--     counter buffers used to record the current byte positions of each
--     transform feedback buffer where the next vertex output data would be
--     captured. This /can/ be used by a subsequent
--     'cmdBeginTransformFeedbackEXT' call to resume transform feedback
--     capture from this position. It can also be used by
--     'cmdDrawIndirectByteCountEXT' to determine the vertex count of the
--     draw call.
--
-- -   @pCounterBufferOffsets@ is an optional array of offsets within each
--     of the @pCounterBuffers@ where the counter values can be written.
--     The location in each counter buffer at these offsets /must/ be large
--     enough to contain 4 bytes of data. The data stored at this location
--     is the byte offset from the start of the transform feedback buffer
--     binding where the next vertex data would be written. If
--     @pCounterBufferOffsets@ is @NULL@, then it is assumed the offsets
--     are zero.
--
-- == Valid Usage
--
-- -   'PhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   Transform feedback /must/ be active
--
-- -   @firstCounterBuffer@ /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   The sum of @firstCounterBuffer@ and @counterBufferCount@ /must/ be
--     less than or equal to
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBuffers@ is not
--     @NULL@, @pCounterBuffers@ /must/ be a valid pointer to an array of
--     @counterBufferCount@ 'Vulkan.Core10.Handles.Buffer' handles that are
--     either valid or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   For each buffer handle in the array, if it is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ reference a
--     buffer large enough to hold 4 bytes at the corresponding offset from
--     the @pCounterBufferOffsets@ array
--
-- -   If @pCounterBuffer@ is @NULL@, then @pCounterBufferOffsets@ /must/
--     also be @NULL@
--
-- -   For each buffer handle in the @pCounterBuffers@ array that is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ have been created
--     with a @usage@ value containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBufferOffsets@ is
--     not @NULL@, @pCounterBufferOffsets@ /must/ be a valid pointer to an
--     array of @counterBufferCount@ 'Vulkan.Core10.BaseType.DeviceSize'
--     values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @commandBuffer@, and the elements of @pCounterBuffers@ that
--     are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.BaseType.DeviceSize'
cmdEndTransformFeedbackEXT :: forall io . MonadIO io => CommandBuffer -> ("firstCounterBuffer" ::: Word32) -> ("counterBuffers" ::: Vector Buffer) -> ("counterBufferOffsets" ::: Vector DeviceSize) -> io ()
cmdEndTransformFeedbackEXT commandBuffer firstCounterBuffer counterBuffers counterBufferOffsets = liftIO . evalContT $ do
  let vkCmdEndTransformFeedbackEXTPtr = pVkCmdEndTransformFeedbackEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdEndTransformFeedbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndTransformFeedbackEXT is null" Nothing Nothing
  let vkCmdEndTransformFeedbackEXT' = mkVkCmdEndTransformFeedbackEXT vkCmdEndTransformFeedbackEXTPtr
  let pCounterBuffersLength = Data.Vector.length $ (counterBuffers)
  let pCounterBufferOffsetsLength = Data.Vector.length $ (counterBufferOffsets)
  lift $ unless (fromIntegral pCounterBufferOffsetsLength == pCounterBuffersLength || pCounterBufferOffsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pCounterBufferOffsets and pCounterBuffers must have the same length" Nothing Nothing
  pPCounterBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (counterBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (counterBuffers)
  pCounterBufferOffsets <- if Data.Vector.null (counterBufferOffsets)
    then pure nullPtr
    else do
      pPCounterBufferOffsets <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (counterBufferOffsets))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBufferOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((counterBufferOffsets))
      pure $ pPCounterBufferOffsets
  lift $ vkCmdEndTransformFeedbackEXT' (commandBufferHandle (commandBuffer)) (firstCounterBuffer) ((fromIntegral pCounterBuffersLength :: Word32)) (pPCounterBuffers) pCounterBufferOffsets
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginQueryIndexedEXT
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> IO ()

-- | vkCmdBeginQueryIndexedEXT - Begin an indexed query
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the query pool that will manage the results of the
--     query.
--
-- -   @query@ is the query index within the query pool that will contain
--     the results.
--
-- -   @flags@ is a bitmask of
--     'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlagBits'
--     specifying constraints on the types of queries that /can/ be
--     performed.
--
-- -   @index@ is the query type specific index. When the query type is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the index represents the vertex stream.
--
-- = Description
--
-- The 'cmdBeginQueryIndexedEXT' command operates the same as the
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery' command, except that
-- it also accepts a query type specific @index@ parameter.
--
-- == Valid Usage
--
-- -   @queryPool@ /must/ have been created with a @queryType@ that differs
--     from that of any queries that are
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--     within @commandBuffer@
--
-- -   All queries used by the command /must/ be unavailable
--
-- -   The @queryType@ used to create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-occlusionQueryPrecise precise occlusion queries>
--     feature is not enabled, or the @queryType@ used to create
--     @queryPool@ was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION', @flags@ /must/
--     not contain
--     'Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT'
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION', the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS' and
--     any of the @pipelineStatistics@ indicate graphics operations, the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS' and
--     any of the @pipelineStatistics@ indicate compute operations, the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   @commandBuffer@ /must/ not be a protected command buffer
--
-- -   If called within a render pass instance, the sum of @query@ and the
--     number of bits set in the current subpass’s view mask /must/ be less
--     than or equal to the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ parameter /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   If the @queryType@ used to create @queryPool@ was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ /must/ be zero
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     then
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackQueries@
--     /must/ be supported
--
-- -   If @queryPool@ was created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#profiling-lock profiling lock>
--     /must/ have been held before
--     'Vulkan.Core10.CommandBuffer.beginCommandBuffer' was called on
--     @commandBuffer@
--
-- -   If @queryPool@ was created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     one of the counters used to create @queryPool@ was
--     'Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR',
--     the query begin /must/ be the first recorded command in
--     @commandBuffer@
--
-- -   If @queryPool@ was created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     one of the counters used to create @queryPool@ was
--     'Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR',
--     the begin command /must/ not be recorded within a render pass
--     instance
--
-- -   If @queryPool@ was created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     another query pool with a @queryType@
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' has
--     been used within @commandBuffer@, its parent primary command buffer
--     or secondary command buffer recorded within the same parent primary
--     command buffer as @commandBuffer@, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-features-performanceCounterMultipleQueryPools performanceCounterMultipleQueryPools>
--     feature /must/ be enabled
--
-- -   If @queryPool@ was created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     this command /must/ not be recorded in a command buffer that, either
--     directly or through secondary command buffers, also contains a
--     'Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool' command
--     affecting the same query
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @queryPool@ /must/ be a valid 'Vulkan.Core10.Handles.QueryPool'
--     handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlagBits'
--     values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlags',
-- 'Vulkan.Core10.Handles.QueryPool'
cmdBeginQueryIndexedEXT :: forall io . MonadIO io => CommandBuffer -> QueryPool -> ("query" ::: Word32) -> QueryControlFlags -> ("index" ::: Word32) -> io ()
cmdBeginQueryIndexedEXT commandBuffer queryPool query flags index = liftIO $ do
  let vkCmdBeginQueryIndexedEXTPtr = pVkCmdBeginQueryIndexedEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBeginQueryIndexedEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginQueryIndexedEXT is null" Nothing Nothing
  let vkCmdBeginQueryIndexedEXT' = mkVkCmdBeginQueryIndexedEXT vkCmdBeginQueryIndexedEXTPtr
  vkCmdBeginQueryIndexedEXT' (commandBufferHandle (commandBuffer)) (queryPool) (query) (flags) (index)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginQueryIndexedEXT' and 'cmdEndQueryIndexedEXT'
--
-- Note that 'cmdEndQueryIndexedEXT' is *not* called if an exception is
-- thrown by the inner action.
cmdUseQueryIndexedEXT :: forall io r . MonadIO io => CommandBuffer -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> io r -> io r
cmdUseQueryIndexedEXT commandBuffer queryPool query flags index a =
  (cmdBeginQueryIndexedEXT commandBuffer queryPool query flags index) *> a <* (cmdEndQueryIndexedEXT commandBuffer queryPool query index)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndQueryIndexedEXT
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()

-- | vkCmdEndQueryIndexedEXT - Ends a query
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the query pool that is managing the results of the
--     query.
--
-- -   @query@ is the query index within the query pool where the result is
--     stored.
--
-- -   @index@ is the query type specific index.
--
-- = Description
--
-- The 'cmdEndQueryIndexedEXT' command operates the same as the
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery' command, except that
-- it also accepts a query type specific @index@ parameter.
--
-- == Valid Usage
--
-- -   All queries used by the command /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- -   @commandBuffer@ /must/ not be a protected command buffer
--
-- -   If 'cmdEndQueryIndexedEXT' is called within a render pass instance,
--     the sum of @query@ and the number of bits set in the current
--     subpass’s view mask /must/ be less than or equal to the number of
--     queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ parameter /must/ be less than
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   If the @queryType@ used to create @queryPool@ was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the @index@ /must/ be zero
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     @index@ /must/ equal the @index@ used to begin the query
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @queryPool@ /must/ be a valid 'Vulkan.Core10.Handles.QueryPool'
--     handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.QueryPool'
cmdEndQueryIndexedEXT :: forall io . MonadIO io => CommandBuffer -> QueryPool -> ("query" ::: Word32) -> ("index" ::: Word32) -> io ()
cmdEndQueryIndexedEXT commandBuffer queryPool query index = liftIO $ do
  let vkCmdEndQueryIndexedEXTPtr = pVkCmdEndQueryIndexedEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndQueryIndexedEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndQueryIndexedEXT is null" Nothing Nothing
  let vkCmdEndQueryIndexedEXT' = mkVkCmdEndQueryIndexedEXT vkCmdEndQueryIndexedEXTPtr
  vkCmdEndQueryIndexedEXT' (commandBufferHandle (commandBuffer)) (queryPool) (query) (index)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirectByteCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawIndirectByteCountEXT - Draw primitives where the vertex count
-- is derived from the counter byte value in the counter buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @instanceCount@ is the number of instances to draw.
--
-- -   @firstInstance@ is the instance ID of the first instance to draw.
--
-- -   @counterBuffer@ is the buffer handle from where the byte count is
--     read.
--
-- -   @counterBufferOffset@ is the offset into the buffer used to read the
--     byte count, which is used to calculate the vertex count for this
--     draw call.
--
-- -   @counterOffset@ is subtracted from the byte count read from the
--     @counterBuffer@ at the @counterBufferOffset@
--
-- -   @vertexStride@ is the stride in bytes between each element of the
--     vertex data that is used to calculate the vertex count from the
--     counter value. This value is typically the same value that was used
--     in the graphics pipeline state when the transform feedback was
--     captured as the @XfbStride@.
--
-- = Description
--
-- When the command is executed, primitives are assembled in the same way
-- as done with 'Vulkan.Core10.CommandBufferBuilding.cmdDraw' except the
-- @vertexCount@ is calculated based on the byte count read from
-- @counterBuffer@ at offset @counterBufferOffset@. The assembled
-- primitives execute the bound graphics pipeline.
--
-- The effective @vertexCount@ is calculated as follows:
--
-- > const uint32_t * counterBufferPtr = (const uint8_t *)counterBuffer.address + counterBufferOffset;
-- > vertexCount = floor(max(0, (*counterBufferPtr - counterOffset)) / vertexStride);
--
-- The effective @firstVertex@ is zero.
--
-- == Valid Usage
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   For each set /n/ that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a descriptor set /must/ have been bound to /n/
--     at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a push constant value /must/ have been set for
--     the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command requires any dynamic state, that
--     state /must/ have been set for @commandBuffer@, and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command, since that pipeline was bound
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   If @commandBuffer@ is an unprotected command buffer, any resource
--     accessed by the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   If the bound graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.BaseType.TRUE' and the current subpass has a
--     depth\/stencil attachment, then that attachment /must/ have been
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     either valid or 'Vulkan.Core10.APIConstants.NULL_HANDLE' buffers
--     bound
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   'PhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   The implementation /must/ support
--     'PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackDraw@
--
-- -   @vertexStride@ /must/ be greater than 0 and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTransformFeedbackBufferDataStride@
--
-- -   @counterBuffer@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @commandBuffer@ /must/ not be a protected command buffer
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @counterBuffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @commandBuffer@, and @counterBuffer@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.BaseType.DeviceSize'
cmdDrawIndirectByteCountEXT :: forall io . MonadIO io => CommandBuffer -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: Buffer) -> ("counterBufferOffset" ::: DeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> io ()
cmdDrawIndirectByteCountEXT commandBuffer instanceCount firstInstance counterBuffer counterBufferOffset counterOffset vertexStride = liftIO $ do
  let vkCmdDrawIndirectByteCountEXTPtr = pVkCmdDrawIndirectByteCountEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndirectByteCountEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndirectByteCountEXT is null" Nothing Nothing
  let vkCmdDrawIndirectByteCountEXT' = mkVkCmdDrawIndirectByteCountEXT vkCmdDrawIndirectByteCountEXTPtr
  vkCmdDrawIndirectByteCountEXT' (commandBufferHandle (commandBuffer)) (instanceCount) (firstInstance) (counterBuffer) (counterBufferOffset) (counterOffset) (vertexStride)
  pure $ ()


-- | VkPhysicalDeviceTransformFeedbackFeaturesEXT - Structure describing
-- transform feedback features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceTransformFeedbackFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceTransformFeedbackFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceTransformFeedbackFeaturesEXT' /can/ also be included in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTransformFeedbackFeaturesEXT = PhysicalDeviceTransformFeedbackFeaturesEXT
  { -- | @transformFeedback@ indicates whether the implementation supports
    -- transform feedback and shader modules /can/ declare the
    -- @TransformFeedback@ capability.
    transformFeedback :: Bool
  , -- | @geometryStreams@ indicates whether the implementation supports the
    -- @GeometryStreams@ SPIR-V capability.
    geometryStreams :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceTransformFeedbackFeaturesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTransformFeedbackFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (transformFeedback))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (geometryStreams))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTransformFeedbackFeaturesEXT where
  peekCStruct p = do
    transformFeedback <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    geometryStreams <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceTransformFeedbackFeaturesEXT
             (bool32ToBool transformFeedback) (bool32ToBool geometryStreams)

instance Storable PhysicalDeviceTransformFeedbackFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTransformFeedbackFeaturesEXT where
  zero = PhysicalDeviceTransformFeedbackFeaturesEXT
           zero
           zero


-- | VkPhysicalDeviceTransformFeedbackPropertiesEXT - Structure describing
-- transform feedback properties that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceTransformFeedbackPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceTransformFeedbackPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits and properties.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32', 'Vulkan.Core10.BaseType.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTransformFeedbackPropertiesEXT = PhysicalDeviceTransformFeedbackPropertiesEXT
  { -- | @maxTransformFeedbackStreams@ is the maximum number of vertex streams
    -- that can be output from geometry shaders declared with the
    -- @GeometryStreams@ capability. If the implementation does not support
    -- 'PhysicalDeviceTransformFeedbackFeaturesEXT'::@geometryStreams@ then
    -- @maxTransformFeedbackStreams@ /must/ be set to @1@.
    maxTransformFeedbackStreams :: Word32
  , -- | @maxTransformFeedbackBuffers@ is the maximum number of transform
    -- feedback buffers that can be bound for capturing shader outputs from the
    -- last vertex processing stage.
    maxTransformFeedbackBuffers :: Word32
  , -- | @maxTransformFeedbackBufferSize@ is the maximum size that can be
    -- specified when binding a buffer for transform feedback in
    -- 'cmdBindTransformFeedbackBuffersEXT'.
    maxTransformFeedbackBufferSize :: DeviceSize
  , -- | @maxTransformFeedbackStreamDataSize@ is the maximum amount of data in
    -- bytes for each vertex that captured to one or more transform feedback
    -- buffers associated with a specific vertex stream.
    maxTransformFeedbackStreamDataSize :: Word32
  , -- | @maxTransformFeedbackBufferDataSize@ is the maximum amount of data in
    -- bytes for each vertex that can be captured to a specific transform
    -- feedback buffer.
    maxTransformFeedbackBufferDataSize :: Word32
  , -- | @maxTransformFeedbackBufferDataStride@ is the maximum stride between
    -- each capture of vertex data to the buffer.
    maxTransformFeedbackBufferDataStride :: Word32
  , -- | @transformFeedbackQueries@ is true if the implementation supports the
    -- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
    -- query type. @transformFeedbackQueries@ is false if queries of this type
    -- /cannot/ be created.
    transformFeedbackQueries :: Bool
  , -- | @transformFeedbackStreamsLinesTriangles@ is true if the implementation
    -- supports the geometry shader @OpExecutionMode@ of @OutputLineStrip@ and
    -- @OutputTriangleStrip@ in addition to @OutputPoints@ when more than one
    -- vertex stream is output. If @transformFeedbackStreamsLinesTriangles@ is
    -- false the implementation only supports an @OpExecutionMode@ of
    -- @OutputPoints@ when more than one vertex stream is output from the
    -- geometry shader.
    transformFeedbackStreamsLinesTriangles :: Bool
  , -- | @transformFeedbackRasterizationStreamSelect@ is true if the
    -- implementation supports the @GeometryStreams@ SPIR-V capability and the
    -- application can use 'PipelineRasterizationStateStreamCreateInfoEXT' to
    -- modify which vertex stream output is used for rasterization. Otherwise
    -- vertex stream @0@ /must/ always be used for rasterization.
    transformFeedbackRasterizationStreamSelect :: Bool
  , -- | @transformFeedbackDraw@ is true if the implementation supports the
    -- 'cmdDrawIndirectByteCountEXT' function otherwise the function /must/ not
    -- be called.
    transformFeedbackDraw :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceTransformFeedbackPropertiesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackPropertiesEXT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTransformFeedbackPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxTransformFeedbackStreams)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxTransformFeedbackBuffers)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (maxTransformFeedbackBufferSize)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxTransformFeedbackStreamDataSize)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxTransformFeedbackBufferDataSize)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxTransformFeedbackBufferDataStride)
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (transformFeedbackQueries))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (transformFeedbackStreamsLinesTriangles))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (transformFeedbackRasterizationStreamSelect))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (transformFeedbackDraw))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTransformFeedbackPropertiesEXT where
  peekCStruct p = do
    maxTransformFeedbackStreams <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxTransformFeedbackBuffers <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxTransformFeedbackBufferSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    maxTransformFeedbackStreamDataSize <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxTransformFeedbackBufferDataSize <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxTransformFeedbackBufferDataStride <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    transformFeedbackQueries <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    transformFeedbackStreamsLinesTriangles <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    transformFeedbackRasterizationStreamSelect <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    transformFeedbackDraw <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    pure $ PhysicalDeviceTransformFeedbackPropertiesEXT
             maxTransformFeedbackStreams maxTransformFeedbackBuffers maxTransformFeedbackBufferSize maxTransformFeedbackStreamDataSize maxTransformFeedbackBufferDataSize maxTransformFeedbackBufferDataStride (bool32ToBool transformFeedbackQueries) (bool32ToBool transformFeedbackStreamsLinesTriangles) (bool32ToBool transformFeedbackRasterizationStreamSelect) (bool32ToBool transformFeedbackDraw)

instance Storable PhysicalDeviceTransformFeedbackPropertiesEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTransformFeedbackPropertiesEXT where
  zero = PhysicalDeviceTransformFeedbackPropertiesEXT
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


-- | VkPipelineRasterizationStateStreamCreateInfoEXT - Structure defining the
-- geometry stream used for rasterization
--
-- = Description
--
-- If this structure is not present, @rasterizationStream@ is assumed to be
-- zero.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineRasterizationStateStreamCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationStateStreamCreateInfoEXT = PipelineRasterizationStateStreamCreateInfoEXT
  { -- | @flags@ /must/ be @0@
    flags :: PipelineRasterizationStateStreamCreateFlagsEXT
  , -- | @rasterizationStream@ /must/ be zero if
    -- 'PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackRasterizationStreamSelect@
    -- is 'Vulkan.Core10.BaseType.FALSE'
    rasterizationStream :: Word32
  }
  deriving (Typeable)
deriving instance Show PipelineRasterizationStateStreamCreateInfoEXT

instance ToCStruct PipelineRasterizationStateStreamCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationStateStreamCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRasterizationStateStreamCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (rasterizationStream)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineRasterizationStateStreamCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PipelineRasterizationStateStreamCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PipelineRasterizationStateStreamCreateFlagsEXT))
    rasterizationStream <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PipelineRasterizationStateStreamCreateInfoEXT
             flags rasterizationStream

instance Storable PipelineRasterizationStateStreamCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationStateStreamCreateInfoEXT where
  zero = PipelineRasterizationStateStreamCreateInfoEXT
           zero
           zero


-- | VkPipelineRasterizationStateStreamCreateFlagsEXT - Reserved for future
-- use
--
-- = Description
--
-- 'PipelineRasterizationStateStreamCreateFlagsEXT' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'PipelineRasterizationStateStreamCreateInfoEXT'
newtype PipelineRasterizationStateStreamCreateFlagsEXT = PipelineRasterizationStateStreamCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineRasterizationStateStreamCreateFlagsEXT where
  showsPrec p = \case
    PipelineRasterizationStateStreamCreateFlagsEXT x -> showParen (p >= 11) (showString "PipelineRasterizationStateStreamCreateFlagsEXT 0x" . showHex x)

instance Read PipelineRasterizationStateStreamCreateFlagsEXT where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineRasterizationStateStreamCreateFlagsEXT")
                       v <- step readPrec
                       pure (PipelineRasterizationStateStreamCreateFlagsEXT v)))


type EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION"
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1


type EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = "VK_EXT_transform_feedback"

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME"
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = "VK_EXT_transform_feedback"

