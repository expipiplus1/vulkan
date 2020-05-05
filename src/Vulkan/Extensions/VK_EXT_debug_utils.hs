{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_debug_utils  ( setDebugUtilsObjectNameEXT
                                             , setDebugUtilsObjectTagEXT
                                             , queueBeginDebugUtilsLabelEXT
                                             , queueEndDebugUtilsLabelEXT
                                             , queueInsertDebugUtilsLabelEXT
                                             , cmdBeginDebugUtilsLabelEXT
                                             , cmdUseDebugUtilsLabelEXT
                                             , cmdEndDebugUtilsLabelEXT
                                             , cmdInsertDebugUtilsLabelEXT
                                             , createDebugUtilsMessengerEXT
                                             , withDebugUtilsMessengerEXT
                                             , destroyDebugUtilsMessengerEXT
                                             , submitDebugUtilsMessageEXT
                                             , DebugUtilsObjectNameInfoEXT(..)
                                             , DebugUtilsObjectTagInfoEXT(..)
                                             , DebugUtilsLabelEXT(..)
                                             , DebugUtilsMessengerCreateInfoEXT(..)
                                             , DebugUtilsMessengerCallbackDataEXT(..)
                                             , DebugUtilsMessengerCreateFlagsEXT(..)
                                             , DebugUtilsMessengerCallbackDataFlagsEXT(..)
                                             , DebugUtilsMessageSeverityFlagBitsEXT( DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
                                                                                   , ..
                                                                                   )
                                             , DebugUtilsMessageSeverityFlagsEXT
                                             , DebugUtilsMessageTypeFlagBitsEXT( DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                                                                               , DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                                                                               , DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
                                                                               , ..
                                                                               )
                                             , DebugUtilsMessageTypeFlagsEXT
                                             , PFN_vkDebugUtilsMessengerCallbackEXT
                                             , FN_vkDebugUtilsMessengerCallbackEXT
                                             , EXT_DEBUG_UTILS_SPEC_VERSION
                                             , pattern EXT_DEBUG_UTILS_SPEC_VERSION
                                             , EXT_DEBUG_UTILS_EXTENSION_NAME
                                             , pattern EXT_DEBUG_UTILS_EXTENSION_NAME
                                             , DebugUtilsMessengerEXT(..)
                                             ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
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
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.Handles (DebugUtilsMessengerEXT)
import Vulkan.Extensions.Handles (DebugUtilsMessengerEXT(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdInsertDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkQueueBeginDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkQueueEndDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkQueueInsertDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkSetDebugUtilsObjectNameEXT))
import Vulkan.Dynamic (DeviceCmds(pVkSetDebugUtilsObjectTagEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.BaseType (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDebugUtilsMessengerEXT))
import Vulkan.Dynamic (InstanceCmds(pVkDestroyDebugUtilsMessengerEXT))
import Vulkan.Dynamic (InstanceCmds(pVkSubmitDebugUtilsMessageEXT))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Enums.ObjectType (ObjectType)
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DebugUtilsMessengerEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetDebugUtilsObjectNameEXT
  :: FunPtr (Ptr Device_T -> Ptr DebugUtilsObjectNameInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DebugUtilsObjectNameInfoEXT -> IO Result

-- | vkSetDebugUtilsObjectNameEXT - Give a user-friendly name to an object
--
-- = Parameters
--
-- -   @device@ is the device that created the object.
--
-- -   @pNameInfo@ is a pointer to a 'DebugUtilsObjectNameInfoEXT'
--     structure specifying parameters of the name to set on the object.
--
-- == Valid Usage
--
-- -   @pNameInfo->objectType@ /must/ not be
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- -   @pNameInfo->objectHandle@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pNameInfo@ /must/ be a valid pointer to a valid
--     'DebugUtilsObjectNameInfoEXT' structure
--
-- == Host Synchronization
--
-- -   Host access to @pNameInfo->objectHandle@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'DebugUtilsObjectNameInfoEXT', 'Vulkan.Core10.Handles.Device'
setDebugUtilsObjectNameEXT :: forall io . MonadIO io => Device -> DebugUtilsObjectNameInfoEXT -> io ()
setDebugUtilsObjectNameEXT device nameInfo = liftIO . evalContT $ do
  let vkSetDebugUtilsObjectNameEXTPtr = pVkSetDebugUtilsObjectNameEXT (deviceCmds (device :: Device))
  lift $ unless (vkSetDebugUtilsObjectNameEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetDebugUtilsObjectNameEXT is null" Nothing Nothing
  let vkSetDebugUtilsObjectNameEXT' = mkVkSetDebugUtilsObjectNameEXT vkSetDebugUtilsObjectNameEXTPtr
  pNameInfo <- ContT $ withCStruct (nameInfo)
  r <- lift $ vkSetDebugUtilsObjectNameEXT' (deviceHandle (device)) pNameInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetDebugUtilsObjectTagEXT
  :: FunPtr (Ptr Device_T -> Ptr DebugUtilsObjectTagInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DebugUtilsObjectTagInfoEXT -> IO Result

-- | vkSetDebugUtilsObjectTagEXT - Attach arbitrary data to an object
--
-- = Parameters
--
-- -   @device@ is the device that created the object.
--
-- -   @pTagInfo@ is a pointer to a 'DebugUtilsObjectTagInfoEXT' structure
--     specifying parameters of the tag to attach to the object.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pTagInfo@ /must/ be a valid pointer to a valid
--     'DebugUtilsObjectTagInfoEXT' structure
--
-- == Host Synchronization
--
-- -   Host access to @pTagInfo->objectHandle@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'DebugUtilsObjectTagInfoEXT', 'Vulkan.Core10.Handles.Device'
setDebugUtilsObjectTagEXT :: forall io . MonadIO io => Device -> DebugUtilsObjectTagInfoEXT -> io ()
setDebugUtilsObjectTagEXT device tagInfo = liftIO . evalContT $ do
  let vkSetDebugUtilsObjectTagEXTPtr = pVkSetDebugUtilsObjectTagEXT (deviceCmds (device :: Device))
  lift $ unless (vkSetDebugUtilsObjectTagEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetDebugUtilsObjectTagEXT is null" Nothing Nothing
  let vkSetDebugUtilsObjectTagEXT' = mkVkSetDebugUtilsObjectTagEXT vkSetDebugUtilsObjectTagEXTPtr
  pTagInfo <- ContT $ withCStruct (tagInfo)
  r <- lift $ vkSetDebugUtilsObjectTagEXT' (deviceHandle (device)) pTagInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueBeginDebugUtilsLabelEXT
  :: FunPtr (Ptr Queue_T -> Ptr DebugUtilsLabelEXT -> IO ()) -> Ptr Queue_T -> Ptr DebugUtilsLabelEXT -> IO ()

-- | vkQueueBeginDebugUtilsLabelEXT - Open a queue debug label region
--
-- = Parameters
--
-- -   @queue@ is the queue in which to start a debug label region.
--
-- -   @pLabelInfo@ is a pointer to a 'DebugUtilsLabelEXT' structure
--     specifying parameters of the label region to open.
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   | -                                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'DebugUtilsLabelEXT', 'Vulkan.Core10.Handles.Queue'
queueBeginDebugUtilsLabelEXT :: forall io . MonadIO io => Queue -> ("labelInfo" ::: DebugUtilsLabelEXT) -> io ()
queueBeginDebugUtilsLabelEXT queue labelInfo = liftIO . evalContT $ do
  let vkQueueBeginDebugUtilsLabelEXTPtr = pVkQueueBeginDebugUtilsLabelEXT (deviceCmds (queue :: Queue))
  lift $ unless (vkQueueBeginDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueBeginDebugUtilsLabelEXT is null" Nothing Nothing
  let vkQueueBeginDebugUtilsLabelEXT' = mkVkQueueBeginDebugUtilsLabelEXT vkQueueBeginDebugUtilsLabelEXTPtr
  pLabelInfo <- ContT $ withCStruct (labelInfo)
  lift $ vkQueueBeginDebugUtilsLabelEXT' (queueHandle (queue)) pLabelInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueEndDebugUtilsLabelEXT
  :: FunPtr (Ptr Queue_T -> IO ()) -> Ptr Queue_T -> IO ()

-- | vkQueueEndDebugUtilsLabelEXT - Close a queue debug label region
--
-- = Parameters
--
-- -   @queue@ is the queue in which a debug label region should be closed.
--
-- = Description
--
-- The calls to 'queueBeginDebugUtilsLabelEXT' and
-- 'queueEndDebugUtilsLabelEXT' /must/ be matched and balanced.
--
-- == Valid Usage
--
-- -   There /must/ be an outstanding 'queueBeginDebugUtilsLabelEXT'
--     command prior to the 'queueEndDebugUtilsLabelEXT' on the queue
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid 'Vulkan.Core10.Handles.Queue' handle
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   | -                                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Queue'
queueEndDebugUtilsLabelEXT :: forall io . MonadIO io => Queue -> io ()
queueEndDebugUtilsLabelEXT queue = liftIO $ do
  let vkQueueEndDebugUtilsLabelEXTPtr = pVkQueueEndDebugUtilsLabelEXT (deviceCmds (queue :: Queue))
  unless (vkQueueEndDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueEndDebugUtilsLabelEXT is null" Nothing Nothing
  let vkQueueEndDebugUtilsLabelEXT' = mkVkQueueEndDebugUtilsLabelEXT vkQueueEndDebugUtilsLabelEXTPtr
  vkQueueEndDebugUtilsLabelEXT' (queueHandle (queue))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueInsertDebugUtilsLabelEXT
  :: FunPtr (Ptr Queue_T -> Ptr DebugUtilsLabelEXT -> IO ()) -> Ptr Queue_T -> Ptr DebugUtilsLabelEXT -> IO ()

-- | vkQueueInsertDebugUtilsLabelEXT - Insert a label into a queue
--
-- = Parameters
--
-- -   @queue@ is the queue into which a debug label will be inserted.
--
-- -   @pLabelInfo@ is a pointer to a 'DebugUtilsLabelEXT' structure
--     specifying parameters of the label to insert.
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   | -                                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'DebugUtilsLabelEXT', 'Vulkan.Core10.Handles.Queue'
queueInsertDebugUtilsLabelEXT :: forall io . MonadIO io => Queue -> ("labelInfo" ::: DebugUtilsLabelEXT) -> io ()
queueInsertDebugUtilsLabelEXT queue labelInfo = liftIO . evalContT $ do
  let vkQueueInsertDebugUtilsLabelEXTPtr = pVkQueueInsertDebugUtilsLabelEXT (deviceCmds (queue :: Queue))
  lift $ unless (vkQueueInsertDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueInsertDebugUtilsLabelEXT is null" Nothing Nothing
  let vkQueueInsertDebugUtilsLabelEXT' = mkVkQueueInsertDebugUtilsLabelEXT vkQueueInsertDebugUtilsLabelEXTPtr
  pLabelInfo <- ContT $ withCStruct (labelInfo)
  lift $ vkQueueInsertDebugUtilsLabelEXT' (queueHandle (queue)) pLabelInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginDebugUtilsLabelEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DebugUtilsLabelEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr DebugUtilsLabelEXT -> IO ()

-- | vkCmdBeginDebugUtilsLabelEXT - Open a command buffer debug label region
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pLabelInfo@ is a pointer to a 'DebugUtilsLabelEXT' structure
--     specifying parameters of the label region to open.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pLabelInfo@ /must/ be a valid pointer to a valid
--     'DebugUtilsLabelEXT' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- == Host Synchronization
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
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DebugUtilsLabelEXT'
cmdBeginDebugUtilsLabelEXT :: forall io . MonadIO io => CommandBuffer -> ("labelInfo" ::: DebugUtilsLabelEXT) -> io ()
cmdBeginDebugUtilsLabelEXT commandBuffer labelInfo = liftIO . evalContT $ do
  let vkCmdBeginDebugUtilsLabelEXTPtr = pVkCmdBeginDebugUtilsLabelEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginDebugUtilsLabelEXT is null" Nothing Nothing
  let vkCmdBeginDebugUtilsLabelEXT' = mkVkCmdBeginDebugUtilsLabelEXT vkCmdBeginDebugUtilsLabelEXTPtr
  pLabelInfo <- ContT $ withCStruct (labelInfo)
  lift $ vkCmdBeginDebugUtilsLabelEXT' (commandBufferHandle (commandBuffer)) pLabelInfo
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginDebugUtilsLabelEXT' and 'cmdEndDebugUtilsLabelEXT'
--
-- Note that 'cmdEndDebugUtilsLabelEXT' is *not* called if an exception is
-- thrown by the inner action.
cmdUseDebugUtilsLabelEXT :: forall io r . MonadIO io => CommandBuffer -> DebugUtilsLabelEXT -> io r -> io r
cmdUseDebugUtilsLabelEXT commandBuffer pLabelInfo a =
  (cmdBeginDebugUtilsLabelEXT commandBuffer pLabelInfo) *> a <* (cmdEndDebugUtilsLabelEXT commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndDebugUtilsLabelEXT
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdEndDebugUtilsLabelEXT - Close a command buffer label region
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- = Description
--
-- An application /may/ open a debug label region in one command buffer and
-- close it in another, or otherwise split debug label regions across
-- multiple command buffers or multiple queue submissions. When viewed from
-- the linear series of submissions to a single queue, the calls to
-- 'cmdBeginDebugUtilsLabelEXT' and 'cmdEndDebugUtilsLabelEXT' /must/ be
-- matched and balanced.
--
-- == Valid Usage
--
-- -   There /must/ be an outstanding 'cmdBeginDebugUtilsLabelEXT' command
--     prior to the 'cmdEndDebugUtilsLabelEXT' on the queue that
--     @commandBuffer@ is submitted to
--
-- -   If @commandBuffer@ is a secondary command buffer, there /must/ be an
--     outstanding 'cmdBeginDebugUtilsLabelEXT' command recorded to
--     @commandBuffer@ that has not previously been ended by a call to
--     'cmdEndDebugUtilsLabelEXT'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- == Host Synchronization
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
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdEndDebugUtilsLabelEXT :: forall io . MonadIO io => CommandBuffer -> io ()
cmdEndDebugUtilsLabelEXT commandBuffer = liftIO $ do
  let vkCmdEndDebugUtilsLabelEXTPtr = pVkCmdEndDebugUtilsLabelEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndDebugUtilsLabelEXT is null" Nothing Nothing
  let vkCmdEndDebugUtilsLabelEXT' = mkVkCmdEndDebugUtilsLabelEXT vkCmdEndDebugUtilsLabelEXTPtr
  vkCmdEndDebugUtilsLabelEXT' (commandBufferHandle (commandBuffer))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdInsertDebugUtilsLabelEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DebugUtilsLabelEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr DebugUtilsLabelEXT -> IO ()

-- | vkCmdInsertDebugUtilsLabelEXT - Insert a label into a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pInfo@ is a pointer to a 'DebugUtilsLabelEXT' structure specifying
--     parameters of the label to insert.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pLabelInfo@ /must/ be a valid pointer to a valid
--     'DebugUtilsLabelEXT' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- == Host Synchronization
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
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DebugUtilsLabelEXT'
cmdInsertDebugUtilsLabelEXT :: forall io . MonadIO io => CommandBuffer -> ("labelInfo" ::: DebugUtilsLabelEXT) -> io ()
cmdInsertDebugUtilsLabelEXT commandBuffer labelInfo = liftIO . evalContT $ do
  let vkCmdInsertDebugUtilsLabelEXTPtr = pVkCmdInsertDebugUtilsLabelEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdInsertDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdInsertDebugUtilsLabelEXT is null" Nothing Nothing
  let vkCmdInsertDebugUtilsLabelEXT' = mkVkCmdInsertDebugUtilsLabelEXT vkCmdInsertDebugUtilsLabelEXTPtr
  pLabelInfo <- ContT $ withCStruct (labelInfo)
  lift $ vkCmdInsertDebugUtilsLabelEXT' (commandBufferHandle (commandBuffer)) pLabelInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDebugUtilsMessengerEXT
  :: FunPtr (Ptr Instance_T -> Ptr DebugUtilsMessengerCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr DebugUtilsMessengerEXT -> IO Result) -> Ptr Instance_T -> Ptr DebugUtilsMessengerCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr DebugUtilsMessengerEXT -> IO Result

-- | vkCreateDebugUtilsMessengerEXT - Create a debug messenger object
--
-- = Parameters
--
-- -   @instance@ is the instance the messenger will be used with.
--
-- -   @pCreateInfo@ is a pointer to a 'DebugUtilsMessengerCreateInfoEXT'
--     structure containing the callback pointer, as well as defining
--     conditions under which this messenger will trigger the callback.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pMessenger@ is a pointer to a
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' handle in which
--     the created object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'DebugUtilsMessengerCreateInfoEXT' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pMessenger@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- The application /must/ ensure that 'createDebugUtilsMessengerEXT' is not
-- executed in parallel with any Vulkan command that is also called with
-- @instance@ or child of @instance@ as the dispatchable argument.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'DebugUtilsMessengerCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT',
-- 'Vulkan.Core10.Handles.Instance'
createDebugUtilsMessengerEXT :: forall io . MonadIO io => Instance -> DebugUtilsMessengerCreateInfoEXT -> ("allocator" ::: Maybe AllocationCallbacks) -> io (DebugUtilsMessengerEXT)
createDebugUtilsMessengerEXT instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateDebugUtilsMessengerEXTPtr = pVkCreateDebugUtilsMessengerEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateDebugUtilsMessengerEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDebugUtilsMessengerEXT is null" Nothing Nothing
  let vkCreateDebugUtilsMessengerEXT' = mkVkCreateDebugUtilsMessengerEXT vkCreateDebugUtilsMessengerEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPMessenger <- ContT $ bracket (callocBytes @DebugUtilsMessengerEXT 8) free
  r <- lift $ vkCreateDebugUtilsMessengerEXT' (instanceHandle (instance')) pCreateInfo pAllocator (pPMessenger)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMessenger <- lift $ peek @DebugUtilsMessengerEXT pPMessenger
  pure $ (pMessenger)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDebugUtilsMessengerEXT' and 'destroyDebugUtilsMessengerEXT'
--
-- To ensure that 'destroyDebugUtilsMessengerEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withDebugUtilsMessengerEXT :: forall io r . MonadIO io => Instance -> DebugUtilsMessengerCreateInfoEXT -> Maybe AllocationCallbacks -> (io (DebugUtilsMessengerEXT) -> ((DebugUtilsMessengerEXT) -> io ()) -> r) -> r
withDebugUtilsMessengerEXT instance' pCreateInfo pAllocator b =
  b (createDebugUtilsMessengerEXT instance' pCreateInfo pAllocator)
    (\(o0) -> destroyDebugUtilsMessengerEXT instance' o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDebugUtilsMessengerEXT
  :: FunPtr (Ptr Instance_T -> DebugUtilsMessengerEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Instance_T -> DebugUtilsMessengerEXT -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyDebugUtilsMessengerEXT - Destroy a debug messenger object
--
-- = Parameters
--
-- -   @instance@ is the instance where the callback was created.
--
-- -   @messenger@ is the
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' object to
--     destroy. @messenger@ is an externally synchronized object and /must/
--     not be used on more than one thread at a time. This means that
--     'destroyDebugUtilsMessengerEXT' /must/ not be called when a callback
--     is active.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @messenger@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @messenger@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   If @messenger@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @messenger@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   If @messenger@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @instance@
--
-- == Host Synchronization
--
-- -   Host access to @messenger@ /must/ be externally synchronized
--
-- The application /must/ ensure that 'destroyDebugUtilsMessengerEXT' is
-- not executed in parallel with any Vulkan command that is also called
-- with @instance@ or child of @instance@ as the dispatchable argument.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT',
-- 'Vulkan.Core10.Handles.Instance'
destroyDebugUtilsMessengerEXT :: forall io . MonadIO io => Instance -> DebugUtilsMessengerEXT -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroyDebugUtilsMessengerEXT instance' messenger allocator = liftIO . evalContT $ do
  let vkDestroyDebugUtilsMessengerEXTPtr = pVkDestroyDebugUtilsMessengerEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkDestroyDebugUtilsMessengerEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDebugUtilsMessengerEXT is null" Nothing Nothing
  let vkDestroyDebugUtilsMessengerEXT' = mkVkDestroyDebugUtilsMessengerEXT vkDestroyDebugUtilsMessengerEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyDebugUtilsMessengerEXT' (instanceHandle (instance')) (messenger) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSubmitDebugUtilsMessageEXT
  :: FunPtr (Ptr Instance_T -> DebugUtilsMessageSeverityFlagBitsEXT -> DebugUtilsMessageTypeFlagsEXT -> Ptr DebugUtilsMessengerCallbackDataEXT -> IO ()) -> Ptr Instance_T -> DebugUtilsMessageSeverityFlagBitsEXT -> DebugUtilsMessageTypeFlagsEXT -> Ptr DebugUtilsMessengerCallbackDataEXT -> IO ()

-- | vkSubmitDebugUtilsMessageEXT - Inject a message into a debug stream
--
-- = Parameters
--
-- -   @instance@ is the debug stream’s 'Vulkan.Core10.Handles.Instance'.
--
-- -   @messageSeverity@ is the 'DebugUtilsMessageSeverityFlagBitsEXT'
--     severity of this event\/message.
--
-- -   @messageTypes@ is a bitmask of 'DebugUtilsMessageTypeFlagBitsEXT'
--     specifying which type of event(s) to identify with this message.
--
-- -   @pCallbackData@ contains all the callback related data in the
--     'DebugUtilsMessengerCallbackDataEXT' structure.
--
-- = Description
--
-- The call will propagate through the layers and generate callback(s) as
-- indicated by the message’s flags. The parameters are passed on to the
-- callback in addition to the @pUserData@ value that was defined at the
-- time the messenger was registered.
--
-- == Valid Usage
--
-- -   The @objectType@ member of each element of @pCallbackData->pObjects@
--     /must/ not be 'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   @messageSeverity@ /must/ be a valid
--     'DebugUtilsMessageSeverityFlagBitsEXT' value
--
-- -   @messageTypes@ /must/ be a valid combination of
--     'DebugUtilsMessageTypeFlagBitsEXT' values
--
-- -   @messageTypes@ /must/ not be @0@
--
-- -   @pCallbackData@ /must/ be a valid pointer to a valid
--     'DebugUtilsMessengerCallbackDataEXT' structure
--
-- = See Also
--
-- 'DebugUtilsMessageSeverityFlagBitsEXT', 'DebugUtilsMessageTypeFlagsEXT',
-- 'DebugUtilsMessengerCallbackDataEXT', 'Vulkan.Core10.Handles.Instance'
submitDebugUtilsMessageEXT :: forall io . MonadIO io => Instance -> DebugUtilsMessageSeverityFlagBitsEXT -> ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT) -> DebugUtilsMessengerCallbackDataEXT -> io ()
submitDebugUtilsMessageEXT instance' messageSeverity messageTypes callbackData = liftIO . evalContT $ do
  let vkSubmitDebugUtilsMessageEXTPtr = pVkSubmitDebugUtilsMessageEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkSubmitDebugUtilsMessageEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSubmitDebugUtilsMessageEXT is null" Nothing Nothing
  let vkSubmitDebugUtilsMessageEXT' = mkVkSubmitDebugUtilsMessageEXT vkSubmitDebugUtilsMessageEXTPtr
  pCallbackData <- ContT $ withCStruct (callbackData)
  lift $ vkSubmitDebugUtilsMessageEXT' (instanceHandle (instance')) (messageSeverity) (messageTypes) pCallbackData
  pure $ ()


-- | VkDebugUtilsObjectNameInfoEXT - Specify parameters of a name to give to
-- an object
--
-- = Description
--
-- Applications /may/ change the name associated with an object simply by
-- calling 'setDebugUtilsObjectNameEXT' again with a new string. If
-- @pObjectName@ is either @NULL@ or an empty string, then any previously
-- set name is removed.
--
-- == Valid Usage
--
-- -   If @objectType@ is
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN', @objectHandle@
--     /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @objectType@ is not
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN', @objectHandle@
--     /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a valid Vulkan
--     handle of the type associated with @objectType@ as defined in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-object-types VkObjectType and Vulkan Handle Relationship>
--     table
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectType@ /must/ be a valid
--     'Vulkan.Core10.Enums.ObjectType.ObjectType' value
--
-- -   If @pObjectName@ is not @NULL@, @pObjectName@ /must/ be a
--     null-terminated UTF-8 string
--
-- = See Also
--
-- 'DebugUtilsMessengerCallbackDataEXT',
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'setDebugUtilsObjectNameEXT'
data DebugUtilsObjectNameInfoEXT = DebugUtilsObjectNameInfoEXT
  { -- | @objectType@ is a 'Vulkan.Core10.Enums.ObjectType.ObjectType' specifying
    -- the type of the object to be named.
    objectType :: ObjectType
  , -- | @objectHandle@ is the object to be named.
    objectHandle :: Word64
  , -- | @pObjectName@ is either @NULL@ or a null-terminated UTF-8 string
    -- specifying the name to apply to @objectHandle@.
    objectName :: Maybe ByteString
  }
  deriving (Typeable)
deriving instance Show DebugUtilsObjectNameInfoEXT

instance ToCStruct DebugUtilsObjectNameInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsObjectNameInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ObjectType)) (objectType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word64)) (objectHandle)
    pObjectName'' <- case (objectName) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pObjectName''
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ObjectType)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct DebugUtilsObjectNameInfoEXT where
  peekCStruct p = do
    objectType <- peek @ObjectType ((p `plusPtr` 16 :: Ptr ObjectType))
    objectHandle <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pObjectName <- peek @(Ptr CChar) ((p `plusPtr` 32 :: Ptr (Ptr CChar)))
    pObjectName' <- maybePeek (\j -> packCString  (j)) pObjectName
    pure $ DebugUtilsObjectNameInfoEXT
             objectType objectHandle pObjectName'

instance Zero DebugUtilsObjectNameInfoEXT where
  zero = DebugUtilsObjectNameInfoEXT
           zero
           zero
           Nothing


-- | VkDebugUtilsObjectTagInfoEXT - Specify parameters of a tag to attach to
-- an object
--
-- = Description
--
-- The @tagName@ parameter gives a name or identifier to the type of data
-- being tagged. This can be used by debugging layers to easily filter for
-- only data that can be used by that implementation.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'setDebugUtilsObjectTagEXT'
data DebugUtilsObjectTagInfoEXT = DebugUtilsObjectTagInfoEXT
  { -- | @objectType@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.ObjectType.ObjectType' value
    objectType :: ObjectType
  , -- | @objectHandle@ /must/ be a valid Vulkan handle of the type associated
    -- with @objectType@ as defined in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-object-types VkObjectType and Vulkan Handle Relationship>
    -- table
    objectHandle :: Word64
  , -- | @tagName@ is a numerical identifier of the tag.
    tagName :: Word64
  , -- | @tagSize@ /must/ be greater than @0@
    tagSize :: Word64
  , -- | @pTag@ /must/ be a valid pointer to an array of @tagSize@ bytes
    tag :: Ptr ()
  }
  deriving (Typeable)
deriving instance Show DebugUtilsObjectTagInfoEXT

instance ToCStruct DebugUtilsObjectTagInfoEXT where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsObjectTagInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ObjectType)) (objectType)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (objectHandle)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (tagName)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (tagSize))
    poke ((p `plusPtr` 48 :: Ptr (Ptr ()))) (tag)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ObjectType)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 48 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct DebugUtilsObjectTagInfoEXT where
  peekCStruct p = do
    objectType <- peek @ObjectType ((p `plusPtr` 16 :: Ptr ObjectType))
    objectHandle <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    tagName <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    tagSize <- peek @CSize ((p `plusPtr` 40 :: Ptr CSize))
    pTag <- peek @(Ptr ()) ((p `plusPtr` 48 :: Ptr (Ptr ())))
    pure $ DebugUtilsObjectTagInfoEXT
             objectType objectHandle tagName ((\(CSize a) -> a) tagSize) pTag

instance Storable DebugUtilsObjectTagInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DebugUtilsObjectTagInfoEXT where
  zero = DebugUtilsObjectTagInfoEXT
           zero
           zero
           zero
           zero
           zero


-- | VkDebugUtilsLabelEXT - Specify parameters of a label region
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DebugUtilsMessengerCallbackDataEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBeginDebugUtilsLabelEXT', 'cmdInsertDebugUtilsLabelEXT',
-- 'queueBeginDebugUtilsLabelEXT', 'queueInsertDebugUtilsLabelEXT'
data DebugUtilsLabelEXT = DebugUtilsLabelEXT
  { -- | @pLabelName@ /must/ be a null-terminated UTF-8 string
    labelName :: ByteString
  , -- | @color@ is an optional RGBA color value that can be associated with the
    -- label. A particular implementation /may/ choose to ignore this color
    -- value. The values contain RGBA values in order, in the range 0.0 to 1.0.
    -- If all elements in @color@ are set to 0.0 then it is ignored.
    color :: (Float, Float, Float, Float)
  }
  deriving (Typeable)
deriving instance Show DebugUtilsLabelEXT

instance ToCStruct DebugUtilsLabelEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsLabelEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pLabelName'' <- ContT $ useAsCString (labelName)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pLabelName''
    let pColor' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 4 CFloat)))
    lift $ case (color) of
      (e0, e1, e2, e3) -> do
        poke (pColor' :: Ptr CFloat) (CFloat (e0))
        poke (pColor' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pColor' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pColor' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pLabelName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pLabelName''
    lift $ f

instance FromCStruct DebugUtilsLabelEXT where
  peekCStruct p = do
    pLabelName <- packCString =<< peek ((p `plusPtr` 16 :: Ptr (Ptr CChar)))
    let pcolor = lowerArrayPtr @CFloat ((p `plusPtr` 24 :: Ptr (FixedArray 4 CFloat)))
    color0 <- peek @CFloat ((pcolor `advancePtrBytes` 0 :: Ptr CFloat))
    color1 <- peek @CFloat ((pcolor `advancePtrBytes` 4 :: Ptr CFloat))
    color2 <- peek @CFloat ((pcolor `advancePtrBytes` 8 :: Ptr CFloat))
    color3 <- peek @CFloat ((pcolor `advancePtrBytes` 12 :: Ptr CFloat))
    pure $ DebugUtilsLabelEXT
             pLabelName ((((\(CFloat a) -> a) color0), ((\(CFloat a) -> a) color1), ((\(CFloat a) -> a) color2), ((\(CFloat a) -> a) color3)))

instance Zero DebugUtilsLabelEXT where
  zero = DebugUtilsLabelEXT
           mempty
           (zero, zero, zero, zero)


-- | VkDebugUtilsMessengerCreateInfoEXT - Structure specifying parameters of
-- a newly created debug messenger
--
-- = Description
--
-- For each 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' that is
-- created the 'DebugUtilsMessengerCreateInfoEXT'::@messageSeverity@ and
-- 'DebugUtilsMessengerCreateInfoEXT'::@messageType@ determine when that
-- 'DebugUtilsMessengerCreateInfoEXT'::@pfnUserCallback@ is called. The
-- process to determine if the user’s @pfnUserCallback@ is triggered when
-- an event occurs is as follows:
--
-- 1.  The implementation will perform a bitwise AND of the event’s
--     'DebugUtilsMessageSeverityFlagBitsEXT' with the @messageSeverity@
--     provided during creation of the
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' object.
--
--     1.  If the value is 0, the message is skipped.
--
-- 2.  The implementation will perform bitwise AND of the event’s
--     'DebugUtilsMessageTypeFlagBitsEXT' with the @messageType@ provided
--     during the creation of the
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' object.
--
--     1.  If the value is 0, the message is skipped.
--
-- 3.  The callback will trigger a debug message for the current event
--
-- The callback will come directly from the component that detected the
-- event, unless some other layer intercepts the calls for its own purposes
-- (filter them in a different way, log to a system error log, etc.).
--
-- An application /can/ receive multiple callbacks if multiple
-- 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' objects are created.
-- A callback will always be executed in the same thread as the originating
-- Vulkan call.
--
-- A callback /can/ be called from multiple threads simultaneously (if the
-- application is making Vulkan calls from multiple threads).
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PFN_vkDebugUtilsMessengerCallbackEXT',
-- 'DebugUtilsMessageSeverityFlagsEXT', 'DebugUtilsMessageTypeFlagsEXT',
-- 'DebugUtilsMessengerCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDebugUtilsMessengerEXT'
data DebugUtilsMessengerCreateInfoEXT = DebugUtilsMessengerCreateInfoEXT
  { -- | @flags@ /must/ be @0@
    flags :: DebugUtilsMessengerCreateFlagsEXT
  , -- | @messageSeverity@ /must/ not be @0@
    messageSeverity :: DebugUtilsMessageSeverityFlagsEXT
  , -- | @messageType@ /must/ not be @0@
    messageType :: DebugUtilsMessageTypeFlagsEXT
  , -- | @pfnUserCallback@ /must/ be a valid
    -- 'PFN_vkDebugUtilsMessengerCallbackEXT' value
    pfnUserCallback :: PFN_vkDebugUtilsMessengerCallbackEXT
  , -- | @pUserData@ is user data to be passed to the callback.
    userData :: Ptr ()
  }
  deriving (Typeable)
deriving instance Show DebugUtilsMessengerCreateInfoEXT

instance ToCStruct DebugUtilsMessengerCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsMessengerCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugUtilsMessengerCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr DebugUtilsMessageSeverityFlagsEXT)) (messageSeverity)
    poke ((p `plusPtr` 24 :: Ptr DebugUtilsMessageTypeFlagsEXT)) (messageType)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkDebugUtilsMessengerCallbackEXT)) (pfnUserCallback)
    poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr DebugUtilsMessageSeverityFlagsEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DebugUtilsMessageTypeFlagsEXT)) (zero)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkDebugUtilsMessengerCallbackEXT)) (zero)
    f

instance FromCStruct DebugUtilsMessengerCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @DebugUtilsMessengerCreateFlagsEXT ((p `plusPtr` 16 :: Ptr DebugUtilsMessengerCreateFlagsEXT))
    messageSeverity <- peek @DebugUtilsMessageSeverityFlagsEXT ((p `plusPtr` 20 :: Ptr DebugUtilsMessageSeverityFlagsEXT))
    messageType <- peek @DebugUtilsMessageTypeFlagsEXT ((p `plusPtr` 24 :: Ptr DebugUtilsMessageTypeFlagsEXT))
    pfnUserCallback <- peek @PFN_vkDebugUtilsMessengerCallbackEXT ((p `plusPtr` 32 :: Ptr PFN_vkDebugUtilsMessengerCallbackEXT))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ DebugUtilsMessengerCreateInfoEXT
             flags messageSeverity messageType pfnUserCallback pUserData

instance Storable DebugUtilsMessengerCreateInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DebugUtilsMessengerCreateInfoEXT where
  zero = DebugUtilsMessengerCreateInfoEXT
           zero
           zero
           zero
           zero
           zero


-- | VkDebugUtilsMessengerCallbackDataEXT - Structure specifying parameters
-- returned to the callback
--
-- = Description
--
-- Note
--
-- This structure should only be considered valid during the lifetime of
-- the triggered callback.
--
-- Since adding queue and command buffer labels behaves like pushing and
-- popping onto a stack, the order of both @pQueueLabels@ and
-- @pCmdBufLabels@ is based on the order the labels were defined. The
-- result is that the first label in either @pQueueLabels@ or
-- @pCmdBufLabels@ will be the first defined (and therefore the oldest)
-- while the last label in each list will be the most recent.
--
-- Note
--
-- @pQueueLabels@ will only be non-@NULL@ if one of the objects in
-- @pObjects@ can be related directly to a defined
-- 'Vulkan.Core10.Handles.Queue' which has had one or more labels
-- associated with it.
--
-- Likewise, @pCmdBufLabels@ will only be non-@NULL@ if one of the objects
-- in @pObjects@ can be related directly to a defined
-- 'Vulkan.Core10.Handles.CommandBuffer' which has had one or more labels
-- associated with it. Additionally, while command buffer labels allow for
-- beginning and ending across different command buffers, the debug
-- messaging framework /cannot/ guarantee that labels in @pCmdBufLables@
-- will contain those defined outside of the associated command buffer.
-- This is partially due to the fact that the association of one command
-- buffer with another may not have been defined at the time the debug
-- message is triggered.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   If @pMessageIdName@ is not @NULL@, @pMessageIdName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   @pMessage@ /must/ be a null-terminated UTF-8 string
--
-- -   If @queueLabelCount@ is not @0@, @pQueueLabels@ /must/ be a valid
--     pointer to an array of @queueLabelCount@ valid 'DebugUtilsLabelEXT'
--     structures
--
-- -   If @cmdBufLabelCount@ is not @0@, @pCmdBufLabels@ /must/ be a valid
--     pointer to an array of @cmdBufLabelCount@ valid 'DebugUtilsLabelEXT'
--     structures
--
-- -   If @objectCount@ is not @0@, @pObjects@ /must/ be a valid pointer to
--     an array of @objectCount@ valid 'DebugUtilsObjectNameInfoEXT'
--     structures
--
-- = See Also
--
-- 'DebugUtilsLabelEXT', 'DebugUtilsMessengerCallbackDataFlagsEXT',
-- 'DebugUtilsObjectNameInfoEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'submitDebugUtilsMessageEXT'
data DebugUtilsMessengerCallbackDataEXT = DebugUtilsMessengerCallbackDataEXT
  { -- | @flags@ is @0@ and is reserved for future use.
    flags :: DebugUtilsMessengerCallbackDataFlagsEXT
  , -- | @pMessageIdName@ is a null-terminated string that identifies the
    -- particular message ID that is associated with the provided message. If
    -- the message corresponds to a validation layer message, then this string
    -- may contain the portion of the Vulkan specification that is believed to
    -- have been violated.
    messageIdName :: Maybe ByteString
  , -- | @messageIdNumber@ is the ID number of the triggering message. If the
    -- message corresponds to a validation layer message, then this number is
    -- related to the internal number associated with the message being
    -- triggered.
    messageIdNumber :: Int32
  , -- | @pMessage@ is a null-terminated string detailing the trigger conditions.
    message :: ByteString
  , -- | @pQueueLabels@ is @NULL@ or a pointer to an array of
    -- 'DebugUtilsLabelEXT' active in the current 'Vulkan.Core10.Handles.Queue'
    -- at the time the callback was triggered. Refer to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-queue-labels Queue Labels>
    -- for more information.
    queueLabels :: Vector DebugUtilsLabelEXT
  , -- | @pCmdBufLabels@ is @NULL@ or a pointer to an array of
    -- 'DebugUtilsLabelEXT' active in the current
    -- 'Vulkan.Core10.Handles.CommandBuffer' at the time the callback was
    -- triggered. Refer to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-command-buffer-labels Command Buffer Labels>
    -- for more information.
    cmdBufLabels :: Vector DebugUtilsLabelEXT
  , -- | @pObjects@ is a pointer to an array of 'DebugUtilsObjectNameInfoEXT'
    -- objects related to the detected issue. The array is roughly in order or
    -- importance, but the 0th element is always guaranteed to be the most
    -- important object for this message.
    objects :: Vector DebugUtilsObjectNameInfoEXT
  }
  deriving (Typeable)
deriving instance Show DebugUtilsMessengerCallbackDataEXT

instance ToCStruct DebugUtilsMessengerCallbackDataEXT where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsMessengerCallbackDataEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DebugUtilsMessengerCallbackDataFlagsEXT)) (flags)
    pMessageIdName'' <- case (messageIdName) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) pMessageIdName''
    lift $ poke ((p `plusPtr` 32 :: Ptr Int32)) (messageIdNumber)
    pMessage'' <- ContT $ useAsCString (message)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr CChar))) pMessage''
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueLabels)) :: Word32))
    pPQueueLabels' <- ContT $ allocaBytesAligned @DebugUtilsLabelEXT ((Data.Vector.length (queueLabels)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPQueueLabels' `plusPtr` (40 * (i)) :: Ptr DebugUtilsLabelEXT) (e) . ($ ())) (queueLabels)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr DebugUtilsLabelEXT))) (pPQueueLabels')
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (cmdBufLabels)) :: Word32))
    pPCmdBufLabels' <- ContT $ allocaBytesAligned @DebugUtilsLabelEXT ((Data.Vector.length (cmdBufLabels)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCmdBufLabels' `plusPtr` (40 * (i)) :: Ptr DebugUtilsLabelEXT) (e) . ($ ())) (cmdBufLabels)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr DebugUtilsLabelEXT))) (pPCmdBufLabels')
    lift $ poke ((p `plusPtr` 80 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (objects)) :: Word32))
    pPObjects' <- ContT $ allocaBytesAligned @DebugUtilsObjectNameInfoEXT ((Data.Vector.length (objects)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPObjects' `plusPtr` (40 * (i)) :: Ptr DebugUtilsObjectNameInfoEXT) (e) . ($ ())) (objects)
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr DebugUtilsObjectNameInfoEXT))) (pPObjects')
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pMessage'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr CChar))) pMessage''
    pPQueueLabels' <- ContT $ allocaBytesAligned @DebugUtilsLabelEXT ((Data.Vector.length (mempty)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPQueueLabels' `plusPtr` (40 * (i)) :: Ptr DebugUtilsLabelEXT) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr DebugUtilsLabelEXT))) (pPQueueLabels')
    pPCmdBufLabels' <- ContT $ allocaBytesAligned @DebugUtilsLabelEXT ((Data.Vector.length (mempty)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCmdBufLabels' `plusPtr` (40 * (i)) :: Ptr DebugUtilsLabelEXT) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr DebugUtilsLabelEXT))) (pPCmdBufLabels')
    pPObjects' <- ContT $ allocaBytesAligned @DebugUtilsObjectNameInfoEXT ((Data.Vector.length (mempty)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPObjects' `plusPtr` (40 * (i)) :: Ptr DebugUtilsObjectNameInfoEXT) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr DebugUtilsObjectNameInfoEXT))) (pPObjects')
    lift $ f

instance FromCStruct DebugUtilsMessengerCallbackDataEXT where
  peekCStruct p = do
    flags <- peek @DebugUtilsMessengerCallbackDataFlagsEXT ((p `plusPtr` 16 :: Ptr DebugUtilsMessengerCallbackDataFlagsEXT))
    pMessageIdName <- peek @(Ptr CChar) ((p `plusPtr` 24 :: Ptr (Ptr CChar)))
    pMessageIdName' <- maybePeek (\j -> packCString  (j)) pMessageIdName
    messageIdNumber <- peek @Int32 ((p `plusPtr` 32 :: Ptr Int32))
    pMessage <- packCString =<< peek ((p `plusPtr` 40 :: Ptr (Ptr CChar)))
    queueLabelCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pQueueLabels <- peek @(Ptr DebugUtilsLabelEXT) ((p `plusPtr` 56 :: Ptr (Ptr DebugUtilsLabelEXT)))
    pQueueLabels' <- generateM (fromIntegral queueLabelCount) (\i -> peekCStruct @DebugUtilsLabelEXT ((pQueueLabels `advancePtrBytes` (40 * (i)) :: Ptr DebugUtilsLabelEXT)))
    cmdBufLabelCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pCmdBufLabels <- peek @(Ptr DebugUtilsLabelEXT) ((p `plusPtr` 72 :: Ptr (Ptr DebugUtilsLabelEXT)))
    pCmdBufLabels' <- generateM (fromIntegral cmdBufLabelCount) (\i -> peekCStruct @DebugUtilsLabelEXT ((pCmdBufLabels `advancePtrBytes` (40 * (i)) :: Ptr DebugUtilsLabelEXT)))
    objectCount <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    pObjects <- peek @(Ptr DebugUtilsObjectNameInfoEXT) ((p `plusPtr` 88 :: Ptr (Ptr DebugUtilsObjectNameInfoEXT)))
    pObjects' <- generateM (fromIntegral objectCount) (\i -> peekCStruct @DebugUtilsObjectNameInfoEXT ((pObjects `advancePtrBytes` (40 * (i)) :: Ptr DebugUtilsObjectNameInfoEXT)))
    pure $ DebugUtilsMessengerCallbackDataEXT
             flags pMessageIdName' messageIdNumber pMessage pQueueLabels' pCmdBufLabels' pObjects'

instance Zero DebugUtilsMessengerCallbackDataEXT where
  zero = DebugUtilsMessengerCallbackDataEXT
           zero
           Nothing
           zero
           mempty
           mempty
           mempty
           mempty


-- No documentation found for TopLevel "VkDebugUtilsMessengerCreateFlagsEXT"
newtype DebugUtilsMessengerCreateFlagsEXT = DebugUtilsMessengerCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show DebugUtilsMessengerCreateFlagsEXT where
  showsPrec p = \case
    DebugUtilsMessengerCreateFlagsEXT x -> showParen (p >= 11) (showString "DebugUtilsMessengerCreateFlagsEXT 0x" . showHex x)

instance Read DebugUtilsMessengerCreateFlagsEXT where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "DebugUtilsMessengerCreateFlagsEXT")
                       v <- step readPrec
                       pure (DebugUtilsMessengerCreateFlagsEXT v)))


-- No documentation found for TopLevel "VkDebugUtilsMessengerCallbackDataFlagsEXT"
newtype DebugUtilsMessengerCallbackDataFlagsEXT = DebugUtilsMessengerCallbackDataFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show DebugUtilsMessengerCallbackDataFlagsEXT where
  showsPrec p = \case
    DebugUtilsMessengerCallbackDataFlagsEXT x -> showParen (p >= 11) (showString "DebugUtilsMessengerCallbackDataFlagsEXT 0x" . showHex x)

instance Read DebugUtilsMessengerCallbackDataFlagsEXT where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "DebugUtilsMessengerCallbackDataFlagsEXT")
                       v <- step readPrec
                       pure (DebugUtilsMessengerCallbackDataFlagsEXT v)))


-- | VkDebugUtilsMessageSeverityFlagBitsEXT - Bitmask specifying which
-- severities of events cause a debug messenger callback
--
-- = See Also
--
-- 'DebugUtilsMessageSeverityFlagsEXT', 'submitDebugUtilsMessageEXT'
newtype DebugUtilsMessageSeverityFlagBitsEXT = DebugUtilsMessageSeverityFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT' specifies the most
-- verbose output indicating all diagnostic messages from the Vulkan
-- loader, layers, and drivers should be captured.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x00000001
-- | 'DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT' specifies an informational
-- message such as resource details that may be handy when debugging an
-- application.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x00000010
-- | 'DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT' specifies use of Vulkan
-- that /may/ expose an app bug. Such cases may not be immediately harmful,
-- such as a fragment shader outputting to a location with no attachment.
-- Other cases /may/ point to behavior that is almost certainly bad when
-- unintended such as using an image whose memory has not been filled. In
-- general if you see a warning but you know that the behavior is
-- intended\/desired, then simply ignore the warning.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x00000100
-- | 'DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT' specifies that the
-- application has violated a valid usage condition of the specification.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x00001000

type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

instance Show DebugUtilsMessageSeverityFlagBitsEXT where
  showsPrec p = \case
    DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT -> showString "DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT"
    DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT -> showString "DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT"
    DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT -> showString "DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT"
    DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT -> showString "DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT"
    DebugUtilsMessageSeverityFlagBitsEXT x -> showParen (p >= 11) (showString "DebugUtilsMessageSeverityFlagBitsEXT 0x" . showHex x)

instance Read DebugUtilsMessageSeverityFlagBitsEXT where
  readPrec = parens (choose [("DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT", pure DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT)
                            , ("DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT", pure DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT)
                            , ("DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT", pure DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT)
                            , ("DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT", pure DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DebugUtilsMessageSeverityFlagBitsEXT")
                       v <- step readPrec
                       pure (DebugUtilsMessageSeverityFlagBitsEXT v)))


-- | VkDebugUtilsMessageTypeFlagBitsEXT - Bitmask specifying which types of
-- events cause a debug messenger callback
--
-- = See Also
--
-- 'DebugUtilsMessageTypeFlagsEXT'
newtype DebugUtilsMessageTypeFlagBitsEXT = DebugUtilsMessageTypeFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT' specifies that some general
-- event has occurred. This is typically a non-specification,
-- non-performance event.
pattern DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT = DebugUtilsMessageTypeFlagBitsEXT 0x00000001
-- | 'DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT' specifies that something
-- has occurred during validation against the Vulkan specification that may
-- indicate invalid behavior.
pattern DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT = DebugUtilsMessageTypeFlagBitsEXT 0x00000002
-- | 'DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT' specifies a potentially
-- non-optimal use of Vulkan, e.g. using
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage' when setting
-- 'Vulkan.Core10.Pass.AttachmentDescription'::@loadOp@ to
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR' would
-- have worked.
pattern DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = DebugUtilsMessageTypeFlagBitsEXT 0x00000004

type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT

instance Show DebugUtilsMessageTypeFlagBitsEXT where
  showsPrec p = \case
    DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT -> showString "DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT"
    DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT -> showString "DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT"
    DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT -> showString "DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT"
    DebugUtilsMessageTypeFlagBitsEXT x -> showParen (p >= 11) (showString "DebugUtilsMessageTypeFlagBitsEXT 0x" . showHex x)

instance Read DebugUtilsMessageTypeFlagBitsEXT where
  readPrec = parens (choose [("DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT", pure DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT)
                            , ("DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT", pure DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT)
                            , ("DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT", pure DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DebugUtilsMessageTypeFlagBitsEXT")
                       v <- step readPrec
                       pure (DebugUtilsMessageTypeFlagBitsEXT v)))


type FN_vkDebugUtilsMessengerCallbackEXT = DebugUtilsMessageSeverityFlagBitsEXT -> ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr DebugUtilsMessengerCallbackDataEXT) -> ("pUserData" ::: Ptr ()) -> IO Bool32
-- | PFN_vkDebugUtilsMessengerCallbackEXT - Application-defined debug
-- messenger callback function
--
-- = Parameters
--
-- -   @messageSeverity@ specifies the
--     'DebugUtilsMessageSeverityFlagBitsEXT' that triggered this callback.
--
-- -   @messageTypes@ is a bitmask of 'DebugUtilsMessageTypeFlagBitsEXT'
--     specifying which type of event(s) triggered this callback.
--
-- -   @pCallbackData@ contains all the callback related data in the
--     'DebugUtilsMessengerCallbackDataEXT' structure.
--
-- -   @pUserData@ is the user data provided when the
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' was created.
--
-- = Description
--
-- The callback /must/ not call 'destroyDebugUtilsMessengerEXT'.
--
-- The callback returns a 'Vulkan.Core10.BaseType.Bool32', which is
-- interpreted in a layer-specified manner. The application /should/ always
-- return 'Vulkan.Core10.BaseType.FALSE'. The 'Vulkan.Core10.BaseType.TRUE'
-- value is reserved for use in layer development.
--
-- = See Also
--
-- 'DebugUtilsMessengerCreateInfoEXT'
type PFN_vkDebugUtilsMessengerCallbackEXT = FunPtr FN_vkDebugUtilsMessengerCallbackEXT


type EXT_DEBUG_UTILS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_SPEC_VERSION"
pattern EXT_DEBUG_UTILS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEBUG_UTILS_SPEC_VERSION = 2


type EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"

-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
pattern EXT_DEBUG_UTILS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"

