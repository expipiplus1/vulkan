{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_debug_utils"
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
                                             , DebugUtilsMessageSeverityFlagsEXT
                                             , DebugUtilsMessageSeverityFlagBitsEXT( DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
                                                                                   , ..
                                                                                   )
                                             , DebugUtilsMessageTypeFlagsEXT
                                             , DebugUtilsMessageTypeFlagBitsEXT( DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                                                                               , DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                                                                               , DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
                                                                               , ..
                                                                               )
                                             , PFN_vkDebugUtilsMessengerCallbackEXT
                                             , FN_vkDebugUtilsMessengerCallbackEXT
                                             , EXT_DEBUG_UTILS_SPEC_VERSION
                                             , pattern EXT_DEBUG_UTILS_SPEC_VERSION
                                             , EXT_DEBUG_UTILS_EXTENSION_NAME
                                             , pattern EXT_DEBUG_UTILS_EXTENSION_NAME
                                             , DebugUtilsMessengerEXT(..)
                                             ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showString)
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
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
import Vulkan.Core10.FundamentalTypes (Flags)
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

-- No documentation found for TopLevel "vkSetDebugUtilsObjectNameEXT"
setDebugUtilsObjectNameEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkSetDebugUtilsObjectNameEXT" "device"
                              Device
                           -> -- No documentation found for Nested "vkSetDebugUtilsObjectNameEXT" "pNameInfo"
                              DebugUtilsObjectNameInfoEXT
                           -> io ()
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

-- No documentation found for TopLevel "vkSetDebugUtilsObjectTagEXT"
setDebugUtilsObjectTagEXT :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkSetDebugUtilsObjectTagEXT" "device"
                             Device
                          -> -- No documentation found for Nested "vkSetDebugUtilsObjectTagEXT" "pTagInfo"
                             DebugUtilsObjectTagInfoEXT
                          -> io ()
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

-- No documentation found for TopLevel "vkQueueBeginDebugUtilsLabelEXT"
queueBeginDebugUtilsLabelEXT :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkQueueBeginDebugUtilsLabelEXT" "queue"
                                Queue
                             -> -- No documentation found for Nested "vkQueueBeginDebugUtilsLabelEXT" "pLabelInfo"
                                ("labelInfo" ::: DebugUtilsLabelEXT)
                             -> io ()
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

-- No documentation found for TopLevel "vkQueueEndDebugUtilsLabelEXT"
queueEndDebugUtilsLabelEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkQueueEndDebugUtilsLabelEXT" "queue"
                              Queue
                           -> io ()
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

-- No documentation found for TopLevel "vkQueueInsertDebugUtilsLabelEXT"
queueInsertDebugUtilsLabelEXT :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkQueueInsertDebugUtilsLabelEXT" "queue"
                                 Queue
                              -> -- No documentation found for Nested "vkQueueInsertDebugUtilsLabelEXT" "pLabelInfo"
                                 ("labelInfo" ::: DebugUtilsLabelEXT)
                              -> io ()
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

-- No documentation found for TopLevel "vkCmdBeginDebugUtilsLabelEXT"
cmdBeginDebugUtilsLabelEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkCmdBeginDebugUtilsLabelEXT" "commandBuffer"
                              CommandBuffer
                           -> -- No documentation found for Nested "vkCmdBeginDebugUtilsLabelEXT" "pLabelInfo"
                              ("labelInfo" ::: DebugUtilsLabelEXT)
                           -> io ()
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

-- No documentation found for TopLevel "vkCmdEndDebugUtilsLabelEXT"
cmdEndDebugUtilsLabelEXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCmdEndDebugUtilsLabelEXT" "commandBuffer"
                            CommandBuffer
                         -> io ()
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

-- No documentation found for TopLevel "vkCmdInsertDebugUtilsLabelEXT"
cmdInsertDebugUtilsLabelEXT :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkCmdInsertDebugUtilsLabelEXT" "commandBuffer"
                               CommandBuffer
                            -> -- No documentation found for Nested "vkCmdInsertDebugUtilsLabelEXT" "pLabelInfo"
                               ("labelInfo" ::: DebugUtilsLabelEXT)
                            -> io ()
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

-- No documentation found for TopLevel "vkCreateDebugUtilsMessengerEXT"
createDebugUtilsMessengerEXT :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCreateDebugUtilsMessengerEXT" "instance"
                                Instance
                             -> -- No documentation found for Nested "vkCreateDebugUtilsMessengerEXT" "pCreateInfo"
                                DebugUtilsMessengerCreateInfoEXT
                             -> -- No documentation found for Nested "vkCreateDebugUtilsMessengerEXT" "pAllocator"
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (DebugUtilsMessengerEXT)
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
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDebugUtilsMessengerEXT :: forall io r . MonadIO io => Instance -> DebugUtilsMessengerCreateInfoEXT -> Maybe AllocationCallbacks -> (io DebugUtilsMessengerEXT -> (DebugUtilsMessengerEXT -> io ()) -> r) -> r
withDebugUtilsMessengerEXT instance' pCreateInfo pAllocator b =
  b (createDebugUtilsMessengerEXT instance' pCreateInfo pAllocator)
    (\(o0) -> destroyDebugUtilsMessengerEXT instance' o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDebugUtilsMessengerEXT
  :: FunPtr (Ptr Instance_T -> DebugUtilsMessengerEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Instance_T -> DebugUtilsMessengerEXT -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyDebugUtilsMessengerEXT"
destroyDebugUtilsMessengerEXT :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkDestroyDebugUtilsMessengerEXT" "instance"
                                 Instance
                              -> -- No documentation found for Nested "vkDestroyDebugUtilsMessengerEXT" "messenger"
                                 DebugUtilsMessengerEXT
                              -> -- No documentation found for Nested "vkDestroyDebugUtilsMessengerEXT" "pAllocator"
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io ()
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

-- No documentation found for TopLevel "vkSubmitDebugUtilsMessageEXT"
submitDebugUtilsMessageEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkSubmitDebugUtilsMessageEXT" "instance"
                              Instance
                           -> -- No documentation found for Nested "vkSubmitDebugUtilsMessageEXT" "messageSeverity"
                              DebugUtilsMessageSeverityFlagBitsEXT
                           -> -- No documentation found for Nested "vkSubmitDebugUtilsMessageEXT" "messageTypes"
                              ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT)
                           -> -- No documentation found for Nested "vkSubmitDebugUtilsMessageEXT" "pCallbackData"
                              DebugUtilsMessengerCallbackDataEXT
                           -> io ()
submitDebugUtilsMessageEXT instance' messageSeverity messageTypes callbackData = liftIO . evalContT $ do
  let vkSubmitDebugUtilsMessageEXTPtr = pVkSubmitDebugUtilsMessageEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkSubmitDebugUtilsMessageEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSubmitDebugUtilsMessageEXT is null" Nothing Nothing
  let vkSubmitDebugUtilsMessageEXT' = mkVkSubmitDebugUtilsMessageEXT vkSubmitDebugUtilsMessageEXTPtr
  pCallbackData <- ContT $ withCStruct (callbackData)
  lift $ vkSubmitDebugUtilsMessageEXT' (instanceHandle (instance')) (messageSeverity) (messageTypes) pCallbackData
  pure $ ()



-- No documentation found for TopLevel "VkDebugUtilsObjectNameInfoEXT"
data DebugUtilsObjectNameInfoEXT = DebugUtilsObjectNameInfoEXT
  { -- No documentation found for Nested "VkDebugUtilsObjectNameInfoEXT" "objectType"
    objectType :: ObjectType
  , -- No documentation found for Nested "VkDebugUtilsObjectNameInfoEXT" "objectHandle"
    objectHandle :: Word64
  , -- No documentation found for Nested "VkDebugUtilsObjectNameInfoEXT" "pObjectName"
    objectName :: Maybe ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsObjectNameInfoEXT)
#endif
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



-- No documentation found for TopLevel "VkDebugUtilsObjectTagInfoEXT"
data DebugUtilsObjectTagInfoEXT = DebugUtilsObjectTagInfoEXT
  { -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "objectType"
    objectType :: ObjectType
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "objectHandle"
    objectHandle :: Word64
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "tagName"
    tagName :: Word64
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "tagSize"
    tagSize :: Word64
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "pTag"
    tag :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsObjectTagInfoEXT)
#endif
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



-- No documentation found for TopLevel "VkDebugUtilsLabelEXT"
data DebugUtilsLabelEXT = DebugUtilsLabelEXT
  { -- No documentation found for Nested "VkDebugUtilsLabelEXT" "pLabelName"
    labelName :: ByteString
  , -- No documentation found for Nested "VkDebugUtilsLabelEXT" "color"
    color :: (Float, Float, Float, Float)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsLabelEXT)
#endif
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



-- No documentation found for TopLevel "VkDebugUtilsMessengerCreateInfoEXT"
data DebugUtilsMessengerCreateInfoEXT = DebugUtilsMessengerCreateInfoEXT
  { -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "flags"
    flags :: DebugUtilsMessengerCreateFlagsEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "messageSeverity"
    messageSeverity :: DebugUtilsMessageSeverityFlagsEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "messageType"
    messageType :: DebugUtilsMessageTypeFlagsEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "pfnUserCallback"
    pfnUserCallback :: PFN_vkDebugUtilsMessengerCallbackEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "pUserData"
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsMessengerCreateInfoEXT)
#endif
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



-- No documentation found for TopLevel "VkDebugUtilsMessengerCallbackDataEXT"
data DebugUtilsMessengerCallbackDataEXT = DebugUtilsMessengerCallbackDataEXT
  { -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "flags"
    flags :: DebugUtilsMessengerCallbackDataFlagsEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pMessageIdName"
    messageIdName :: Maybe ByteString
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "messageIdNumber"
    messageIdNumber :: Int32
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pMessage"
    message :: ByteString
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pQueueLabels"
    queueLabels :: Vector DebugUtilsLabelEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pCmdBufLabels"
    cmdBufLabels :: Vector DebugUtilsLabelEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pObjects"
    objects :: Vector DebugUtilsObjectNameInfoEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsMessengerCallbackDataEXT)
#endif
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
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDebugUtilsMessengerCreateFlagsEXT :: String
conNameDebugUtilsMessengerCreateFlagsEXT = "DebugUtilsMessengerCreateFlagsEXT"

enumPrefixDebugUtilsMessengerCreateFlagsEXT :: String
enumPrefixDebugUtilsMessengerCreateFlagsEXT = ""

showTableDebugUtilsMessengerCreateFlagsEXT :: [(DebugUtilsMessengerCreateFlagsEXT, String)]
showTableDebugUtilsMessengerCreateFlagsEXT = []


instance Show DebugUtilsMessengerCreateFlagsEXT where
showsPrec = enumShowsPrec enumPrefixDebugUtilsMessengerCreateFlagsEXT
                          showTableDebugUtilsMessengerCreateFlagsEXT
                          conNameDebugUtilsMessengerCreateFlagsEXT
                          (\(DebugUtilsMessengerCreateFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DebugUtilsMessengerCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixDebugUtilsMessengerCreateFlagsEXT
                          showTableDebugUtilsMessengerCreateFlagsEXT
                          conNameDebugUtilsMessengerCreateFlagsEXT
                          DebugUtilsMessengerCreateFlagsEXT


-- No documentation found for TopLevel "VkDebugUtilsMessengerCallbackDataFlagsEXT"
newtype DebugUtilsMessengerCallbackDataFlagsEXT = DebugUtilsMessengerCallbackDataFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDebugUtilsMessengerCallbackDataFlagsEXT :: String
conNameDebugUtilsMessengerCallbackDataFlagsEXT = "DebugUtilsMessengerCallbackDataFlagsEXT"

enumPrefixDebugUtilsMessengerCallbackDataFlagsEXT :: String
enumPrefixDebugUtilsMessengerCallbackDataFlagsEXT = ""

showTableDebugUtilsMessengerCallbackDataFlagsEXT :: [(DebugUtilsMessengerCallbackDataFlagsEXT, String)]
showTableDebugUtilsMessengerCallbackDataFlagsEXT = []


instance Show DebugUtilsMessengerCallbackDataFlagsEXT where
showsPrec = enumShowsPrec enumPrefixDebugUtilsMessengerCallbackDataFlagsEXT
                          showTableDebugUtilsMessengerCallbackDataFlagsEXT
                          conNameDebugUtilsMessengerCallbackDataFlagsEXT
                          (\(DebugUtilsMessengerCallbackDataFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DebugUtilsMessengerCallbackDataFlagsEXT where
  readPrec = enumReadPrec enumPrefixDebugUtilsMessengerCallbackDataFlagsEXT
                          showTableDebugUtilsMessengerCallbackDataFlagsEXT
                          conNameDebugUtilsMessengerCallbackDataFlagsEXT
                          DebugUtilsMessengerCallbackDataFlagsEXT


type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessageSeverityFlagBitsEXT"
newtype DebugUtilsMessageSeverityFlagBitsEXT = DebugUtilsMessageSeverityFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDebugUtilsMessageSeverityFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x00000001
-- No documentation found for Nested "VkDebugUtilsMessageSeverityFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT    = DebugUtilsMessageSeverityFlagBitsEXT 0x00000010
-- No documentation found for Nested "VkDebugUtilsMessageSeverityFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x00000100
-- No documentation found for Nested "VkDebugUtilsMessageSeverityFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT   = DebugUtilsMessageSeverityFlagBitsEXT 0x00001000

conNameDebugUtilsMessageSeverityFlagBitsEXT :: String
conNameDebugUtilsMessageSeverityFlagBitsEXT = "DebugUtilsMessageSeverityFlagBitsEXT"

enumPrefixDebugUtilsMessageSeverityFlagBitsEXT :: String
enumPrefixDebugUtilsMessageSeverityFlagBitsEXT = "DEBUG_UTILS_MESSAGE_SEVERITY_"

showTableDebugUtilsMessageSeverityFlagBitsEXT :: [(DebugUtilsMessageSeverityFlagBitsEXT, String)]
showTableDebugUtilsMessageSeverityFlagBitsEXT =
  [ (DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT, "VERBOSE_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT   , "INFO_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT, "WARNING_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT  , "ERROR_BIT_EXT")
  ]


instance Show DebugUtilsMessageSeverityFlagBitsEXT where
showsPrec = enumShowsPrec enumPrefixDebugUtilsMessageSeverityFlagBitsEXT
                          showTableDebugUtilsMessageSeverityFlagBitsEXT
                          conNameDebugUtilsMessageSeverityFlagBitsEXT
                          (\(DebugUtilsMessageSeverityFlagBitsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DebugUtilsMessageSeverityFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixDebugUtilsMessageSeverityFlagBitsEXT
                          showTableDebugUtilsMessageSeverityFlagBitsEXT
                          conNameDebugUtilsMessageSeverityFlagBitsEXT
                          DebugUtilsMessageSeverityFlagBitsEXT


type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessageTypeFlagBitsEXT"
newtype DebugUtilsMessageTypeFlagBitsEXT = DebugUtilsMessageTypeFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDebugUtilsMessageTypeFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT     = DebugUtilsMessageTypeFlagBitsEXT 0x00000001
-- No documentation found for Nested "VkDebugUtilsMessageTypeFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT  = DebugUtilsMessageTypeFlagBitsEXT 0x00000002
-- No documentation found for Nested "VkDebugUtilsMessageTypeFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = DebugUtilsMessageTypeFlagBitsEXT 0x00000004

conNameDebugUtilsMessageTypeFlagBitsEXT :: String
conNameDebugUtilsMessageTypeFlagBitsEXT = "DebugUtilsMessageTypeFlagBitsEXT"

enumPrefixDebugUtilsMessageTypeFlagBitsEXT :: String
enumPrefixDebugUtilsMessageTypeFlagBitsEXT = "DEBUG_UTILS_MESSAGE_TYPE_"

showTableDebugUtilsMessageTypeFlagBitsEXT :: [(DebugUtilsMessageTypeFlagBitsEXT, String)]
showTableDebugUtilsMessageTypeFlagBitsEXT =
  [ (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT    , "GENERAL_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT , "VALIDATION_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT, "PERFORMANCE_BIT_EXT")
  ]


instance Show DebugUtilsMessageTypeFlagBitsEXT where
showsPrec = enumShowsPrec enumPrefixDebugUtilsMessageTypeFlagBitsEXT
                          showTableDebugUtilsMessageTypeFlagBitsEXT
                          conNameDebugUtilsMessageTypeFlagBitsEXT
                          (\(DebugUtilsMessageTypeFlagBitsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DebugUtilsMessageTypeFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixDebugUtilsMessageTypeFlagBitsEXT
                          showTableDebugUtilsMessageTypeFlagBitsEXT
                          conNameDebugUtilsMessageTypeFlagBitsEXT
                          DebugUtilsMessageTypeFlagBitsEXT


type FN_vkDebugUtilsMessengerCallbackEXT = DebugUtilsMessageSeverityFlagBitsEXT -> ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr DebugUtilsMessengerCallbackDataEXT) -> ("pUserData" ::: Ptr ()) -> IO Bool32
-- No documentation found for TopLevel "PFN_vkDebugUtilsMessengerCallbackEXT"
type PFN_vkDebugUtilsMessengerCallbackEXT = FunPtr FN_vkDebugUtilsMessengerCallbackEXT


type EXT_DEBUG_UTILS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_SPEC_VERSION"
pattern EXT_DEBUG_UTILS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEBUG_UTILS_SPEC_VERSION = 2


type EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"

-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
pattern EXT_DEBUG_UTILS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"

