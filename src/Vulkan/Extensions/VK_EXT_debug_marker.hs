{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_debug_marker  ( debugMarkerSetObjectNameEXT
                                              , debugMarkerSetObjectTagEXT
                                              , cmdDebugMarkerBeginEXT
                                              , cmdDebugMarkerEndEXT
                                              , cmdDebugMarkerInsertEXT
                                              , DebugMarkerObjectNameInfoEXT(..)
                                              , DebugMarkerObjectTagInfoEXT(..)
                                              , DebugMarkerMarkerInfoEXT(..)
                                              , EXT_DEBUG_MARKER_SPEC_VERSION
                                              , pattern EXT_DEBUG_MARKER_SPEC_VERSION
                                              , EXT_DEBUG_MARKER_EXTENSION_NAME
                                              , pattern EXT_DEBUG_MARKER_EXTENSION_NAME
                                              , DebugReportObjectTypeEXT(..)
                                              ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDebugMarkerBeginEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDebugMarkerEndEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDebugMarkerInsertEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDebugMarkerSetObjectNameEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDebugMarkerSetObjectTagEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugMarkerSetObjectNameEXT
  :: FunPtr (Ptr Device_T -> Ptr DebugMarkerObjectNameInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DebugMarkerObjectNameInfoEXT -> IO Result

-- | vkDebugMarkerSetObjectNameEXT - Give a user-friendly name to an object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDebugMarkerSetObjectNameEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDebugMarkerSetObjectNameEXT-pNameInfo-parameter# @pNameInfo@
--     /must/ be a valid pointer to a valid 'DebugMarkerObjectNameInfoEXT'
--     structure
--
-- == Host Synchronization
--
-- -   Host access to @pNameInfo->object@ /must/ be externally synchronized
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
-- 'DebugMarkerObjectNameInfoEXT', 'Vulkan.Core10.Handles.Device'
debugMarkerSetObjectNameEXT :: forall io
                             . (MonadIO io)
                            => -- | @device@ is the device that created the object.
                               Device
                            -> -- | @pNameInfo@ is a pointer to a 'DebugMarkerObjectNameInfoEXT' structure
                               -- specifying the parameters of the name to set on the object.
                               DebugMarkerObjectNameInfoEXT
                            -> io ()
debugMarkerSetObjectNameEXT device nameInfo = liftIO . evalContT $ do
  let vkDebugMarkerSetObjectNameEXTPtr = pVkDebugMarkerSetObjectNameEXT (deviceCmds (device :: Device))
  lift $ unless (vkDebugMarkerSetObjectNameEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDebugMarkerSetObjectNameEXT is null" Nothing Nothing
  let vkDebugMarkerSetObjectNameEXT' = mkVkDebugMarkerSetObjectNameEXT vkDebugMarkerSetObjectNameEXTPtr
  pNameInfo <- ContT $ withCStruct (nameInfo)
  r <- lift $ vkDebugMarkerSetObjectNameEXT' (deviceHandle (device)) pNameInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugMarkerSetObjectTagEXT
  :: FunPtr (Ptr Device_T -> Ptr DebugMarkerObjectTagInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DebugMarkerObjectTagInfoEXT -> IO Result

-- | vkDebugMarkerSetObjectTagEXT - Attach arbitrary data to an object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDebugMarkerSetObjectTagEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDebugMarkerSetObjectTagEXT-pTagInfo-parameter# @pTagInfo@
--     /must/ be a valid pointer to a valid 'DebugMarkerObjectTagInfoEXT'
--     structure
--
-- == Host Synchronization
--
-- -   Host access to @pTagInfo->object@ /must/ be externally synchronized
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
-- 'DebugMarkerObjectTagInfoEXT', 'Vulkan.Core10.Handles.Device'
debugMarkerSetObjectTagEXT :: forall io
                            . (MonadIO io)
                           => -- | @device@ is the device that created the object.
                              Device
                           -> -- | @pTagInfo@ is a pointer to a 'DebugMarkerObjectTagInfoEXT' structure
                              -- specifying the parameters of the tag to attach to the object.
                              DebugMarkerObjectTagInfoEXT
                           -> io ()
debugMarkerSetObjectTagEXT device tagInfo = liftIO . evalContT $ do
  let vkDebugMarkerSetObjectTagEXTPtr = pVkDebugMarkerSetObjectTagEXT (deviceCmds (device :: Device))
  lift $ unless (vkDebugMarkerSetObjectTagEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDebugMarkerSetObjectTagEXT is null" Nothing Nothing
  let vkDebugMarkerSetObjectTagEXT' = mkVkDebugMarkerSetObjectTagEXT vkDebugMarkerSetObjectTagEXTPtr
  pTagInfo <- ContT $ withCStruct (tagInfo)
  r <- lift $ vkDebugMarkerSetObjectTagEXT' (deviceHandle (device)) pTagInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerBeginEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DebugMarkerMarkerInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr DebugMarkerMarkerInfoEXT -> IO ()

-- | vkCmdDebugMarkerBeginEXT - Open a command buffer marker region
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDebugMarkerBeginEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDebugMarkerBeginEXT-pMarkerInfo-parameter# @pMarkerInfo@
--     /must/ be a valid pointer to a valid 'DebugMarkerMarkerInfoEXT'
--     structure
--
-- -   #VUID-vkCmdDebugMarkerBeginEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDebugMarkerBeginEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
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
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DebugMarkerMarkerInfoEXT'
cmdDebugMarkerBeginEXT :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command is
                          -- recorded.
                          CommandBuffer
                       -> -- | @pMarkerInfo@ is a pointer to a 'DebugMarkerMarkerInfoEXT' structure
                          -- specifying the parameters of the marker region to open.
                          DebugMarkerMarkerInfoEXT
                       -> io ()
cmdDebugMarkerBeginEXT commandBuffer markerInfo = liftIO . evalContT $ do
  let vkCmdDebugMarkerBeginEXTPtr = pVkCmdDebugMarkerBeginEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdDebugMarkerBeginEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDebugMarkerBeginEXT is null" Nothing Nothing
  let vkCmdDebugMarkerBeginEXT' = mkVkCmdDebugMarkerBeginEXT vkCmdDebugMarkerBeginEXTPtr
  pMarkerInfo <- ContT $ withCStruct (markerInfo)
  lift $ vkCmdDebugMarkerBeginEXT' (commandBufferHandle (commandBuffer)) pMarkerInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerEndEXT
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdDebugMarkerEndEXT - Close a command buffer marker region
--
-- = Description
--
-- An application /may/ open a marker region in one command buffer and
-- close it in another, or otherwise split marker regions across multiple
-- command buffers or multiple queue submissions. When viewed from the
-- linear series of submissions to a single queue, the calls to
-- 'cmdDebugMarkerBeginEXT' and 'cmdDebugMarkerEndEXT' /must/ be matched
-- and balanced.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDebugMarkerEndEXT-commandBuffer-01239# There /must/ be an
--     outstanding 'cmdDebugMarkerBeginEXT' command prior to the
--     'cmdDebugMarkerEndEXT' on the queue that @commandBuffer@ is
--     submitted to
--
-- -   #VUID-vkCmdDebugMarkerEndEXT-commandBuffer-01240# If @commandBuffer@
--     is a secondary command buffer, there /must/ be an outstanding
--     'cmdDebugMarkerBeginEXT' command recorded to @commandBuffer@ that
--     has not previously been ended by a call to 'cmdDebugMarkerEndEXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDebugMarkerEndEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDebugMarkerEndEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDebugMarkerEndEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
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
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdDebugMarkerEndEXT :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command is
                        -- recorded.
                        CommandBuffer
                     -> io ()
cmdDebugMarkerEndEXT commandBuffer = liftIO $ do
  let vkCmdDebugMarkerEndEXTPtr = pVkCmdDebugMarkerEndEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDebugMarkerEndEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDebugMarkerEndEXT is null" Nothing Nothing
  let vkCmdDebugMarkerEndEXT' = mkVkCmdDebugMarkerEndEXT vkCmdDebugMarkerEndEXTPtr
  vkCmdDebugMarkerEndEXT' (commandBufferHandle (commandBuffer))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerInsertEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DebugMarkerMarkerInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr DebugMarkerMarkerInfoEXT -> IO ()

-- | vkCmdDebugMarkerInsertEXT - Insert a marker label into a command buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDebugMarkerInsertEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDebugMarkerInsertEXT-pMarkerInfo-parameter# @pMarkerInfo@
--     /must/ be a valid pointer to a valid 'DebugMarkerMarkerInfoEXT'
--     structure
--
-- -   #VUID-vkCmdDebugMarkerInsertEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDebugMarkerInsertEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
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
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DebugMarkerMarkerInfoEXT'
cmdDebugMarkerInsertEXT :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command is
                           -- recorded.
                           CommandBuffer
                        -> -- | @pMarkerInfo@ is a pointer to a 'DebugMarkerMarkerInfoEXT' structure
                           -- specifying the parameters of the marker to insert.
                           DebugMarkerMarkerInfoEXT
                        -> io ()
cmdDebugMarkerInsertEXT commandBuffer markerInfo = liftIO . evalContT $ do
  let vkCmdDebugMarkerInsertEXTPtr = pVkCmdDebugMarkerInsertEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdDebugMarkerInsertEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDebugMarkerInsertEXT is null" Nothing Nothing
  let vkCmdDebugMarkerInsertEXT' = mkVkCmdDebugMarkerInsertEXT vkCmdDebugMarkerInsertEXTPtr
  pMarkerInfo <- ContT $ withCStruct (markerInfo)
  lift $ vkCmdDebugMarkerInsertEXT' (commandBufferHandle (commandBuffer)) pMarkerInfo
  pure $ ()


-- | VkDebugMarkerObjectNameInfoEXT - Specify parameters of a name to give to
-- an object
--
-- = Description
--
-- Applications /may/ change the name associated with an object simply by
-- calling 'debugMarkerSetObjectNameEXT' again with a new string. To remove
-- a previously set name, @pObjectName@ /should/ be set to an empty string.
--
-- == Valid Usage
--
-- -   #VUID-VkDebugMarkerObjectNameInfoEXT-objectType-01490# @objectType@
--     /must/ not be
--     'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT'
--
-- -   #VUID-VkDebugMarkerObjectNameInfoEXT-object-01491# @object@ /must/
--     not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkDebugMarkerObjectNameInfoEXT-object-01492# @object@ /must/
--     be a Vulkan object of the type associated with @objectType@ as
--     defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debug-report-object-types>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDebugMarkerObjectNameInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT'
--
-- -   #VUID-VkDebugMarkerObjectNameInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDebugMarkerObjectNameInfoEXT-objectType-parameter#
--     @objectType@ /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT'
--     value
--
-- -   #VUID-VkDebugMarkerObjectNameInfoEXT-pObjectName-parameter#
--     @pObjectName@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'debugMarkerSetObjectNameEXT'
data DebugMarkerObjectNameInfoEXT = DebugMarkerObjectNameInfoEXT
  { -- | @objectType@ is a
    -- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT'
    -- specifying the type of the object to be named.
    objectType :: DebugReportObjectTypeEXT
  , -- | @object@ is the object to be named.
    object :: Word64
  , -- | @pObjectName@ is a null-terminated UTF-8 string specifying the name to
    -- apply to @object@.
    objectName :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugMarkerObjectNameInfoEXT)
#endif
deriving instance Show DebugMarkerObjectNameInfoEXT

instance ToCStruct DebugMarkerObjectNameInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugMarkerObjectNameInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT)) (objectType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word64)) (object)
    pObjectName'' <- ContT $ useAsCString (objectName)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pObjectName''
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    pObjectName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pObjectName''
    lift $ f

instance FromCStruct DebugMarkerObjectNameInfoEXT where
  peekCStruct p = do
    objectType <- peek @DebugReportObjectTypeEXT ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT))
    object <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pObjectName <- packCString =<< peek ((p `plusPtr` 32 :: Ptr (Ptr CChar)))
    pure $ DebugMarkerObjectNameInfoEXT
             objectType object pObjectName

instance Zero DebugMarkerObjectNameInfoEXT where
  zero = DebugMarkerObjectNameInfoEXT
           zero
           zero
           mempty


-- | VkDebugMarkerObjectTagInfoEXT - Specify parameters of a tag to attach to
-- an object
--
-- = Description
--
-- The @tagName@ parameter gives a name or identifier to the type of data
-- being tagged. This can be used by debugging layers to easily filter for
-- only data that can be used by that implementation.
--
-- == Valid Usage
--
-- -   #VUID-VkDebugMarkerObjectTagInfoEXT-objectType-01493# @objectType@
--     /must/ not be
--     'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT'
--
-- -   #VUID-VkDebugMarkerObjectTagInfoEXT-object-01494# @object@ /must/
--     not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkDebugMarkerObjectTagInfoEXT-object-01495# @object@ /must/ be
--     a Vulkan object of the type associated with @objectType@ as defined
--     in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debug-report-object-types>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDebugMarkerObjectTagInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT'
--
-- -   #VUID-VkDebugMarkerObjectTagInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDebugMarkerObjectTagInfoEXT-objectType-parameter#
--     @objectType@ /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT'
--     value
--
-- -   #VUID-VkDebugMarkerObjectTagInfoEXT-pTag-parameter# @pTag@ /must/ be
--     a valid pointer to an array of @tagSize@ bytes
--
-- -   #VUID-VkDebugMarkerObjectTagInfoEXT-tagSize-arraylength# @tagSize@
--     /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'debugMarkerSetObjectTagEXT'
data DebugMarkerObjectTagInfoEXT = DebugMarkerObjectTagInfoEXT
  { -- | @objectType@ is a
    -- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT'
    -- specifying the type of the object to be named.
    objectType :: DebugReportObjectTypeEXT
  , -- | @object@ is the object to be tagged.
    object :: Word64
  , -- | @tagName@ is a numerical identifier of the tag.
    tagName :: Word64
  , -- | @tagSize@ is the number of bytes of data to attach to the object.
    tagSize :: Word64
  , -- | @pTag@ is a pointer to an array of @tagSize@ bytes containing the data
    -- to be associated with the object.
    tag :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugMarkerObjectTagInfoEXT)
#endif
deriving instance Show DebugMarkerObjectTagInfoEXT

instance ToCStruct DebugMarkerObjectTagInfoEXT where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugMarkerObjectTagInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT)) (objectType)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (object)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (tagName)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (tagSize))
    poke ((p `plusPtr` 48 :: Ptr (Ptr ()))) (tag)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 48 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct DebugMarkerObjectTagInfoEXT where
  peekCStruct p = do
    objectType <- peek @DebugReportObjectTypeEXT ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT))
    object <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    tagName <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    tagSize <- peek @CSize ((p `plusPtr` 40 :: Ptr CSize))
    pTag <- peek @(Ptr ()) ((p `plusPtr` 48 :: Ptr (Ptr ())))
    pure $ DebugMarkerObjectTagInfoEXT
             objectType object tagName ((\(CSize a) -> a) tagSize) pTag

instance Storable DebugMarkerObjectTagInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DebugMarkerObjectTagInfoEXT where
  zero = DebugMarkerObjectTagInfoEXT
           zero
           zero
           zero
           zero
           zero


-- | VkDebugMarkerMarkerInfoEXT - Specify parameters of a command buffer
-- marker region
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDebugMarkerMarkerInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT'
--
-- -   #VUID-VkDebugMarkerMarkerInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDebugMarkerMarkerInfoEXT-pMarkerName-parameter#
--     @pMarkerName@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdDebugMarkerBeginEXT', 'cmdDebugMarkerInsertEXT'
data DebugMarkerMarkerInfoEXT = DebugMarkerMarkerInfoEXT
  { -- | @pMarkerName@ is a pointer to a null-terminated UTF-8 string containing
    -- the name of the marker.
    markerName :: ByteString
  , -- | @color@ is an /optional/ RGBA color value that can be associated with
    -- the marker. A particular implementation /may/ choose to ignore this
    -- color value. The values contain RGBA values in order, in the range 0.0
    -- to 1.0. If all elements in @color@ are set to 0.0 then it is ignored.
    color :: (Float, Float, Float, Float)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugMarkerMarkerInfoEXT)
#endif
deriving instance Show DebugMarkerMarkerInfoEXT

instance ToCStruct DebugMarkerMarkerInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugMarkerMarkerInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pMarkerName'' <- ContT $ useAsCString (markerName)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pMarkerName''
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pMarkerName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pMarkerName''
    lift $ f

instance FromCStruct DebugMarkerMarkerInfoEXT where
  peekCStruct p = do
    pMarkerName <- packCString =<< peek ((p `plusPtr` 16 :: Ptr (Ptr CChar)))
    let pcolor = lowerArrayPtr @CFloat ((p `plusPtr` 24 :: Ptr (FixedArray 4 CFloat)))
    color0 <- peek @CFloat ((pcolor `advancePtrBytes` 0 :: Ptr CFloat))
    color1 <- peek @CFloat ((pcolor `advancePtrBytes` 4 :: Ptr CFloat))
    color2 <- peek @CFloat ((pcolor `advancePtrBytes` 8 :: Ptr CFloat))
    color3 <- peek @CFloat ((pcolor `advancePtrBytes` 12 :: Ptr CFloat))
    pure $ DebugMarkerMarkerInfoEXT
             pMarkerName ((((\(CFloat a) -> a) color0), ((\(CFloat a) -> a) color1), ((\(CFloat a) -> a) color2), ((\(CFloat a) -> a) color3)))

instance Zero DebugMarkerMarkerInfoEXT where
  zero = DebugMarkerMarkerInfoEXT
           mempty
           (zero, zero, zero, zero)


type EXT_DEBUG_MARKER_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_SPEC_VERSION"
pattern EXT_DEBUG_MARKER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEBUG_MARKER_SPEC_VERSION = 4


type EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"

-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_EXTENSION_NAME"
pattern EXT_DEBUG_MARKER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"

