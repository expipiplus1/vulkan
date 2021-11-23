{-# language CPP #-}
-- | = Name
--
-- XR_KHR_vulkan_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_enable  XR_KHR_vulkan_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 26
--
-- = Revision
--
-- 7
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingVulkanKHR', 'GraphicsRequirementsVulkanKHR',
-- 'SwapchainImageVulkanKHR', 'getVulkanDeviceExtensionsKHR',
-- 'getVulkanGraphicsDeviceKHR', 'getVulkanGraphicsRequirementsKHR',
-- 'getVulkanInstanceExtensionsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_vulkan_enable  ( getVulkanInstanceExtensionsKHR
                                               , getVulkanDeviceExtensionsKHR
                                               , getVulkanGraphicsDeviceKHR
                                               , getVulkanGraphicsRequirementsKHR
                                               , GraphicsBindingVulkanKHR(..)
                                               , SwapchainImageVulkanKHR(..)
                                               , GraphicsRequirementsVulkanKHR(..)
                                               , KHR_vulkan_enable_SPEC_VERSION
                                               , pattern KHR_vulkan_enable_SPEC_VERSION
                                               , KHR_VULKAN_ENABLE_EXTENSION_NAME
                                               , pattern KHR_VULKAN_ENABLE_EXTENSION_NAME
                                               ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import qualified OpenXR.VulkanTypes (Device_T)
import qualified OpenXR.VulkanTypes (Image)
import qualified OpenXR.VulkanTypes (Instance_T)
import qualified OpenXR.VulkanTypes (PhysicalDevice_T)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CChar(..))
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
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
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.NamedType ((:::))
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Core10.Handles (Instance(Instance))
import OpenXR.Dynamic (InstanceCmds(pXrGetVulkanDeviceExtensionsKHR))
import OpenXR.Dynamic (InstanceCmds(pXrGetVulkanGraphicsDeviceKHR))
import OpenXR.Dynamic (InstanceCmds(pXrGetVulkanGraphicsRequirementsKHR))
import OpenXR.Dynamic (InstanceCmds(pXrGetVulkanInstanceExtensionsKHR))
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Core10.Image (IsSwapchainImage(..))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Image (SwapchainImageBaseHeader(..))
import OpenXR.Core10.Device (SystemId)
import OpenXR.Core10.Device (SystemId(..))
import OpenXR.Version (Version)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_VULKAN_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetVulkanInstanceExtensionsKHR
  :: FunPtr (Ptr Instance_T -> SystemId -> Word32 -> Ptr Word32 -> Ptr CChar -> IO Result) -> Ptr Instance_T -> SystemId -> Word32 -> Ptr Word32 -> Ptr CChar -> IO Result

-- | xrGetVulkanInstanceExtensionsKHR - Get list of required Vulkan instance
-- extensions for an OpenXR instance and system
--
-- == Parameter Descriptions
--
-- -   @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
--     created with 'OpenXR.Core10.Instance.createInstance'.
--
-- -   @systemId@ is an
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
--     handle for the system which will be used to create a session.
--
-- -   @bufferCapacityInput@ is the capacity of the @buffer@, or 0 to
--     indicate a request to retrieve the required capacity.
--
-- -   @bufferCountOutput@ is a pointer to the count of characters written
--     (including terminating @\\0@), or a pointer to the required capacity
--     in the case that @bufferCapacityInput@ is 0.
--
-- -   @buffer@ is a pointer to an array of characters, but /can/ be @NULL@
--     if @bufferCapacityInput@ is 0. The format of the output is a single
--     space (ASCII @0x20@) delimited string of extension names.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @buffer@ size.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetVulkanInstanceExtensionsKHR-extension-notenabled# The
--     @XR_KHR_vulkan_enable@ extension /must/ be enabled prior to calling
--     'getVulkanInstanceExtensionsKHR'
--
-- -   #VUID-xrGetVulkanInstanceExtensionsKHR-instance-parameter#
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetVulkanInstanceExtensionsKHR-bufferCountOutput-parameter#
--     @bufferCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrGetVulkanInstanceExtensionsKHR-buffer-parameter# If
--     @bufferCapacityInput@ is not @0@, @buffer@ /must/ be a pointer to an
--     array of @bufferCapacityInput@ null-terminated UTF-8 strings
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'getVulkanDeviceExtensionsKHR'
getVulkanInstanceExtensionsKHR :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "xrGetVulkanInstanceExtensionsKHR" "instance"
                                  Instance
                               -> -- No documentation found for Nested "xrGetVulkanInstanceExtensionsKHR" "systemId"
                                  SystemId
                               -> io (("buffer" ::: ByteString))
getVulkanInstanceExtensionsKHR instance' systemId = liftIO . evalContT $ do
  let xrGetVulkanInstanceExtensionsKHRPtr = pXrGetVulkanInstanceExtensionsKHR (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (xrGetVulkanInstanceExtensionsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetVulkanInstanceExtensionsKHR is null" Nothing Nothing
  let xrGetVulkanInstanceExtensionsKHR' = mkXrGetVulkanInstanceExtensionsKHR xrGetVulkanInstanceExtensionsKHRPtr
  let instance'' = instanceHandle (instance')
  pBufferCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrGetVulkanInstanceExtensionsKHR" (xrGetVulkanInstanceExtensionsKHR' instance'' (systemId) (0) (pBufferCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  bufferCountOutput <- lift $ peek @Word32 pBufferCountOutput
  pBuffer <- ContT $ bracket (callocBytes @CChar (fromIntegral (bufferCountOutput))) free
  r' <- lift $ traceAroundEvent "xrGetVulkanInstanceExtensionsKHR" (xrGetVulkanInstanceExtensionsKHR' instance'' (systemId) ((bufferCountOutput)) (pBufferCountOutput) (pBuffer))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  buffer' <- lift $ packCString pBuffer
  pure $ (buffer')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetVulkanDeviceExtensionsKHR
  :: FunPtr (Ptr Instance_T -> SystemId -> Word32 -> Ptr Word32 -> Ptr CChar -> IO Result) -> Ptr Instance_T -> SystemId -> Word32 -> Ptr Word32 -> Ptr CChar -> IO Result

-- | xrGetVulkanDeviceExtensionsKHR - Get list of required Vulkan device
-- extensions for an OpenXR instance and system
--
-- == Parameter Descriptions
--
-- -   @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
--     created with 'OpenXR.Core10.Instance.createInstance'.
--
-- -   @systemId@ is an
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
--     handle for the system which will be used to create a session.
--
-- -   @bufferCapacityInput@ is the capacity of the @buffer@, or 0 to
--     indicate a request to retrieve the required capacity.
--
-- -   @bufferCountOutput@ is a pointer to the count of characters written
--     (including terminating @\\0@), or a pointer to the required capacity
--     in the case that @bufferCapacityInput@ is 0.
--
-- -   @buffer@ is a pointer to an array of characters, but /can/ be @NULL@
--     if @bufferCapacityInput@ is 0. The format of the output is a single
--     space (ASCII @0x20@) delimited string of extension names.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @buffer@ size.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetVulkanDeviceExtensionsKHR-extension-notenabled# The
--     @XR_KHR_vulkan_enable@ extension /must/ be enabled prior to calling
--     'getVulkanDeviceExtensionsKHR'
--
-- -   #VUID-xrGetVulkanDeviceExtensionsKHR-instance-parameter# @instance@
--     /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetVulkanDeviceExtensionsKHR-bufferCountOutput-parameter#
--     @bufferCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrGetVulkanDeviceExtensionsKHR-buffer-parameter# If
--     @bufferCapacityInput@ is not @0@, @buffer@ /must/ be a pointer to an
--     array of @bufferCapacityInput@ null-terminated UTF-8 strings
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'getVulkanInstanceExtensionsKHR'
getVulkanDeviceExtensionsKHR :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "xrGetVulkanDeviceExtensionsKHR" "instance"
                                Instance
                             -> -- No documentation found for Nested "xrGetVulkanDeviceExtensionsKHR" "systemId"
                                SystemId
                             -> io (("buffer" ::: ByteString))
getVulkanDeviceExtensionsKHR instance' systemId = liftIO . evalContT $ do
  let xrGetVulkanDeviceExtensionsKHRPtr = pXrGetVulkanDeviceExtensionsKHR (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (xrGetVulkanDeviceExtensionsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetVulkanDeviceExtensionsKHR is null" Nothing Nothing
  let xrGetVulkanDeviceExtensionsKHR' = mkXrGetVulkanDeviceExtensionsKHR xrGetVulkanDeviceExtensionsKHRPtr
  let instance'' = instanceHandle (instance')
  pBufferCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrGetVulkanDeviceExtensionsKHR" (xrGetVulkanDeviceExtensionsKHR' instance'' (systemId) (0) (pBufferCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  bufferCountOutput <- lift $ peek @Word32 pBufferCountOutput
  pBuffer <- ContT $ bracket (callocBytes @CChar (fromIntegral (bufferCountOutput))) free
  r' <- lift $ traceAroundEvent "xrGetVulkanDeviceExtensionsKHR" (xrGetVulkanDeviceExtensionsKHR' instance'' (systemId) ((bufferCountOutput)) (pBufferCountOutput) (pBuffer))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  buffer' <- lift $ packCString pBuffer
  pure $ (buffer')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetVulkanGraphicsDeviceKHR
  :: FunPtr (Ptr Instance_T -> SystemId -> Ptr OpenXR.VulkanTypes.Instance_T -> Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T) -> IO Result) -> Ptr Instance_T -> SystemId -> Ptr OpenXR.VulkanTypes.Instance_T -> Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T) -> IO Result

-- | xrGetVulkanGraphicsDeviceKHR - Retrieve the Vulkan physical device
-- associated with an OpenXR instance and system
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'getVulkanGraphicsDeviceKHR' function identifies to the application what
-- graphics device (Vulkan @VkPhysicalDevice@) needs to be used.
-- 'getVulkanGraphicsDeviceKHR' /must/ be called prior to calling
-- 'OpenXR.Core10.Device.createSession', and the @VkPhysicalDevice@ that
-- 'getVulkanGraphicsDeviceKHR' returns should be passed to
-- 'OpenXR.Core10.Device.createSession' in the 'GraphicsBindingVulkanKHR'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetVulkanGraphicsDeviceKHR-extension-notenabled# The
--     @XR_KHR_vulkan_enable@ extension /must/ be enabled prior to calling
--     'getVulkanGraphicsDeviceKHR'
--
-- -   #VUID-xrGetVulkanGraphicsDeviceKHR-instance-parameter# @instance@
--     /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetVulkanGraphicsDeviceKHR-vkInstance-parameter#
--     @vkInstance@ /must/ be a valid @VkInstance@ value
--
-- -   #VUID-xrGetVulkanGraphicsDeviceKHR-vkPhysicalDevice-parameter#
--     @vkPhysicalDevice@ /must/ be a pointer to a @VkPhysicalDevice@ value
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
getVulkanGraphicsDeviceKHR :: forall io
                            . (MonadIO io)
                           => -- | @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
                              -- created with 'OpenXR.Core10.Instance.createInstance'.
                              Instance
                           -> -- | @systemId@ is an
                              -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
                              -- handle for the system which will be used to create a session.
                              SystemId
                           -> -- | @vkInstance@ is a valid Vulkan @VkInstance@.
                              ("vkInstance" ::: Ptr OpenXR.VulkanTypes.Instance_T)
                           -> io (("vkPhysicalDevice" ::: Ptr OpenXR.VulkanTypes.PhysicalDevice_T))
getVulkanGraphicsDeviceKHR instance' systemId vkInstance = liftIO . evalContT $ do
  let xrGetVulkanGraphicsDeviceKHRPtr = pXrGetVulkanGraphicsDeviceKHR (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (xrGetVulkanGraphicsDeviceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetVulkanGraphicsDeviceKHR is null" Nothing Nothing
  let xrGetVulkanGraphicsDeviceKHR' = mkXrGetVulkanGraphicsDeviceKHR xrGetVulkanGraphicsDeviceKHRPtr
  pVkPhysicalDevice <- ContT $ bracket (callocBytes @(Ptr OpenXR.VulkanTypes.PhysicalDevice_T) 8) free
  r <- lift $ traceAroundEvent "xrGetVulkanGraphicsDeviceKHR" (xrGetVulkanGraphicsDeviceKHR' (instanceHandle (instance')) (systemId) (vkInstance) (pVkPhysicalDevice))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  vkPhysicalDevice <- lift $ peek @(Ptr OpenXR.VulkanTypes.PhysicalDevice_T) pVkPhysicalDevice
  pure $ (vkPhysicalDevice)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetVulkanGraphicsRequirementsKHR
  :: FunPtr (Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsVulkanKHR -> IO Result) -> Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsVulkanKHR -> IO Result

-- | xrGetVulkanGraphicsRequirementsKHR - Retrieve the Vulkan version
-- requirements for an instance and system
--
-- == Parameter Descriptions
--
-- = Description
--
-- The 'getVulkanGraphicsRequirementsKHR' function identifies to the
-- application the minimum Vulkan version requirement and the highest known
-- tested Vulkan version. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING'
-- ('OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE' /may/ be returned
-- due to legacy behavior) on calls to 'OpenXR.Core10.Device.createSession'
-- if 'getVulkanGraphicsRequirementsKHR' has not been called for the same
-- @instance@ and @systemId@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetVulkanGraphicsRequirementsKHR-extension-notenabled# The
--     @XR_KHR_vulkan_enable@ extension /must/ be enabled prior to calling
--     'getVulkanGraphicsRequirementsKHR'
--
-- -   #VUID-xrGetVulkanGraphicsRequirementsKHR-instance-parameter#
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetVulkanGraphicsRequirementsKHR-graphicsRequirements-parameter#
--     @graphicsRequirements@ /must/ be a pointer to an
--     'GraphicsRequirementsVulkanKHR' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'GraphicsRequirementsVulkanKHR', 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
getVulkanGraphicsRequirementsKHR :: forall io
                                  . (MonadIO io)
                                 => -- | @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
                                    -- created with 'OpenXR.Core10.Instance.createInstance'.
                                    Instance
                                 -> -- | @systemId@ is an
                                    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
                                    -- handle for the system which will be used to create a session.
                                    SystemId
                                 -> io (GraphicsRequirementsVulkanKHR)
getVulkanGraphicsRequirementsKHR instance' systemId = liftIO . evalContT $ do
  let xrGetVulkanGraphicsRequirementsKHRPtr = pXrGetVulkanGraphicsRequirementsKHR (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (xrGetVulkanGraphicsRequirementsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetVulkanGraphicsRequirementsKHR is null" Nothing Nothing
  let xrGetVulkanGraphicsRequirementsKHR' = mkXrGetVulkanGraphicsRequirementsKHR xrGetVulkanGraphicsRequirementsKHRPtr
  pGraphicsRequirements <- ContT (withZeroCStruct @GraphicsRequirementsVulkanKHR)
  r <- lift $ traceAroundEvent "xrGetVulkanGraphicsRequirementsKHR" (xrGetVulkanGraphicsRequirementsKHR' (instanceHandle (instance')) (systemId) (pGraphicsRequirements))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  graphicsRequirements <- lift $ peekCStruct @GraphicsRequirementsVulkanKHR pGraphicsRequirements
  pure $ (graphicsRequirements)


-- | XrGraphicsBindingVulkanKHR - The graphics binding structure to be passed
-- at session creation to use Vulkan
--
-- == Member Descriptions
--
-- = Description
--
-- When creating a Vulkan-backed 'OpenXR.Core10.Handles.Session', the
-- application will provide a pointer to an 'GraphicsBindingVulkanKHR' in
-- the @next@ chain of the 'OpenXR.Core10.Device.SessionCreateInfo'.
--
-- == Valid Usage
--
-- -   @instance@ /must/ have enabled a Vulkan API version in the range
--     specified by 'GraphicsBindingVulkanKHR'
--
-- -   @instance@ /must/ have enabled all the instance extensions specified
--     by 'getVulkanInstanceExtensionsKHR'
--
-- -   @physicalDevice@ @VkPhysicalDevice@ /must/ match the device
--     specified by 'getVulkanGraphicsDeviceKHR'
--
-- -   @device@ /must/ have enabled all the device extensions specified by
--     'getVulkanDeviceExtensionsKHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingVulkanKHR-extension-notenabled# The
--     @XR_KHR_vulkan_enable@ extension /must/ be enabled prior to using
--     'GraphicsBindingVulkanKHR'
--
-- -   #VUID-XrGraphicsBindingVulkanKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_VULKAN_KHR'
--
-- -   #VUID-XrGraphicsBindingVulkanKHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsBindingVulkanKHR-instance-parameter# @instance@
--     /must/ be a valid @VkInstance@ value
--
-- -   #VUID-XrGraphicsBindingVulkanKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ value
--
-- -   #VUID-XrGraphicsBindingVulkanKHR-device-parameter# @device@ /must/
--     be a valid @VkDevice@ value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data GraphicsBindingVulkanKHR = GraphicsBindingVulkanKHR
  { -- | @instance@ is a valid Vulkan @VkInstance@.
    instance' :: Ptr OpenXR.VulkanTypes.Instance_T
  , -- | @physicalDevice@ is a valid Vulkan @VkPhysicalDevice@.
    physicalDevice :: Ptr OpenXR.VulkanTypes.PhysicalDevice_T
  , -- | @device@ is a valid Vulkan @VkDevice@.
    device :: Ptr OpenXR.VulkanTypes.Device_T
  , -- | @queueFamilyIndex@ is a valid queue family index on @device@.
    queueFamilyIndex :: Word32
  , -- | @queueIndex@ is a valid queue index on @device@ to be used for
    -- synchronization.
    queueIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsBindingVulkanKHR)
#endif
deriving instance Show GraphicsBindingVulkanKHR

instance ToCStruct GraphicsBindingVulkanKHR where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsBindingVulkanKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_VULKAN_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr OpenXR.VulkanTypes.Instance_T))) (instance')
    poke ((p `plusPtr` 24 :: Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T))) (physicalDevice)
    poke ((p `plusPtr` 32 :: Ptr (Ptr OpenXR.VulkanTypes.Device_T))) (device)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (queueFamilyIndex)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (queueIndex)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_VULKAN_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr OpenXR.VulkanTypes.Instance_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T))) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr OpenXR.VulkanTypes.Device_T))) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    f

instance FromCStruct GraphicsBindingVulkanKHR where
  peekCStruct p = do
    instance' <- peek @(Ptr OpenXR.VulkanTypes.Instance_T) ((p `plusPtr` 16 :: Ptr (Ptr OpenXR.VulkanTypes.Instance_T)))
    physicalDevice <- peek @(Ptr OpenXR.VulkanTypes.PhysicalDevice_T) ((p `plusPtr` 24 :: Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T)))
    device <- peek @(Ptr OpenXR.VulkanTypes.Device_T) ((p `plusPtr` 32 :: Ptr (Ptr OpenXR.VulkanTypes.Device_T)))
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    queueIndex <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pure $ GraphicsBindingVulkanKHR
             instance' physicalDevice device queueFamilyIndex queueIndex

instance Storable GraphicsBindingVulkanKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsBindingVulkanKHR where
  zero = GraphicsBindingVulkanKHR
           zero
           zero
           zero
           zero
           zero


-- | XrSwapchainImageVulkanKHR - Vulkan-specific swapchain image structure
--
-- == Member Descriptions
--
-- = Description
--
-- If a given session was created with 'GraphicsBindingVulkanKHR', the
-- following conditions /must/ apply.
--
-- -   Calls to 'OpenXR.Core10.Image.enumerateSwapchainImages' on an
--     'OpenXR.Core10.Handles.Swapchain' in that session /must/ return an
--     array of 'SwapchainImageVulkanKHR' structures.
--
-- -   Whenever an OpenXR function accepts an
--     'OpenXR.Core10.Image.SwapchainImageBaseHeader' pointer as a
--     parameter in that session, the runtime /must/ also accept a pointer
--     to an 'SwapchainImageVulkanKHR'.
--
-- The OpenXR runtime /must/ interpret the top-left corner of the swapchain
-- image as the coordinate origin unless specified otherwise by extension
-- functionality.
--
-- The OpenXR runtime /must/ interpret the swapchain images in a clip space
-- of positive Y pointing down, near Z plane at 0, and far Z plane at 1.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSwapchainImageVulkanKHR-extension-notenabled# The
--     @XR_KHR_vulkan_enable@ extension /must/ be enabled prior to using
--     'SwapchainImageVulkanKHR'
--
-- -   #VUID-XrSwapchainImageVulkanKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR'
--
-- -   #VUID-XrSwapchainImageVulkanKHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSwapchainImageVulkanKHR-image-parameter# @image@ /must/ be a
--     valid @VkImage@ value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Image.SwapchainImageBaseHeader'
data SwapchainImageVulkanKHR = SwapchainImageVulkanKHR
  { -- | @image@ is a valid Vulkan @VkImage@ to use.
    image :: OpenXR.VulkanTypes.Image }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainImageVulkanKHR)
#endif
deriving instance Show SwapchainImageVulkanKHR

instance IsSwapchainImage SwapchainImageVulkanKHR where
  toSwapchainImageBaseHeader SwapchainImageVulkanKHR{} = SwapchainImageBaseHeader{type' = TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR}

instance ToCStruct SwapchainImageVulkanKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainImageVulkanKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OpenXR.VulkanTypes.Image)) (image)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OpenXR.VulkanTypes.Image)) (zero)
    f

instance FromCStruct SwapchainImageVulkanKHR where
  peekCStruct p = do
    image <- peek @OpenXR.VulkanTypes.Image ((p `plusPtr` 16 :: Ptr OpenXR.VulkanTypes.Image))
    pure $ SwapchainImageVulkanKHR
             image

instance Storable SwapchainImageVulkanKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainImageVulkanKHR where
  zero = SwapchainImageVulkanKHR
           zero


-- | XrGraphicsRequirementsVulkanKHR - Vulkan API version requirements
--
-- == Member Descriptions
--
-- = Description
--
-- 'GraphicsRequirementsVulkanKHR' is populated by
-- 'getVulkanGraphicsRequirementsKHR' with the runtime’s Vulkan API version
-- requirements.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsRequirementsVulkanKHR-extension-notenabled# The
--     @XR_KHR_vulkan_enable@ extension /must/ be enabled prior to using
--     'GraphicsRequirementsVulkanKHR'
--
-- -   #VUID-XrGraphicsRequirementsVulkanKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR'
--
-- -   #VUID-XrGraphicsRequirementsVulkanKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrVersion >,
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.getVulkanGraphicsRequirements2KHR',
-- 'getVulkanGraphicsRequirementsKHR'
data GraphicsRequirementsVulkanKHR = GraphicsRequirementsVulkanKHR
  { -- | @minApiVersionSupported@ is the minimum Vulkan Instance API version that
    -- the runtime supports. Uses 'OpenXR.Version.MAKE_VERSION' on major and
    -- minor API version, ignoring any patch version component.
    minApiVersionSupported :: Version
  , -- | @maxApiVersionSupported@ is the maximum Vulkan Instance API version that
    -- the runtime has been tested on and is known to support. Newer Vulkan
    -- Instance API versions might work if they are compatible. Uses
    -- 'OpenXR.Version.MAKE_VERSION' on major and minor API version, ignoring
    -- any patch version component.
    maxApiVersionSupported :: Version
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsRequirementsVulkanKHR)
#endif
deriving instance Show GraphicsRequirementsVulkanKHR

instance ToCStruct GraphicsRequirementsVulkanKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsRequirementsVulkanKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Version)) (minApiVersionSupported)
    poke ((p `plusPtr` 24 :: Ptr Version)) (maxApiVersionSupported)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Version)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Version)) (zero)
    f

instance FromCStruct GraphicsRequirementsVulkanKHR where
  peekCStruct p = do
    minApiVersionSupported <- peek @Version ((p `plusPtr` 16 :: Ptr Version))
    maxApiVersionSupported <- peek @Version ((p `plusPtr` 24 :: Ptr Version))
    pure $ GraphicsRequirementsVulkanKHR
             minApiVersionSupported maxApiVersionSupported

instance Storable GraphicsRequirementsVulkanKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsRequirementsVulkanKHR where
  zero = GraphicsRequirementsVulkanKHR
           zero
           zero


type KHR_vulkan_enable_SPEC_VERSION = 7

-- No documentation found for TopLevel "XR_KHR_vulkan_enable_SPEC_VERSION"
pattern KHR_vulkan_enable_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_vulkan_enable_SPEC_VERSION = 7


type KHR_VULKAN_ENABLE_EXTENSION_NAME = "XR_KHR_vulkan_enable"

-- No documentation found for TopLevel "XR_KHR_VULKAN_ENABLE_EXTENSION_NAME"
pattern KHR_VULKAN_ENABLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VULKAN_ENABLE_EXTENSION_NAME = "XR_KHR_vulkan_enable"

