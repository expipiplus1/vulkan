{-# language CPP #-}
-- | = Name
--
-- XR_KHR_vulkan_enable2 - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_enable2  XR_KHR_vulkan_enable2>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 91
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingVulkan2KHR', 'GraphicsRequirementsVulkan2KHR',
-- 'SwapchainImageVulkan2KHR', 'VulkanDeviceCreateInfoKHR',
-- 'VulkanGraphicsDeviceGetInfoKHR', 'VulkanInstanceCreateInfoKHR',
-- 'createVulkanDeviceKHR', 'createVulkanInstanceKHR',
-- 'getVulkanGraphicsDevice2KHR', 'getVulkanGraphicsRequirements2KHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_enable2 OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_vulkan_enable2  ( createVulkanInstanceKHR
                                                , createVulkanDeviceKHR
                                                , getVulkanGraphicsDevice2KHR
                                                , pattern TYPE_GRAPHICS_BINDING_VULKAN2_KHR
                                                , pattern TYPE_SWAPCHAIN_IMAGE_VULKAN2_KHR
                                                , pattern TYPE_GRAPHICS_REQUIREMENTS_VULKAN2_KHR
                                                , getVulkanGraphicsRequirements2KHR
                                                , VulkanInstanceCreateInfoKHR(..)
                                                , VulkanDeviceCreateInfoKHR(..)
                                                , VulkanGraphicsDeviceGetInfoKHR(..)
                                                , VulkanInstanceCreateFlagsKHR
                                                , VulkanInstanceCreateFlagBitsKHR(..)
                                                , VulkanDeviceCreateFlagsKHR
                                                , VulkanDeviceCreateFlagBitsKHR(..)
                                                , GraphicsBindingVulkan2KHR
                                                , SwapchainImageVulkan2KHR
                                                , GraphicsRequirementsVulkan2KHR
                                                , KHR_vulkan_enable2_SPEC_VERSION
                                                , pattern KHR_vulkan_enable2_SPEC_VERSION
                                                , KHR_VULKAN_ENABLE2_EXTENSION_NAME
                                                , pattern KHR_VULKAN_ENABLE2_EXTENSION_NAME
                                                , GraphicsBindingVulkanKHR(..)
                                                , SwapchainImageVulkanKHR(..)
                                                , GraphicsRequirementsVulkanKHR(..)
                                                , getVulkanGraphicsRequirementsKHR
                                                ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import OpenXR.Internal.Utils (traceAroundEvent)
import qualified OpenXR.VulkanTypes (AllocationCallbacks)
import qualified OpenXR.VulkanTypes (DeviceCreateInfo)
import qualified OpenXR.VulkanTypes (Device_T)
import qualified OpenXR.VulkanTypes (InstanceCreateInfo)
import qualified OpenXR.VulkanTypes (Instance_T)
import qualified OpenXR.VulkanTypes (PFN_vkGetInstanceProcAddr)
import qualified OpenXR.VulkanTypes (PhysicalDevice_T)
import qualified OpenXR.VulkanTypes (Result)
import qualified OpenXR.VulkanTypes (SomeStruct)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import qualified OpenXR.VulkanTypes (Result(..))
import OpenXR.Zero (Zero)
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.Extensions.XR_KHR_vulkan_enable (getVulkanGraphicsRequirementsKHR)
import OpenXR.NamedType ((:::))
import OpenXR.Core10.FundamentalTypes (Flags64)
import OpenXR.Extensions.XR_KHR_vulkan_enable (GraphicsBindingVulkanKHR)
import OpenXR.Extensions.XR_KHR_vulkan_enable (GraphicsRequirementsVulkanKHR)
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Dynamic (InstanceCmds(pXrCreateVulkanDeviceKHR))
import OpenXR.Dynamic (InstanceCmds(pXrCreateVulkanInstanceKHR))
import OpenXR.Dynamic (InstanceCmds(pXrGetVulkanGraphicsDevice2KHR))
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Extensions.XR_KHR_vulkan_enable (SwapchainImageVulkanKHR)
import OpenXR.Core10.Device (SystemId)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_VULKAN_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VULKAN_DEVICE_CREATE_INFO_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR))
import OpenXR.Extensions.XR_KHR_vulkan_enable (getVulkanGraphicsRequirementsKHR)
import OpenXR.Extensions.XR_KHR_vulkan_enable (GraphicsBindingVulkanKHR(..))
import OpenXR.Extensions.XR_KHR_vulkan_enable (GraphicsRequirementsVulkanKHR(..))
import OpenXR.Extensions.XR_KHR_vulkan_enable (SwapchainImageVulkanKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateVulkanInstanceKHR
  :: FunPtr (Ptr Instance_T -> Ptr VulkanInstanceCreateInfoKHR -> Ptr (Ptr OpenXR.VulkanTypes.Instance_T) -> Ptr OpenXR.VulkanTypes.Result -> IO Result) -> Ptr Instance_T -> Ptr VulkanInstanceCreateInfoKHR -> Ptr (Ptr OpenXR.VulkanTypes.Instance_T) -> Ptr OpenXR.VulkanTypes.Result -> IO Result

-- | xrCreateVulkanInstanceKHR - Create an OpenXR compatible VkInstance
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateVulkanInstanceKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'createVulkanInstanceKHR'
--
-- -   #VUID-xrCreateVulkanInstanceKHR-instance-parameter# @instance@
--     /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrCreateVulkanInstanceKHR-createInfo-parameter# @createInfo@
--     /must/ be a pointer to a valid 'VulkanInstanceCreateInfoKHR'
--     structure
--
-- -   #VUID-xrCreateVulkanInstanceKHR-vulkanInstance-parameter#
--     @vulkanInstance@ /must/ be a pointer to a @VkInstance@ value
--
-- -   #VUID-xrCreateVulkanInstanceKHR-vulkanResult-parameter#
--     @vulkanResult@ /must/ be a pointer to a @VkResult@ value
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance', 'VulkanInstanceCreateInfoKHR',
-- 'createVulkanDeviceKHR'
createVulkanInstanceKHR :: forall io
                         . (MonadIO io)
                        => -- | @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
                           -- created with 'OpenXR.Core10.Instance.createInstance'.
                           Instance
                        -> -- | @createInfo@ extensible input struct of type
                           -- @XrCreateVulkanInstanceCreateInfoKHR@
                           VulkanInstanceCreateInfoKHR
                        -> io (("vulkanInstance" ::: Ptr OpenXR.VulkanTypes.Instance_T), ("vulkanResult" ::: OpenXR.VulkanTypes.Result))
createVulkanInstanceKHR instance' createInfo = liftIO . evalContT $ do
  let xrCreateVulkanInstanceKHRPtr = pXrCreateVulkanInstanceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (xrCreateVulkanInstanceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateVulkanInstanceKHR is null" Nothing Nothing
  let xrCreateVulkanInstanceKHR' = mkXrCreateVulkanInstanceKHR xrCreateVulkanInstanceKHRPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pVulkanInstance <- ContT $ bracket (callocBytes @(Ptr OpenXR.VulkanTypes.Instance_T) 8) free
  pVulkanResult <- ContT $ bracket (callocBytes @OpenXR.VulkanTypes.Result 4) free
  r <- lift $ traceAroundEvent "xrCreateVulkanInstanceKHR" (xrCreateVulkanInstanceKHR' (instanceHandle (instance')) createInfo' (pVulkanInstance) (pVulkanResult))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  vulkanInstance <- lift $ peek @(Ptr OpenXR.VulkanTypes.Instance_T) pVulkanInstance
  vulkanResult <- lift $ peek @OpenXR.VulkanTypes.Result pVulkanResult
  pure $ (vulkanInstance, vulkanResult)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateVulkanDeviceKHR
  :: FunPtr (Ptr Instance_T -> Ptr VulkanDeviceCreateInfoKHR -> Ptr (Ptr OpenXR.VulkanTypes.Device_T) -> Ptr OpenXR.VulkanTypes.Result -> IO Result) -> Ptr Instance_T -> Ptr VulkanDeviceCreateInfoKHR -> Ptr (Ptr OpenXR.VulkanTypes.Device_T) -> Ptr OpenXR.VulkanTypes.Result -> IO Result

-- | xrCreateVulkanDeviceKHR - Create an OpenXR compatible VkDevice
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateVulkanDeviceKHR-extension-notenabled# The @@ extension
--     /must/ be enabled prior to calling 'createVulkanDeviceKHR'
--
-- -   #VUID-xrCreateVulkanDeviceKHR-instance-parameter# @instance@ /must/
--     be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrCreateVulkanDeviceKHR-createInfo-parameter# @createInfo@
--     /must/ be a pointer to a valid 'VulkanDeviceCreateInfoKHR' structure
--
-- -   #VUID-xrCreateVulkanDeviceKHR-vulkanDevice-parameter# @vulkanDevice@
--     /must/ be a pointer to a @VkDevice@ value
--
-- -   #VUID-xrCreateVulkanDeviceKHR-vulkanResult-parameter# @vulkanResult@
--     /must/ be a pointer to a @VkResult@ value
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance', 'VulkanDeviceCreateInfoKHR',
-- 'createVulkanInstanceKHR'
createVulkanDeviceKHR :: forall io
                       . (MonadIO io)
                      => -- | @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
                         -- created with 'OpenXR.Core10.Instance.createInstance'.
                         Instance
                      -> -- | @createInfo@ extensible input struct of type
                         -- @XrCreateVulkanDeviceCreateInfoKHR@
                         VulkanDeviceCreateInfoKHR
                      -> io (("vulkanDevice" ::: Ptr OpenXR.VulkanTypes.Device_T), ("vulkanResult" ::: OpenXR.VulkanTypes.Result))
createVulkanDeviceKHR instance' createInfo = liftIO . evalContT $ do
  let xrCreateVulkanDeviceKHRPtr = pXrCreateVulkanDeviceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (xrCreateVulkanDeviceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateVulkanDeviceKHR is null" Nothing Nothing
  let xrCreateVulkanDeviceKHR' = mkXrCreateVulkanDeviceKHR xrCreateVulkanDeviceKHRPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pVulkanDevice <- ContT $ bracket (callocBytes @(Ptr OpenXR.VulkanTypes.Device_T) 8) free
  pVulkanResult <- ContT $ bracket (callocBytes @OpenXR.VulkanTypes.Result 4) free
  r <- lift $ traceAroundEvent "xrCreateVulkanDeviceKHR" (xrCreateVulkanDeviceKHR' (instanceHandle (instance')) createInfo' (pVulkanDevice) (pVulkanResult))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  vulkanDevice <- lift $ peek @(Ptr OpenXR.VulkanTypes.Device_T) pVulkanDevice
  vulkanResult <- lift $ peek @OpenXR.VulkanTypes.Result pVulkanResult
  pure $ (vulkanDevice, vulkanResult)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetVulkanGraphicsDevice2KHR
  :: FunPtr (Ptr Instance_T -> Ptr VulkanGraphicsDeviceGetInfoKHR -> Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T) -> IO Result) -> Ptr Instance_T -> Ptr VulkanGraphicsDeviceGetInfoKHR -> Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T) -> IO Result

-- | xrGetVulkanGraphicsDevice2KHR - Retrieve the Vulkan physical device
-- associated with an OpenXR instance and system
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetVulkanGraphicsDevice2KHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'getVulkanGraphicsDevice2KHR'
--
-- -   #VUID-xrGetVulkanGraphicsDevice2KHR-instance-parameter# @instance@
--     /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetVulkanGraphicsDevice2KHR-getInfo-parameter# @getInfo@
--     /must/ be a pointer to a valid 'VulkanGraphicsDeviceGetInfoKHR'
--     structure
--
-- -   #VUID-xrGetVulkanGraphicsDevice2KHR-vulkanPhysicalDevice-parameter#
--     @vulkanPhysicalDevice@ /must/ be a pointer to a @VkPhysicalDevice@
--     value
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
-- 'OpenXR.Core10.Handles.Instance', 'VulkanGraphicsDeviceGetInfoKHR'
getVulkanGraphicsDevice2KHR :: forall io
                             . (MonadIO io)
                            => -- | @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
                               -- created with 'OpenXR.Core10.Instance.createInstance'.
                               Instance
                            -> -- | @getInfo@ extensible input struct of type
                               -- 'VulkanGraphicsDeviceGetInfoKHR'
                               VulkanGraphicsDeviceGetInfoKHR
                            -> io (("vulkanPhysicalDevice" ::: Ptr OpenXR.VulkanTypes.PhysicalDevice_T))
getVulkanGraphicsDevice2KHR instance' getInfo = liftIO . evalContT $ do
  let xrGetVulkanGraphicsDevice2KHRPtr = pXrGetVulkanGraphicsDevice2KHR (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetVulkanGraphicsDevice2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetVulkanGraphicsDevice2KHR is null" Nothing Nothing
  let xrGetVulkanGraphicsDevice2KHR' = mkXrGetVulkanGraphicsDevice2KHR xrGetVulkanGraphicsDevice2KHRPtr
  getInfo' <- ContT $ withCStruct (getInfo)
  pVulkanPhysicalDevice <- ContT $ bracket (callocBytes @(Ptr OpenXR.VulkanTypes.PhysicalDevice_T) 8) free
  r <- lift $ traceAroundEvent "xrGetVulkanGraphicsDevice2KHR" (xrGetVulkanGraphicsDevice2KHR' (instanceHandle (instance')) getInfo' (pVulkanPhysicalDevice))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  vulkanPhysicalDevice <- lift $ peek @(Ptr OpenXR.VulkanTypes.PhysicalDevice_T) pVulkanPhysicalDevice
  pure $ (vulkanPhysicalDevice)


-- No documentation found for TopLevel "XR_TYPE_GRAPHICS_BINDING_VULKAN2_KHR"
pattern TYPE_GRAPHICS_BINDING_VULKAN2_KHR = TYPE_GRAPHICS_BINDING_VULKAN_KHR


-- No documentation found for TopLevel "XR_TYPE_SWAPCHAIN_IMAGE_VULKAN2_KHR"
pattern TYPE_SWAPCHAIN_IMAGE_VULKAN2_KHR = TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR


-- No documentation found for TopLevel "XR_TYPE_GRAPHICS_REQUIREMENTS_VULKAN2_KHR"
pattern TYPE_GRAPHICS_REQUIREMENTS_VULKAN2_KHR = TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR


-- | xrGetVulkanGraphicsRequirements2KHR - Retrieve the Vulkan version
-- requirements for an instance and system
--
-- = Parameters
--
-- The
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanGraphicsRequirementsKHR'
-- function identifies to the application the runtime’s minimum Vulkan
-- version requirement and the highest known tested Vulkan version.
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanGraphicsRequirementsKHR'
-- /must/ be called prior to calling 'OpenXR.Core10.Device.createSession'.
-- The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING' on
-- calls to 'OpenXR.Core10.Device.createSession' if
-- 'getVulkanGraphicsRequirements2KHR' has not been called for the same
-- @instance@ and @systemId@.
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
-- -   @graphicsRequirements@ is the 'GraphicsRequirementsVulkan2KHR'
--     output structure.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetVulkanGraphicsRequirements2KHR-extension-notenabled# The
--     @@ extension /must/ be enabled prior to calling
--     'getVulkanGraphicsRequirements2KHR'
--
-- -   #VUID-xrGetVulkanGraphicsRequirements2KHR-instance-parameter#
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetVulkanGraphicsRequirements2KHR-graphicsRequirements-parameter#
--     @graphicsRequirements@ /must/ be a pointer to an
--     'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsRequirementsVulkanKHR'
--     structure
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
-- 'GraphicsRequirementsVulkan2KHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsRequirementsVulkanKHR',
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
getVulkanGraphicsRequirements2KHR = getVulkanGraphicsRequirementsKHR


-- | XrVulkanInstanceCreateInfoKHR - Vulkan Instance Create Info
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrVulkanInstanceCreateInfoKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'VulkanInstanceCreateInfoKHR'
--
-- -   #VUID-XrVulkanInstanceCreateInfoKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR'
--
-- -   #VUID-XrVulkanInstanceCreateInfoKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrVulkanInstanceCreateInfoKHR-createFlags-zerobitmask#
--     @createFlags@ /must/ be @0@
--
-- -   #VUID-XrVulkanInstanceCreateInfoKHR-pfnGetInstanceProcAddr-parameter#
--     @pfnGetInstanceProcAddr@ /must/ be a valid
--     @PFN_vkGetInstanceProcAddr@ value
--
-- -   #VUID-XrVulkanInstanceCreateInfoKHR-vulkanCreateInfo-parameter#
--     @vulkanCreateInfo@ /must/ be a pointer to a valid
--     @VkInstanceCreateInfo@ value
--
-- -   #VUID-XrVulkanInstanceCreateInfoKHR-vulkanAllocator-parameter#
--     @vulkanAllocator@ /must/ be a pointer to a valid
--     @VkAllocationCallbacks@ value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'VulkanInstanceCreateFlagsKHR', 'createVulkanInstanceKHR'
data VulkanInstanceCreateInfoKHR = VulkanInstanceCreateInfoKHR
  { -- | @systemId@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
    -- handle for the system which will be used to create a session.
    systemId :: SystemId
  , -- No documentation found for Nested "XrVulkanInstanceCreateInfoKHR" "createFlags"
    createFlags :: VulkanInstanceCreateFlagsKHR
  , -- | @pfnGetInstanceProcAddr@ is a function pointer to
    -- @vkGetInstanceProcAddr@ or a compatible entry point.
    pfnGetInstanceProcAddr :: OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr
  , -- | @vulkanCreateInfo@ is the
    -- <{vkRefPageRoot}/VkInstanceCreateInfo.html VkInstanceCreateInfo as specified by Vulkan>.
    vulkanCreateInfo :: Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.InstanceCreateInfo)
  , -- | @vulkanAllocator@ is the
    -- <{vkRefPageRoot}/VkAllocationCallbacks.html VkAllocationCallbacks as specified by Vulkan>.
    vulkanAllocator :: Ptr OpenXR.VulkanTypes.AllocationCallbacks
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VulkanInstanceCreateInfoKHR)
#endif
deriving instance Show VulkanInstanceCreateInfoKHR

instance ToCStruct VulkanInstanceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VulkanInstanceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SystemId)) (systemId)
    poke ((p `plusPtr` 24 :: Ptr VulkanInstanceCreateFlagsKHR)) (createFlags)
    poke ((p `plusPtr` 32 :: Ptr OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr)) (pfnGetInstanceProcAddr)
    poke ((p `plusPtr` 40 :: Ptr (Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.InstanceCreateInfo)))) (vulkanCreateInfo)
    poke ((p `plusPtr` 48 :: Ptr (Ptr OpenXR.VulkanTypes.AllocationCallbacks))) (vulkanAllocator)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SystemId)) (zero)
    poke ((p `plusPtr` 24 :: Ptr VulkanInstanceCreateFlagsKHR)) (zero)
    poke ((p `plusPtr` 32 :: Ptr OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr)) (zero)
    poke ((p `plusPtr` 40 :: Ptr (Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.InstanceCreateInfo)))) (zero)
    poke ((p `plusPtr` 48 :: Ptr (Ptr OpenXR.VulkanTypes.AllocationCallbacks))) (zero)
    f

instance FromCStruct VulkanInstanceCreateInfoKHR where
  peekCStruct p = do
    systemId <- peek @SystemId ((p `plusPtr` 16 :: Ptr SystemId))
    createFlags <- peek @VulkanInstanceCreateFlagsKHR ((p `plusPtr` 24 :: Ptr VulkanInstanceCreateFlagsKHR))
    pfnGetInstanceProcAddr <- peek @OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr ((p `plusPtr` 32 :: Ptr OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr))
    vulkanCreateInfo <- peek @(Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.InstanceCreateInfo)) ((p `plusPtr` 40 :: Ptr (Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.InstanceCreateInfo))))
    vulkanAllocator <- peek @(Ptr OpenXR.VulkanTypes.AllocationCallbacks) ((p `plusPtr` 48 :: Ptr (Ptr OpenXR.VulkanTypes.AllocationCallbacks)))
    pure $ VulkanInstanceCreateInfoKHR
             systemId createFlags pfnGetInstanceProcAddr vulkanCreateInfo vulkanAllocator

instance Storable VulkanInstanceCreateInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VulkanInstanceCreateInfoKHR where
  zero = VulkanInstanceCreateInfoKHR
           zero
           zero
           zero
           zero
           zero


-- | XrVulkanDeviceCreateInfoKHR - Vulkan Device Create Info
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrVulkanDeviceCreateInfoKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'VulkanDeviceCreateInfoKHR'
--
-- -   #VUID-XrVulkanDeviceCreateInfoKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_VULKAN_DEVICE_CREATE_INFO_KHR'
--
-- -   #VUID-XrVulkanDeviceCreateInfoKHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrVulkanDeviceCreateInfoKHR-createFlags-zerobitmask#
--     @createFlags@ /must/ be @0@
--
-- -   #VUID-XrVulkanDeviceCreateInfoKHR-pfnGetInstanceProcAddr-parameter#
--     @pfnGetInstanceProcAddr@ /must/ be a valid
--     @PFN_vkGetInstanceProcAddr@ value
--
-- -   #VUID-XrVulkanDeviceCreateInfoKHR-vulkanPhysicalDevice-parameter#
--     @vulkanPhysicalDevice@ /must/ be a valid @VkPhysicalDevice@ value
--
-- -   #VUID-XrVulkanDeviceCreateInfoKHR-vulkanCreateInfo-parameter#
--     @vulkanCreateInfo@ /must/ be a pointer to a valid
--     @VkDeviceCreateInfo@ value
--
-- -   #VUID-XrVulkanDeviceCreateInfoKHR-vulkanAllocator-parameter#
--     @vulkanAllocator@ /must/ be a pointer to a valid
--     @VkAllocationCallbacks@ value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'VulkanDeviceCreateFlagsKHR', 'createVulkanDeviceKHR'
data VulkanDeviceCreateInfoKHR = VulkanDeviceCreateInfoKHR
  { -- | @systemId@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
    -- handle for the system which will be used to create a session.
    systemId :: SystemId
  , -- No documentation found for Nested "XrVulkanDeviceCreateInfoKHR" "createFlags"
    createFlags :: VulkanDeviceCreateFlagsKHR
  , -- | @pfnGetInstanceProcAddr@ is a function pointer to
    -- @vkGetInstanceProcAddr@ or a compatible entry point.
    pfnGetInstanceProcAddr :: OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr
  , -- | @vulkanPhysicalDevice@ /must/ match
    -- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanGraphicsDeviceKHR'.
    vulkanPhysicalDevice :: Ptr OpenXR.VulkanTypes.PhysicalDevice_T
  , -- | @vulkanCreateInfo@ is the
    -- <{vkRefPageRoot}/VkDeviceCreateInfo.html VkDeviceCreateInfo as specified by Vulkan>.
    vulkanCreateInfo :: Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.DeviceCreateInfo)
  , -- | @vulkanAllocator@ is the
    -- <{vkRefPageRoot}/VkAllocationCallbacks.html VkAllocationCallbacks as specified by Vulkan>.
    vulkanAllocator :: Ptr OpenXR.VulkanTypes.AllocationCallbacks
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VulkanDeviceCreateInfoKHR)
#endif
deriving instance Show VulkanDeviceCreateInfoKHR

instance ToCStruct VulkanDeviceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VulkanDeviceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VULKAN_DEVICE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SystemId)) (systemId)
    poke ((p `plusPtr` 24 :: Ptr VulkanDeviceCreateFlagsKHR)) (createFlags)
    poke ((p `plusPtr` 32 :: Ptr OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr)) (pfnGetInstanceProcAddr)
    poke ((p `plusPtr` 40 :: Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T))) (vulkanPhysicalDevice)
    poke ((p `plusPtr` 48 :: Ptr (Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.DeviceCreateInfo)))) (vulkanCreateInfo)
    poke ((p `plusPtr` 56 :: Ptr (Ptr OpenXR.VulkanTypes.AllocationCallbacks))) (vulkanAllocator)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VULKAN_DEVICE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SystemId)) (zero)
    poke ((p `plusPtr` 24 :: Ptr VulkanDeviceCreateFlagsKHR)) (zero)
    poke ((p `plusPtr` 32 :: Ptr OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr)) (zero)
    poke ((p `plusPtr` 40 :: Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T))) (zero)
    poke ((p `plusPtr` 48 :: Ptr (Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.DeviceCreateInfo)))) (zero)
    poke ((p `plusPtr` 56 :: Ptr (Ptr OpenXR.VulkanTypes.AllocationCallbacks))) (zero)
    f

instance FromCStruct VulkanDeviceCreateInfoKHR where
  peekCStruct p = do
    systemId <- peek @SystemId ((p `plusPtr` 16 :: Ptr SystemId))
    createFlags <- peek @VulkanDeviceCreateFlagsKHR ((p `plusPtr` 24 :: Ptr VulkanDeviceCreateFlagsKHR))
    pfnGetInstanceProcAddr <- peek @OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr ((p `plusPtr` 32 :: Ptr OpenXR.VulkanTypes.PFN_vkGetInstanceProcAddr))
    vulkanPhysicalDevice <- peek @(Ptr OpenXR.VulkanTypes.PhysicalDevice_T) ((p `plusPtr` 40 :: Ptr (Ptr OpenXR.VulkanTypes.PhysicalDevice_T)))
    vulkanCreateInfo <- peek @(Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.DeviceCreateInfo)) ((p `plusPtr` 48 :: Ptr (Ptr (OpenXR.VulkanTypes.SomeStruct OpenXR.VulkanTypes.DeviceCreateInfo))))
    vulkanAllocator <- peek @(Ptr OpenXR.VulkanTypes.AllocationCallbacks) ((p `plusPtr` 56 :: Ptr (Ptr OpenXR.VulkanTypes.AllocationCallbacks)))
    pure $ VulkanDeviceCreateInfoKHR
             systemId createFlags pfnGetInstanceProcAddr vulkanPhysicalDevice vulkanCreateInfo vulkanAllocator

instance Storable VulkanDeviceCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VulkanDeviceCreateInfoKHR where
  zero = VulkanDeviceCreateInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | XrVulkanGraphicsDeviceGetInfoKHR - Vulkan Graphics Device Get Info
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrVulkanGraphicsDeviceGetInfoKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'VulkanGraphicsDeviceGetInfoKHR'
--
-- -   #VUID-XrVulkanGraphicsDeviceGetInfoKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR'
--
-- -   #VUID-XrVulkanGraphicsDeviceGetInfoKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrVulkanGraphicsDeviceGetInfoKHR-vulkanInstance-parameter#
--     @vulkanInstance@ /must/ be a valid @VkInstance@ value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'getVulkanGraphicsDevice2KHR'
data VulkanGraphicsDeviceGetInfoKHR = VulkanGraphicsDeviceGetInfoKHR
  { -- | @systemId@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
    -- handle for the system which will be used to create a session.
    systemId :: SystemId
  , -- | @vulkanInstance@ is a valid Vulkan @VkInstance@.
    vulkanInstance :: Ptr OpenXR.VulkanTypes.Instance_T
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VulkanGraphicsDeviceGetInfoKHR)
#endif
deriving instance Show VulkanGraphicsDeviceGetInfoKHR

instance ToCStruct VulkanGraphicsDeviceGetInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VulkanGraphicsDeviceGetInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SystemId)) (systemId)
    poke ((p `plusPtr` 24 :: Ptr (Ptr OpenXR.VulkanTypes.Instance_T))) (vulkanInstance)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SystemId)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr OpenXR.VulkanTypes.Instance_T))) (zero)
    f

instance FromCStruct VulkanGraphicsDeviceGetInfoKHR where
  peekCStruct p = do
    systemId <- peek @SystemId ((p `plusPtr` 16 :: Ptr SystemId))
    vulkanInstance <- peek @(Ptr OpenXR.VulkanTypes.Instance_T) ((p `plusPtr` 24 :: Ptr (Ptr OpenXR.VulkanTypes.Instance_T)))
    pure $ VulkanGraphicsDeviceGetInfoKHR
             systemId vulkanInstance

instance Storable VulkanGraphicsDeviceGetInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VulkanGraphicsDeviceGetInfoKHR where
  zero = VulkanGraphicsDeviceGetInfoKHR
           zero
           zero


type VulkanInstanceCreateFlagsKHR = VulkanInstanceCreateFlagBitsKHR

-- No documentation found for TopLevel "XrVulkanInstanceCreateFlagBitsKHR"
newtype VulkanInstanceCreateFlagBitsKHR = VulkanInstanceCreateFlagBitsKHR Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameVulkanInstanceCreateFlagBitsKHR :: String
conNameVulkanInstanceCreateFlagBitsKHR = "VulkanInstanceCreateFlagBitsKHR"

enumPrefixVulkanInstanceCreateFlagBitsKHR :: String
enumPrefixVulkanInstanceCreateFlagBitsKHR = ""

showTableVulkanInstanceCreateFlagBitsKHR :: [(VulkanInstanceCreateFlagBitsKHR, String)]
showTableVulkanInstanceCreateFlagBitsKHR = []

instance Show VulkanInstanceCreateFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixVulkanInstanceCreateFlagBitsKHR
                            showTableVulkanInstanceCreateFlagBitsKHR
                            conNameVulkanInstanceCreateFlagBitsKHR
                            (\(VulkanInstanceCreateFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read VulkanInstanceCreateFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixVulkanInstanceCreateFlagBitsKHR
                          showTableVulkanInstanceCreateFlagBitsKHR
                          conNameVulkanInstanceCreateFlagBitsKHR
                          VulkanInstanceCreateFlagBitsKHR


type VulkanDeviceCreateFlagsKHR = VulkanDeviceCreateFlagBitsKHR

-- No documentation found for TopLevel "XrVulkanDeviceCreateFlagBitsKHR"
newtype VulkanDeviceCreateFlagBitsKHR = VulkanDeviceCreateFlagBitsKHR Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameVulkanDeviceCreateFlagBitsKHR :: String
conNameVulkanDeviceCreateFlagBitsKHR = "VulkanDeviceCreateFlagBitsKHR"

enumPrefixVulkanDeviceCreateFlagBitsKHR :: String
enumPrefixVulkanDeviceCreateFlagBitsKHR = ""

showTableVulkanDeviceCreateFlagBitsKHR :: [(VulkanDeviceCreateFlagBitsKHR, String)]
showTableVulkanDeviceCreateFlagBitsKHR = []

instance Show VulkanDeviceCreateFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixVulkanDeviceCreateFlagBitsKHR
                            showTableVulkanDeviceCreateFlagBitsKHR
                            conNameVulkanDeviceCreateFlagBitsKHR
                            (\(VulkanDeviceCreateFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read VulkanDeviceCreateFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixVulkanDeviceCreateFlagBitsKHR
                          showTableVulkanDeviceCreateFlagBitsKHR
                          conNameVulkanDeviceCreateFlagBitsKHR
                          VulkanDeviceCreateFlagBitsKHR


-- | XrGraphicsBindingVulkan2KHR - The graphics binding structure to be
-- passed at session creation to use Vulkan
--
-- == Valid Usage
--
-- -   @instance@ /must/ have enabled a Vulkan API version in the range
--     specified by 'getVulkanGraphicsRequirements2KHR'
--
-- -   @instance@ /must/ have been created using 'createVulkanInstanceKHR'
--
-- -   @physicalDevice@ @VkPhysicalDevice@ /must/ match the device
--     specified by 'getVulkanGraphicsDevice2KHR'
--
-- -   @device@ /must/ have been created using 'createVulkanDeviceKHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingVulkan2KHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'GraphicsBindingVulkan2KHR'
--
-- -   __Note:__ 'GraphicsBindingVulkan2KHR' is an alias for
--     'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsBindingVulkanKHR',
--     so implicit valid usage for
--     'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsBindingVulkanKHR'
--     has been replicated below.
--
-- -   @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_VULKAN_KHR'
--
-- -   @next@ /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   @instance@ /must/ be a valid @VkInstance@ value
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ value
--
-- -   @device@ /must/ be a valid @VkDevice@ value
--
-- = See Also
--
-- 'OpenXR.Core10.Device.createSession'
type GraphicsBindingVulkan2KHR = GraphicsBindingVulkanKHR


-- | XrSwapchainImageVulkan2KHR - Vulkan-specific swapchain image structure
--
-- == Member Descriptions
--
-- = Description
--
-- If a given session was created with 'GraphicsBindingVulkan2KHR', the
-- following conditions /must/ apply.
--
-- -   Calls to 'OpenXR.Core10.Image.enumerateSwapchainImages' on an
--     'OpenXR.Core10.Handles.Swapchain' in that session /must/ return an
--     array of 'SwapchainImageVulkan2KHR' structures.
--
-- -   Whenever an OpenXR function accepts an
--     'OpenXR.Core10.Image.SwapchainImageBaseHeader' pointer as a
--     parameter in that session, the runtime /must/ also accept a pointer
--     to an 'SwapchainImageVulkan2KHR'.
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
-- -   #VUID-XrSwapchainImageVulkan2KHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'SwapchainImageVulkan2KHR'
--
-- -   __Note:__ 'SwapchainImageVulkan2KHR' is an alias for
--     'OpenXR.Extensions.XR_KHR_vulkan_enable.SwapchainImageVulkanKHR', so
--     implicit valid usage for
--     'OpenXR.Extensions.XR_KHR_vulkan_enable.SwapchainImageVulkanKHR' has
--     been replicated below.
--
-- -   @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR'
--
-- -   @next@ /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   @image@ /must/ be a valid @VkImage@ value
--
-- = See Also
--
-- 'OpenXR.Core10.Image.SwapchainImageBaseHeader'
type SwapchainImageVulkan2KHR = SwapchainImageVulkanKHR


-- | XrGraphicsRequirementsVulkan2KHR - Vulkan API version requirements
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsRequirementsVulkan2KHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'GraphicsRequirementsVulkan2KHR'
--
-- -   __Note:__ 'GraphicsRequirementsVulkan2KHR' is an alias for
--     'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsRequirementsVulkanKHR',
--     so implicit valid usage for
--     'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsRequirementsVulkanKHR'
--     has been replicated below.
--
-- -   @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR'
--
-- -   @next@ /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'getVulkanGraphicsRequirements2KHR'
type GraphicsRequirementsVulkan2KHR = GraphicsRequirementsVulkanKHR


type KHR_vulkan_enable2_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_KHR_vulkan_enable2_SPEC_VERSION"
pattern KHR_vulkan_enable2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_vulkan_enable2_SPEC_VERSION = 1


type KHR_VULKAN_ENABLE2_EXTENSION_NAME = "XR_KHR_vulkan_enable2"

-- No documentation found for TopLevel "XR_KHR_VULKAN_ENABLE2_EXTENSION_NAME"
pattern KHR_VULKAN_ENABLE2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VULKAN_ENABLE2_EXTENSION_NAME = "XR_KHR_vulkan_enable2"

