{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_surface  ( destroySurfaceKHR
                                                  , getPhysicalDeviceSurfaceSupportKHR
                                                  , getPhysicalDeviceSurfaceCapabilitiesKHR
                                                  , getPhysicalDeviceSurfaceFormatsKHR
                                                  , getPhysicalDeviceSurfacePresentModesKHR
                                                  , SurfaceCapabilitiesKHR(..)
                                                  , SurfaceFormatKHR(..)
                                                  , KHR_SURFACE_SPEC_VERSION
                                                  , pattern KHR_SURFACE_SPEC_VERSION
                                                  , KHR_SURFACE_EXTENSION_NAME
                                                  , pattern KHR_SURFACE_EXTENSION_NAME
                                                  , SurfaceKHR(..)
                                                  , PresentModeKHR(..)
                                                  , ColorSpaceKHR(..)
                                                  , CompositeAlphaFlagBitsKHR(..)
                                                  , CompositeAlphaFlagsKHR
                                                  , SurfaceTransformFlagBitsKHR(..)
                                                  , SurfaceTransformFlagsKHR
                                                  ) where

import Control.Exception.Base (bracket)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.BaseType (Bool32(..))
import Graphics.Vulkan.Extensions.VK_EXT_swapchain_colorspace (ColorSpaceKHR)
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter (CompositeAlphaFlagsKHR)
import Graphics.Vulkan.Core10.SharedTypes (Extent2D)
import Graphics.Vulkan.Core10.Enums.Format (Format)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Graphics.Vulkan.Core10.Handles (Instance)
import Graphics.Vulkan.Core10.Handles (Instance(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkDestroySurfaceKHR))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceCapabilitiesKHR))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceFormatsKHR))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfacePresentModesKHR))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceSupportKHR))
import Graphics.Vulkan.Core10.Handles (Instance_T)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image (PresentModeKHR)
import Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image (PresentModeKHR(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagBitsKHR)
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagsKHR)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.VK_EXT_swapchain_colorspace (ColorSpaceKHR(..))
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter (CompositeAlphaFlagBitsKHR(..))
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter (CompositeAlphaFlagsKHR)
import Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image (PresentModeKHR(..))
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagBitsKHR(..))
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySurfaceKHR
  :: FunPtr (Ptr Instance_T -> SurfaceKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Instance_T -> SurfaceKHR -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroySurfaceKHR - Destroy a VkSurfaceKHR object
--
-- = Parameters
--
-- -   @instance@ is the instance used to create the surface.
--
-- -   @surface@ is the surface to destroy.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- = Description
--
-- Destroying a 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' merely
-- severs the connection between Vulkan and the native surface, and does
-- not imply destroying the native surface, closing a window, or similar
-- behavior.
--
-- == Valid Usage
--
-- -   All 'Graphics.Vulkan.Extensions.Handles.SwapchainKHR' objects
--     created for @surface@ /must/ have been destroyed prior to destroying
--     @surface@
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @surface@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @surface@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Instance' handle
--
-- -   If @surface@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @surface@ /must/
--     be a valid 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If @surface@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @instance@
--
-- == Host Synchronization
--
-- -   Host access to @surface@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Instance',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
destroySurfaceKHR :: Instance -> SurfaceKHR -> ("allocator" ::: Maybe AllocationCallbacks) -> IO ()
destroySurfaceKHR instance' surface allocator = evalContT $ do
  let vkDestroySurfaceKHR' = mkVkDestroySurfaceKHR (pVkDestroySurfaceKHR (instanceCmds (instance' :: Instance)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroySurfaceKHR' (instanceHandle (instance')) (surface) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> SurfaceKHR -> Ptr Bool32 -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> SurfaceKHR -> Ptr Bool32 -> IO Result

-- | vkGetPhysicalDeviceSurfaceSupportKHR - Query if presentation is
-- supported
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family.
--
-- -   @surface@ is the surface.
--
-- -   @pSupported@ is a pointer to a
--     'Graphics.Vulkan.Core10.BaseType.Bool32', which is set to
--     'Graphics.Vulkan.Core10.BaseType.TRUE' to indicate support, and
--     'Graphics.Vulkan.Core10.BaseType.FALSE' otherwise.
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     for the given @physicalDevice@
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   @pSupported@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.BaseType.Bool32' value
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Instance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfaceSupportKHR :: PhysicalDevice -> ("queueFamilyIndex" ::: Word32) -> SurfaceKHR -> IO (("supported" ::: Bool))
getPhysicalDeviceSurfaceSupportKHR physicalDevice queueFamilyIndex surface = evalContT $ do
  let vkGetPhysicalDeviceSurfaceSupportKHR' = mkVkGetPhysicalDeviceSurfaceSupportKHR (pVkGetPhysicalDeviceSurfaceSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice)))
  pPSupported <- ContT $ bracket (callocBytes @Bool32 4) free
  r <- lift $ vkGetPhysicalDeviceSurfaceSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (surface) (pPSupported)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSupported <- lift $ peek @Bool32 pPSupported
  pure $ ((bool32ToBool pSupported))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilitiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilitiesKHR -> IO Result

-- | vkGetPhysicalDeviceSurfaceCapabilitiesKHR - Query surface capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pSurfaceCapabilities@ is a pointer to a 'SurfaceCapabilitiesKHR'
--     structure in which the capabilities are returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   @pSurfaceCapabilities@ /must/ be a valid pointer to a
--     'SurfaceCapabilitiesKHR' structure
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Instance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice',
-- 'SurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfaceCapabilitiesKHR :: PhysicalDevice -> SurfaceKHR -> IO (SurfaceCapabilitiesKHR)
getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface = evalContT $ do
  let vkGetPhysicalDeviceSurfaceCapabilitiesKHR' = mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR (pVkGetPhysicalDeviceSurfaceCapabilitiesKHR (instanceCmds (physicalDevice :: PhysicalDevice)))
  pPSurfaceCapabilities <- ContT (withZeroCStruct @SurfaceCapabilitiesKHR)
  r <- lift $ vkGetPhysicalDeviceSurfaceCapabilitiesKHR' (physicalDeviceHandle (physicalDevice)) (surface) (pPSurfaceCapabilities)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceCapabilities <- lift $ peekCStruct @SurfaceCapabilitiesKHR pPSurfaceCapabilities
  pure $ (pSurfaceCapabilities)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceFormatsKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr SurfaceFormatKHR -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr SurfaceFormatKHR -> IO Result

-- | vkGetPhysicalDeviceSurfaceFormatsKHR - Query color formats supported by
-- surface
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pSurfaceFormatCount@ is a pointer to an integer related to the
--     number of format pairs available or queried, as described below.
--
-- -   @pSurfaceFormats@ is either @NULL@ or a pointer to an array of
--     'SurfaceFormatKHR' structures.
--
-- = Description
--
-- If @pSurfaceFormats@ is @NULL@, then the number of format pairs
-- supported for the given @surface@ is returned in @pSurfaceFormatCount@.
-- Otherwise, @pSurfaceFormatCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pSurfaceFormats@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pSurfaceFormats@. If the value of
-- @pSurfaceFormatCount@ is less than the number of format pairs supported,
-- at most @pSurfaceFormatCount@ structures will be written. If
-- @pSurfaceFormatCount@ is smaller than the number of format pairs
-- supported for the given @surface@,
-- 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned
-- instead of 'Graphics.Vulkan.Core10.Enums.Result.SUCCESS' to indicate
-- that not all the available values were returned.
--
-- The number of format pairs supported /must/ be greater than or equal to
-- 1. @pSurfaceFormats@ /must/ not contain an entry whose value for
-- @format@ is 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'.
--
-- If @pSurfaceFormats@ includes an entry whose value for @colorSpace@ is
-- 'Graphics.Vulkan.Extensions.VK_EXT_swapchain_colorspace.COLOR_SPACE_SRGB_NONLINEAR_KHR'
-- and whose value for @format@ is a UNORM (or SRGB) format and the
-- corresponding SRGB (or UNORM) format is a color renderable format for
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', then
-- @pSurfaceFormats@ /must/ also contain an entry with the same value for
-- @colorSpace@ and @format@ equal to the corresponding SRGB (or UNORM)
-- format.
--
-- == Valid Usage
--
-- -   @surface@ must be supported by @physicalDevice@, as reported by
--     'getPhysicalDeviceSurfaceSupportKHR' or an equivalent
--     platform-specific mechanism.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   @pSurfaceFormatCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pSurfaceFormatCount@ is not @0@, and
--     @pSurfaceFormats@ is not @NULL@, @pSurfaceFormats@ /must/ be a valid
--     pointer to an array of @pSurfaceFormatCount@ 'SurfaceFormatKHR'
--     structures
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Instance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice', 'SurfaceFormatKHR',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice -> SurfaceKHR -> IO (Result, ("surfaceFormats" ::: Vector SurfaceFormatKHR))
getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface = evalContT $ do
  let vkGetPhysicalDeviceSurfaceFormatsKHR' = mkVkGetPhysicalDeviceSurfaceFormatsKHR (pVkGetPhysicalDeviceSurfaceFormatsKHR (instanceCmds (physicalDevice :: PhysicalDevice)))
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPSurfaceFormatCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceSurfaceFormatsKHR' physicalDevice' (surface) (pPSurfaceFormatCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceFormatCount <- lift $ peek @Word32 pPSurfaceFormatCount
  pPSurfaceFormats <- ContT $ bracket (callocBytes @SurfaceFormatKHR ((fromIntegral (pSurfaceFormatCount)) * 8)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSurfaceFormats `advancePtrBytes` (i * 8) :: Ptr SurfaceFormatKHR) . ($ ())) [0..(fromIntegral (pSurfaceFormatCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceSurfaceFormatsKHR' physicalDevice' (surface) (pPSurfaceFormatCount) ((pPSurfaceFormats))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pSurfaceFormatCount' <- lift $ peek @Word32 pPSurfaceFormatCount
  pSurfaceFormats' <- lift $ generateM (fromIntegral (pSurfaceFormatCount')) (\i -> peekCStruct @SurfaceFormatKHR (((pPSurfaceFormats) `advancePtrBytes` (8 * (i)) :: Ptr SurfaceFormatKHR)))
  pure $ ((r'), pSurfaceFormats')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result

-- | vkGetPhysicalDeviceSurfacePresentModesKHR - Query supported presentation
-- modes
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pPresentModeCount@ is a pointer to an integer related to the number
--     of presentation modes available or queried, as described below.
--
-- -   @pPresentModes@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PresentModeKHR'
--     values, indicating the supported presentation modes.
--
-- = Description
--
-- If @pPresentModes@ is @NULL@, then the number of presentation modes
-- supported for the given @surface@ is returned in @pPresentModeCount@.
-- Otherwise, @pPresentModeCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pPresentModes@ array, and on
-- return the variable is overwritten with the number of values actually
-- written to @pPresentModes@. If the value of @pPresentModeCount@ is less
-- than the number of presentation modes supported, at most
-- @pPresentModeCount@ values will be written. If @pPresentModeCount@ is
-- smaller than the number of presentation modes supported for the given
-- @surface@, 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Graphics.Vulkan.Core10.Enums.Result.SUCCESS' to
-- indicate that not all the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   @pPresentModeCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPresentModeCount@ is not @0@, and
--     @pPresentModes@ is not @NULL@, @pPresentModes@ /must/ be a valid
--     pointer to an array of @pPresentModeCount@
--     'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PresentModeKHR'
--     values
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Instance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice',
-- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PresentModeKHR',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice -> SurfaceKHR -> IO (Result, ("presentModes" ::: Vector PresentModeKHR))
getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface = evalContT $ do
  let vkGetPhysicalDeviceSurfacePresentModesKHR' = mkVkGetPhysicalDeviceSurfacePresentModesKHR (pVkGetPhysicalDeviceSurfacePresentModesKHR (instanceCmds (physicalDevice :: PhysicalDevice)))
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPresentModeCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceSurfacePresentModesKHR' physicalDevice' (surface) (pPPresentModeCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPresentModeCount <- lift $ peek @Word32 pPPresentModeCount
  pPPresentModes <- ContT $ bracket (callocBytes @PresentModeKHR ((fromIntegral (pPresentModeCount)) * 4)) free
  r' <- lift $ vkGetPhysicalDeviceSurfacePresentModesKHR' physicalDevice' (surface) (pPPresentModeCount) (pPPresentModes)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPresentModeCount' <- lift $ peek @Word32 pPPresentModeCount
  pPresentModes' <- lift $ generateM (fromIntegral (pPresentModeCount')) (\i -> peek @PresentModeKHR ((pPPresentModes `advancePtrBytes` (4 * (i)) :: Ptr PresentModeKHR)))
  pure $ ((r'), pPresentModes')


-- | VkSurfaceCapabilitiesKHR - Structure describing capabilities of a
-- surface
--
-- = Description
--
-- Note
--
-- Supported usage flags of a presentable image when using
-- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
-- or
-- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR'
-- presentation mode are provided by
-- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.SharedPresentSurfaceCapabilitiesKHR'::@sharedPresentSupportedUsageFlags@.
--
-- Note
--
-- Formulas such as min(N, @maxImageCount@) are not correct, since
-- @maxImageCount@ /may/ be zero.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.CompositeAlphaFlagsKHR',
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent2D',
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.SurfaceTransformFlagBitsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.SurfaceTransformFlagsKHR',
-- 'getPhysicalDeviceSurfaceCapabilitiesKHR'
data SurfaceCapabilitiesKHR = SurfaceCapabilitiesKHR
  { -- | @minImageCount@ is the minimum number of images the specified device
    -- supports for a swapchain created for the surface, and will be at least
    -- one.
    minImageCount :: Word32
  , -- | @maxImageCount@ is the maximum number of images the specified device
    -- supports for a swapchain created for the surface, and will be either 0,
    -- or greater than or equal to @minImageCount@. A value of 0 means that
    -- there is no limit on the number of images, though there /may/ be limits
    -- related to the total amount of memory used by presentable images.
    maxImageCount :: Word32
  , -- | @currentExtent@ is the current width and height of the surface, or the
    -- special value (0xFFFFFFFF, 0xFFFFFFFF) indicating that the surface size
    -- will be determined by the extent of a swapchain targeting the surface.
    currentExtent :: Extent2D
  , -- | @minImageExtent@ contains the smallest valid swapchain extent for the
    -- surface on the specified device. The @width@ and @height@ of the extent
    -- will each be less than or equal to the corresponding @width@ and
    -- @height@ of @currentExtent@, unless @currentExtent@ has the special
    -- value described above.
    minImageExtent :: Extent2D
  , -- | @maxImageExtent@ contains the largest valid swapchain extent for the
    -- surface on the specified device. The @width@ and @height@ of the extent
    -- will each be greater than or equal to the corresponding @width@ and
    -- @height@ of @minImageExtent@. The @width@ and @height@ of the extent
    -- will each be greater than or equal to the corresponding @width@ and
    -- @height@ of @currentExtent@, unless @currentExtent@ has the special
    -- value described above.
    maxImageExtent :: Extent2D
  , -- | @maxImageArrayLayers@ is the maximum number of layers presentable images
    -- /can/ have for a swapchain created for this device and surface, and will
    -- be at least one.
    maxImageArrayLayers :: Word32
  , -- | @supportedTransforms@ is a bitmask of
    -- 'Graphics.Vulkan.Extensions.VK_KHR_display.SurfaceTransformFlagBitsKHR'
    -- indicating the presentation transforms supported for the surface on the
    -- specified device. At least one bit will be set.
    supportedTransforms :: SurfaceTransformFlagsKHR
  , -- | @currentTransform@ is
    -- 'Graphics.Vulkan.Extensions.VK_KHR_display.SurfaceTransformFlagBitsKHR'
    -- value indicating the surface’s current transform relative to the
    -- presentation engine’s natural orientation.
    currentTransform :: SurfaceTransformFlagBitsKHR
  , -- | @supportedCompositeAlpha@ is a bitmask of
    -- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.CompositeAlphaFlagBitsKHR',
    -- representing the alpha compositing modes supported by the presentation
    -- engine for the surface on the specified device, and at least one bit
    -- will be set. Opaque composition /can/ be achieved in any alpha
    -- compositing mode by either using an image format that has no alpha
    -- component, or by ensuring that all pixels in the presentable images have
    -- an alpha value of 1.0.
    supportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- | @supportedUsageFlags@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits'
    -- representing the ways the application /can/ use the presentable images
    -- of a swapchain created with
    -- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PresentModeKHR'
    -- set to
    -- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PRESENT_MODE_IMMEDIATE_KHR',
    -- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PRESENT_MODE_MAILBOX_KHR',
    -- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PRESENT_MODE_FIFO_KHR'
    -- or
    -- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.PRESENT_MODE_FIFO_RELAXED_KHR'
    -- for the surface on the specified device.
    -- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
    -- /must/ be included in the set but implementations /may/ support
    -- additional usages.
    supportedUsageFlags :: ImageUsageFlags
  }
  deriving (Typeable)
deriving instance Show SurfaceCapabilitiesKHR

instance ToCStruct SurfaceCapabilitiesKHR where
  withCStruct x f = allocaBytesAligned 52 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilitiesKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (minImageCount)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (maxImageCount)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr Extent2D)) (currentExtent) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (minImageExtent) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr Extent2D)) (maxImageExtent) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (maxImageArrayLayers)
    lift $ poke ((p `plusPtr` 36 :: Ptr SurfaceTransformFlagsKHR)) (supportedTransforms)
    lift $ poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (currentTransform)
    lift $ poke ((p `plusPtr` 44 :: Ptr CompositeAlphaFlagsKHR)) (supportedCompositeAlpha)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageUsageFlags)) (supportedUsageFlags)
    lift $ f
  cStructSize = 52
  cStructAlignment = 4
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr Extent2D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    lift $ f

instance FromCStruct SurfaceCapabilitiesKHR where
  peekCStruct p = do
    minImageCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    maxImageCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    currentExtent <- peekCStruct @Extent2D ((p `plusPtr` 8 :: Ptr Extent2D))
    minImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    maxImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    maxImageArrayLayers <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    supportedTransforms <- peek @SurfaceTransformFlagsKHR ((p `plusPtr` 36 :: Ptr SurfaceTransformFlagsKHR))
    currentTransform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR))
    supportedCompositeAlpha <- peek @CompositeAlphaFlagsKHR ((p `plusPtr` 44 :: Ptr CompositeAlphaFlagsKHR))
    supportedUsageFlags <- peek @ImageUsageFlags ((p `plusPtr` 48 :: Ptr ImageUsageFlags))
    pure $ SurfaceCapabilitiesKHR
             minImageCount maxImageCount currentExtent minImageExtent maxImageExtent maxImageArrayLayers supportedTransforms currentTransform supportedCompositeAlpha supportedUsageFlags

instance Zero SurfaceCapabilitiesKHR where
  zero = SurfaceCapabilitiesKHR
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


-- | VkSurfaceFormatKHR - Structure describing a supported swapchain
-- format-color space pair
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_swapchain_colorspace.ColorSpaceKHR',
-- 'Graphics.Vulkan.Core10.Enums.Format.Format',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceFormat2KHR',
-- 'getPhysicalDeviceSurfaceFormatsKHR'
data SurfaceFormatKHR = SurfaceFormatKHR
  { -- | @format@ is a 'Graphics.Vulkan.Core10.Enums.Format.Format' that is
    -- compatible with the specified surface.
    format :: Format
  , -- | @colorSpace@ is a presentation
    -- 'Graphics.Vulkan.Extensions.VK_EXT_swapchain_colorspace.ColorSpaceKHR'
    -- that is compatible with the surface.
    colorSpace :: ColorSpaceKHR
  }
  deriving (Typeable)
deriving instance Show SurfaceFormatKHR

instance ToCStruct SurfaceFormatKHR where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceFormatKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Format)) (format)
    poke ((p `plusPtr` 4 :: Ptr ColorSpaceKHR)) (colorSpace)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ColorSpaceKHR)) (zero)
    f

instance FromCStruct SurfaceFormatKHR where
  peekCStruct p = do
    format <- peek @Format ((p `plusPtr` 0 :: Ptr Format))
    colorSpace <- peek @ColorSpaceKHR ((p `plusPtr` 4 :: Ptr ColorSpaceKHR))
    pure $ SurfaceFormatKHR
             format colorSpace

instance Storable SurfaceFormatKHR where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceFormatKHR where
  zero = SurfaceFormatKHR
           zero
           zero


type KHR_SURFACE_SPEC_VERSION = 25

-- No documentation found for TopLevel "VK_KHR_SURFACE_SPEC_VERSION"
pattern KHR_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SURFACE_SPEC_VERSION = 25


type KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"

-- No documentation found for TopLevel "VK_KHR_SURFACE_EXTENSION_NAME"
pattern KHR_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"

