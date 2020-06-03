{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_swapchain  ( createSwapchainKHR
                                           , withSwapchainKHR
                                           , destroySwapchainKHR
                                           , getSwapchainImagesKHR
                                           , acquireNextImageKHR
                                           , acquireNextImageKHRSafe
                                           , queuePresentKHR
                                           , getDeviceGroupPresentCapabilitiesKHR
                                           , getDeviceGroupSurfacePresentModesKHR
                                           , acquireNextImage2KHR
                                           , acquireNextImage2KHRSafe
                                           , getPhysicalDevicePresentRectanglesKHR
                                           , SwapchainCreateInfoKHR(..)
                                           , PresentInfoKHR(..)
                                           , DeviceGroupPresentCapabilitiesKHR(..)
                                           , ImageSwapchainCreateInfoKHR(..)
                                           , BindImageMemorySwapchainInfoKHR(..)
                                           , AcquireNextImageInfoKHR(..)
                                           , DeviceGroupPresentInfoKHR(..)
                                           , DeviceGroupSwapchainCreateInfoKHR(..)
                                           , DeviceGroupPresentModeFlagBitsKHR( DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
                                                                              , DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
                                                                              , DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
                                                                              , DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
                                                                              , ..
                                                                              )
                                           , DeviceGroupPresentModeFlagsKHR
                                           , SwapchainCreateFlagBitsKHR( SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR
                                                                       , SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
                                                                       , SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
                                                                       , ..
                                                                       )
                                           , SwapchainCreateFlagsKHR
                                           , KHR_SWAPCHAIN_SPEC_VERSION
                                           , pattern KHR_SWAPCHAIN_SPEC_VERSION
                                           , KHR_SWAPCHAIN_EXTENSION_NAME
                                           , pattern KHR_SWAPCHAIN_EXTENSION_NAME
                                           , SurfaceKHR(..)
                                           , SwapchainKHR(..)
                                           , PresentModeKHR(..)
                                           , ColorSpaceKHR(..)
                                           , CompositeAlphaFlagBitsKHR(..)
                                           , CompositeAlphaFlagsKHR
                                           , SurfaceTransformFlagBitsKHR(..)
                                           , SurfaceTransformFlagsKHR
                                           ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
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
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.String (IsString)
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
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR)
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagBitsKHR)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAcquireNextImage2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkAcquireNextImageKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCreateSwapchainKHR))
import Vulkan.Dynamic (DeviceCmds(pVkDestroySwapchainKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceGroupPresentCapabilitiesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceGroupSurfacePresentModesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetSwapchainImagesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkQueuePresentKHR))
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display_swapchain (DisplayPresentInfoKHR)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDevicePresentRectanglesKHR))
import Vulkan.Core10.APIConstants (MAX_DEVICE_GROUP_SIZE)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_GGP_frame_token (PresentFrameTokenGGP)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_incremental_present (PresentRegionsKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (PresentTimesInfoGOOGLE)
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core10.Enums.SharingMode (SharingMode)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveWin32InfoEXT)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (SwapchainCounterCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_display_native_hdr (SwapchainDisplayNativeHdrCreateInfoAMD)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.APIConstants (pattern MAX_DEVICE_GROUP_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSwapchainKHR
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct SwapchainCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr SwapchainKHR -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct SwapchainCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr SwapchainKHR -> IO Result

-- | vkCreateSwapchainKHR - Create a swapchain
--
-- = Description
--
-- If the @oldSwapchain@ parameter of @pCreateInfo@ is a valid swapchain,
-- which has exclusive full-screen access, that access is released from
-- @oldSwapchain@. If the command succeeds in this case, the newly created
-- swapchain will automatically acquire exclusive full-screen access from
-- @oldSwapchain@.
--
-- Note
--
-- This implicit transfer is intended to avoid exiting and entering
-- full-screen exclusive mode, which may otherwise cause unwanted visual
-- updates to the display.
--
-- In some cases, swapchain creation /may/ fail if exclusive full-screen
-- mode is requested for application control, but for some
-- implementation-specific reason exclusive full-screen access is
-- unavailable for the particular combination of parameters provided. If
-- this occurs, 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
-- will be returned.
--
-- Note
--
-- In particular, it will fail if the @imageExtent@ member of @pCreateInfo@
-- does not match the extents of the monitor. Other reasons for failure may
-- include the app not being set as high-dpi aware, or if the physical
-- device and monitor are not compatible in this mode.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'SwapchainCreateInfoKHR' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pSwapchain@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- == Host Synchronization
--
-- -   Host access to @pCreateInfo->surface@ /must/ be externally
--     synchronized
--
-- -   Host access to @pCreateInfo->oldSwapchain@ /must/ be externally
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
createSwapchainKHR :: forall a io
                    . (Extendss SwapchainCreateInfoKHR a, PokeChain a, MonadIO io)
                   => -- | @device@ is the device to create the swapchain for.
                      Device
                   -> -- | @pCreateInfo@ is a pointer to a 'SwapchainCreateInfoKHR' structure
                      -- specifying the parameters of the created swapchain.
                      (SwapchainCreateInfoKHR a)
                   -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                      -- swapchain object when there is no more specific allocator available (see
                      -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                      ("allocator" ::: Maybe AllocationCallbacks)
                   -> io (SwapchainKHR)
createSwapchainKHR device createInfo allocator = liftIO . evalContT $ do
  let vkCreateSwapchainKHRPtr = pVkCreateSwapchainKHR (deviceCmds (device :: Device))
  lift $ unless (vkCreateSwapchainKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateSwapchainKHR is null" Nothing Nothing
  let vkCreateSwapchainKHR' = mkVkCreateSwapchainKHR vkCreateSwapchainKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSwapchain <- ContT $ bracket (callocBytes @SwapchainKHR 8) free
  r <- lift $ vkCreateSwapchainKHR' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPSwapchain)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSwapchain <- lift $ peek @SwapchainKHR pPSwapchain
  pure $ (pSwapchain)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSwapchainKHR' and 'destroySwapchainKHR'
--
-- To ensure that 'destroySwapchainKHR' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withSwapchainKHR :: forall a io r . (Extendss SwapchainCreateInfoKHR a, PokeChain a, MonadIO io) => Device -> SwapchainCreateInfoKHR a -> Maybe AllocationCallbacks -> (io (SwapchainKHR) -> ((SwapchainKHR) -> io ()) -> r) -> r
withSwapchainKHR device pCreateInfo pAllocator b =
  b (createSwapchainKHR device pCreateInfo pAllocator)
    (\(o0) -> destroySwapchainKHR device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySwapchainKHR
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> SwapchainKHR -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroySwapchainKHR - Destroy a swapchain object
--
-- = Description
--
-- The application /must/ not destroy a swapchain until after completion of
-- all outstanding operations on images that were acquired from the
-- swapchain. @swapchain@ and all associated 'Vulkan.Core10.Handles.Image'
-- handles are destroyed, and /must/ not be acquired or used any more by
-- the application. The memory of each 'Vulkan.Core10.Handles.Image' will
-- only be freed after that image is no longer used by the presentation
-- engine. For example, if one image of the swapchain is being displayed in
-- a window, the memory for that image /may/ not be freed until the window
-- is destroyed, or another swapchain is created for the window. Destroying
-- the swapchain does not invalidate the parent
-- 'Vulkan.Extensions.Handles.SurfaceKHR', and a new swapchain /can/ be
-- created with it.
--
-- When a swapchain associated with a display surface is destroyed, if the
-- image most recently presented to the display surface is from the
-- swapchain being destroyed, then either any display resources modified by
-- presenting images from any swapchain associated with the display surface
-- /must/ be reverted by the implementation to their state prior to the
-- first present performed on one of these swapchains, or such resources
-- /must/ be left in their current state.
--
-- If @swapchain@ has exclusive full-screen access, it is released before
-- the swapchain is destroyed.
--
-- == Valid Usage
--
-- -   All uses of presentable images acquired from @swapchain@ /must/ have
--     completed execution
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @swapchain@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @swapchain@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @swapchain@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   Both of @device@, and @swapchain@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.SwapchainKHR'
destroySwapchainKHR :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the 'Vulkan.Core10.Handles.Device' associated with
                       -- @swapchain@.
                       Device
                    -> -- | @swapchain@ is the swapchain to destroy.
                       SwapchainKHR
                    -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                       -- swapchain object when there is no more specific allocator available (see
                       -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io ()
destroySwapchainKHR device swapchain allocator = liftIO . evalContT $ do
  let vkDestroySwapchainKHRPtr = pVkDestroySwapchainKHR (deviceCmds (device :: Device))
  lift $ unless (vkDestroySwapchainKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroySwapchainKHR is null" Nothing Nothing
  let vkDestroySwapchainKHR' = mkVkDestroySwapchainKHR vkDestroySwapchainKHRPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroySwapchainKHR' (deviceHandle (device)) (swapchain) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainImagesKHR
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr Word32 -> Ptr Image -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr Word32 -> Ptr Image -> IO Result

-- | vkGetSwapchainImagesKHR - Obtain the array of presentable images
-- associated with a swapchain
--
-- = Description
--
-- If @pSwapchainImages@ is @NULL@, then the number of presentable images
-- for @swapchain@ is returned in @pSwapchainImageCount@. Otherwise,
-- @pSwapchainImageCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pSwapchainImages@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pSwapchainImages@. If the value of @pSwapchainImageCount@ is less
-- than the number of presentable images for @swapchain@, at most
-- @pSwapchainImageCount@ structures will be written. If
-- @pSwapchainImageCount@ is smaller than the number of presentable images
-- for @swapchain@, 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS' to indicate
-- that not all the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   @pSwapchainImageCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pSwapchainImageCount@ is not @0@, and
--     @pSwapchainImages@ is not @NULL@, @pSwapchainImages@ /must/ be a
--     valid pointer to an array of @pSwapchainImageCount@
--     'Vulkan.Core10.Handles.Image' handles
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
getSwapchainImagesKHR :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the device associated with @swapchain@.
                         Device
                      -> -- | @swapchain@ is the swapchain to query.
                         SwapchainKHR
                      -> io (Result, ("swapchainImages" ::: Vector Image))
getSwapchainImagesKHR device swapchain = liftIO . evalContT $ do
  let vkGetSwapchainImagesKHRPtr = pVkGetSwapchainImagesKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetSwapchainImagesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSwapchainImagesKHR is null" Nothing Nothing
  let vkGetSwapchainImagesKHR' = mkVkGetSwapchainImagesKHR vkGetSwapchainImagesKHRPtr
  let device' = deviceHandle (device)
  pPSwapchainImageCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetSwapchainImagesKHR' device' (swapchain) (pPSwapchainImageCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSwapchainImageCount <- lift $ peek @Word32 pPSwapchainImageCount
  pPSwapchainImages <- ContT $ bracket (callocBytes @Image ((fromIntegral (pSwapchainImageCount)) * 8)) free
  r' <- lift $ vkGetSwapchainImagesKHR' device' (swapchain) (pPSwapchainImageCount) (pPSwapchainImages)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pSwapchainImageCount' <- lift $ peek @Word32 pPSwapchainImageCount
  pSwapchainImages' <- lift $ generateM (fromIntegral (pSwapchainImageCount')) (\i -> peek @Image ((pPSwapchainImages `advancePtrBytes` (8 * (i)) :: Ptr Image)))
  pure $ ((r'), pSwapchainImages')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireNextImageKHRUnsafe
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result

foreign import ccall
  "dynamic" mkVkAcquireNextImageKHRSafe
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result

-- | acquireNextImageKHR with selectable safeness
acquireNextImageKHRSafeOrUnsafe :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for TopLevel ""
                                   (FunPtr (Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result)
                                -> -- | @device@ is the device associated with @swapchain@.
                                   Device
                                -> -- | @swapchain@ is the non-retired swapchain from which an image is being
                                   -- acquired.
                                   SwapchainKHR
                                -> -- | @timeout@ specifies how long the function waits, in nanoseconds, if no
                                   -- image is available.
                                   ("timeout" ::: Word64)
                                -> -- | @semaphore@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a semaphore
                                   -- to signal.
                                   Semaphore
                                -> -- | @fence@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a fence to
                                   -- signal.
                                   Fence
                                -> io (Result, ("imageIndex" ::: Word32))
acquireNextImageKHRSafeOrUnsafe mkVkAcquireNextImageKHR device swapchain timeout semaphore fence = liftIO . evalContT $ do
  let vkAcquireNextImageKHRPtr = pVkAcquireNextImageKHR (deviceCmds (device :: Device))
  lift $ unless (vkAcquireNextImageKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireNextImageKHR is null" Nothing Nothing
  let vkAcquireNextImageKHR' = mkVkAcquireNextImageKHR vkAcquireNextImageKHRPtr
  pPImageIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkAcquireNextImageKHR' (deviceHandle (device)) (swapchain) (timeout) (semaphore) (fence) (pPImageIndex)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImageIndex <- lift $ peek @Word32 pPImageIndex
  pure $ (r, pImageIndex)

-- | vkAcquireNextImageKHR - Retrieve the index of the next available
-- presentable image
--
-- == Valid Usage
--
-- -   @swapchain@ /must/ not be in the retired state
--
-- -   If @semaphore@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' it
--     /must/ be unsignaled
--
-- -   If @semaphore@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' it
--     /must/ not have any uncompleted signal or wait operations pending
--
-- -   If @fence@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/
--     be unsignaled and /must/ not be associated with any other queue
--     command that has not yet completed execution on that queue
--
-- -   @semaphore@ and @fence@ /must/ not both be equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If the number of currently acquired images is greater than the
--     difference between the number of images in @swapchain@ and the value
--     of
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@minImageCount@
--     as returned by a call to
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     with the @surface@ used to create @swapchain@, @timeout@ /must/ not
--     be @UINT64_MAX@
--
-- -   @semaphore@ /must/ have a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   If @semaphore@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @semaphore@ /must/ be a valid 'Vulkan.Core10.Handles.Semaphore'
--     handle
--
-- -   If @fence@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@
--     /must/ be a valid 'Vulkan.Core10.Handles.Fence' handle
--
-- -   @pImageIndex@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If @semaphore@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- -   If @fence@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- -   Both of @device@, and @swapchain@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
--
--     -   'Vulkan.Core10.Enums.Result.NOT_READY'
--
--     -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Fence',
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
acquireNextImageKHR :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the device associated with @swapchain@.
                       Device
                    -> -- | @swapchain@ is the non-retired swapchain from which an image is being
                       -- acquired.
                       SwapchainKHR
                    -> -- | @timeout@ specifies how long the function waits, in nanoseconds, if no
                       -- image is available.
                       ("timeout" ::: Word64)
                    -> -- | @semaphore@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a semaphore
                       -- to signal.
                       Semaphore
                    -> -- | @fence@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a fence to
                       -- signal.
                       Fence
                    -> io (Result, ("imageIndex" ::: Word32))
acquireNextImageKHR = acquireNextImageKHRSafeOrUnsafe mkVkAcquireNextImageKHRUnsafe

-- | A variant of 'acquireNextImageKHR' which makes a *safe* FFI call
acquireNextImageKHRSafe :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the device associated with @swapchain@.
                           Device
                        -> -- | @swapchain@ is the non-retired swapchain from which an image is being
                           -- acquired.
                           SwapchainKHR
                        -> -- | @timeout@ specifies how long the function waits, in nanoseconds, if no
                           -- image is available.
                           ("timeout" ::: Word64)
                        -> -- | @semaphore@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a semaphore
                           -- to signal.
                           Semaphore
                        -> -- | @fence@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a fence to
                           -- signal.
                           Fence
                        -> io (Result, ("imageIndex" ::: Word32))
acquireNextImageKHRSafe = acquireNextImageKHRSafeOrUnsafe mkVkAcquireNextImageKHRSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueuePresentKHR
  :: FunPtr (Ptr Queue_T -> Ptr (SomeStruct PresentInfoKHR) -> IO Result) -> Ptr Queue_T -> Ptr (SomeStruct PresentInfoKHR) -> IO Result

-- | vkQueuePresentKHR - Queue an image for presentation
--
-- = Description
--
-- Note
--
-- There is no requirement for an application to present images in the same
-- order that they were acquired - applications can arbitrarily present any
-- image that is currently acquired.
--
-- == Valid Usage
--
-- -   Each element of @pSwapchains@ member of @pPresentInfo@ /must/ be a
--     swapchain that is created for a surface for which presentation is
--     supported from @queue@ as determined using a call to
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
--
-- -   If more than one member of @pSwapchains@ was created from a display
--     surface, all display surfaces referenced that refer to the same
--     display /must/ use the same display mode
--
-- -   When a semaphore wait operation referring to a binary semaphore
--     defined by the elements of the @pWaitSemaphores@ member of
--     @pPresentInfo@ executes on @queue@, there /must/ be no other queues
--     waiting on the same semaphore
--
-- -   All elements of the @pWaitSemaphores@ member of @pPresentInfo@
--     /must/ be semaphores that are signaled, or have
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>
--     previously submitted for execution
--
-- -   All elements of the @pWaitSemaphores@ member of @pPresentInfo@
--     /must/ be created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'
--
-- -   All elements of the @pWaitSemaphores@ member of @pPresentInfo@
--     /must/ reference a semaphore signal operation that has been
--     submitted for execution and any semaphore signal operations on which
--     it depends (if any) /must/ have also been submitted for execution
--
-- Any writes to memory backing the images referenced by the
-- @pImageIndices@ and @pSwapchains@ members of @pPresentInfo@, that are
-- available before 'queuePresentKHR' is executed, are automatically made
-- visible to the read access performed by the presentation engine. This
-- automatic visibility operation for an image happens-after the semaphore
-- signal operation, and happens-before the presentation engine accesses
-- the image.
--
-- Queueing an image for presentation defines a set of /queue operations/,
-- including waiting on the semaphores and submitting a presentation
-- request to the presentation engine. However, the scope of this set of
-- queue operations does not include the actual processing of the image by
-- the presentation engine.
--
-- Note
--
-- The origin of the native orientation of the surface coordinate system is
-- not specified in the Vulkan specification; it depends on the platform.
-- For most platforms the origin is by default upper-left, meaning the
-- pixel of the presented 'Vulkan.Core10.Handles.Image' at coordinates
-- (0,0) would appear at the upper left pixel of the platform surface
-- (assuming
-- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
-- and the display standing the right way up).
--
-- If 'queuePresentKHR' fails to enqueue the corresponding set of queue
-- operations, it /may/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' or
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'. If it does, the
-- implementation /must/ ensure that the state and contents of any
-- resources or synchronization primitives referenced is unaffected by the
-- call or its failure.
--
-- If 'queuePresentKHR' fails in such a way that the implementation is
-- unable to make that guarantee, the implementation /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'.
--
-- However, if the presentation request is rejected by the presentation
-- engine with an error 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR',
-- 'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT',
-- or 'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR', the set of queue
-- operations are still considered to be enqueued and thus any semaphore
-- wait operation specified in 'PresentInfoKHR' will execute when the
-- corresponding queue operation is complete.
--
-- If any @swapchain@ member of @pPresentInfo@ was created with
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT',
-- 'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
-- will be returned if that swapchain does not have exclusive full-screen
-- access, possibly for implementation-specific reasons outside of the
-- application’s control.
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid 'Vulkan.Core10.Handles.Queue' handle
--
-- -   @pPresentInfo@ /must/ be a valid pointer to a valid 'PresentInfoKHR'
--     structure
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized
--
-- -   Host access to @pPresentInfo->pWaitSemaphores@[] /must/ be
--     externally synchronized
--
-- -   Host access to @pPresentInfo->pSwapchains@[] /must/ be externally
--     synchronized
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
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- = See Also
--
-- 'PresentInfoKHR', 'Vulkan.Core10.Handles.Queue'
queuePresentKHR :: forall a io
                 . (Extendss PresentInfoKHR a, PokeChain a, MonadIO io)
                => -- | @queue@ is a queue that is capable of presentation to the target
                   -- surface’s platform on the same device as the image’s swapchain.
                   Queue
                -> -- | @pPresentInfo@ is a pointer to a 'PresentInfoKHR' structure specifying
                   -- parameters of the presentation.
                   (PresentInfoKHR a)
                -> io (Result)
queuePresentKHR queue presentInfo = liftIO . evalContT $ do
  let vkQueuePresentKHRPtr = pVkQueuePresentKHR (deviceCmds (queue :: Queue))
  lift $ unless (vkQueuePresentKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueuePresentKHR is null" Nothing Nothing
  let vkQueuePresentKHR' = mkVkQueuePresentKHR vkQueuePresentKHRPtr
  pPresentInfo <- ContT $ withCStruct (presentInfo)
  r <- lift $ vkQueuePresentKHR' (queueHandle (queue)) (forgetExtensions pPresentInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupPresentCapabilitiesKHR
  :: FunPtr (Ptr Device_T -> Ptr DeviceGroupPresentCapabilitiesKHR -> IO Result) -> Ptr Device_T -> Ptr DeviceGroupPresentCapabilitiesKHR -> IO Result

-- | vkGetDeviceGroupPresentCapabilitiesKHR - Query present capabilities from
-- other physical devices
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
-- 'Vulkan.Core10.Handles.Device', 'DeviceGroupPresentCapabilitiesKHR'
getDeviceGroupPresentCapabilitiesKHR :: forall io
                                      . (MonadIO io)
                                     => -- | @device@ is the logical device.
                                        --
                                        -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                        Device
                                     -> io (DeviceGroupPresentCapabilitiesKHR)
getDeviceGroupPresentCapabilitiesKHR device = liftIO . evalContT $ do
  let vkGetDeviceGroupPresentCapabilitiesKHRPtr = pVkGetDeviceGroupPresentCapabilitiesKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceGroupPresentCapabilitiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceGroupPresentCapabilitiesKHR is null" Nothing Nothing
  let vkGetDeviceGroupPresentCapabilitiesKHR' = mkVkGetDeviceGroupPresentCapabilitiesKHR vkGetDeviceGroupPresentCapabilitiesKHRPtr
  pPDeviceGroupPresentCapabilities <- ContT (withZeroCStruct @DeviceGroupPresentCapabilitiesKHR)
  r <- lift $ vkGetDeviceGroupPresentCapabilitiesKHR' (deviceHandle (device)) (pPDeviceGroupPresentCapabilities)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDeviceGroupPresentCapabilities <- lift $ peekCStruct @DeviceGroupPresentCapabilitiesKHR pPDeviceGroupPresentCapabilities
  pure $ (pDeviceGroupPresentCapabilities)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupSurfacePresentModesKHR
  :: FunPtr (Ptr Device_T -> SurfaceKHR -> Ptr DeviceGroupPresentModeFlagsKHR -> IO Result) -> Ptr Device_T -> SurfaceKHR -> Ptr DeviceGroupPresentModeFlagsKHR -> IO Result

-- | vkGetDeviceGroupSurfacePresentModesKHR - Query present capabilities for
-- a surface
--
-- = Description
--
-- The modes returned by this command are not invariant, and /may/ change
-- in response to the surface being moved, resized, or occluded. These
-- modes /must/ be a subset of the modes returned by
-- 'getDeviceGroupPresentCapabilitiesKHR'.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   @pModes@ /must/ be a valid pointer to a
--     'DeviceGroupPresentModeFlagsKHR' value
--
-- -   Both of @device@, and @surface@ /must/ have been created, allocated,
--     or retrieved from the same 'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @surface@ /must/ be externally synchronized
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'DeviceGroupPresentModeFlagsKHR',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
getDeviceGroupSurfacePresentModesKHR :: forall io
                                      . (MonadIO io)
                                     => -- | @device@ is the logical device.
                                        Device
                                     -> -- | @surface@ is the surface.
                                        SurfaceKHR
                                     -> io (("modes" ::: DeviceGroupPresentModeFlagsKHR))
getDeviceGroupSurfacePresentModesKHR device surface = liftIO . evalContT $ do
  let vkGetDeviceGroupSurfacePresentModesKHRPtr = pVkGetDeviceGroupSurfacePresentModesKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceGroupSurfacePresentModesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceGroupSurfacePresentModesKHR is null" Nothing Nothing
  let vkGetDeviceGroupSurfacePresentModesKHR' = mkVkGetDeviceGroupSurfacePresentModesKHR vkGetDeviceGroupSurfacePresentModesKHRPtr
  pPModes <- ContT $ bracket (callocBytes @DeviceGroupPresentModeFlagsKHR 4) free
  r <- lift $ vkGetDeviceGroupSurfacePresentModesKHR' (deviceHandle (device)) (surface) (pPModes)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pModes <- lift $ peek @DeviceGroupPresentModeFlagsKHR pPModes
  pure $ (pModes)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireNextImage2KHRUnsafe
  :: FunPtr (Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result) -> Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result

foreign import ccall
  "dynamic" mkVkAcquireNextImage2KHRSafe
  :: FunPtr (Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result) -> Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result

-- | acquireNextImage2KHR with selectable safeness
acquireNextImage2KHRSafeOrUnsafe :: forall io
                                  . (MonadIO io)
                                 => -- No documentation found for TopLevel ""
                                    (FunPtr (Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result) -> Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result)
                                 -> -- | @device@ is the device associated with @swapchain@.
                                    Device
                                 -> -- | @pAcquireInfo@ is a pointer to a 'AcquireNextImageInfoKHR' structure
                                    -- containing parameters of the acquire.
                                    ("acquireInfo" ::: AcquireNextImageInfoKHR)
                                 -> io (Result, ("imageIndex" ::: Word32))
acquireNextImage2KHRSafeOrUnsafe mkVkAcquireNextImage2KHR device acquireInfo = liftIO . evalContT $ do
  let vkAcquireNextImage2KHRPtr = pVkAcquireNextImage2KHR (deviceCmds (device :: Device))
  lift $ unless (vkAcquireNextImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireNextImage2KHR is null" Nothing Nothing
  let vkAcquireNextImage2KHR' = mkVkAcquireNextImage2KHR vkAcquireNextImage2KHRPtr
  pAcquireInfo <- ContT $ withCStruct (acquireInfo)
  pPImageIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkAcquireNextImage2KHR' (deviceHandle (device)) pAcquireInfo (pPImageIndex)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImageIndex <- lift $ peek @Word32 pPImageIndex
  pure $ (r, pImageIndex)

-- | vkAcquireNextImage2KHR - Retrieve the index of the next available
-- presentable image
--
-- == Valid Usage
--
-- -   If the number of currently acquired images is greater than the
--     difference between the number of images in the @swapchain@ member of
--     @pAcquireInfo@ and the value of
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@minImageCount@
--     as returned by a call to
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     with the @surface@ used to create @swapchain@, the @timeout@ member
--     of @pAcquireInfo@ /must/ not be @UINT64_MAX@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pAcquireInfo@ /must/ be a valid pointer to a valid
--     'AcquireNextImageInfoKHR' structure
--
-- -   @pImageIndex@ /must/ be a valid pointer to a @uint32_t@ value
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
--
--     -   'Vulkan.Core10.Enums.Result.NOT_READY'
--
--     -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- = See Also
--
-- 'AcquireNextImageInfoKHR', 'Vulkan.Core10.Handles.Device'
acquireNextImage2KHR :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the device associated with @swapchain@.
                        Device
                     -> -- | @pAcquireInfo@ is a pointer to a 'AcquireNextImageInfoKHR' structure
                        -- containing parameters of the acquire.
                        ("acquireInfo" ::: AcquireNextImageInfoKHR)
                     -> io (Result, ("imageIndex" ::: Word32))
acquireNextImage2KHR = acquireNextImage2KHRSafeOrUnsafe mkVkAcquireNextImage2KHRUnsafe

-- | A variant of 'acquireNextImage2KHR' which makes a *safe* FFI call
acquireNextImage2KHRSafe :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the device associated with @swapchain@.
                            Device
                         -> -- | @pAcquireInfo@ is a pointer to a 'AcquireNextImageInfoKHR' structure
                            -- containing parameters of the acquire.
                            ("acquireInfo" ::: AcquireNextImageInfoKHR)
                         -> io (Result, ("imageIndex" ::: Word32))
acquireNextImage2KHRSafe = acquireNextImage2KHRSafeOrUnsafe mkVkAcquireNextImage2KHRSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDevicePresentRectanglesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr Rect2D -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr Rect2D -> IO Result

-- | vkGetPhysicalDevicePresentRectanglesKHR - Query present rectangles for a
-- surface on a physical device
--
-- = Description
--
-- If @pRects@ is @NULL@, then the number of rectangles used when
-- presenting the given @surface@ is returned in @pRectCount@. Otherwise,
-- @pRectCount@ /must/ point to a variable set by the user to the number of
-- elements in the @pRects@ array, and on return the variable is
-- overwritten with the number of structures actually written to @pRects@.
-- If the value of @pRectCount@ is less than the number of rectangles, at
-- most @pRectCount@ structures will be written. If @pRectCount@ is smaller
-- than the number of rectangles used for the given @surface@,
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS' to indicate that not all the
-- available values were returned.
--
-- The values returned by this command are not invariant, and /may/ change
-- in response to the surface being moved, resized, or occluded.
--
-- The rectangles returned by this command /must/ not overlap.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   @pRectCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pRectCount@ is not @0@, and @pRects@ is
--     not @NULL@, @pRects@ /must/ be a valid pointer to an array of
--     @pRectCount@ 'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @surface@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDevicePresentRectanglesKHR :: forall io
                                       . (MonadIO io)
                                      => -- | @physicalDevice@ is the physical device.
                                         PhysicalDevice
                                      -> -- | @surface@ is the surface.
                                         SurfaceKHR
                                      -> io (Result, ("rects" ::: Vector Rect2D))
getPhysicalDevicePresentRectanglesKHR physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDevicePresentRectanglesKHRPtr = pVkGetPhysicalDevicePresentRectanglesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDevicePresentRectanglesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDevicePresentRectanglesKHR is null" Nothing Nothing
  let vkGetPhysicalDevicePresentRectanglesKHR' = mkVkGetPhysicalDevicePresentRectanglesKHR vkGetPhysicalDevicePresentRectanglesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPRectCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDevicePresentRectanglesKHR' physicalDevice' (surface) (pPRectCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pRectCount <- lift $ peek @Word32 pPRectCount
  pPRects <- ContT $ bracket (callocBytes @Rect2D ((fromIntegral (pRectCount)) * 16)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPRects `advancePtrBytes` (i * 16) :: Ptr Rect2D) . ($ ())) [0..(fromIntegral (pRectCount)) - 1]
  r' <- lift $ vkGetPhysicalDevicePresentRectanglesKHR' physicalDevice' (surface) (pPRectCount) ((pPRects))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pRectCount' <- lift $ peek @Word32 pPRectCount
  pRects' <- lift $ generateM (fromIntegral (pRectCount')) (\i -> peekCStruct @Rect2D (((pPRects) `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
  pure $ ((r'), pRects')


-- | VkSwapchainCreateInfoKHR - Structure specifying parameters of a newly
-- created swapchain object
--
-- = Description
--
-- Note
--
-- On some platforms, it is normal that @maxImageExtent@ /may/ become @(0,
-- 0)@, for example when the window is minimized. In such a case, it is not
-- possible to create a swapchain due to the Valid Usage requirements.
--
-- -   @imageArrayLayers@ is the number of views in a multiview\/stereo
--     surface. For non-stereoscopic-3D applications, this value is 1.
--
-- -   @imageUsage@ is a bitmask of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits'
--     describing the intended usage of the (acquired) swapchain images.
--
-- -   @imageSharingMode@ is the sharing mode used for the image(s) of the
--     swapchain.
--
-- -   @queueFamilyIndexCount@ is the number of queue families having
--     access to the image(s) of the swapchain when @imageSharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT'.
--
-- -   @pQueueFamilyIndices@ is a pointer to an array of queue family
--     indices having access to the images(s) of the swapchain when
--     @imageSharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT'.
--
-- -   @preTransform@ is a
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR' value
--     describing the transform, relative to the presentation engine’s
--     natural orientation, applied to the image content prior to
--     presentation. If it does not match the @currentTransform@ value
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
--     the presentation engine will transform the image content as part of
--     the presentation operation.
--
-- -   @compositeAlpha@ is a
--     'Vulkan.Extensions.VK_KHR_surface.CompositeAlphaFlagBitsKHR' value
--     indicating the alpha compositing mode to use when this surface is
--     composited together with other surfaces on certain window systems.
--
-- -   @presentMode@ is the presentation mode the swapchain will use. A
--     swapchain’s present mode determines how incoming present requests
--     will be processed and queued internally.
--
-- -   @clipped@ specifies whether the Vulkan implementation is allowed to
--     discard rendering operations that affect regions of the surface that
--     are not visible.
--
--     -   If set to 'Vulkan.Core10.FundamentalTypes.TRUE', the presentable
--         images associated with the swapchain /may/ not own all of their
--         pixels. Pixels in the presentable images that correspond to
--         regions of the target surface obscured by another window on the
--         desktop, or subject to some other clipping mechanism will have
--         undefined content when read back. Fragment shaders /may/ not
--         execute for these pixels, and thus any side effects they would
--         have had will not occur. 'Vulkan.Core10.FundamentalTypes.TRUE'
--         value does not guarantee any clipping will occur, but allows
--         more optimal presentation methods to be used on some platforms.
--
--     -   If set to 'Vulkan.Core10.FundamentalTypes.FALSE', presentable
--         images associated with the swapchain will own all of the pixels
--         they contain.
--
-- Note
--
-- Applications /should/ set this value to
-- 'Vulkan.Core10.FundamentalTypes.TRUE' if they do not expect to read back
-- the content of presentable images before presenting them or after
-- reacquiring them, and if their fragment shaders do not have any side
-- effects that require them to run for all pixels in the presentable
-- image.
--
-- -   @oldSwapchain@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', or the
--     existing non-retired swapchain currently associated with @surface@.
--     Providing a valid @oldSwapchain@ /may/ aid in the resource reuse,
--     and also allows the application to still present any images that are
--     already acquired from it.
--
-- Upon calling 'createSwapchainKHR' with an @oldSwapchain@ that is not
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', @oldSwapchain@ is
-- retired — even if creation of the new swapchain fails. The new swapchain
-- is created in the non-retired state whether or not @oldSwapchain@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- Upon calling 'createSwapchainKHR' with an @oldSwapchain@ that is not
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', any images from @oldSwapchain@
-- that are not acquired by the application /may/ be freed by the
-- implementation, which /may/ occur even if creation of the new swapchain
-- fails. The application /can/ destroy @oldSwapchain@ to free all memory
-- associated with @oldSwapchain@.
--
-- Note
--
-- Multiple retired swapchains /can/ be associated with the same
-- 'Vulkan.Extensions.Handles.SurfaceKHR' through multiple uses of
-- @oldSwapchain@ that outnumber calls to 'destroySwapchainKHR'.
--
-- After @oldSwapchain@ is retired, the application /can/ pass to
-- 'queuePresentKHR' any images it had already acquired from
-- @oldSwapchain@. E.g., an application may present an image from the old
-- swapchain before an image from the new swapchain is ready to be
-- presented. As usual, 'queuePresentKHR' /may/ fail if @oldSwapchain@ has
-- entered a state that causes
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR' to be returned.
--
-- The application /can/ continue to use a shared presentable image
-- obtained from @oldSwapchain@ until a presentable image is acquired from
-- the new swapchain, as long as it has not entered a state that causes it
-- to return 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'.
--
-- == Valid Usage
--
-- -   @surface@ /must/ be a surface that is supported by the device as
--     determined using
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
--
-- -   @minImageCount@ /must/ be less than or equal to the value returned
--     in the @maxImageCount@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface if the returned @maxImageCount@ is not zero
--
-- -   If @presentMode@ is not
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
--     nor
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR',
--     then @minImageCount@ /must/ be greater than or equal to the value
--     returned in the @minImageCount@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   @minImageCount@ /must/ be @1@ if @presentMode@ is either
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR'
--
-- -   @imageFormat@ and @imageColorSpace@ /must/ match the @format@ and
--     @colorSpace@ members, respectively, of one of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceFormatKHR' structures
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR'
--     for the surface
--
-- -   @imageExtent@ /must/ be between @minImageExtent@ and
--     @maxImageExtent@, inclusive, where @minImageExtent@ and
--     @maxImageExtent@ are members of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   @imageExtent@ members @width@ and @height@ /must/ both be non-zero
--
-- -   @imageArrayLayers@ /must/ be greater than @0@ and less than or equal
--     to the @maxImageArrayLayers@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   If @presentMode@ is
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_IMMEDIATE_KHR',
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR',
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR',
--     @imageUsage@ /must/ be a subset of the supported usage flags present
--     in the @supportedUsageFlags@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for @surface@
--
-- -   If @presentMode@ is
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR',
--     @imageUsage@ /must/ be a subset of the supported usage flags present
--     in the @sharedPresentSupportedUsageFlags@ member of the
--     'Vulkan.Extensions.VK_KHR_shared_presentable_image.SharedPresentSurfaceCapabilitiesKHR'
--     structure returned by
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     for @surface@
--
-- -   If @imageSharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   If @imageSharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   If @imageSharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT', each
--     element of @pQueueFamilyIndices@ /must/ be unique and /must/ be less
--     than @pQueueFamilyPropertyCount@ returned by either
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   @preTransform@ /must/ be one of the bits present in the
--     @supportedTransforms@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   @compositeAlpha@ /must/ be one of the bits present in the
--     @supportedCompositeAlpha@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   @presentMode@ /must/ be one of the
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR'
--     for the surface
--
-- -   If the logical device was created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo'::@physicalDeviceCount@
--     equal to 1, @flags@ /must/ not contain
--     'SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR'
--
-- -   If @oldSwapchain@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @oldSwapchain@ /must/ be a non-retired swapchain associated with
--     native window referred to by @surface@
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#swapchain-wsi-image-create-info implied image creation parameters>
--     of the swapchain /must/ be supported as reported by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties'
--
-- -   If @flags@ contains 'SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR' then
--     the @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure with a @viewFormatCount@ greater than zero and
--     @pViewFormats@ /must/ have an element equal to @imageFormat@
--
-- -   If @flags@ contains 'SWAPCHAIN_CREATE_PROTECTED_BIT_KHR', then
--     'Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR'::@supportsProtected@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE' in the
--     'Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR'
--     structure returned by
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     for @surface@
--
-- -   If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveInfoEXT'
--     structure with its @fullScreenExclusive@ member set to
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT',
--     and @surface@ was created using
--     'Vulkan.Extensions.VK_KHR_win32_surface.createWin32SurfaceKHR', a
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveWin32InfoEXT'
--     structure /must/ be included in the @pNext@ chain
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of 'DeviceGroupSwapchainCreateInfoKHR',
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo',
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveInfoEXT',
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveWin32InfoEXT',
--     'Vulkan.Extensions.VK_EXT_display_control.SwapchainCounterCreateInfoEXT',
--     or
--     'Vulkan.Extensions.VK_AMD_display_native_hdr.SwapchainDisplayNativeHdrCreateInfoAMD'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'SwapchainCreateFlagBitsKHR' values
--
-- -   @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   @imageFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   @imageColorSpace@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR' value
--
-- -   @imageUsage@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
--
-- -   @imageUsage@ /must/ not be @0@
--
-- -   @imageSharingMode@ /must/ be a valid
--     'Vulkan.Core10.Enums.SharingMode.SharingMode' value
--
-- -   @preTransform@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR' value
--
-- -   @compositeAlpha@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_surface.CompositeAlphaFlagBitsKHR' value
--
-- -   @presentMode@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' value
--
-- -   If @oldSwapchain@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @oldSwapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   If @oldSwapchain@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @surface@
--
-- -   Both of @oldSwapchain@, and @surface@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Instance'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.CompositeAlphaFlagBitsKHR',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.SharingMode.SharingMode',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR',
-- 'SwapchainCreateFlagsKHR', 'Vulkan.Extensions.Handles.SwapchainKHR',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.createSharedSwapchainsKHR',
-- 'createSwapchainKHR'
data SwapchainCreateInfoKHR (es :: [Type]) = SwapchainCreateInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of 'SwapchainCreateFlagBitsKHR' indicating
    -- parameters of the swapchain creation.
    flags :: SwapchainCreateFlagsKHR
  , -- | @surface@ is the surface onto which the swapchain will present images.
    -- If the creation succeeds, the swapchain becomes associated with
    -- @surface@.
    surface :: SurfaceKHR
  , -- | @minImageCount@ is the minimum number of presentable images that the
    -- application needs. The implementation will either create the swapchain
    -- with at least that many images, or it will fail to create the swapchain.
    minImageCount :: Word32
  , -- | @imageFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value specifying
    -- the format the swapchain image(s) will be created with.
    imageFormat :: Format
  , -- | @imageColorSpace@ is a 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR'
    -- value specifying the way the swapchain interprets image data.
    imageColorSpace :: ColorSpaceKHR
  , -- | @imageExtent@ is the size (in pixels) of the swapchain image(s). The
    -- behavior is platform-dependent if the image extent does not match the
    -- surface’s @currentExtent@ as returned by
    -- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'.
    imageExtent :: Extent2D
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageArrayLayers"
    imageArrayLayers :: Word32
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageUsage"
    imageUsage :: ImageUsageFlags
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageSharingMode"
    imageSharingMode :: SharingMode
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "pQueueFamilyIndices"
    queueFamilyIndices :: Vector Word32
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "preTransform"
    preTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "compositeAlpha"
    compositeAlpha :: CompositeAlphaFlagBitsKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "presentMode"
    presentMode :: PresentModeKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "clipped"
    clipped :: Bool
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "oldSwapchain"
    oldSwapchain :: SwapchainKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainCreateInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SwapchainCreateInfoKHR es)

instance Extensible SwapchainCreateInfoKHR where
  extensibleType = STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  setNext x next = x{next = next}
  getNext SwapchainCreateInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SwapchainCreateInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SurfaceFullScreenExclusiveWin32InfoEXT = Just f
    | Just Refl <- eqT @e @SurfaceFullScreenExclusiveInfoEXT = Just f
    | Just Refl <- eqT @e @ImageFormatListCreateInfo = Just f
    | Just Refl <- eqT @e @SwapchainDisplayNativeHdrCreateInfoAMD = Just f
    | Just Refl <- eqT @e @DeviceGroupSwapchainCreateInfoKHR = Just f
    | Just Refl <- eqT @e @SwapchainCounterCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss SwapchainCreateInfoKHR es, PokeChain es) => ToCStruct (SwapchainCreateInfoKHR es) where
  withCStruct x f = allocaBytesAligned 104 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SwapchainCreateFlagsKHR)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr SurfaceKHR)) (surface)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (minImageCount)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (imageFormat)
    lift $ poke ((p `plusPtr` 40 :: Ptr ColorSpaceKHR)) (imageColorSpace)
    ContT $ pokeCStruct ((p `plusPtr` 44 :: Ptr Extent2D)) (imageExtent) . ($ ())
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (imageArrayLayers)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageUsageFlags)) (imageUsage)
    lift $ poke ((p `plusPtr` 60 :: Ptr SharingMode)) (imageSharingMode)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ poke ((p `plusPtr` 80 :: Ptr SurfaceTransformFlagBitsKHR)) (preTransform)
    lift $ poke ((p `plusPtr` 84 :: Ptr CompositeAlphaFlagBitsKHR)) (compositeAlpha)
    lift $ poke ((p `plusPtr` 88 :: Ptr PresentModeKHR)) (presentMode)
    lift $ poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (clipped))
    lift $ poke ((p `plusPtr` 96 :: Ptr SwapchainKHR)) (oldSwapchain)
    lift $ f
  cStructSize = 104
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr SurfaceKHR)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ColorSpaceKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 44 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageUsageFlags)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr SharingMode)) (zero)
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ poke ((p `plusPtr` 80 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    lift $ poke ((p `plusPtr` 84 :: Ptr CompositeAlphaFlagBitsKHR)) (zero)
    lift $ poke ((p `plusPtr` 88 :: Ptr PresentModeKHR)) (zero)
    lift $ poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance (Extendss SwapchainCreateInfoKHR es, PeekChain es) => FromCStruct (SwapchainCreateInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @SwapchainCreateFlagsKHR ((p `plusPtr` 16 :: Ptr SwapchainCreateFlagsKHR))
    surface <- peek @SurfaceKHR ((p `plusPtr` 24 :: Ptr SurfaceKHR))
    minImageCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    imageFormat <- peek @Format ((p `plusPtr` 36 :: Ptr Format))
    imageColorSpace <- peek @ColorSpaceKHR ((p `plusPtr` 40 :: Ptr ColorSpaceKHR))
    imageExtent <- peekCStruct @Extent2D ((p `plusPtr` 44 :: Ptr Extent2D))
    imageArrayLayers <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    imageUsage <- peek @ImageUsageFlags ((p `plusPtr` 56 :: Ptr ImageUsageFlags))
    imageSharingMode <- peek @SharingMode ((p `plusPtr` 60 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 72 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    preTransform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 80 :: Ptr SurfaceTransformFlagBitsKHR))
    compositeAlpha <- peek @CompositeAlphaFlagBitsKHR ((p `plusPtr` 84 :: Ptr CompositeAlphaFlagBitsKHR))
    presentMode <- peek @PresentModeKHR ((p `plusPtr` 88 :: Ptr PresentModeKHR))
    clipped <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    oldSwapchain <- peek @SwapchainKHR ((p `plusPtr` 96 :: Ptr SwapchainKHR))
    pure $ SwapchainCreateInfoKHR
             next flags surface minImageCount imageFormat imageColorSpace imageExtent imageArrayLayers imageUsage imageSharingMode pQueueFamilyIndices' preTransform compositeAlpha presentMode (bool32ToBool clipped) oldSwapchain

instance es ~ '[] => Zero (SwapchainCreateInfoKHR es) where
  zero = SwapchainCreateInfoKHR
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           zero
           zero
           zero
           zero
           zero


-- | VkPresentInfoKHR - Structure describing parameters of a queue
-- presentation
--
-- = Description
--
-- Before an application /can/ present an image, the image’s layout /must/
-- be transitioned to the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR' layout,
-- or for a shared presentable image the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
-- layout.
--
-- Note
--
-- When transitioning the image to
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR' or
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR', there is
-- no need to delay subsequent processing, or perform any visibility
-- operations (as 'queuePresentKHR' performs automatic visibility
-- operations). To achieve this, the @dstAccessMask@ member of the
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' /should/ be set to @0@,
-- and the @dstStageMask@ parameter /should/ be set to
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'.
--
-- == Valid Usage
--
-- -   Each element of @pImageIndices@ /must/ be the index of a presentable
--     image acquired from the swapchain specified by the corresponding
--     element of the @pSwapchains@ array, and the presented image
--     subresource /must/ be in the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--     layout at the time the operation is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   All elements of the @pWaitSemaphores@ /must/ have a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_INFO_KHR'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of 'DeviceGroupPresentInfoKHR',
--     'Vulkan.Extensions.VK_KHR_display_swapchain.DisplayPresentInfoKHR',
--     'Vulkan.Extensions.VK_GGP_frame_token.PresentFrameTokenGGP',
--     'Vulkan.Extensions.VK_KHR_incremental_present.PresentRegionsKHR', or
--     'Vulkan.Extensions.VK_GOOGLE_display_timing.PresentTimesInfoGOOGLE'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   If @waitSemaphoreCount@ is not @0@, @pWaitSemaphores@ /must/ be a
--     valid pointer to an array of @waitSemaphoreCount@ valid
--     'Vulkan.Core10.Handles.Semaphore' handles
--
-- -   @pSwapchains@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid 'Vulkan.Extensions.Handles.SwapchainKHR'
--     handles
--
-- -   @pImageIndices@ /must/ be a valid pointer to an array of
--     @swapchainCount@ @uint32_t@ values
--
-- -   If @pResults@ is not @NULL@, @pResults@ /must/ be a valid pointer to
--     an array of @swapchainCount@ 'Vulkan.Core10.Enums.Result.Result'
--     values
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- -   Both of the elements of @pSwapchains@, and the elements of
--     @pWaitSemaphores@ that are valid handles of non-ignored parameters
--     /must/ have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.Result.Result', 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR', 'queuePresentKHR'
data PresentInfoKHR (es :: [Type]) = PresentInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @pWaitSemaphores@ is @NULL@ or a pointer to an array of
    -- 'Vulkan.Core10.Handles.Semaphore' objects with @waitSemaphoreCount@
    -- entries, and specifies the semaphores to wait for before issuing the
    -- present request.
    waitSemaphores :: Vector Semaphore
  , -- | @pSwapchains@ is a pointer to an array of
    -- 'Vulkan.Extensions.Handles.SwapchainKHR' objects with @swapchainCount@
    -- entries. A given swapchain /must/ not appear in this list more than
    -- once.
    swapchains :: Vector SwapchainKHR
  , -- | @pImageIndices@ is a pointer to an array of indices into the array of
    -- each swapchain’s presentable images, with @swapchainCount@ entries. Each
    -- entry in this array identifies the image to present on the corresponding
    -- entry in the @pSwapchains@ array.
    imageIndices :: Vector Word32
  , -- | @pResults@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.Result.Result' typed elements with @swapchainCount@
    -- entries. Applications that do not need per-swapchain results /can/ use
    -- @NULL@ for @pResults@. If non-@NULL@, each entry in @pResults@ will be
    -- set to the 'Vulkan.Core10.Enums.Result.Result' for presenting the
    -- swapchain corresponding to the same index in @pSwapchains@.
    results :: Ptr Result
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PresentInfoKHR es)

instance Extensible PresentInfoKHR where
  extensibleType = STRUCTURE_TYPE_PRESENT_INFO_KHR
  setNext x next = x{next = next}
  getNext PresentInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PresentInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PresentFrameTokenGGP = Just f
    | Just Refl <- eqT @e @PresentTimesInfoGOOGLE = Just f
    | Just Refl <- eqT @e @DeviceGroupPresentInfoKHR = Just f
    | Just Refl <- eqT @e @PresentRegionsKHR = Just f
    | Just Refl <- eqT @e @DisplayPresentInfoKHR = Just f
    | otherwise = Nothing

instance (Extendss PresentInfoKHR es, PokeChain es) => ToCStruct (PresentInfoKHR es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (waitSemaphores)) :: Word32))
    pPWaitSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (waitSemaphores)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (waitSemaphores)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPWaitSemaphores')
    let pSwapchainsLength = Data.Vector.length $ (swapchains)
    lift $ unless ((Data.Vector.length $ (imageIndices)) == pSwapchainsLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pImageIndices and pSwapchains must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral pSwapchainsLength :: Word32))
    pPSwapchains' <- ContT $ allocaBytesAligned @SwapchainKHR ((Data.Vector.length (swapchains)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSwapchains' `plusPtr` (8 * (i)) :: Ptr SwapchainKHR) (e)) (swapchains)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SwapchainKHR))) (pPSwapchains')
    pPImageIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (imageIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPImageIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (imageIndices)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPImageIndices')
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Result))) (results)
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPWaitSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPWaitSemaphores')
    pPSwapchains' <- ContT $ allocaBytesAligned @SwapchainKHR ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSwapchains' `plusPtr` (8 * (i)) :: Ptr SwapchainKHR) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SwapchainKHR))) (pPSwapchains')
    pPImageIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPImageIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPImageIndices')
    lift $ f

instance (Extendss PresentInfoKHR es, PeekChain es) => FromCStruct (PresentInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    waitSemaphoreCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pWaitSemaphores <- peek @(Ptr Semaphore) ((p `plusPtr` 24 :: Ptr (Ptr Semaphore)))
    pWaitSemaphores' <- generateM (fromIntegral waitSemaphoreCount) (\i -> peek @Semaphore ((pWaitSemaphores `advancePtrBytes` (8 * (i)) :: Ptr Semaphore)))
    swapchainCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pSwapchains <- peek @(Ptr SwapchainKHR) ((p `plusPtr` 40 :: Ptr (Ptr SwapchainKHR)))
    pSwapchains' <- generateM (fromIntegral swapchainCount) (\i -> peek @SwapchainKHR ((pSwapchains `advancePtrBytes` (8 * (i)) :: Ptr SwapchainKHR)))
    pImageIndices <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pImageIndices' <- generateM (fromIntegral swapchainCount) (\i -> peek @Word32 ((pImageIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pResults <- peek @(Ptr Result) ((p `plusPtr` 56 :: Ptr (Ptr Result)))
    pure $ PresentInfoKHR
             next pWaitSemaphores' pSwapchains' pImageIndices' pResults

instance es ~ '[] => Zero (PresentInfoKHR es) where
  zero = PresentInfoKHR
           ()
           mempty
           mempty
           mempty
           zero


-- | VkDeviceGroupPresentCapabilitiesKHR - Present capabilities from other
-- physical devices
--
-- = Description
--
-- @modes@ always has 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR' set.
--
-- The present mode flags are also used when presenting an image, in
-- 'DeviceGroupPresentInfoKHR'::@mode@.
--
-- If a device group only includes a single physical device, then @modes@
-- /must/ equal 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DeviceGroupPresentModeFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceGroupPresentCapabilitiesKHR'
data DeviceGroupPresentCapabilitiesKHR = DeviceGroupPresentCapabilitiesKHR
  { -- | @presentMask@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DEVICE_GROUP_SIZE' @uint32_t@ masks,
    -- where the mask at element i is non-zero if physical device i has a
    -- presentation engine, and where bit j is set in element i if physical
    -- device i /can/ present swapchain images from physical device j. If
    -- element i is non-zero, then bit i /must/ be set.
    presentMask :: Vector Word32
  , -- | @modes@ is a bitmask of 'DeviceGroupPresentModeFlagBitsKHR' indicating
    -- which device group presentation modes are supported.
    modes :: DeviceGroupPresentModeFlagsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupPresentCapabilitiesKHR)
#endif
deriving instance Show DeviceGroupPresentCapabilitiesKHR

instance ToCStruct DeviceGroupPresentCapabilitiesKHR where
  withCStruct x f = allocaBytesAligned 152 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupPresentCapabilitiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    unless ((Data.Vector.length $ (presentMask)) <= MAX_DEVICE_GROUP_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "presentMask is too long, a maximum of MAX_DEVICE_GROUP_SIZE elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE Word32)))) `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (presentMask)
    poke ((p `plusPtr` 144 :: Ptr DeviceGroupPresentModeFlagsKHR)) (modes)
    f
  cStructSize = 152
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    unless ((Data.Vector.length $ (mempty)) <= MAX_DEVICE_GROUP_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "presentMask is too long, a maximum of MAX_DEVICE_GROUP_SIZE elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE Word32)))) `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    poke ((p `plusPtr` 144 :: Ptr DeviceGroupPresentModeFlagsKHR)) (zero)
    f

instance FromCStruct DeviceGroupPresentCapabilitiesKHR where
  peekCStruct p = do
    presentMask <- generateM (MAX_DEVICE_GROUP_SIZE) (\i -> peek @Word32 (((lowerArrayPtr @Word32 ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE Word32)))) `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    modes <- peek @DeviceGroupPresentModeFlagsKHR ((p `plusPtr` 144 :: Ptr DeviceGroupPresentModeFlagsKHR))
    pure $ DeviceGroupPresentCapabilitiesKHR
             presentMask modes

instance Storable DeviceGroupPresentCapabilitiesKHR where
  sizeOf ~_ = 152
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGroupPresentCapabilitiesKHR where
  zero = DeviceGroupPresentCapabilitiesKHR
           mempty
           zero


-- | VkImageSwapchainCreateInfoKHR - Specify that an image will be bound to
-- swapchain memory
--
-- == Valid Usage
--
-- -   If @swapchain@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     fields of 'Vulkan.Core10.Image.ImageCreateInfo' /must/ match the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#swapchain-wsi-image-create-info implied image creation parameters>
--     of the swapchain
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR'
--
-- -   If @swapchain@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
data ImageSwapchainCreateInfoKHR = ImageSwapchainCreateInfoKHR
  { -- | @swapchain@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a handle of a
    -- swapchain that the image will be bound to.
    swapchain :: SwapchainKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSwapchainCreateInfoKHR)
#endif
deriving instance Show ImageSwapchainCreateInfoKHR

instance ToCStruct ImageSwapchainCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSwapchainCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (swapchain)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImageSwapchainCreateInfoKHR where
  peekCStruct p = do
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 16 :: Ptr SwapchainKHR))
    pure $ ImageSwapchainCreateInfoKHR
             swapchain

instance Storable ImageSwapchainCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSwapchainCreateInfoKHR where
  zero = ImageSwapchainCreateInfoKHR
           zero


-- | VkBindImageMemorySwapchainInfoKHR - Structure specifying swapchain image
-- memory to bind to
--
-- = Description
--
-- If @swapchain@ is not @NULL@, the @swapchain@ and @imageIndex@ are used
-- to determine the memory that the image is bound to, instead of @memory@
-- and @memoryOffset@.
--
-- Memory /can/ be bound to a swapchain and use the @pDeviceIndices@ or
-- @pSplitInstanceBindRegions@ members of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'.
--
-- == Valid Usage
--
-- -   @imageIndex@ /must/ be less than the number of images in @swapchain@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR'
--
-- -   @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
data BindImageMemorySwapchainInfoKHR = BindImageMemorySwapchainInfoKHR
  { -- | @swapchain@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a swapchain
    -- handle.
    swapchain :: SwapchainKHR
  , -- | @imageIndex@ is an image index within @swapchain@.
    imageIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindImageMemorySwapchainInfoKHR)
#endif
deriving instance Show BindImageMemorySwapchainInfoKHR

instance ToCStruct BindImageMemorySwapchainInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindImageMemorySwapchainInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (swapchain)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (imageIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct BindImageMemorySwapchainInfoKHR where
  peekCStruct p = do
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 16 :: Ptr SwapchainKHR))
    imageIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ BindImageMemorySwapchainInfoKHR
             swapchain imageIndex

instance Storable BindImageMemorySwapchainInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindImageMemorySwapchainInfoKHR where
  zero = BindImageMemorySwapchainInfoKHR
           zero
           zero


-- | VkAcquireNextImageInfoKHR - Structure specifying parameters of the
-- acquire
--
-- = Description
--
-- If 'acquireNextImageKHR' is used, the device mask is considered to
-- include all physical devices in the logical device.
--
-- Note
--
-- 'acquireNextImage2KHR' signals at most one semaphore, even if the
-- application requests waiting for multiple physical devices to be ready
-- via the @deviceMask@. However, only a single physical device /can/ wait
-- on that semaphore, since the semaphore becomes unsignaled when the wait
-- succeeds. For other physical devices to wait for the image to be ready,
-- it is necessary for the application to submit semaphore signal
-- operation(s) to that first physical device to signal additional
-- semaphore(s) after the wait succeeds, which the other physical device(s)
-- /can/ wait upon.
--
-- == Valid Usage
--
-- -   @swapchain@ /must/ not be in the retired state
--
-- -   If @semaphore@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' it
--     /must/ be unsignaled
--
-- -   If @semaphore@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' it
--     /must/ not have any uncompleted signal or wait operations pending
--
-- -   If @fence@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/
--     be unsignaled and /must/ not be associated with any other queue
--     command that has not yet completed execution on that queue
--
-- -   @semaphore@ and @fence@ /must/ not both be equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   @deviceMask@ /must/ be a valid device mask
--
-- -   @deviceMask@ /must/ not be zero
--
-- -   @semaphore@ /must/ have a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   If @semaphore@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @semaphore@ /must/ be a valid 'Vulkan.Core10.Handles.Semaphore'
--     handle
--
-- -   If @fence@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@
--     /must/ be a valid 'Vulkan.Core10.Handles.Fence' handle
--
-- -   Each of @fence@, @semaphore@, and @swapchain@ that are valid handles
--     of non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Fence', 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR', 'acquireNextImage2KHR'
data AcquireNextImageInfoKHR = AcquireNextImageInfoKHR
  { -- | @swapchain@ is a non-retired swapchain from which an image is acquired.
    swapchain :: SwapchainKHR
  , -- | @timeout@ specifies how long the function waits, in nanoseconds, if no
    -- image is available.
    timeout :: Word64
  , -- | @semaphore@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a semaphore
    -- to signal.
    semaphore :: Semaphore
  , -- | @fence@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a fence to
    -- signal.
    fence :: Fence
  , -- | @deviceMask@ is a mask of physical devices for which the swapchain image
    -- will be ready to use when the semaphore or fence is signaled.
    deviceMask :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AcquireNextImageInfoKHR)
#endif
deriving instance Show AcquireNextImageInfoKHR

instance ToCStruct AcquireNextImageInfoKHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AcquireNextImageInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (swapchain)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (timeout)
    poke ((p `plusPtr` 32 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 40 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (deviceMask)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    f

instance FromCStruct AcquireNextImageInfoKHR where
  peekCStruct p = do
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 16 :: Ptr SwapchainKHR))
    timeout <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    semaphore <- peek @Semaphore ((p `plusPtr` 32 :: Ptr Semaphore))
    fence <- peek @Fence ((p `plusPtr` 40 :: Ptr Fence))
    deviceMask <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pure $ AcquireNextImageInfoKHR
             swapchain timeout semaphore fence deviceMask

instance Storable AcquireNextImageInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AcquireNextImageInfoKHR where
  zero = AcquireNextImageInfoKHR
           zero
           zero
           zero
           zero
           zero


-- | VkDeviceGroupPresentInfoKHR - Mode and mask controlling which physical
-- devices\' images are presented
--
-- = Description
--
-- If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR', then each
-- element of @pDeviceMasks@ selects which instance of the swapchain image
-- is presented. Each element of @pDeviceMasks@ /must/ have exactly one bit
-- set, and the corresponding physical device /must/ have a presentation
-- engine as reported by 'DeviceGroupPresentCapabilitiesKHR'.
--
-- If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR', then each
-- element of @pDeviceMasks@ selects which instance of the swapchain image
-- is presented. Each element of @pDeviceMasks@ /must/ have exactly one bit
-- set, and some physical device in the logical device /must/ include that
-- bit in its 'DeviceGroupPresentCapabilitiesKHR'::@presentMask@.
--
-- If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR', then each element
-- of @pDeviceMasks@ selects which instances of the swapchain image are
-- component-wise summed and the sum of those images is presented. If the
-- sum in any component is outside the representable range, the value of
-- that component is undefined. Each element of @pDeviceMasks@ /must/ have
-- a value for which all set bits are set in one of the elements of
-- 'DeviceGroupPresentCapabilitiesKHR'::@presentMask@.
--
-- If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR',
-- then each element of @pDeviceMasks@ selects which instance(s) of the
-- swapchain images are presented. For each bit set in each element of
-- @pDeviceMasks@, the corresponding physical device /must/ have a
-- presentation engine as reported by 'DeviceGroupPresentCapabilitiesKHR'.
--
-- If 'DeviceGroupPresentInfoKHR' is not provided or @swapchainCount@ is
-- zero then the masks are considered to be @1@. If
-- 'DeviceGroupPresentInfoKHR' is not provided, @mode@ is considered to be
-- 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'.
--
-- == Valid Usage
--
-- -   @swapchainCount@ /must/ equal @0@ or
--     'PresentInfoKHR'::@swapchainCount@
--
-- -   If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR', then each
--     element of @pDeviceMasks@ /must/ have exactly one bit set, and the
--     corresponding element of
--     'DeviceGroupPresentCapabilitiesKHR'::@presentMask@ /must/ be
--     non-zero
--
-- -   If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR', then each
--     element of @pDeviceMasks@ /must/ have exactly one bit set, and some
--     physical device in the logical device /must/ include that bit in its
--     'DeviceGroupPresentCapabilitiesKHR'::@presentMask@
--
-- -   If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR', then each
--     element of @pDeviceMasks@ /must/ have a value for which all set bits
--     are set in one of the elements of
--     'DeviceGroupPresentCapabilitiesKHR'::@presentMask@
--
-- -   If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR',
--     then for each bit set in each element of @pDeviceMasks@, the
--     corresponding element of
--     'DeviceGroupPresentCapabilitiesKHR'::@presentMask@ /must/ be
--     non-zero
--
-- -   The value of each element of @pDeviceMasks@ /must/ be equal to the
--     device mask passed in 'AcquireNextImageInfoKHR'::@deviceMask@ when
--     the image index was last acquired
--
-- -   @mode@ /must/ have exactly one bit set, and that bit /must/ have
--     been included in 'DeviceGroupSwapchainCreateInfoKHR'::@modes@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR'
--
-- -   If @swapchainCount@ is not @0@, @pDeviceMasks@ /must/ be a valid
--     pointer to an array of @swapchainCount@ @uint32_t@ values
--
-- -   @mode@ /must/ be a valid 'DeviceGroupPresentModeFlagBitsKHR' value
--
-- = See Also
--
-- 'DeviceGroupPresentModeFlagBitsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupPresentInfoKHR = DeviceGroupPresentInfoKHR
  { -- | @pDeviceMasks@ is a pointer to an array of device masks, one for each
    -- element of 'PresentInfoKHR'::pSwapchains.
    deviceMasks :: Vector Word32
  , -- | @mode@ is the device group present mode that will be used for this
    -- present.
    mode :: DeviceGroupPresentModeFlagBitsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupPresentInfoKHR)
#endif
deriving instance Show DeviceGroupPresentInfoKHR

instance ToCStruct DeviceGroupPresentInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupPresentInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceMasks)) :: Word32))
    pPDeviceMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (deviceMasks)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (deviceMasks)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDeviceMasks')
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceGroupPresentModeFlagBitsKHR)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPDeviceMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDeviceMasks')
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceGroupPresentModeFlagBitsKHR)) (zero)
    lift $ f

instance FromCStruct DeviceGroupPresentInfoKHR where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDeviceMasks <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pDeviceMasks' <- generateM (fromIntegral swapchainCount) (\i -> peek @Word32 ((pDeviceMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    mode <- peek @DeviceGroupPresentModeFlagBitsKHR ((p `plusPtr` 32 :: Ptr DeviceGroupPresentModeFlagBitsKHR))
    pure $ DeviceGroupPresentInfoKHR
             pDeviceMasks' mode

instance Zero DeviceGroupPresentInfoKHR where
  zero = DeviceGroupPresentInfoKHR
           mempty
           zero


-- | VkDeviceGroupSwapchainCreateInfoKHR - Structure specifying parameters of
-- a newly created swapchain object
--
-- = Description
--
-- If this structure is not present, @modes@ is considered to be
-- 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DeviceGroupPresentModeFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupSwapchainCreateInfoKHR = DeviceGroupSwapchainCreateInfoKHR
  { -- | @modes@ is a bitfield of modes that the swapchain /can/ be used with.
    --
    -- @modes@ /must/ be a valid combination of
    -- 'DeviceGroupPresentModeFlagBitsKHR' values
    --
    -- @modes@ /must/ not be @0@
    modes :: DeviceGroupPresentModeFlagsKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupSwapchainCreateInfoKHR)
#endif
deriving instance Show DeviceGroupSwapchainCreateInfoKHR

instance ToCStruct DeviceGroupSwapchainCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupSwapchainCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceGroupPresentModeFlagsKHR)) (modes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceGroupPresentModeFlagsKHR)) (zero)
    f

instance FromCStruct DeviceGroupSwapchainCreateInfoKHR where
  peekCStruct p = do
    modes <- peek @DeviceGroupPresentModeFlagsKHR ((p `plusPtr` 16 :: Ptr DeviceGroupPresentModeFlagsKHR))
    pure $ DeviceGroupSwapchainCreateInfoKHR
             modes

instance Storable DeviceGroupSwapchainCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGroupSwapchainCreateInfoKHR where
  zero = DeviceGroupSwapchainCreateInfoKHR
           zero


-- | VkDeviceGroupPresentModeFlagBitsKHR - Bitmask specifying supported
-- device group present modes
--
-- = See Also
--
-- 'DeviceGroupPresentInfoKHR', 'DeviceGroupPresentModeFlagsKHR'
newtype DeviceGroupPresentModeFlagBitsKHR = DeviceGroupPresentModeFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR' specifies that any physical
-- device with a presentation engine /can/ present its own swapchain
-- images.
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR = DeviceGroupPresentModeFlagBitsKHR 0x00000001
-- | 'DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR' specifies that any physical
-- device with a presentation engine /can/ present swapchain images from
-- any physical device in its @presentMask@.
pattern DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR = DeviceGroupPresentModeFlagBitsKHR 0x00000002
-- | 'DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR' specifies that any physical
-- device with a presentation engine /can/ present the sum of swapchain
-- images from any physical devices in its @presentMask@.
pattern DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR = DeviceGroupPresentModeFlagBitsKHR 0x00000004
-- | 'DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR' specifies that
-- multiple physical devices with a presentation engine /can/ each present
-- their own swapchain images.
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR = DeviceGroupPresentModeFlagBitsKHR 0x00000008

type DeviceGroupPresentModeFlagsKHR = DeviceGroupPresentModeFlagBitsKHR

instance Show DeviceGroupPresentModeFlagBitsKHR where
  showsPrec p = \case
    DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR -> showString "DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR"
    DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR -> showString "DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR"
    DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR -> showString "DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR"
    DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR -> showString "DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR"
    DeviceGroupPresentModeFlagBitsKHR x -> showParen (p >= 11) (showString "DeviceGroupPresentModeFlagBitsKHR 0x" . showHex x)

instance Read DeviceGroupPresentModeFlagBitsKHR where
  readPrec = parens (choose [("DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR", pure DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR)
                            , ("DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR", pure DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR)
                            , ("DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR", pure DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR)
                            , ("DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR", pure DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "DeviceGroupPresentModeFlagBitsKHR")
                       v <- step readPrec
                       pure (DeviceGroupPresentModeFlagBitsKHR v)))


-- | VkSwapchainCreateFlagBitsKHR - Bitmask controlling swapchain creation
--
-- = See Also
--
-- 'SwapchainCreateFlagsKHR'
newtype SwapchainCreateFlagBitsKHR = SwapchainCreateFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR' specifies that the images of
-- the swapchain /can/ be used to create a
-- 'Vulkan.Core10.Handles.ImageView' with a different format than what the
-- swapchain was created with. The list of allowed image view formats are
-- specified by adding a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
-- structure to the @pNext@ chain of 'SwapchainCreateInfoKHR'. In addition,
-- this flag also specifies that the swapchain /can/ be created with usage
-- flags that are not supported for the format the swapchain is created
-- with but are supported for at least one of the allowed image view
-- formats.
pattern SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR = SwapchainCreateFlagBitsKHR 0x00000004
-- | 'SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR' specifies that
-- images created from the swapchain (i.e. with the @swapchain@ member of
-- 'ImageSwapchainCreateInfoKHR' set to this swapchain’s handle) /must/ use
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT'.
pattern SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = SwapchainCreateFlagBitsKHR 0x00000001
-- | 'SWAPCHAIN_CREATE_PROTECTED_BIT_KHR' specifies that images created from
-- the swapchain are protected images.
pattern SWAPCHAIN_CREATE_PROTECTED_BIT_KHR = SwapchainCreateFlagBitsKHR 0x00000002

type SwapchainCreateFlagsKHR = SwapchainCreateFlagBitsKHR

instance Show SwapchainCreateFlagBitsKHR where
  showsPrec p = \case
    SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR -> showString "SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR"
    SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR -> showString "SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
    SWAPCHAIN_CREATE_PROTECTED_BIT_KHR -> showString "SWAPCHAIN_CREATE_PROTECTED_BIT_KHR"
    SwapchainCreateFlagBitsKHR x -> showParen (p >= 11) (showString "SwapchainCreateFlagBitsKHR 0x" . showHex x)

instance Read SwapchainCreateFlagBitsKHR where
  readPrec = parens (choose [("SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR", pure SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR)
                            , ("SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR", pure SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR)
                            , ("SWAPCHAIN_CREATE_PROTECTED_BIT_KHR", pure SWAPCHAIN_CREATE_PROTECTED_BIT_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "SwapchainCreateFlagBitsKHR")
                       v <- step readPrec
                       pure (SwapchainCreateFlagBitsKHR v)))


type KHR_SWAPCHAIN_SPEC_VERSION = 70

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_SPEC_VERSION"
pattern KHR_SWAPCHAIN_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SWAPCHAIN_SPEC_VERSION = 70


type KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_EXTENSION_NAME"
pattern KHR_SWAPCHAIN_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"

