{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_swapchain"
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
                                           , DeviceGroupPresentModeFlagsKHR
                                           , DeviceGroupPresentModeFlagBitsKHR( DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
                                                                              , DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
                                                                              , DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
                                                                              , DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
                                                                              , ..
                                                                              )
                                           , SwapchainCreateFlagsKHR
                                           , SwapchainCreateFlagBitsKHR( SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR
                                                                       , SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
                                                                       , SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
                                                                       , ..
                                                                       )
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
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
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

-- No documentation found for TopLevel "vkCreateSwapchainKHR"
createSwapchainKHR :: forall a io
                    . (Extendss SwapchainCreateInfoKHR a, PokeChain a, MonadIO io)
                   => -- No documentation found for Nested "vkCreateSwapchainKHR" "device"
                      Device
                   -> -- No documentation found for Nested "vkCreateSwapchainKHR" "pCreateInfo"
                      (SwapchainCreateInfoKHR a)
                   -> -- No documentation found for Nested "vkCreateSwapchainKHR" "pAllocator"
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
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSwapchainKHR :: forall a io r . (Extendss SwapchainCreateInfoKHR a, PokeChain a, MonadIO io) => Device -> SwapchainCreateInfoKHR a -> Maybe AllocationCallbacks -> (io SwapchainKHR -> (SwapchainKHR -> io ()) -> r) -> r
withSwapchainKHR device pCreateInfo pAllocator b =
  b (createSwapchainKHR device pCreateInfo pAllocator)
    (\(o0) -> destroySwapchainKHR device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySwapchainKHR
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> SwapchainKHR -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroySwapchainKHR"
destroySwapchainKHR :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkDestroySwapchainKHR" "device"
                       Device
                    -> -- No documentation found for Nested "vkDestroySwapchainKHR" "swapchain"
                       SwapchainKHR
                    -> -- No documentation found for Nested "vkDestroySwapchainKHR" "pAllocator"
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

-- No documentation found for TopLevel "vkGetSwapchainImagesKHR"
getSwapchainImagesKHR :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkGetSwapchainImagesKHR" "device"
                         Device
                      -> -- No documentation found for Nested "vkGetSwapchainImagesKHR" "swapchain"
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
                                -> -- No documentation found for Nested "vkAcquireNextImageKHR" "device"
                                   Device
                                -> -- No documentation found for Nested "vkAcquireNextImageKHR" "swapchain"
                                   SwapchainKHR
                                -> -- No documentation found for Nested "vkAcquireNextImageKHR" "timeout"
                                   ("timeout" ::: Word64)
                                -> -- No documentation found for Nested "vkAcquireNextImageKHR" "semaphore"
                                   Semaphore
                                -> -- No documentation found for Nested "vkAcquireNextImageKHR" "fence"
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

-- No documentation found for TopLevel "vkAcquireNextImageKHR"
acquireNextImageKHR :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkAcquireNextImageKHR" "device"
                       Device
                    -> -- No documentation found for Nested "vkAcquireNextImageKHR" "swapchain"
                       SwapchainKHR
                    -> -- No documentation found for Nested "vkAcquireNextImageKHR" "timeout"
                       ("timeout" ::: Word64)
                    -> -- No documentation found for Nested "vkAcquireNextImageKHR" "semaphore"
                       Semaphore
                    -> -- No documentation found for Nested "vkAcquireNextImageKHR" "fence"
                       Fence
                    -> io (Result, ("imageIndex" ::: Word32))
acquireNextImageKHR = acquireNextImageKHRSafeOrUnsafe mkVkAcquireNextImageKHRUnsafe

-- | A variant of 'acquireNextImageKHR' which makes a *safe* FFI call
acquireNextImageKHRSafe :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkAcquireNextImageKHR" "device"
                           Device
                        -> -- No documentation found for Nested "vkAcquireNextImageKHR" "swapchain"
                           SwapchainKHR
                        -> -- No documentation found for Nested "vkAcquireNextImageKHR" "timeout"
                           ("timeout" ::: Word64)
                        -> -- No documentation found for Nested "vkAcquireNextImageKHR" "semaphore"
                           Semaphore
                        -> -- No documentation found for Nested "vkAcquireNextImageKHR" "fence"
                           Fence
                        -> io (Result, ("imageIndex" ::: Word32))
acquireNextImageKHRSafe = acquireNextImageKHRSafeOrUnsafe mkVkAcquireNextImageKHRSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueuePresentKHR
  :: FunPtr (Ptr Queue_T -> Ptr (SomeStruct PresentInfoKHR) -> IO Result) -> Ptr Queue_T -> Ptr (SomeStruct PresentInfoKHR) -> IO Result

-- No documentation found for TopLevel "vkQueuePresentKHR"
queuePresentKHR :: forall a io
                 . (Extendss PresentInfoKHR a, PokeChain a, MonadIO io)
                => -- No documentation found for Nested "vkQueuePresentKHR" "queue"
                   Queue
                -> -- No documentation found for Nested "vkQueuePresentKHR" "pPresentInfo"
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

-- No documentation found for TopLevel "vkGetDeviceGroupPresentCapabilitiesKHR"
getDeviceGroupPresentCapabilitiesKHR :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkGetDeviceGroupPresentCapabilitiesKHR" "device"
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

-- No documentation found for TopLevel "vkGetDeviceGroupSurfacePresentModesKHR"
getDeviceGroupSurfacePresentModesKHR :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkGetDeviceGroupSurfacePresentModesKHR" "device"
                                        Device
                                     -> -- No documentation found for Nested "vkGetDeviceGroupSurfacePresentModesKHR" "surface"
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
                                 -> -- No documentation found for Nested "vkAcquireNextImage2KHR" "device"
                                    Device
                                 -> -- No documentation found for Nested "vkAcquireNextImage2KHR" "pAcquireInfo"
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

-- No documentation found for TopLevel "vkAcquireNextImage2KHR"
acquireNextImage2KHR :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkAcquireNextImage2KHR" "device"
                        Device
                     -> -- No documentation found for Nested "vkAcquireNextImage2KHR" "pAcquireInfo"
                        ("acquireInfo" ::: AcquireNextImageInfoKHR)
                     -> io (Result, ("imageIndex" ::: Word32))
acquireNextImage2KHR = acquireNextImage2KHRSafeOrUnsafe mkVkAcquireNextImage2KHRUnsafe

-- | A variant of 'acquireNextImage2KHR' which makes a *safe* FFI call
acquireNextImage2KHRSafe :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkAcquireNextImage2KHR" "device"
                            Device
                         -> -- No documentation found for Nested "vkAcquireNextImage2KHR" "pAcquireInfo"
                            ("acquireInfo" ::: AcquireNextImageInfoKHR)
                         -> io (Result, ("imageIndex" ::: Word32))
acquireNextImage2KHRSafe = acquireNextImage2KHRSafeOrUnsafe mkVkAcquireNextImage2KHRSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDevicePresentRectanglesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr Rect2D -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr Rect2D -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDevicePresentRectanglesKHR"
getPhysicalDevicePresentRectanglesKHR :: forall io
                                       . (MonadIO io)
                                      => -- No documentation found for Nested "vkGetPhysicalDevicePresentRectanglesKHR" "physicalDevice"
                                         PhysicalDevice
                                      -> -- No documentation found for Nested "vkGetPhysicalDevicePresentRectanglesKHR" "surface"
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



-- No documentation found for TopLevel "VkSwapchainCreateInfoKHR"
data SwapchainCreateInfoKHR (es :: [Type]) = SwapchainCreateInfoKHR
  { -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "flags"
    flags :: SwapchainCreateFlagsKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "surface"
    surface :: SurfaceKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "minImageCount"
    minImageCount :: Word32
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageFormat"
    imageFormat :: Format
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageColorSpace"
    imageColorSpace :: ColorSpaceKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageExtent"
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
    lift $ poke ((p `plusPtr` 44 :: Ptr Extent2D)) (imageExtent)
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
    lift $ poke ((p `plusPtr` 44 :: Ptr Extent2D)) (zero)
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



-- No documentation found for TopLevel "VkPresentInfoKHR"
data PresentInfoKHR (es :: [Type]) = PresentInfoKHR
  { -- No documentation found for Nested "VkPresentInfoKHR" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkPresentInfoKHR" "pWaitSemaphores"
    waitSemaphores :: Vector Semaphore
  , -- No documentation found for Nested "VkPresentInfoKHR" "pSwapchains"
    swapchains :: Vector SwapchainKHR
  , -- No documentation found for Nested "VkPresentInfoKHR" "pImageIndices"
    imageIndices :: Vector Word32
  , -- No documentation found for Nested "VkPresentInfoKHR" "pResults"
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



-- No documentation found for TopLevel "VkDeviceGroupPresentCapabilitiesKHR"
data DeviceGroupPresentCapabilitiesKHR = DeviceGroupPresentCapabilitiesKHR
  { -- No documentation found for Nested "VkDeviceGroupPresentCapabilitiesKHR" "presentMask"
    presentMask :: Vector Word32
  , -- No documentation found for Nested "VkDeviceGroupPresentCapabilitiesKHR" "modes"
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



-- No documentation found for TopLevel "VkImageSwapchainCreateInfoKHR"
data ImageSwapchainCreateInfoKHR = ImageSwapchainCreateInfoKHR
  { -- No documentation found for Nested "VkImageSwapchainCreateInfoKHR" "swapchain"
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



-- No documentation found for TopLevel "VkBindImageMemorySwapchainInfoKHR"
data BindImageMemorySwapchainInfoKHR = BindImageMemorySwapchainInfoKHR
  { -- No documentation found for Nested "VkBindImageMemorySwapchainInfoKHR" "swapchain"
    swapchain :: SwapchainKHR
  , -- No documentation found for Nested "VkBindImageMemorySwapchainInfoKHR" "imageIndex"
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



-- No documentation found for TopLevel "VkAcquireNextImageInfoKHR"
data AcquireNextImageInfoKHR = AcquireNextImageInfoKHR
  { -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "swapchain"
    swapchain :: SwapchainKHR
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "timeout"
    timeout :: Word64
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "semaphore"
    semaphore :: Semaphore
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "fence"
    fence :: Fence
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "deviceMask"
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



-- No documentation found for TopLevel "VkDeviceGroupPresentInfoKHR"
data DeviceGroupPresentInfoKHR = DeviceGroupPresentInfoKHR
  { -- No documentation found for Nested "VkDeviceGroupPresentInfoKHR" "pDeviceMasks"
    deviceMasks :: Vector Word32
  , -- No documentation found for Nested "VkDeviceGroupPresentInfoKHR" "mode"
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



-- No documentation found for TopLevel "VkDeviceGroupSwapchainCreateInfoKHR"
data DeviceGroupSwapchainCreateInfoKHR = DeviceGroupSwapchainCreateInfoKHR
  { -- No documentation found for Nested "VkDeviceGroupSwapchainCreateInfoKHR" "modes"
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


type DeviceGroupPresentModeFlagsKHR = DeviceGroupPresentModeFlagBitsKHR

-- No documentation found for TopLevel "VkDeviceGroupPresentModeFlagBitsKHR"
newtype DeviceGroupPresentModeFlagBitsKHR = DeviceGroupPresentModeFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDeviceGroupPresentModeFlagBitsKHR" "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR"
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR              = DeviceGroupPresentModeFlagBitsKHR 0x00000001
-- No documentation found for Nested "VkDeviceGroupPresentModeFlagBitsKHR" "VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR"
pattern DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR             = DeviceGroupPresentModeFlagBitsKHR 0x00000002
-- No documentation found for Nested "VkDeviceGroupPresentModeFlagBitsKHR" "VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR"
pattern DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR                = DeviceGroupPresentModeFlagBitsKHR 0x00000004
-- No documentation found for Nested "VkDeviceGroupPresentModeFlagBitsKHR" "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR"
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR = DeviceGroupPresentModeFlagBitsKHR 0x00000008

conNameDeviceGroupPresentModeFlagBitsKHR :: String
conNameDeviceGroupPresentModeFlagBitsKHR = "DeviceGroupPresentModeFlagBitsKHR"

enumPrefixDeviceGroupPresentModeFlagBitsKHR :: String
enumPrefixDeviceGroupPresentModeFlagBitsKHR = "DEVICE_GROUP_PRESENT_MODE_"

showTableDeviceGroupPresentModeFlagBitsKHR :: [(DeviceGroupPresentModeFlagBitsKHR, String)]
showTableDeviceGroupPresentModeFlagBitsKHR =
  [ (DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR             , "LOCAL_BIT_KHR")
  , (DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR            , "REMOTE_BIT_KHR")
  , (DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR               , "SUM_BIT_KHR")
  , (DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR, "LOCAL_MULTI_DEVICE_BIT_KHR")
  ]


instance Show DeviceGroupPresentModeFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixDeviceGroupPresentModeFlagBitsKHR
                          showTableDeviceGroupPresentModeFlagBitsKHR
                          conNameDeviceGroupPresentModeFlagBitsKHR
                          (\(DeviceGroupPresentModeFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DeviceGroupPresentModeFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixDeviceGroupPresentModeFlagBitsKHR
                          showTableDeviceGroupPresentModeFlagBitsKHR
                          conNameDeviceGroupPresentModeFlagBitsKHR
                          DeviceGroupPresentModeFlagBitsKHR


type SwapchainCreateFlagsKHR = SwapchainCreateFlagBitsKHR

-- No documentation found for TopLevel "VkSwapchainCreateFlagBitsKHR"
newtype SwapchainCreateFlagBitsKHR = SwapchainCreateFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSwapchainCreateFlagBitsKHR" "VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR"
pattern SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR              = SwapchainCreateFlagBitsKHR 0x00000004
-- No documentation found for Nested "VkSwapchainCreateFlagBitsKHR" "VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
pattern SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = SwapchainCreateFlagBitsKHR 0x00000001
-- No documentation found for Nested "VkSwapchainCreateFlagBitsKHR" "VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR"
pattern SWAPCHAIN_CREATE_PROTECTED_BIT_KHR                   = SwapchainCreateFlagBitsKHR 0x00000002

conNameSwapchainCreateFlagBitsKHR :: String
conNameSwapchainCreateFlagBitsKHR = "SwapchainCreateFlagBitsKHR"

enumPrefixSwapchainCreateFlagBitsKHR :: String
enumPrefixSwapchainCreateFlagBitsKHR = "SWAPCHAIN_CREATE_"

showTableSwapchainCreateFlagBitsKHR :: [(SwapchainCreateFlagBitsKHR, String)]
showTableSwapchainCreateFlagBitsKHR =
  [ (SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR             , "MUTABLE_FORMAT_BIT_KHR")
  , (SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR, "SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR")
  , (SWAPCHAIN_CREATE_PROTECTED_BIT_KHR                  , "PROTECTED_BIT_KHR")
  ]


instance Show SwapchainCreateFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixSwapchainCreateFlagBitsKHR
                          showTableSwapchainCreateFlagBitsKHR
                          conNameSwapchainCreateFlagBitsKHR
                          (\(SwapchainCreateFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read SwapchainCreateFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixSwapchainCreateFlagBitsKHR
                          showTableSwapchainCreateFlagBitsKHR
                          conNameSwapchainCreateFlagBitsKHR
                          SwapchainCreateFlagBitsKHR


type KHR_SWAPCHAIN_SPEC_VERSION = 70

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_SPEC_VERSION"
pattern KHR_SWAPCHAIN_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SWAPCHAIN_SPEC_VERSION = 70


type KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_EXTENSION_NAME"
pattern KHR_SWAPCHAIN_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"

