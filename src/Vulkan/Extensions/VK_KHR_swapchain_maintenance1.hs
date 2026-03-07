{-# language CPP #-}
-- | = Name
--
-- VK_KHR_swapchain_maintenance1 - device extension
--
-- = VK_KHR_swapchain_maintenance1
--
-- [__Name String__]
--     @VK_KHR_swapchain_maintenance1@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     488
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_maintenance1 VK_KHR_surface_maintenance1>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_swapchain_maintenance1] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_swapchain_maintenance1 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_swapchain_maintenance1.adoc VK_KHR_swapchain_maintenance1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-03-31
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Shahbaz Youssefi, Google
--
--     -   Chris Forbes, Google
--
--     -   Ian Elliott, Google
--
--     -   Yiwei Zhang, Google
--
--     -   Charlie Lao, Google
--
--     -   Lina Versace, Google
--
--     -   Ralph Potter, Samsung
--
--     -   Igor Nazarov, Samsung
--
--     -   Hyunchang Kim, Samsung
--
--     -   Suenghwan Lee, Samsung
--
--     -   Munseong Kang, Samsung
--
--     -   Joonyong Park, Samsung
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Lisa Wu, Arm
--
--     -   Daniel Stone, Collabora
--
--     -   Pan Gao, Huawei
--
-- == Description
--
-- This extension is based off the @VK_EXT_swapchain_maintenance1@
-- extension.
--
-- @VK_KHR_swapchain_maintenance1@ adds a collection of window system
-- integration features that were intentionally left out or overlooked in
-- the original @VK_KHR_swapchain@ extension.
--
-- The new features are as follows:
--
-- -   Specify a fence that will be signaled when the resources associated
--     with a present operation /can/ be safely destroyed.
--
-- -   Allow changing the present mode a swapchain is using at per-present
--     granularity.
--
-- -   Allow applications to define the behavior when presenting a
--     swapchain image to a surface with different dimensions than the
--     image. Using this feature /may/ allow implementations to avoid
--     returning 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR' in this
--     situation.
--
-- -   Allow applications to defer swapchain memory allocation for improved
--     startup time and memory footprint.
--
-- -   Allow applications to release previously acquired images without
--     presenting them.
--
-- == New Commands
--
-- -   'releaseSwapchainImagesKHR'
--
-- == New Structures
--
-- -   'ReleaseSwapchainImagesInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSwapchainMaintenance1FeaturesKHR'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'SwapchainPresentFenceInfoKHR'
--
--     -   'SwapchainPresentModeInfoKHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SwapchainPresentModesCreateInfoKHR'
--
--     -   'SwapchainPresentScalingCreateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'KHR_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_KHR'
--
-- == Version History
--
-- -   Revision 1, 2025-03-31 (Shahbaz Youssefi)
--
--     -   Based on VK_EXT_swapchain_maintenance1
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_swapchain_maintenance1 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_swapchain_maintenance1  ( releaseSwapchainImagesKHR
                                                        , PhysicalDeviceSwapchainMaintenance1FeaturesKHR(..)
                                                        , SwapchainPresentFenceInfoKHR(..)
                                                        , SwapchainPresentModesCreateInfoKHR(..)
                                                        , SwapchainPresentModeInfoKHR(..)
                                                        , SwapchainPresentScalingCreateInfoKHR(..)
                                                        , ReleaseSwapchainImagesInfoKHR(..)
                                                        , KHR_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION
                                                        , pattern KHR_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION
                                                        , KHR_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME
                                                        , pattern KHR_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME
                                                        , SwapchainKHR(..)
                                                        , PresentModeKHR(..)
                                                        , SwapchainCreateFlagBitsKHR(..)
                                                        , SwapchainCreateFlagsKHR
                                                        , PresentScalingFlagBitsKHR(..)
                                                        , PresentScalingFlagsKHR
                                                        , PresentGravityFlagBitsKHR(..)
                                                        , PresentGravityFlagsKHR
                                                        ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkReleaseSwapchainImagesKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentGravityFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentScalingFlagsKHR)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentGravityFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentGravityFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentScalingFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentScalingFlagsKHR)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseSwapchainImagesKHR
  :: FunPtr (Ptr Device_T -> Ptr ReleaseSwapchainImagesInfoKHR -> IO Result) -> Ptr Device_T -> Ptr ReleaseSwapchainImagesInfoKHR -> IO Result

-- | vkReleaseSwapchainImagesKHR - Release previously acquired but unused
-- images
--
-- = Description
--
-- Only images that are not in use by the device /can/ be released.
--
-- Releasing images is a read-only operation that will not affect the
-- content of the released images. Upon reacquiring the image, the image
-- contents and its layout will be the same as they were prior to releasing
-- it. However, if a mechanism other than Vulkan is used to modify the
-- platform window associated with the swapchain, the content of all
-- presentable images in the swapchain becomes undefined.
--
-- This functionality is useful during swapchain recreation, where acquired
-- images from the old swapchain can be released instead of presented.
--
-- == Valid Usage
--
-- -   #VUID-vkReleaseSwapchainImagesKHR-swapchainMaintenance1-10159#
--     Feature
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-swapchainMaintenance1 swapchainMaintenance1>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkReleaseSwapchainImagesKHR-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkReleaseSwapchainImagesKHR-pReleaseInfo-parameter#
--     @pReleaseInfo@ /must/ be a valid pointer to a valid
--     'ReleaseSwapchainImagesInfoKHR' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain_maintenance1 VK_KHR_swapchain_maintenance1>,
-- 'Vulkan.Core10.Handles.Device', 'ReleaseSwapchainImagesInfoKHR'
releaseSwapchainImagesKHR :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the device associated with
                             -- 'ReleaseSwapchainImagesInfoKHR'::@swapchain@.
                             Device
                          -> -- | @pReleaseInfo@ is a pointer to a 'ReleaseSwapchainImagesInfoKHR'
                             -- structure containing parameters of the release.
                             ("releaseInfo" ::: ReleaseSwapchainImagesInfoKHR)
                          -> io ()
releaseSwapchainImagesKHR device releaseInfo = liftIO . evalContT $ do
  let vkReleaseSwapchainImagesKHRPtr = pVkReleaseSwapchainImagesKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkReleaseSwapchainImagesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseSwapchainImagesKHR is null" Nothing Nothing
  let vkReleaseSwapchainImagesKHR' = mkVkReleaseSwapchainImagesKHR vkReleaseSwapchainImagesKHRPtr
  pReleaseInfo <- ContT $ withCStruct (releaseInfo)
  r <- lift $ traceAroundEvent "vkReleaseSwapchainImagesKHR" (vkReleaseSwapchainImagesKHR'
                                                                (deviceHandle (device))
                                                                pReleaseInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkPhysicalDeviceSwapchainMaintenance1FeaturesKHR - Structure describing
-- whether implementation supports swapchain maintenance1 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceSwapchainMaintenance1FeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceSwapchainMaintenance1FeaturesKHR', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain_maintenance1 VK_KHR_swapchain_maintenance1>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSwapchainMaintenance1FeaturesKHR = PhysicalDeviceSwapchainMaintenance1FeaturesKHR
  { -- | #features-swapchainMaintenance1# @swapchainMaintenance1@ indicates that
    -- the implementation supports the following:
    --
    -- -   'SwapchainPresentFenceInfoKHR', specifying a fence that is signaled
    --     when the resources associated with a present operation /can/ be
    --     safely destroyed.
    --
    -- -   'SwapchainPresentModesCreateInfoKHR' and
    --     'SwapchainPresentModeInfoKHR', allowing the swapchain to switch
    --     present modes without a need for recreation.
    --
    -- -   'SwapchainPresentScalingCreateInfoKHR', specifying the scaling
    --     behavior of the swapchain in presence of window resizing.
    --
    -- -   The
    --     'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_KHR'
    --     flag, allowing the implementation to defer the allocation of
    --     swapchain image memory until first acquisition.
    --
    -- -   'releaseSwapchainImagesKHR', allowing acquired swapchain images to
    --     be released without presenting them.
    swapchainMaintenance1 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSwapchainMaintenance1FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceSwapchainMaintenance1FeaturesKHR

instance ToCStruct PhysicalDeviceSwapchainMaintenance1FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSwapchainMaintenance1FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (swapchainMaintenance1))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSwapchainMaintenance1FeaturesKHR where
  peekCStruct p = do
    swapchainMaintenance1 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSwapchainMaintenance1FeaturesKHR
             (bool32ToBool swapchainMaintenance1)

instance Storable PhysicalDeviceSwapchainMaintenance1FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSwapchainMaintenance1FeaturesKHR where
  zero = PhysicalDeviceSwapchainMaintenance1FeaturesKHR
           zero


-- | VkSwapchainPresentFenceInfoKHR - Fences associated with a
-- vkQueuePresentKHR operation
--
-- = Description
--
-- The set of /queue operations/ defined by queuing an image for
-- presentation, as well as operations performed by the presentation
-- engine, access the payloads of objects associated with the presentation
-- operation. The associated objects include:
--
-- -   The swapchain image, its implicitly bound memory, and any other
--     resources bound to that memory.
--
-- -   The wait semaphores specified when queuing the image for
--     presentation.
--
-- The application /can/ provide a fence that the implementation will
-- signal after all such queue operations have completed, and after the
-- presentation engine has taken a reference to the payloads of all objects
-- provided in 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' that the
-- presentation engine accesses as part of the present operation. The fence
-- /may/ not wait for the present operation to complete. For all binary
-- wait semaphores imported by the presentation engine using the equivalent
-- of reference transference, as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>,
-- this fence /must/ not signal until all such semaphore payloads have been
-- reset by the presentation engine.
--
-- The application /can/ destroy the wait semaphores associated with a
-- given presentation operation when at least one of the associated fences
-- is signaled, and /can/ destroy the swapchain when the fences associated
-- with all past presentation requests referring to that swapchain have
-- signaled.
--
-- Fences associated with presentations to the same swapchain on the same
-- 'Vulkan.Core10.Handles.Queue' /must/ be signaled in the same order as
-- the present operations.
--
-- To specify a fence for each swapchain in a present operation, include
-- the 'SwapchainPresentFenceInfoKHR' structure in the @pNext@ chain of the
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' structure.
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainPresentFenceInfoKHR-swapchainCount-07757#
--     @swapchainCount@ /must/ be equal to
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@
--
-- -   #VUID-VkSwapchainPresentFenceInfoKHR-pFences-07758# Each element of
--     @pFences@ that is not 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     /must/ be unsignaled
--
-- -   #VUID-VkSwapchainPresentFenceInfoKHR-pFences-07759# Each element of
--     @pFences@ that is not 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     /must/ not be associated with any other queue command that has not
--     yet completed execution on that queue
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainPresentFenceInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR'
--
-- -   #VUID-VkSwapchainPresentFenceInfoKHR-pFences-parameter# @pFences@
--     /must/ be a valid pointer to an array of @swapchainCount@ valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Core10.Handles.Fence' handles
--
-- -   #VUID-VkSwapchainPresentFenceInfoKHR-swapchainCount-arraylength#
--     @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain_maintenance1 VK_KHR_swapchain_maintenance1>,
-- 'Vulkan.Core10.Handles.Fence',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainPresentFenceInfoKHR = SwapchainPresentFenceInfoKHR
  { -- | @pFences@ is a list of fences with @swapchainCount@ entries. Each entry
    -- /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE' or the handle of a
    -- fence to signal when the relevant operations on the associated swapchain
    -- have completed.
    fences :: Vector Fence }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainPresentFenceInfoKHR)
#endif
deriving instance Show SwapchainPresentFenceInfoKHR

instance ToCStruct SwapchainPresentFenceInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainPresentFenceInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (fences)) :: Word32))
    pPFences' <- ContT $ allocaBytes @Fence ((Data.Vector.length (fences)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPFences' `plusPtr` (8 * (i)) :: Ptr Fence) (e)) (fences)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Fence))) (pPFences')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainPresentFenceInfoKHR where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pFences <- peek @(Ptr Fence) ((p `plusPtr` 24 :: Ptr (Ptr Fence)))
    pFences' <- generateM (fromIntegral swapchainCount) (\i -> peek @Fence ((pFences `advancePtrBytes` (8 * (i)) :: Ptr Fence)))
    pure $ SwapchainPresentFenceInfoKHR
             pFences'

instance Zero SwapchainPresentFenceInfoKHR where
  zero = SwapchainPresentFenceInfoKHR
           mempty


-- | VkSwapchainPresentModesCreateInfoKHR - All presentation modes usable by
-- the swapchain
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoKHR-None-07762# Each entry in
--     pPresentModes /must/ be one of the
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR'
--     for the surface
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoKHR-presentModeFifoLatestReady-10160#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-presentModeFifoLatestReady presentModeFifoLatestReady>
--     feature is not enabled, pPresentModes /must/ not contain
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_LATEST_READY_KHR'
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoKHR-pPresentModes-07763# The
--     entries in pPresentModes /must/ be a subset of the present modes
--     returned in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentModeCompatibilityKHR'::@pPresentModes@,
--     given
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentModeKHR'
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoKHR-presentMode-07764#
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     /must/ be included in @pPresentModes@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR'
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoKHR-pPresentModes-parameter#
--     @pPresentModes@ /must/ be a valid pointer to an array of
--     @presentModeCount@ valid
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoKHR-presentModeCount-arraylength#
--     @presentModeCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain_maintenance1 VK_KHR_swapchain_maintenance1>,
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainPresentModesCreateInfoKHR = SwapchainPresentModesCreateInfoKHR
  { -- | @pPresentModes@ is a list of presentation modes with @presentModeCount@
    -- entries
    presentModes :: Vector PresentModeKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainPresentModesCreateInfoKHR)
#endif
deriving instance Show SwapchainPresentModesCreateInfoKHR

instance ToCStruct SwapchainPresentModesCreateInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainPresentModesCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (presentModes)) :: Word32))
    pPPresentModes' <- ContT $ allocaBytes @PresentModeKHR ((Data.Vector.length (presentModes)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPresentModes' `plusPtr` (4 * (i)) :: Ptr PresentModeKHR) (e)) (presentModes)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR))) (pPPresentModes')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainPresentModesCreateInfoKHR where
  peekCStruct p = do
    presentModeCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPresentModes <- peek @(Ptr PresentModeKHR) ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR)))
    pPresentModes' <- generateM (fromIntegral presentModeCount) (\i -> peek @PresentModeKHR ((pPresentModes `advancePtrBytes` (4 * (i)) :: Ptr PresentModeKHR)))
    pure $ SwapchainPresentModesCreateInfoKHR
             pPresentModes'

instance Zero SwapchainPresentModesCreateInfoKHR where
  zero = SwapchainPresentModesCreateInfoKHR
           mempty


-- | VkSwapchainPresentModeInfoKHR - Presentation modes for a
-- vkQueuePresentKHR operation
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' includes a
-- 'SwapchainPresentModeInfoKHR' structure, then that structure defines the
-- presentation modes used for the current and subsequent presentation
-- operations.
--
-- When the application changes present modes with
-- 'SwapchainPresentModeInfoKHR', images that have already been queued for
-- presentation will continue to be presented according to the previous
-- present mode. The current image being queued for presentation and
-- subsequent images will be presented according to the new present mode.
-- The behavior during the transition between the two modes is defined as
-- follows.
--
-- -   Transition from
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR'
--     to
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR':
--     the presentation engine updates the shared presentable image
--     according to the behavior of
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'.
--
-- -   Transition from
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
--     to
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR':
--     the presentation engine /may/ update the shared presentable image or
--     defer that to its regular refresh cycle, according to the behavior
--     of
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR'.
--
-- -   Transition between
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' and
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR':
--     Images continue to be appended to the same FIFO queue, and the
--     behavior with respect to waiting for vertical blanking period will
--     follow the new mode for current and subsequent images.
--
-- -   Transition from
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_IMMEDIATE_KHR' to
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_LATEST_READY_KHR'
--     : As all prior present requests in the
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_IMMEDIATE_KHR' mode
--     are applied immediately, there are no outstanding present operations
--     in this mode, and current and subsequent images are appended to the
--     FIFO queue and presented according to the new mode.
--
-- -   Transition from
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR' to
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_LATEST_READY_KHR'
--     : Presentation in FIFO modes require waiting for the next vertical
--     blanking period, with
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR' allowing
--     the pending present operation to be replaced by a new one. In this
--     case, the current present operation will replace the pending present
--     operation and is applied according to the new mode.
--
-- -   Transition from
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_LATEST_READY_KHR'
--     to 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_IMMEDIATE_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR': If the
--     FIFO queue is empty, presentation is done according to the behavior
--     of the new mode. If there are present operations in the FIFO queue,
--     once the last present operation is performed based on the respective
--     vertical blanking period, the current and subsequent updates are
--     applied according to the new mode.
--
-- -   Transition between
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR',
--     and
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_LATEST_READY_KHR':
--     Images continue to be appended to the same FIFO queue, and the
--     behavior with respect to waiting for vertical blanking period and
--     dequeuing requests will follow the new mode for current and
--     subsequent images.
--
-- -   The behavior during transition between any other present modes, if
--     possible, is implementation defined.
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainPresentModeInfoKHR-swapchainCount-07760#
--     @swapchainCount@ /must/ be equal to
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@
--
-- -   #VUID-VkSwapchainPresentModeInfoKHR-pPresentModes-07761# Each entry
--     in @pPresentModes@ /must/ be a presentation mode specified in
--     'SwapchainPresentModesCreateInfoKHR'::@pPresentModes@ when creating
--     the entry’s corresponding swapchain
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainPresentModeInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR'
--
-- -   #VUID-VkSwapchainPresentModeInfoKHR-pPresentModes-parameter#
--     @pPresentModes@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values
--
-- -   #VUID-VkSwapchainPresentModeInfoKHR-swapchainCount-arraylength#
--     @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain_maintenance1 VK_KHR_swapchain_maintenance1>,
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainPresentModeInfoKHR = SwapchainPresentModeInfoKHR
  { -- | @pPresentModes@ is a list of presentation modes with @swapchainCount@
    -- entries.
    presentModes :: Vector PresentModeKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainPresentModeInfoKHR)
#endif
deriving instance Show SwapchainPresentModeInfoKHR

instance ToCStruct SwapchainPresentModeInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainPresentModeInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (presentModes)) :: Word32))
    pPPresentModes' <- ContT $ allocaBytes @PresentModeKHR ((Data.Vector.length (presentModes)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPresentModes' `plusPtr` (4 * (i)) :: Ptr PresentModeKHR) (e)) (presentModes)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR))) (pPPresentModes')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainPresentModeInfoKHR where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPresentModes <- peek @(Ptr PresentModeKHR) ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR)))
    pPresentModes' <- generateM (fromIntegral swapchainCount) (\i -> peek @PresentModeKHR ((pPresentModes `advancePtrBytes` (4 * (i)) :: Ptr PresentModeKHR)))
    pure $ SwapchainPresentModeInfoKHR
             pPresentModes'

instance Zero SwapchainPresentModeInfoKHR where
  zero = SwapchainPresentModeInfoKHR
           mempty


-- | VkSwapchainPresentScalingCreateInfoKHR - Scaling behavior when
-- presenting to the surface
--
-- = Description
--
-- If @scalingBehavior@ is @0@, the result of presenting a swapchain image
-- with dimensions that do not match the surface dimensions is
-- implementation and platform-dependent. If @presentGravityX@ or
-- @presentGravityY@ are @0@, the presentation gravity /must/ match that
-- defined by the native platform surface on platforms which define surface
-- gravity.
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityX-07765#
--     If @presentGravityX@ is @0@, @presentGravityY@ /must/ be @0@
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityX-07766#
--     If @presentGravityX@ is not @0@, @presentGravityY@ /must/ not be @0@
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-scalingBehavior-07767#
--     @scalingBehavior@ /must/ not have more than one bit set
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityX-07768#
--     @presentGravityX@ /must/ not have more than one bit set
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityY-07769#
--     @presentGravityY@ /must/ not have more than one bit set
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-scalingBehavior-07770#
--     @scalingBehavior@ /must/ be @0@ or a valid scaling method for the
--     surface as returned in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentScalingCapabilitiesKHR'::@supportedPresentScaling@,
--     given
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentModeKHR'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-scalingBehavior-07771#
--     If the swapchain is created with
--     'SwapchainPresentModesCreateInfoKHR', @scalingBehavior@ /must/ be
--     @0@ or a valid scaling method for the surface as returned in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentScalingCapabilitiesKHR'::@supportedPresentScaling@,
--     given each present mode in
--     'SwapchainPresentModesCreateInfoKHR'::@pPresentModes@ in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentModeKHR'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityX-07772#
--     @presentGravityX@ /must/ be @0@ or a valid x-axis present gravity
--     for the surface as returned in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentScalingCapabilitiesKHR'::@supportedPresentGravityX@,
--     given
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentModeKHR'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityX-07773#
--     If the swapchain is created with
--     'SwapchainPresentModesCreateInfoKHR', @presentGravityX@ /must/ be
--     @0@ or a valid x-axis present gravity for the surface as returned in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentScalingCapabilitiesKHR'::@supportedPresentGravityX@,
--     given each present mode in
--     'SwapchainPresentModesCreateInfoKHR'::@pPresentModes@ in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentModeKHR'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityY-07774#
--     @presentGravityY@ /must/ be @0@ or a valid y-axis present gravity
--     for the surface as returned in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentScalingCapabilitiesKHR'::@supportedPresentGravityY@,
--     given
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentModeKHR'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityY-07775#
--     If the swapchain is created with
--     'SwapchainPresentModesCreateInfoKHR', @presentGravityY@ /must/ be
--     @0@ or a valid y-axis present gravity for the surface as returned in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentScalingCapabilitiesKHR'::@supportedPresentGravityY@,
--     given each present mode in
--     'SwapchainPresentModesCreateInfoKHR'::@pPresentModes@ in
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.SurfacePresentModeKHR'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-swapchainMaintenance1-10154#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-swapchainMaintenance1 swapchainMaintenance1>
--     feature is not enabled, then @scalingBehavior@, @presentGravityX@,
--     and @presentGravityY@ /must/ be @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-scalingBehavior-parameter#
--     @scalingBehavior@ /must/ be a valid combination of
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.PresentScalingFlagBitsKHR'
--     values
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityX-parameter#
--     @presentGravityX@ /must/ be a valid combination of
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.PresentGravityFlagBitsKHR'
--     values
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoKHR-presentGravityY-parameter#
--     @presentGravityY@ /must/ be a valid combination of
--     'Vulkan.Extensions.VK_KHR_surface_maintenance1.PresentGravityFlagBitsKHR'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain_maintenance1 VK_KHR_swapchain_maintenance1>,
-- 'Vulkan.Extensions.VK_KHR_surface_maintenance1.PresentGravityFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_surface_maintenance1.PresentScalingFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainPresentScalingCreateInfoKHR = SwapchainPresentScalingCreateInfoKHR
  { -- | @scalingBehavior@ is @0@ or the scaling method to use when the
    -- dimensions of the surface and swapchain images differ.
    scalingBehavior :: PresentScalingFlagsKHR
  , -- | @presentGravityX@ is @0@ or the x-axis direction in which swapchain
    -- image pixels gravitate relative to the surface when @scalingBehavior@
    -- does not result in a one-to-one pixel mapping between the scaled
    -- swapchain image and the surface.
    presentGravityX :: PresentGravityFlagsKHR
  , -- | @presentGravityY@ is @0@ or the y-axis direction in which swapchain
    -- image pixels gravitate relative to the surface when @scalingBehavior@
    -- does not result in a one-to-one pixel mapping between the scaled
    -- swapchain image and the surface.
    presentGravityY :: PresentGravityFlagsKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainPresentScalingCreateInfoKHR)
#endif
deriving instance Show SwapchainPresentScalingCreateInfoKHR

instance ToCStruct SwapchainPresentScalingCreateInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainPresentScalingCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PresentScalingFlagsKHR)) (scalingBehavior)
    poke ((p `plusPtr` 20 :: Ptr PresentGravityFlagsKHR)) (presentGravityX)
    poke ((p `plusPtr` 24 :: Ptr PresentGravityFlagsKHR)) (presentGravityY)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainPresentScalingCreateInfoKHR where
  peekCStruct p = do
    scalingBehavior <- peek @PresentScalingFlagsKHR ((p `plusPtr` 16 :: Ptr PresentScalingFlagsKHR))
    presentGravityX <- peek @PresentGravityFlagsKHR ((p `plusPtr` 20 :: Ptr PresentGravityFlagsKHR))
    presentGravityY <- peek @PresentGravityFlagsKHR ((p `plusPtr` 24 :: Ptr PresentGravityFlagsKHR))
    pure $ SwapchainPresentScalingCreateInfoKHR
             scalingBehavior presentGravityX presentGravityY

instance Storable SwapchainPresentScalingCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainPresentScalingCreateInfoKHR where
  zero = SwapchainPresentScalingCreateInfoKHR
           zero
           zero
           zero


-- | VkReleaseSwapchainImagesInfoKHR - Structure describing a list of
-- swapchain image indices to be released
--
-- == Valid Usage
--
-- -   #VUID-VkReleaseSwapchainImagesInfoKHR-pImageIndices-07785# Each
--     element of @pImageIndices@ /must/ be the index of a presentable
--     image acquired from the swapchain specified by @swapchain@
--
-- -   #VUID-VkReleaseSwapchainImagesInfoKHR-pImageIndices-07786# All uses
--     of presentable images identified by elements of @pImageIndices@
--     /must/ have completed execution
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkReleaseSwapchainImagesInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR'
--
-- -   #VUID-VkReleaseSwapchainImagesInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkReleaseSwapchainImagesInfoKHR-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-VkReleaseSwapchainImagesInfoKHR-pImageIndices-parameter#
--     @pImageIndices@ /must/ be a valid pointer to an array of
--     @imageIndexCount@ @uint32_t@ values
--
-- -   #VUID-VkReleaseSwapchainImagesInfoKHR-imageIndexCount-arraylength#
--     @imageIndexCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain_maintenance1 VK_KHR_swapchain_maintenance1>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR', 'releaseSwapchainImagesKHR',
-- 'releaseSwapchainImagesKHR'
data ReleaseSwapchainImagesInfoKHR = ReleaseSwapchainImagesInfoKHR
  { -- | @swapchain@ is a swapchain to which images are being released.
    swapchain :: SwapchainKHR
  , -- | @pImageIndices@ is a pointer to an array of indices into the array of
    -- @swapchain@’s presentable images, with @imageIndexCount@ entries.
    imageIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ReleaseSwapchainImagesInfoKHR)
#endif
deriving instance Show ReleaseSwapchainImagesInfoKHR

instance ToCStruct ReleaseSwapchainImagesInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ReleaseSwapchainImagesInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (swapchain)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (imageIndices)) :: Word32))
    pPImageIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (imageIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPImageIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (imageIndices)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word32))) (pPImageIndices')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (zero)
    f

instance FromCStruct ReleaseSwapchainImagesInfoKHR where
  peekCStruct p = do
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 16 :: Ptr SwapchainKHR))
    imageIndexCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pImageIndices <- peek @(Ptr Word32) ((p `plusPtr` 32 :: Ptr (Ptr Word32)))
    pImageIndices' <- generateM (fromIntegral imageIndexCount) (\i -> peek @Word32 ((pImageIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ ReleaseSwapchainImagesInfoKHR
             swapchain pImageIndices'

instance Zero ReleaseSwapchainImagesInfoKHR where
  zero = ReleaseSwapchainImagesInfoKHR
           zero
           mempty


type KHR_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION"
pattern KHR_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION = 1


type KHR_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME = "VK_KHR_swapchain_maintenance1"

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME"
pattern KHR_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME = "VK_KHR_swapchain_maintenance1"

