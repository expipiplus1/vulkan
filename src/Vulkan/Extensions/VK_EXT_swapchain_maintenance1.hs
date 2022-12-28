{-# language CPP #-}
-- | = Name
--
-- VK_EXT_swapchain_maintenance1 - device extension
--
-- == VK_EXT_swapchain_maintenance1
--
-- [__Name String__]
--     @VK_EXT_swapchain_maintenance1@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     276
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@ to be enabled for any device-level
--         functionality
--
--     -   Requires @VK_EXT_surface_maintenance1@ to be enabled for any
--         device-level functionality
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_swapchain_maintenance1] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_swapchain_maintenance1 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_swapchain_maintenance1.adoc VK_EXT_swapchain_maintenance1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-10-28
--
-- [__Contributors__]
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
--     -   Chad Versace, Google
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
-- @VK_EXT_swapchain_maintenance1@ adds a collection of window system
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
-- -   'releaseSwapchainImagesEXT'
--
-- == New Structures
--
-- -   'ReleaseSwapchainImagesInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSwapchainMaintenance1FeaturesEXT'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'SwapchainPresentFenceInfoEXT'
--
--     -   'SwapchainPresentModeInfoEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SwapchainPresentModesCreateInfoEXT'
--
--     -   'SwapchainPresentScalingCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_EXT'
--
-- == Version History
--
-- -   Revision 0, 2019-05-28
--
--     -   Initial revisions
--
-- -   Revision 1, 2022-08-21 (Shahbaz Youssefi)
--
--     -   Add functionality and complete spec
--
-- == See Also
--
-- 'PhysicalDeviceSwapchainMaintenance1FeaturesEXT',
-- 'ReleaseSwapchainImagesInfoEXT', 'SwapchainPresentFenceInfoEXT',
-- 'SwapchainPresentModeInfoEXT', 'SwapchainPresentModesCreateInfoEXT',
-- 'SwapchainPresentScalingCreateInfoEXT', 'releaseSwapchainImagesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_swapchain_maintenance1  ( releaseSwapchainImagesEXT
                                                        , PhysicalDeviceSwapchainMaintenance1FeaturesEXT(..)
                                                        , SwapchainPresentFenceInfoEXT(..)
                                                        , SwapchainPresentModesCreateInfoEXT(..)
                                                        , SwapchainPresentModeInfoEXT(..)
                                                        , SwapchainPresentScalingCreateInfoEXT(..)
                                                        , ReleaseSwapchainImagesInfoEXT(..)
                                                        , EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION
                                                        , pattern EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION
                                                        , EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME
                                                        , pattern EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME
                                                        , SwapchainKHR(..)
                                                        , PresentModeKHR(..)
                                                        , SwapchainCreateFlagBitsKHR(..)
                                                        , SwapchainCreateFlagsKHR
                                                        , PresentScalingFlagBitsEXT(..)
                                                        , PresentScalingFlagsEXT
                                                        , PresentGravityFlagBitsEXT(..)
                                                        , PresentGravityFlagsEXT
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
import Vulkan.Dynamic (DeviceCmds(pVkReleaseSwapchainImagesEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Extensions.VK_EXT_surface_maintenance1 (PresentGravityFlagsEXT)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR)
import Vulkan.Extensions.VK_EXT_surface_maintenance1 (PresentScalingFlagsEXT)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_EXT_surface_maintenance1 (PresentGravityFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_surface_maintenance1 (PresentGravityFlagsEXT)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.VK_EXT_surface_maintenance1 (PresentScalingFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_surface_maintenance1 (PresentScalingFlagsEXT)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseSwapchainImagesEXT
  :: FunPtr (Ptr Device_T -> Ptr ReleaseSwapchainImagesInfoEXT -> IO Result) -> Ptr Device_T -> Ptr ReleaseSwapchainImagesInfoEXT -> IO Result

-- | vkReleaseSwapchainImagesEXT - Release previously acquired but unused
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
-- Note
--
-- This functionality is useful during swapchain recreation, where acquired
-- images from the old swapchain can be released instead of presented.
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- 'Vulkan.Core10.Handles.Device', 'ReleaseSwapchainImagesInfoEXT'
releaseSwapchainImagesEXT :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the device associated with
                             -- 'ReleaseSwapchainImagesInfoEXT'::@swapchain@.
                             --
                             -- #VUID-vkReleaseSwapchainImagesEXT-device-parameter# @device@ /must/ be a
                             -- valid 'Vulkan.Core10.Handles.Device' handle
                             Device
                          -> -- | @pReleaseInfo@ is a pointer to a 'ReleaseSwapchainImagesInfoEXT'
                             -- structure containing parameters of the release.
                             --
                             -- #VUID-vkReleaseSwapchainImagesEXT-pReleaseInfo-parameter# @pReleaseInfo@
                             -- /must/ be a valid pointer to a valid 'ReleaseSwapchainImagesInfoEXT'
                             -- structure
                             ("releaseInfo" ::: ReleaseSwapchainImagesInfoEXT)
                          -> io ()
releaseSwapchainImagesEXT device releaseInfo = liftIO . evalContT $ do
  let vkReleaseSwapchainImagesEXTPtr = pVkReleaseSwapchainImagesEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkReleaseSwapchainImagesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseSwapchainImagesEXT is null" Nothing Nothing
  let vkReleaseSwapchainImagesEXT' = mkVkReleaseSwapchainImagesEXT vkReleaseSwapchainImagesEXTPtr
  pReleaseInfo <- ContT $ withCStruct (releaseInfo)
  r <- lift $ traceAroundEvent "vkReleaseSwapchainImagesEXT" (vkReleaseSwapchainImagesEXT'
                                                                (deviceHandle (device))
                                                                pReleaseInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkPhysicalDeviceSwapchainMaintenance1FeaturesEXT - Structure describing
-- whether implementation supports swapchain maintenance1 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceSwapchainMaintenance1FeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceSwapchainMaintenance1FeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSwapchainMaintenance1FeaturesEXT = PhysicalDeviceSwapchainMaintenance1FeaturesEXT
  { -- | #features-swapchainMaintenance1# @swapchainMaintenance1@ indicates that
    -- the implementation supports the following:
    --
    -- -   'SwapchainPresentFenceInfoEXT', specifying a fence that is signaled
    --     when the resources associated with a present operation /can/ be
    --     safely destroyed.
    --
    -- -   'SwapchainPresentModesCreateInfoEXT' and
    --     'SwapchainPresentModeInfoEXT', allowing the swapchain to switch
    --     present modes without a need for recreation.
    --
    -- -   'SwapchainPresentScalingCreateInfoEXT', specifying the scaling
    --     behavior of the swapchain in presence of window resizing.
    --
    -- -   The
    --     'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_EXT'
    --     flag, allowing the implementation to defer the allocation of
    --     swapchain image memory until first acquisition.
    --
    -- -   'releaseSwapchainImagesEXT', allowing acquired swapchain images to
    --     be released without presenting them.
    swapchainMaintenance1 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSwapchainMaintenance1FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceSwapchainMaintenance1FeaturesEXT

instance ToCStruct PhysicalDeviceSwapchainMaintenance1FeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSwapchainMaintenance1FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (swapchainMaintenance1))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSwapchainMaintenance1FeaturesEXT where
  peekCStruct p = do
    swapchainMaintenance1 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSwapchainMaintenance1FeaturesEXT
             (bool32ToBool swapchainMaintenance1)

instance Storable PhysicalDeviceSwapchainMaintenance1FeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSwapchainMaintenance1FeaturesEXT where
  zero = PhysicalDeviceSwapchainMaintenance1FeaturesEXT
           zero


-- | VkSwapchainPresentFenceInfoEXT - Fences associated with a
-- vkQueuePresentKHR operation
--
-- = Description
--
-- The set of /queue operations/ defined by queuing an image for
-- presentation, as well as operations performed by the presentation engine
-- access the payloads of objects associated with the presentation
-- operation. The associated objects include:
--
-- -   The swapchain image, its implicitly bound memory, and any other
--     resources bound to that memory.
--
-- -   The wait semaphores specified when queuing the image for
--     presentation.
--
-- The application /can/ provide a fence that the implementation will
-- signal when all such queue operations have completed and the
-- presentation engine has taken a reference to the payload of any objects
-- it accesses as part of the present operation. For all binary wait
-- semaphores imported by the presentation engine using the equivalent of
-- reference transference, as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>,
-- this fence /must/ not signal until all such semaphore payloads have been
-- reset by the presentation engine.
--
-- The application /can/ destroy the wait semaphores associated with a
-- given presentation operation when the associated fence is signaled, and
-- /can/ destroy the swapchain when the fences associated with all past
-- presentation requests have signaled.
--
-- Fences associated with presentations to the same swapchain on the same
-- 'Vulkan.Core10.Handles.Queue' /must/ be signaled in the same order as
-- the present operations.
--
-- To specify a fence for each swapchain in a present operation, include
-- the 'SwapchainPresentFenceInfoEXT' structure in the @pNext@ chain of the
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' structure.
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainPresentFenceInfoEXT-swapchainCount-07757#
--     @swapchainCount@ /must/ be equal to
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@
--
-- -   #VUID-VkSwapchainPresentFenceInfoEXT-pFences-07758# Each element of
--     @pFences@ /must/ be unsignaled
--
-- -   #VUID-VkSwapchainPresentFenceInfoEXT-pFences-07759# Each element of
--     @pFences@ /must/ not be associated with any other queue command that
--     has not yet completed execution on that queue
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainPresentFenceInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT'
--
-- -   #VUID-VkSwapchainPresentFenceInfoEXT-pFences-parameter# @pFences@
--     /must/ be a valid pointer to an array of @swapchainCount@ valid
--     'Vulkan.Core10.Handles.Fence' handles
--
-- -   #VUID-VkSwapchainPresentFenceInfoEXT-swapchainCount-arraylength#
--     @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- 'Vulkan.Core10.Handles.Fence',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainPresentFenceInfoEXT = SwapchainPresentFenceInfoEXT
  { -- | @pFences@ is a list of fences with @swapchainCount@ entries. Each entry
    -- /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE' or the handle of a
    -- fence to signal when the relevant operations on the associated swapchain
    -- have completed.
    fences :: Vector Fence }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainPresentFenceInfoEXT)
#endif
deriving instance Show SwapchainPresentFenceInfoEXT

instance ToCStruct SwapchainPresentFenceInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainPresentFenceInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (fences)) :: Word32))
    pPFences' <- ContT $ allocaBytes @Fence ((Data.Vector.length (fences)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPFences' `plusPtr` (8 * (i)) :: Ptr Fence) (e)) (fences)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Fence))) (pPFences')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainPresentFenceInfoEXT where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pFences <- peek @(Ptr Fence) ((p `plusPtr` 24 :: Ptr (Ptr Fence)))
    pFences' <- generateM (fromIntegral swapchainCount) (\i -> peek @Fence ((pFences `advancePtrBytes` (8 * (i)) :: Ptr Fence)))
    pure $ SwapchainPresentFenceInfoEXT
             pFences'

instance Zero SwapchainPresentFenceInfoEXT where
  zero = SwapchainPresentFenceInfoEXT
           mempty


-- | VkSwapchainPresentModesCreateInfoEXT - All presentation modes usable by
-- the swapchain
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoEXT-None-07762# Each entry in
--     pPresentModes /must/ be one of the
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR'
--     for the surface
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoEXT-pPresentModes-07763# The
--     entries in pPresentModes /must/ be a subset of the present modes
--     returned in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentModeCompatibilityEXT'::@pPresentModes@,
--     given
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentModeEXT'
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoEXT-presentMode-07764#
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     /must/ be included in @pPresentModes@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT'
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoEXT-pPresentModes-parameter#
--     @pPresentModes@ /must/ be a valid pointer to an array of
--     @presentModeCount@ valid
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values
--
-- -   #VUID-VkSwapchainPresentModesCreateInfoEXT-presentModeCount-arraylength#
--     @presentModeCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainPresentModesCreateInfoEXT = SwapchainPresentModesCreateInfoEXT
  { -- | @pPresentModes@ is a list of presentation modes with @presentModeCount@
    -- entries
    presentModes :: Vector PresentModeKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainPresentModesCreateInfoEXT)
#endif
deriving instance Show SwapchainPresentModesCreateInfoEXT

instance ToCStruct SwapchainPresentModesCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainPresentModesCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (presentModes)) :: Word32))
    pPPresentModes' <- ContT $ allocaBytes @PresentModeKHR ((Data.Vector.length (presentModes)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPresentModes' `plusPtr` (4 * (i)) :: Ptr PresentModeKHR) (e)) (presentModes)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR))) (pPPresentModes')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainPresentModesCreateInfoEXT where
  peekCStruct p = do
    presentModeCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPresentModes <- peek @(Ptr PresentModeKHR) ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR)))
    pPresentModes' <- generateM (fromIntegral presentModeCount) (\i -> peek @PresentModeKHR ((pPresentModes `advancePtrBytes` (4 * (i)) :: Ptr PresentModeKHR)))
    pure $ SwapchainPresentModesCreateInfoEXT
             pPresentModes'

instance Zero SwapchainPresentModesCreateInfoEXT where
  zero = SwapchainPresentModesCreateInfoEXT
           mempty


-- | VkSwapchainPresentModeInfoEXT - Presentation modes for a
-- vkQueuePresentKHR operation
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' includes a
-- 'SwapchainPresentModeInfoEXT' structure, then that structure defines the
-- presentation modes used for the current and subsequent presentation
-- operations.
--
-- When the application changes present modes with
-- 'SwapchainPresentModeInfoEXT', images that have already been queued for
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
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR': As
--     all prior present requests in the
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_IMMEDIATE_KHR' mode
--     are applied immediately, there are no outstanding present operations
--     in this mode, and current and subsequent images are appended to the
--     FIFO queue and presented according to the new mode.
--
-- -   Transition from
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR' to
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR':
--     Presentation in both modes require waiting for the next vertical
--     blanking period, with
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR' allowing
--     the pending present operation to be replaced by a new one. In this
--     case, the current present operation will replace the pending present
--     operation and is applied according to the new mode.
--
-- -   Transition from
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR' to
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_IMMEDIATE_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR': If the
--     FIFO queue is empty, presentation is done according to the behavior
--     of the new mode. If there are present operations in the FIFO queue,
--     once the last present operation is performed based on the respective
--     vertical blanking period, the current and subsequent updates are
--     applied according to the new mode.
--
-- -   The behavior during transition between any other present modes, if
--     possible, is implementation defined.
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainPresentModeInfoEXT-swapchainCount-07760#
--     @swapchainCount@ /must/ be equal to
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@
--
-- -   #VUID-VkSwapchainPresentModeInfoEXT-pPresentModes-07761# Each entry
--     in @pPresentModes@ must be a presentation mode specified in
--     'SwapchainPresentModesCreateInfoEXT'::pPresentModes when creating
--     the entry’s corresponding swapchain
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainPresentModeInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT'
--
-- -   #VUID-VkSwapchainPresentModeInfoEXT-pPresentModes-parameter#
--     @pPresentModes@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values
--
-- -   #VUID-VkSwapchainPresentModeInfoEXT-swapchainCount-arraylength#
--     @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainPresentModeInfoEXT = SwapchainPresentModeInfoEXT
  { -- | @pPresentModes@ is a list of presentation modes with @swapchainCount@
    -- entries.
    presentModes :: Vector PresentModeKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainPresentModeInfoEXT)
#endif
deriving instance Show SwapchainPresentModeInfoEXT

instance ToCStruct SwapchainPresentModeInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainPresentModeInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (presentModes)) :: Word32))
    pPPresentModes' <- ContT $ allocaBytes @PresentModeKHR ((Data.Vector.length (presentModes)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPresentModes' `plusPtr` (4 * (i)) :: Ptr PresentModeKHR) (e)) (presentModes)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR))) (pPPresentModes')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainPresentModeInfoEXT where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPresentModes <- peek @(Ptr PresentModeKHR) ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR)))
    pPresentModes' <- generateM (fromIntegral swapchainCount) (\i -> peek @PresentModeKHR ((pPresentModes `advancePtrBytes` (4 * (i)) :: Ptr PresentModeKHR)))
    pure $ SwapchainPresentModeInfoEXT
             pPresentModes'

instance Zero SwapchainPresentModeInfoEXT where
  zero = SwapchainPresentModeInfoEXT
           mempty


-- | VkSwapchainPresentScalingCreateInfoEXT - Scaling behavior when
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
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityX-07765#
--     If @presentGravityX@ is @0@, @presentGravityY@ /must/ be @0@
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityX-07766#
--     If @presentGravityX@ is not @0@, @presentGravityY@ /must/ not be @0@
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-scalingBehavior-07767#
--     @scalingBehavior@ /must/ not have more than one bit set
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityX-07768#
--     @presentGravityX@ /must/ not have more than one bit set
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityY-07769#
--     @presentGravityY@ /must/ not have more than one bit set
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-scalingBehavior-07770#
--     @scalingBehavior@ /must/ be a valid scaling method for the surface
--     as returned in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentScalingCapabilitiesEXT'::@supportedPresentScaling@,
--     given
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentModeEXT'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-scalingBehavior-07771#
--     If the swapchain is created with
--     'SwapchainPresentModesCreateInfoEXT', @scalingBehavior@ /must/ be a
--     valid scaling method for the surface as returned in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentScalingCapabilitiesEXT'::@supportedPresentScaling@,
--     given each present mode in
--     'SwapchainPresentModesCreateInfoEXT'::@pPresentModes@ in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentModeEXT'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityX-07772#
--     @presentGravityX@ /must/ be a valid x-axis present gravity for the
--     surface as returned in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentScalingCapabilitiesEXT'::@supportedPresentGravityX@,
--     given
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentModeEXT'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityX-07773#
--     If the swapchain is created with
--     'SwapchainPresentModesCreateInfoEXT', @presentGravityX@ /must/ be a
--     valid x-axis present gravity for the surface as returned in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentScalingCapabilitiesEXT'::@supportedPresentGravityX@,
--     given each present mode in
--     'SwapchainPresentModesCreateInfoEXT'::@pPresentModes@ in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentModeEXT'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityY-07774#
--     @presentGravityY@ /must/ be a valid y-axis present gravity for the
--     surface as returned in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentScalingCapabilitiesEXT'::@supportedPresentGravityY@,
--     given
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@presentMode@
--     in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentModeEXT'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityY-07775#
--     If the swapchain is created with
--     'SwapchainPresentModesCreateInfoEXT', @presentGravityY@ /must/ be a
--     valid y-axis present gravity for the surface as returned in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentScalingCapabilitiesEXT'::@supportedPresentGravityY@,
--     given each present mode in
--     'SwapchainPresentModesCreateInfoEXT'::@pPresentModes@ in
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.SurfacePresentModeEXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT'
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-scalingBehavior-parameter#
--     @scalingBehavior@ /must/ be a valid combination of
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.PresentScalingFlagBitsEXT'
--     values
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityX-parameter#
--     @presentGravityX@ /must/ be a valid combination of
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.PresentGravityFlagBitsEXT'
--     values
--
-- -   #VUID-VkSwapchainPresentScalingCreateInfoEXT-presentGravityY-parameter#
--     @presentGravityY@ /must/ be a valid combination of
--     'Vulkan.Extensions.VK_EXT_surface_maintenance1.PresentGravityFlagBitsEXT'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- 'Vulkan.Extensions.VK_EXT_surface_maintenance1.PresentGravityFlagsEXT',
-- 'Vulkan.Extensions.VK_EXT_surface_maintenance1.PresentScalingFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainPresentScalingCreateInfoEXT = SwapchainPresentScalingCreateInfoEXT
  { -- | @scalingBehavior@ is @0@ or the scaling method to use when the
    -- dimensions of the surface and swapchain images differ.
    scalingBehavior :: PresentScalingFlagsEXT
  , -- | @presentGravityX@ is @0@ or the x-axis direction in which swapchain
    -- image pixels gravitate relative to the surface when @scalingBehavior@
    -- does not result in a one-to-one pixel mapping between the scaled
    -- swapchain image and the surface.
    presentGravityX :: PresentGravityFlagsEXT
  , -- | @presentGravityY@ is @0@ or the y-axis direction in which swapchain
    -- image pixels gravitate relative to the surface when @scalingBehavior@
    -- does not result in a one-to-one pixel mapping between the scaled
    -- swapchain image and the surface.
    presentGravityY :: PresentGravityFlagsEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainPresentScalingCreateInfoEXT)
#endif
deriving instance Show SwapchainPresentScalingCreateInfoEXT

instance ToCStruct SwapchainPresentScalingCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainPresentScalingCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PresentScalingFlagsEXT)) (scalingBehavior)
    poke ((p `plusPtr` 20 :: Ptr PresentGravityFlagsEXT)) (presentGravityX)
    poke ((p `plusPtr` 24 :: Ptr PresentGravityFlagsEXT)) (presentGravityY)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainPresentScalingCreateInfoEXT where
  peekCStruct p = do
    scalingBehavior <- peek @PresentScalingFlagsEXT ((p `plusPtr` 16 :: Ptr PresentScalingFlagsEXT))
    presentGravityX <- peek @PresentGravityFlagsEXT ((p `plusPtr` 20 :: Ptr PresentGravityFlagsEXT))
    presentGravityY <- peek @PresentGravityFlagsEXT ((p `plusPtr` 24 :: Ptr PresentGravityFlagsEXT))
    pure $ SwapchainPresentScalingCreateInfoEXT
             scalingBehavior presentGravityX presentGravityY

instance Storable SwapchainPresentScalingCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainPresentScalingCreateInfoEXT where
  zero = SwapchainPresentScalingCreateInfoEXT
           zero
           zero
           zero


-- | VkReleaseSwapchainImagesInfoEXT - Structure describing a list of
-- swapchain image indices to be released
--
-- == Valid Usage
--
-- -   #VUID-VkReleaseSwapchainImagesInfoEXT-pImageIndices-07785# Each
--     element of @pImageIndices@ /must/ be the index of a presentable
--     image acquired from the swapchain specified by @swapchain@
--
-- -   #VUID-VkReleaseSwapchainImagesInfoEXT-pImageIndices-07786# All uses
--     of presentable images identified by elements of @pImageIndices@
--     /must/ have completed execution
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkReleaseSwapchainImagesInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT'
--
-- -   #VUID-VkReleaseSwapchainImagesInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkReleaseSwapchainImagesInfoEXT-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-VkReleaseSwapchainImagesInfoEXT-pImageIndices-parameter#
--     @pImageIndices@ /must/ be a valid pointer to an array of
--     @imageIndexCount@ @uint32_t@ values
--
-- -   #VUID-VkReleaseSwapchainImagesInfoEXT-imageIndexCount-arraylength#
--     @imageIndexCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 VK_EXT_swapchain_maintenance1>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR', 'releaseSwapchainImagesEXT'
data ReleaseSwapchainImagesInfoEXT = ReleaseSwapchainImagesInfoEXT
  { -- | @swapchain@ is a swapchain to which images are being released.
    swapchain :: SwapchainKHR
  , -- | @pImageIndices@ is a pointer to an array of indices into the array of
    -- @swapchain@’s presentable images, with @imageIndexCount@ entries.
    imageIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ReleaseSwapchainImagesInfoEXT)
#endif
deriving instance Show ReleaseSwapchainImagesInfoEXT

instance ToCStruct ReleaseSwapchainImagesInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ReleaseSwapchainImagesInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (zero)
    f

instance FromCStruct ReleaseSwapchainImagesInfoEXT where
  peekCStruct p = do
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 16 :: Ptr SwapchainKHR))
    imageIndexCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pImageIndices <- peek @(Ptr Word32) ((p `plusPtr` 32 :: Ptr (Ptr Word32)))
    pImageIndices' <- generateM (fromIntegral imageIndexCount) (\i -> peek @Word32 ((pImageIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ ReleaseSwapchainImagesInfoEXT
             swapchain pImageIndices'

instance Zero ReleaseSwapchainImagesInfoEXT where
  zero = ReleaseSwapchainImagesInfoEXT
           zero
           mempty


type EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION"
pattern EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION = 1


type EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME = "VK_EXT_swapchain_maintenance1"

-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME"
pattern EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME = "VK_EXT_swapchain_maintenance1"

