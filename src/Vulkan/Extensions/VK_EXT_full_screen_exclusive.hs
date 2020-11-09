{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_full_screen_exclusive  ( getPhysicalDeviceSurfacePresentModes2EXT
                                                       , getDeviceGroupSurfacePresentModes2EXT
                                                       , acquireFullScreenExclusiveModeEXT
                                                       , releaseFullScreenExclusiveModeEXT
                                                       , SurfaceFullScreenExclusiveInfoEXT(..)
                                                       , SurfaceFullScreenExclusiveWin32InfoEXT(..)
                                                       , SurfaceCapabilitiesFullScreenExclusiveEXT(..)
                                                       , FullScreenExclusiveEXT( FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT
                                                                               , FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT
                                                                               , FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT
                                                                               , FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT
                                                                               , ..
                                                                               )
                                                       , EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
                                                       , pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
                                                       , EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
                                                       , pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
                                                       , HMONITOR
                                                       , SurfaceKHR(..)
                                                       , SwapchainKHR(..)
                                                       , PhysicalDeviceSurfaceInfo2KHR(..)
                                                       , PresentModeKHR(..)
                                                       , DeviceGroupPresentModeFlagBitsKHR(..)
                                                       , DeviceGroupPresentModeFlagsKHR
                                                       ) where

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
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAcquireFullScreenExclusiveModeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceGroupSurfacePresentModes2EXT))
import Vulkan.Dynamic (DeviceCmds(pVkReleaseFullScreenExclusiveModeEXT))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagsKHR)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfacePresentModes2EXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (PhysicalDeviceSurfaceInfo2KHR)
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagsKHR)
import Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (PhysicalDeviceSurfaceInfo2KHR(..))
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModes2EXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result

-- | vkGetPhysicalDeviceSurfacePresentModes2EXT - Query supported
-- presentation modes
--
-- = Description
--
-- 'getPhysicalDeviceSurfacePresentModes2EXT' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR',
-- with the ability to specify extended inputs via chained input
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModes2EXT-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModes2EXT-pSurfaceInfo-parameter#
--     @pSurfaceInfo@ /must/ be a valid pointer to a valid
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
--     structure
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModes2EXT-pPresentModeCount-parameter#
--     @pPresentModeCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModes2EXT-pPresentModes-parameter#
--     If the value referenced by @pPresentModeCount@ is not @0@, and
--     @pPresentModes@ is not @NULL@, @pPresentModes@ /must/ be a valid
--     pointer to an array of @pPresentModeCount@
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR'
getPhysicalDeviceSurfacePresentModes2EXT :: forall a io
                                          . (Extendss PhysicalDeviceSurfaceInfo2KHR a, PokeChain a, MonadIO io)
                                         => -- | @physicalDevice@ is the physical device that will be associated with the
                                            -- swapchain to be created, as described for
                                            -- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                            PhysicalDevice
                                         -> -- | @pSurfaceInfo@ is a pointer to a
                                            -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
                                            -- structure describing the surface and other fixed parameters that would
                                            -- be consumed by 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                            (PhysicalDeviceSurfaceInfo2KHR a)
                                         -> io (Result, ("presentModes" ::: Vector PresentModeKHR))
getPhysicalDeviceSurfacePresentModes2EXT physicalDevice surfaceInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfacePresentModes2EXTPtr = pVkGetPhysicalDeviceSurfacePresentModes2EXT (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfacePresentModes2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfacePresentModes2EXT is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfacePresentModes2EXT' = mkVkGetPhysicalDeviceSurfacePresentModes2EXT vkGetPhysicalDeviceSurfacePresentModes2EXTPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pSurfaceInfo <- ContT $ withCStruct (surfaceInfo)
  let x9 = forgetExtensions pSurfaceInfo
  pPPresentModeCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceSurfacePresentModes2EXT' physicalDevice' x9 (pPPresentModeCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPresentModeCount <- lift $ peek @Word32 pPPresentModeCount
  pPPresentModes <- ContT $ bracket (callocBytes @PresentModeKHR ((fromIntegral (pPresentModeCount)) * 4)) free
  r' <- lift $ vkGetPhysicalDeviceSurfacePresentModes2EXT' physicalDevice' x9 (pPPresentModeCount) (pPPresentModes)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPresentModeCount' <- lift $ peek @Word32 pPPresentModeCount
  pPresentModes' <- lift $ generateM (fromIntegral (pPresentModeCount')) (\i -> peek @PresentModeKHR ((pPPresentModes `advancePtrBytes` (4 * (i)) :: Ptr PresentModeKHR)))
  pure $ ((r'), pPresentModes')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupSurfacePresentModes2EXT
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr DeviceGroupPresentModeFlagsKHR -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr DeviceGroupPresentModeFlagsKHR -> IO Result

-- | vkGetDeviceGroupSurfacePresentModes2EXT - Query device group present
-- capabilities for a surface
--
-- = Description
--
-- 'getDeviceGroupSurfacePresentModes2EXT' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupSurfacePresentModesKHR',
-- with the ability to specify extended inputs via chained input
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceGroupSurfacePresentModes2EXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceGroupSurfacePresentModes2EXT-pSurfaceInfo-parameter#
--     @pSurfaceInfo@ /must/ be a valid pointer to a valid
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
--     structure
--
-- -   #VUID-vkGetDeviceGroupSurfacePresentModes2EXT-pModes-parameter#
--     @pModes@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentModeFlagsKHR'
--     value
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
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentModeFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
getDeviceGroupSurfacePresentModes2EXT :: forall a io
                                       . (Extendss PhysicalDeviceSurfaceInfo2KHR a, PokeChain a, MonadIO io)
                                      => -- | @device@ is the logical device.
                                         Device
                                      -> -- | @pSurfaceInfo@ is a pointer to a
                                         -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
                                         -- structure describing the surface and other fixed parameters that would
                                         -- be consumed by 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                         (PhysicalDeviceSurfaceInfo2KHR a)
                                      -> io (("modes" ::: DeviceGroupPresentModeFlagsKHR))
getDeviceGroupSurfacePresentModes2EXT device surfaceInfo = liftIO . evalContT $ do
  let vkGetDeviceGroupSurfacePresentModes2EXTPtr = pVkGetDeviceGroupSurfacePresentModes2EXT (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceGroupSurfacePresentModes2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceGroupSurfacePresentModes2EXT is null" Nothing Nothing
  let vkGetDeviceGroupSurfacePresentModes2EXT' = mkVkGetDeviceGroupSurfacePresentModes2EXT vkGetDeviceGroupSurfacePresentModes2EXTPtr
  pSurfaceInfo <- ContT $ withCStruct (surfaceInfo)
  pPModes <- ContT $ bracket (callocBytes @DeviceGroupPresentModeFlagsKHR 4) free
  r <- lift $ vkGetDeviceGroupSurfacePresentModes2EXT' (deviceHandle (device)) (forgetExtensions pSurfaceInfo) (pPModes)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pModes <- lift $ peek @DeviceGroupPresentModeFlagsKHR pPModes
  pure $ (pModes)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireFullScreenExclusiveModeEXT
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> IO Result) -> Ptr Device_T -> SwapchainKHR -> IO Result

-- | vkAcquireFullScreenExclusiveModeEXT - Acquire full-screen exclusive mode
-- for a swapchain
--
-- == Valid Usage
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-swapchain-02674#
--     @swapchain@ /must/ not be in the retired state
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-swapchain-02675#
--     @swapchain@ /must/ be a swapchain created with a
--     'SurfaceFullScreenExclusiveInfoEXT' structure, with
--     @fullScreenExclusive@ set to
--     'FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-swapchain-02676#
--     @swapchain@ /must/ not currently have exclusive full-screen access
--
-- A return value of 'Vulkan.Core10.Enums.Result.SUCCESS' indicates that
-- the @swapchain@ successfully acquired exclusive full-screen access. The
-- swapchain will retain this exclusivity until either the application
-- releases exclusive full-screen access with
-- 'releaseFullScreenExclusiveModeEXT', destroys the swapchain, or if any
-- of the swapchain commands return
-- 'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
-- indicating that the mode was lost because of platform-specific changes.
--
-- If the swapchain was unable to acquire exclusive full-screen access to
-- the display then
-- 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED' is returned. An
-- application /can/ attempt to acquire exclusive full-screen access again
-- for the same swapchain even if this command fails, or if
-- 'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
-- has been returned by a swapchain command.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-commonparent# Both of
--     @device@, and @swapchain@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Instance'
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.SwapchainKHR'
acquireFullScreenExclusiveModeEXT :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the device associated with @swapchain@.
                                     Device
                                  -> -- | @swapchain@ is the swapchain to acquire exclusive full-screen access
                                     -- for.
                                     SwapchainKHR
                                  -> io ()
acquireFullScreenExclusiveModeEXT device swapchain = liftIO $ do
  let vkAcquireFullScreenExclusiveModeEXTPtr = pVkAcquireFullScreenExclusiveModeEXT (deviceCmds (device :: Device))
  unless (vkAcquireFullScreenExclusiveModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireFullScreenExclusiveModeEXT is null" Nothing Nothing
  let vkAcquireFullScreenExclusiveModeEXT' = mkVkAcquireFullScreenExclusiveModeEXT vkAcquireFullScreenExclusiveModeEXTPtr
  r <- vkAcquireFullScreenExclusiveModeEXT' (deviceHandle (device)) (swapchain)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseFullScreenExclusiveModeEXT
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> IO Result) -> Ptr Device_T -> SwapchainKHR -> IO Result

-- | vkReleaseFullScreenExclusiveModeEXT - Release full-screen exclusive mode
-- from a swapchain
--
-- = Description
--
-- Note
--
-- Applications will not be able to present to @swapchain@ after this call
-- until exclusive full-screen access is reacquired. This is usually useful
-- to handle when an application is minimised or otherwise intends to stop
-- presenting for a time.
--
-- == Valid Usage
--
-- -   #VUID-vkReleaseFullScreenExclusiveModeEXT-swapchain-02677#
--     @swapchain@ /must/ not be in the retired state
--
-- -   #VUID-vkReleaseFullScreenExclusiveModeEXT-swapchain-02678#
--     @swapchain@ /must/ be a swapchain created with a
--     'SurfaceFullScreenExclusiveInfoEXT' structure, with
--     @fullScreenExclusive@ set to
--     'FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.SwapchainKHR'
releaseFullScreenExclusiveModeEXT :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the device associated with @swapchain@.
                                     Device
                                  -> -- | @swapchain@ is the swapchain to release exclusive full-screen access
                                     -- from.
                                     SwapchainKHR
                                  -> io ()
releaseFullScreenExclusiveModeEXT device swapchain = liftIO $ do
  let vkReleaseFullScreenExclusiveModeEXTPtr = pVkReleaseFullScreenExclusiveModeEXT (deviceCmds (device :: Device))
  unless (vkReleaseFullScreenExclusiveModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseFullScreenExclusiveModeEXT is null" Nothing Nothing
  let vkReleaseFullScreenExclusiveModeEXT' = mkVkReleaseFullScreenExclusiveModeEXT vkReleaseFullScreenExclusiveModeEXTPtr
  r <- vkReleaseFullScreenExclusiveModeEXT' (deviceHandle (device)) (swapchain)
  when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkSurfaceFullScreenExclusiveInfoEXT - Structure specifying the preferred
-- full-screen transition behavior
--
-- = Description
--
-- If this structure is not present, @fullScreenExclusive@ is considered to
-- be 'FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSurfaceFullScreenExclusiveInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT'
--
-- -   #VUID-VkSurfaceFullScreenExclusiveInfoEXT-fullScreenExclusive-parameter#
--     @fullScreenExclusive@ /must/ be a valid 'FullScreenExclusiveEXT'
--     value
--
-- = See Also
--
-- 'FullScreenExclusiveEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceFullScreenExclusiveInfoEXT = SurfaceFullScreenExclusiveInfoEXT
  { -- | @fullScreenExclusive@ is a 'FullScreenExclusiveEXT' value specifying the
    -- preferred full-screen transition behavior.
    fullScreenExclusive :: FullScreenExclusiveEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceFullScreenExclusiveInfoEXT)
#endif
deriving instance Show SurfaceFullScreenExclusiveInfoEXT

instance ToCStruct SurfaceFullScreenExclusiveInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceFullScreenExclusiveInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FullScreenExclusiveEXT)) (fullScreenExclusive)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FullScreenExclusiveEXT)) (zero)
    f

instance FromCStruct SurfaceFullScreenExclusiveInfoEXT where
  peekCStruct p = do
    fullScreenExclusive <- peek @FullScreenExclusiveEXT ((p `plusPtr` 16 :: Ptr FullScreenExclusiveEXT))
    pure $ SurfaceFullScreenExclusiveInfoEXT
             fullScreenExclusive

instance Storable SurfaceFullScreenExclusiveInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceFullScreenExclusiveInfoEXT where
  zero = SurfaceFullScreenExclusiveInfoEXT
           zero


-- | VkSurfaceFullScreenExclusiveWin32InfoEXT - Structure specifying
-- additional creation parameters specific to Win32 fullscreen exclusive
-- mode
--
-- = Description
--
-- Note
--
-- If @hmonitor@ is invalidated (e.g. the monitor is unplugged) during the
-- lifetime of a swapchain created with this structure, operations on that
-- swapchain will return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'.
--
-- Note
--
-- It is the responsibility of the application to change the display
-- settings of the targeted Win32 display using the appropriate platform
-- APIs. Such changes /may/ alter the surface capabilities reported for the
-- created surface.
--
-- == Valid Usage
--
-- -   #VUID-VkSurfaceFullScreenExclusiveWin32InfoEXT-hmonitor-02673#
--     @hmonitor@ /must/ be a valid 'HMONITOR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSurfaceFullScreenExclusiveWin32InfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceFullScreenExclusiveWin32InfoEXT = SurfaceFullScreenExclusiveWin32InfoEXT
  { -- | @hmonitor@ is the Win32 'HMONITOR' handle identifying the display to
    -- create the surface with.
    hmonitor :: HMONITOR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceFullScreenExclusiveWin32InfoEXT)
#endif
deriving instance Show SurfaceFullScreenExclusiveWin32InfoEXT

instance ToCStruct SurfaceFullScreenExclusiveWin32InfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceFullScreenExclusiveWin32InfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HMONITOR)) (hmonitor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HMONITOR)) (zero)
    f

instance FromCStruct SurfaceFullScreenExclusiveWin32InfoEXT where
  peekCStruct p = do
    hmonitor <- peek @HMONITOR ((p `plusPtr` 16 :: Ptr HMONITOR))
    pure $ SurfaceFullScreenExclusiveWin32InfoEXT
             hmonitor

instance Storable SurfaceFullScreenExclusiveWin32InfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceFullScreenExclusiveWin32InfoEXT where
  zero = SurfaceFullScreenExclusiveWin32InfoEXT
           zero


-- | VkSurfaceCapabilitiesFullScreenExclusiveEXT - Structure describing full
-- screen exclusive capabilities of a surface
--
-- = Description
--
-- This structure /can/ be included in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR'
-- to determine support for exclusive full-screen access. If
-- @fullScreenExclusiveSupported@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE', it indicates that exclusive
-- full-screen access is not obtainable for this surface.
--
-- Applications /must/ not attempt to create swapchains with
-- 'FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT' set if
-- @fullScreenExclusiveSupported@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSurfaceCapabilitiesFullScreenExclusiveEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceCapabilitiesFullScreenExclusiveEXT = SurfaceCapabilitiesFullScreenExclusiveEXT
  { -- No documentation found for Nested "VkSurfaceCapabilitiesFullScreenExclusiveEXT" "fullScreenExclusiveSupported"
    fullScreenExclusiveSupported :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilitiesFullScreenExclusiveEXT)
#endif
deriving instance Show SurfaceCapabilitiesFullScreenExclusiveEXT

instance ToCStruct SurfaceCapabilitiesFullScreenExclusiveEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilitiesFullScreenExclusiveEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fullScreenExclusiveSupported))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SurfaceCapabilitiesFullScreenExclusiveEXT where
  peekCStruct p = do
    fullScreenExclusiveSupported <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SurfaceCapabilitiesFullScreenExclusiveEXT
             (bool32ToBool fullScreenExclusiveSupported)

instance Storable SurfaceCapabilitiesFullScreenExclusiveEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceCapabilitiesFullScreenExclusiveEXT where
  zero = SurfaceCapabilitiesFullScreenExclusiveEXT
           zero


-- | VkFullScreenExclusiveEXT - Hint values an application can specify
-- affecting full-screen transition behavior
--
-- = See Also
--
-- 'SurfaceFullScreenExclusiveInfoEXT'
newtype FullScreenExclusiveEXT = FullScreenExclusiveEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT' indicates the implementation
-- /should/ determine the appropriate full-screen method by whatever means
-- it deems appropriate.
pattern FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT = FullScreenExclusiveEXT 0
-- | 'FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT' indicates the implementation /may/
-- use full-screen exclusive mechanisms when available. Such mechanisms
-- /may/ result in better performance and\/or the availability of different
-- presentation capabilities, but /may/ require a more disruptive
-- transition during swapchain initialization, first presentation and\/or
-- destruction.
pattern FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT = FullScreenExclusiveEXT 1
-- | 'FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT' indicates the implementation
-- /should/ avoid using full-screen mechanisms which rely on disruptive
-- transitions.
pattern FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT = FullScreenExclusiveEXT 2
-- | 'FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT' indicates the
-- application will manage full-screen exclusive mode by using the
-- 'acquireFullScreenExclusiveModeEXT' and
-- 'releaseFullScreenExclusiveModeEXT' commands.
pattern FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = FullScreenExclusiveEXT 3
{-# complete FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT,
             FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT,
             FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT,
             FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT :: FullScreenExclusiveEXT #-}

instance Show FullScreenExclusiveEXT where
  showsPrec p = \case
    FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT -> showString "FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT"
    FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT -> showString "FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT"
    FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT -> showString "FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT"
    FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT -> showString "FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT"
    FullScreenExclusiveEXT x -> showParen (p >= 11) (showString "FullScreenExclusiveEXT " . showsPrec 11 x)

instance Read FullScreenExclusiveEXT where
  readPrec = parens (choose [("FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT", pure FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT)
                            , ("FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT", pure FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT)
                            , ("FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT", pure FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT)
                            , ("FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT", pure FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "FullScreenExclusiveEXT")
                       v <- step readPrec
                       pure (FullScreenExclusiveEXT v)))


type EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION"
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 4


type EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = "VK_EXT_full_screen_exclusive"

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME"
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = "VK_EXT_full_screen_exclusive"


type HMONITOR = Ptr ()

