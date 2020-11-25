{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_full_screen_exclusive"
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

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showsPrec)
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
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
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

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfacePresentModes2EXT"
getPhysicalDeviceSurfacePresentModes2EXT :: forall a io
                                          . (Extendss PhysicalDeviceSurfaceInfo2KHR a, PokeChain a, MonadIO io)
                                         => -- No documentation found for Nested "vkGetPhysicalDeviceSurfacePresentModes2EXT" "physicalDevice"
                                            PhysicalDevice
                                         -> -- No documentation found for Nested "vkGetPhysicalDeviceSurfacePresentModes2EXT" "pSurfaceInfo"
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

-- No documentation found for TopLevel "vkGetDeviceGroupSurfacePresentModes2EXT"
getDeviceGroupSurfacePresentModes2EXT :: forall a io
                                       . (Extendss PhysicalDeviceSurfaceInfo2KHR a, PokeChain a, MonadIO io)
                                      => -- No documentation found for Nested "vkGetDeviceGroupSurfacePresentModes2EXT" "device"
                                         Device
                                      -> -- No documentation found for Nested "vkGetDeviceGroupSurfacePresentModes2EXT" "pSurfaceInfo"
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

-- No documentation found for TopLevel "vkAcquireFullScreenExclusiveModeEXT"
acquireFullScreenExclusiveModeEXT :: forall io
                                   . (MonadIO io)
                                  => -- No documentation found for Nested "vkAcquireFullScreenExclusiveModeEXT" "device"
                                     Device
                                  -> -- No documentation found for Nested "vkAcquireFullScreenExclusiveModeEXT" "swapchain"
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

-- No documentation found for TopLevel "vkReleaseFullScreenExclusiveModeEXT"
releaseFullScreenExclusiveModeEXT :: forall io
                                   . (MonadIO io)
                                  => -- No documentation found for Nested "vkReleaseFullScreenExclusiveModeEXT" "device"
                                     Device
                                  -> -- No documentation found for Nested "vkReleaseFullScreenExclusiveModeEXT" "swapchain"
                                     SwapchainKHR
                                  -> io ()
releaseFullScreenExclusiveModeEXT device swapchain = liftIO $ do
  let vkReleaseFullScreenExclusiveModeEXTPtr = pVkReleaseFullScreenExclusiveModeEXT (deviceCmds (device :: Device))
  unless (vkReleaseFullScreenExclusiveModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseFullScreenExclusiveModeEXT is null" Nothing Nothing
  let vkReleaseFullScreenExclusiveModeEXT' = mkVkReleaseFullScreenExclusiveModeEXT vkReleaseFullScreenExclusiveModeEXTPtr
  r <- vkReleaseFullScreenExclusiveModeEXT' (deviceHandle (device)) (swapchain)
  when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkSurfaceFullScreenExclusiveInfoEXT"
data SurfaceFullScreenExclusiveInfoEXT = SurfaceFullScreenExclusiveInfoEXT
  { -- No documentation found for Nested "VkSurfaceFullScreenExclusiveInfoEXT" "fullScreenExclusive"
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



-- No documentation found for TopLevel "VkSurfaceFullScreenExclusiveWin32InfoEXT"
data SurfaceFullScreenExclusiveWin32InfoEXT = SurfaceFullScreenExclusiveWin32InfoEXT
  { -- No documentation found for Nested "VkSurfaceFullScreenExclusiveWin32InfoEXT" "hmonitor"
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



-- No documentation found for TopLevel "VkSurfaceCapabilitiesFullScreenExclusiveEXT"
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


-- No documentation found for TopLevel "VkFullScreenExclusiveEXT"
newtype FullScreenExclusiveEXT = FullScreenExclusiveEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFullScreenExclusiveEXT" "VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT"
pattern FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT                = FullScreenExclusiveEXT 0
-- No documentation found for Nested "VkFullScreenExclusiveEXT" "VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT"
pattern FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT                = FullScreenExclusiveEXT 1
-- No documentation found for Nested "VkFullScreenExclusiveEXT" "VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT"
pattern FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT             = FullScreenExclusiveEXT 2
-- No documentation found for Nested "VkFullScreenExclusiveEXT" "VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT"
pattern FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = FullScreenExclusiveEXT 3
{-# complete FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT,
             FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT,
             FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT,
             FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT :: FullScreenExclusiveEXT #-}

conNameFullScreenExclusiveEXT :: String
conNameFullScreenExclusiveEXT = "FullScreenExclusiveEXT"

enumPrefixFullScreenExclusiveEXT :: String
enumPrefixFullScreenExclusiveEXT = "FULL_SCREEN_EXCLUSIVE_"

showTableFullScreenExclusiveEXT :: [(FullScreenExclusiveEXT, String)]
showTableFullScreenExclusiveEXT =
  [ (FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT               , "DEFAULT_EXT")
  , (FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT               , "ALLOWED_EXT")
  , (FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT            , "DISALLOWED_EXT")
  , (FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT, "APPLICATION_CONTROLLED_EXT")
  ]


instance Show FullScreenExclusiveEXT where
showsPrec = enumShowsPrec enumPrefixFullScreenExclusiveEXT
                          showTableFullScreenExclusiveEXT
                          conNameFullScreenExclusiveEXT
                          (\(FullScreenExclusiveEXT x) -> x)
                          (showsPrec 11)


instance Read FullScreenExclusiveEXT where
  readPrec = enumReadPrec enumPrefixFullScreenExclusiveEXT
                          showTableFullScreenExclusiveEXT
                          conNameFullScreenExclusiveEXT
                          FullScreenExclusiveEXT


type EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION"
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 4


type EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = "VK_EXT_full_screen_exclusive"

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME"
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = "VK_EXT_full_screen_exclusive"


type HMONITOR = Ptr ()

