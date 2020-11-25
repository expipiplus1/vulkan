{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_wayland_surface"
module Vulkan.Extensions.VK_KHR_wayland_surface  ( createWaylandSurfaceKHR
                                                 , getPhysicalDeviceWaylandPresentationSupportKHR
                                                 , WaylandSurfaceCreateInfoKHR(..)
                                                 , WaylandSurfaceCreateFlagsKHR(..)
                                                 , KHR_WAYLAND_SURFACE_SPEC_VERSION
                                                 , pattern KHR_WAYLAND_SURFACE_SPEC_VERSION
                                                 , KHR_WAYLAND_SURFACE_EXTENSION_NAME
                                                 , pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME
                                                 , Wl_display
                                                 , Wl_surface
                                                 , SurfaceKHR(..)
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
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateWaylandSurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceWaylandPresentationSupportKHR))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateWaylandSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr WaylandSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr WaylandSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateWaylandSurfaceKHR"
createWaylandSurfaceKHR :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCreateWaylandSurfaceKHR" "instance"
                           Instance
                        -> -- No documentation found for Nested "vkCreateWaylandSurfaceKHR" "pCreateInfo"
                           WaylandSurfaceCreateInfoKHR
                        -> -- No documentation found for Nested "vkCreateWaylandSurfaceKHR" "pAllocator"
                           ("allocator" ::: Maybe AllocationCallbacks)
                        -> io (SurfaceKHR)
createWaylandSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateWaylandSurfaceKHRPtr = pVkCreateWaylandSurfaceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateWaylandSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateWaylandSurfaceKHR is null" Nothing Nothing
  let vkCreateWaylandSurfaceKHR' = mkVkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateWaylandSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceWaylandPresentationSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Wl_display -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Wl_display -> IO Bool32

-- No documentation found for TopLevel "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
getPhysicalDeviceWaylandPresentationSupportKHR :: forall io
                                                . (MonadIO io)
                                               => -- No documentation found for Nested "vkGetPhysicalDeviceWaylandPresentationSupportKHR" "physicalDevice"
                                                  PhysicalDevice
                                               -> -- No documentation found for Nested "vkGetPhysicalDeviceWaylandPresentationSupportKHR" "queueFamilyIndex"
                                                  ("queueFamilyIndex" ::: Word32)
                                               -> -- No documentation found for Nested "vkGetPhysicalDeviceWaylandPresentationSupportKHR" "display"
                                                  (Ptr Wl_display)
                                               -> io (Bool)
getPhysicalDeviceWaylandPresentationSupportKHR physicalDevice queueFamilyIndex display = liftIO $ do
  let vkGetPhysicalDeviceWaylandPresentationSupportKHRPtr = pVkGetPhysicalDeviceWaylandPresentationSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceWaylandPresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceWaylandPresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceWaylandPresentationSupportKHR' = mkVkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHRPtr
  r <- vkGetPhysicalDeviceWaylandPresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (display)
  pure $ ((bool32ToBool r))



-- No documentation found for TopLevel "VkWaylandSurfaceCreateInfoKHR"
data WaylandSurfaceCreateInfoKHR = WaylandSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkWaylandSurfaceCreateInfoKHR" "flags"
    flags :: WaylandSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkWaylandSurfaceCreateInfoKHR" "display"
    display :: Ptr Wl_display
  , -- No documentation found for Nested "VkWaylandSurfaceCreateInfoKHR" "surface"
    surface :: Ptr Wl_surface
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WaylandSurfaceCreateInfoKHR)
#endif
deriving instance Show WaylandSurfaceCreateInfoKHR

instance ToCStruct WaylandSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WaylandSurfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr WaylandSurfaceCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Wl_display))) (display)
    poke ((p `plusPtr` 32 :: Ptr (Ptr Wl_surface))) (surface)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Wl_display))) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr Wl_surface))) (zero)
    f

instance FromCStruct WaylandSurfaceCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @WaylandSurfaceCreateFlagsKHR ((p `plusPtr` 16 :: Ptr WaylandSurfaceCreateFlagsKHR))
    display <- peek @(Ptr Wl_display) ((p `plusPtr` 24 :: Ptr (Ptr Wl_display)))
    surface <- peek @(Ptr Wl_surface) ((p `plusPtr` 32 :: Ptr (Ptr Wl_surface)))
    pure $ WaylandSurfaceCreateInfoKHR
             flags display surface


instance Storable WaylandSurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero WaylandSurfaceCreateInfoKHR where
  zero = WaylandSurfaceCreateInfoKHR
           zero
           zero
           zero


-- No documentation found for TopLevel "VkWaylandSurfaceCreateFlagsKHR"
newtype WaylandSurfaceCreateFlagsKHR = WaylandSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameWaylandSurfaceCreateFlagsKHR :: String
conNameWaylandSurfaceCreateFlagsKHR = "WaylandSurfaceCreateFlagsKHR"

enumPrefixWaylandSurfaceCreateFlagsKHR :: String
enumPrefixWaylandSurfaceCreateFlagsKHR = ""

showTableWaylandSurfaceCreateFlagsKHR :: [(WaylandSurfaceCreateFlagsKHR, String)]
showTableWaylandSurfaceCreateFlagsKHR = []


instance Show WaylandSurfaceCreateFlagsKHR where
showsPrec = enumShowsPrec enumPrefixWaylandSurfaceCreateFlagsKHR
                          showTableWaylandSurfaceCreateFlagsKHR
                          conNameWaylandSurfaceCreateFlagsKHR
                          (\(WaylandSurfaceCreateFlagsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read WaylandSurfaceCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixWaylandSurfaceCreateFlagsKHR
                          showTableWaylandSurfaceCreateFlagsKHR
                          conNameWaylandSurfaceCreateFlagsKHR
                          WaylandSurfaceCreateFlagsKHR


type KHR_WAYLAND_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_SPEC_VERSION"
pattern KHR_WAYLAND_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_WAYLAND_SURFACE_SPEC_VERSION = 6


type KHR_WAYLAND_SURFACE_EXTENSION_NAME = "VK_KHR_wayland_surface"

-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME"
pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME = "VK_KHR_wayland_surface"


data Wl_display


data Wl_surface

