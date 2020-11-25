{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_xlib_surface"
module Vulkan.Extensions.VK_KHR_xlib_surface  ( createXlibSurfaceKHR
                                              , getPhysicalDeviceXlibPresentationSupportKHR
                                              , XlibSurfaceCreateInfoKHR(..)
                                              , XlibSurfaceCreateFlagsKHR(..)
                                              , KHR_XLIB_SURFACE_SPEC_VERSION
                                              , pattern KHR_XLIB_SURFACE_SPEC_VERSION
                                              , KHR_XLIB_SURFACE_EXTENSION_NAME
                                              , pattern KHR_XLIB_SURFACE_EXTENSION_NAME
                                              , Display
                                              , VisualID
                                              , Window
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
import Data.Word (Word64)
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateXlibSurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceXlibPresentationSupportKHR))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateXlibSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr XlibSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr XlibSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateXlibSurfaceKHR"
createXlibSurfaceKHR :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCreateXlibSurfaceKHR" "instance"
                        Instance
                     -> -- No documentation found for Nested "vkCreateXlibSurfaceKHR" "pCreateInfo"
                        XlibSurfaceCreateInfoKHR
                     -> -- No documentation found for Nested "vkCreateXlibSurfaceKHR" "pAllocator"
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io (SurfaceKHR)
createXlibSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateXlibSurfaceKHRPtr = pVkCreateXlibSurfaceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateXlibSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateXlibSurfaceKHR is null" Nothing Nothing
  let vkCreateXlibSurfaceKHR' = mkVkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateXlibSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceXlibPresentationSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Display -> VisualID -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Display -> VisualID -> IO Bool32

-- No documentation found for TopLevel "vkGetPhysicalDeviceXlibPresentationSupportKHR"
getPhysicalDeviceXlibPresentationSupportKHR :: forall io
                                             . (MonadIO io)
                                            => -- No documentation found for Nested "vkGetPhysicalDeviceXlibPresentationSupportKHR" "physicalDevice"
                                               PhysicalDevice
                                            -> -- No documentation found for Nested "vkGetPhysicalDeviceXlibPresentationSupportKHR" "queueFamilyIndex"
                                               ("queueFamilyIndex" ::: Word32)
                                            -> -- No documentation found for Nested "vkGetPhysicalDeviceXlibPresentationSupportKHR" "dpy"
                                               ("dpy" ::: Ptr Display)
                                            -> -- No documentation found for Nested "vkGetPhysicalDeviceXlibPresentationSupportKHR" "visualID"
                                               VisualID
                                            -> io (Bool)
getPhysicalDeviceXlibPresentationSupportKHR physicalDevice queueFamilyIndex dpy visualID = liftIO $ do
  let vkGetPhysicalDeviceXlibPresentationSupportKHRPtr = pVkGetPhysicalDeviceXlibPresentationSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceXlibPresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceXlibPresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceXlibPresentationSupportKHR' = mkVkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHRPtr
  r <- vkGetPhysicalDeviceXlibPresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (dpy) (visualID)
  pure $ ((bool32ToBool r))



-- No documentation found for TopLevel "VkXlibSurfaceCreateInfoKHR"
data XlibSurfaceCreateInfoKHR = XlibSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "flags"
    flags :: XlibSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "dpy"
    dpy :: Ptr Display
  , -- No documentation found for Nested "VkXlibSurfaceCreateInfoKHR" "window"
    window :: Window
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (XlibSurfaceCreateInfoKHR)
#endif
deriving instance Show XlibSurfaceCreateInfoKHR

instance ToCStruct XlibSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p XlibSurfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr XlibSurfaceCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Display))) (dpy)
    poke ((p `plusPtr` 32 :: Ptr Window)) (window)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Display))) (zero)
    poke ((p `plusPtr` 32 :: Ptr Window)) (zero)
    f

instance FromCStruct XlibSurfaceCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @XlibSurfaceCreateFlagsKHR ((p `plusPtr` 16 :: Ptr XlibSurfaceCreateFlagsKHR))
    dpy <- peek @(Ptr Display) ((p `plusPtr` 24 :: Ptr (Ptr Display)))
    window <- peek @Window ((p `plusPtr` 32 :: Ptr Window))
    pure $ XlibSurfaceCreateInfoKHR
             flags dpy window


instance Storable XlibSurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero XlibSurfaceCreateInfoKHR where
  zero = XlibSurfaceCreateInfoKHR
           zero
           zero
           zero


-- No documentation found for TopLevel "VkXlibSurfaceCreateFlagsKHR"
newtype XlibSurfaceCreateFlagsKHR = XlibSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameXlibSurfaceCreateFlagsKHR :: String
conNameXlibSurfaceCreateFlagsKHR = "XlibSurfaceCreateFlagsKHR"

enumPrefixXlibSurfaceCreateFlagsKHR :: String
enumPrefixXlibSurfaceCreateFlagsKHR = ""

showTableXlibSurfaceCreateFlagsKHR :: [(XlibSurfaceCreateFlagsKHR, String)]
showTableXlibSurfaceCreateFlagsKHR = []


instance Show XlibSurfaceCreateFlagsKHR where
showsPrec = enumShowsPrec enumPrefixXlibSurfaceCreateFlagsKHR
                          showTableXlibSurfaceCreateFlagsKHR
                          conNameXlibSurfaceCreateFlagsKHR
                          (\(XlibSurfaceCreateFlagsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read XlibSurfaceCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixXlibSurfaceCreateFlagsKHR
                          showTableXlibSurfaceCreateFlagsKHR
                          conNameXlibSurfaceCreateFlagsKHR
                          XlibSurfaceCreateFlagsKHR


type KHR_XLIB_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_SPEC_VERSION"
pattern KHR_XLIB_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_XLIB_SURFACE_SPEC_VERSION = 6


type KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"

-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_EXTENSION_NAME"
pattern KHR_XLIB_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"


type Display = Ptr ()


type VisualID = Word64


type Window = Word64

