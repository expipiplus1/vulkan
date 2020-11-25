{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_xcb_surface"
module Vulkan.Extensions.VK_KHR_xcb_surface  ( createXcbSurfaceKHR
                                             , getPhysicalDeviceXcbPresentationSupportKHR
                                             , XcbSurfaceCreateInfoKHR(..)
                                             , XcbSurfaceCreateFlagsKHR(..)
                                             , KHR_XCB_SURFACE_SPEC_VERSION
                                             , pattern KHR_XCB_SURFACE_SPEC_VERSION
                                             , KHR_XCB_SURFACE_EXTENSION_NAME
                                             , pattern KHR_XCB_SURFACE_EXTENSION_NAME
                                             , Xcb_visualid_t
                                             , Xcb_window_t
                                             , Xcb_connection_t
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateXcbSurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceXcbPresentationSupportKHR))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateXcbSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr XcbSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr XcbSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateXcbSurfaceKHR"
createXcbSurfaceKHR :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkCreateXcbSurfaceKHR" "instance"
                       Instance
                    -> -- No documentation found for Nested "vkCreateXcbSurfaceKHR" "pCreateInfo"
                       XcbSurfaceCreateInfoKHR
                    -> -- No documentation found for Nested "vkCreateXcbSurfaceKHR" "pAllocator"
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io (SurfaceKHR)
createXcbSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateXcbSurfaceKHRPtr = pVkCreateXcbSurfaceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateXcbSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateXcbSurfaceKHR is null" Nothing Nothing
  let vkCreateXcbSurfaceKHR' = mkVkCreateXcbSurfaceKHR vkCreateXcbSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateXcbSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceXcbPresentationSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Xcb_connection_t -> Xcb_visualid_t -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Xcb_connection_t -> Xcb_visualid_t -> IO Bool32

-- No documentation found for TopLevel "vkGetPhysicalDeviceXcbPresentationSupportKHR"
getPhysicalDeviceXcbPresentationSupportKHR :: forall io
                                            . (MonadIO io)
                                           => -- No documentation found for Nested "vkGetPhysicalDeviceXcbPresentationSupportKHR" "physicalDevice"
                                              PhysicalDevice
                                           -> -- No documentation found for Nested "vkGetPhysicalDeviceXcbPresentationSupportKHR" "queueFamilyIndex"
                                              ("queueFamilyIndex" ::: Word32)
                                           -> -- No documentation found for Nested "vkGetPhysicalDeviceXcbPresentationSupportKHR" "connection"
                                              (Ptr Xcb_connection_t)
                                           -> -- No documentation found for Nested "vkGetPhysicalDeviceXcbPresentationSupportKHR" "visual_id"
                                              ("visual_id" ::: Xcb_visualid_t)
                                           -> io (Bool)
getPhysicalDeviceXcbPresentationSupportKHR physicalDevice queueFamilyIndex connection visual_id = liftIO $ do
  let vkGetPhysicalDeviceXcbPresentationSupportKHRPtr = pVkGetPhysicalDeviceXcbPresentationSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceXcbPresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceXcbPresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceXcbPresentationSupportKHR' = mkVkGetPhysicalDeviceXcbPresentationSupportKHR vkGetPhysicalDeviceXcbPresentationSupportKHRPtr
  r <- vkGetPhysicalDeviceXcbPresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (connection) (visual_id)
  pure $ ((bool32ToBool r))



-- No documentation found for TopLevel "VkXcbSurfaceCreateInfoKHR"
data XcbSurfaceCreateInfoKHR = XcbSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkXcbSurfaceCreateInfoKHR" "flags"
    flags :: XcbSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkXcbSurfaceCreateInfoKHR" "connection"
    connection :: Ptr Xcb_connection_t
  , -- No documentation found for Nested "VkXcbSurfaceCreateInfoKHR" "window"
    window :: Xcb_window_t
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (XcbSurfaceCreateInfoKHR)
#endif
deriving instance Show XcbSurfaceCreateInfoKHR

instance ToCStruct XcbSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p XcbSurfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr XcbSurfaceCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Xcb_connection_t))) (connection)
    poke ((p `plusPtr` 32 :: Ptr Xcb_window_t)) (window)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Xcb_connection_t))) (zero)
    poke ((p `plusPtr` 32 :: Ptr Xcb_window_t)) (zero)
    f

instance FromCStruct XcbSurfaceCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @XcbSurfaceCreateFlagsKHR ((p `plusPtr` 16 :: Ptr XcbSurfaceCreateFlagsKHR))
    connection <- peek @(Ptr Xcb_connection_t) ((p `plusPtr` 24 :: Ptr (Ptr Xcb_connection_t)))
    window <- peek @Xcb_window_t ((p `plusPtr` 32 :: Ptr Xcb_window_t))
    pure $ XcbSurfaceCreateInfoKHR
             flags connection window


instance Storable XcbSurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero XcbSurfaceCreateInfoKHR where
  zero = XcbSurfaceCreateInfoKHR
           zero
           zero
           zero


-- No documentation found for TopLevel "VkXcbSurfaceCreateFlagsKHR"
newtype XcbSurfaceCreateFlagsKHR = XcbSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameXcbSurfaceCreateFlagsKHR :: String
conNameXcbSurfaceCreateFlagsKHR = "XcbSurfaceCreateFlagsKHR"

enumPrefixXcbSurfaceCreateFlagsKHR :: String
enumPrefixXcbSurfaceCreateFlagsKHR = ""

showTableXcbSurfaceCreateFlagsKHR :: [(XcbSurfaceCreateFlagsKHR, String)]
showTableXcbSurfaceCreateFlagsKHR = []


instance Show XcbSurfaceCreateFlagsKHR where
showsPrec = enumShowsPrec enumPrefixXcbSurfaceCreateFlagsKHR
                          showTableXcbSurfaceCreateFlagsKHR
                          conNameXcbSurfaceCreateFlagsKHR
                          (\(XcbSurfaceCreateFlagsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read XcbSurfaceCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixXcbSurfaceCreateFlagsKHR
                          showTableXcbSurfaceCreateFlagsKHR
                          conNameXcbSurfaceCreateFlagsKHR
                          XcbSurfaceCreateFlagsKHR


type KHR_XCB_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_SPEC_VERSION"
pattern KHR_XCB_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_XCB_SURFACE_SPEC_VERSION = 6


type KHR_XCB_SURFACE_EXTENSION_NAME = "VK_KHR_xcb_surface"

-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_EXTENSION_NAME"
pattern KHR_XCB_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_XCB_SURFACE_EXTENSION_NAME = "VK_KHR_xcb_surface"


type Xcb_visualid_t = Word32


type Xcb_window_t = Word32


data Xcb_connection_t

