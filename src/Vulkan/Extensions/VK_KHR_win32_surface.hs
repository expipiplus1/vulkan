{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_win32_surface"
module Vulkan.Extensions.VK_KHR_win32_surface  ( createWin32SurfaceKHR
                                               , getPhysicalDeviceWin32PresentationSupportKHR
                                               , Win32SurfaceCreateInfoKHR(..)
                                               , Win32SurfaceCreateFlagsKHR(..)
                                               , KHR_WIN32_SURFACE_SPEC_VERSION
                                               , pattern KHR_WIN32_SURFACE_SPEC_VERSION
                                               , KHR_WIN32_SURFACE_EXTENSION_NAME
                                               , pattern KHR_WIN32_SURFACE_EXTENSION_NAME
                                               , HINSTANCE
                                               , HWND
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateWin32SurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceWin32PresentationSupportKHR))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateWin32SurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr Win32SurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr Win32SurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateWin32SurfaceKHR"
createWin32SurfaceKHR :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkCreateWin32SurfaceKHR" "instance"
                         Instance
                      -> -- No documentation found for Nested "vkCreateWin32SurfaceKHR" "pCreateInfo"
                         Win32SurfaceCreateInfoKHR
                      -> -- No documentation found for Nested "vkCreateWin32SurfaceKHR" "pAllocator"
                         ("allocator" ::: Maybe AllocationCallbacks)
                      -> io (SurfaceKHR)
createWin32SurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateWin32SurfaceKHRPtr = pVkCreateWin32SurfaceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateWin32SurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateWin32SurfaceKHR is null" Nothing Nothing
  let vkCreateWin32SurfaceKHR' = mkVkCreateWin32SurfaceKHR vkCreateWin32SurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateWin32SurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceWin32PresentationSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> IO Bool32

-- No documentation found for TopLevel "vkGetPhysicalDeviceWin32PresentationSupportKHR"
getPhysicalDeviceWin32PresentationSupportKHR :: forall io
                                              . (MonadIO io)
                                             => -- No documentation found for Nested "vkGetPhysicalDeviceWin32PresentationSupportKHR" "physicalDevice"
                                                PhysicalDevice
                                             -> -- No documentation found for Nested "vkGetPhysicalDeviceWin32PresentationSupportKHR" "queueFamilyIndex"
                                                ("queueFamilyIndex" ::: Word32)
                                             -> io (Bool)
getPhysicalDeviceWin32PresentationSupportKHR physicalDevice queueFamilyIndex = liftIO $ do
  let vkGetPhysicalDeviceWin32PresentationSupportKHRPtr = pVkGetPhysicalDeviceWin32PresentationSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceWin32PresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceWin32PresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceWin32PresentationSupportKHR' = mkVkGetPhysicalDeviceWin32PresentationSupportKHR vkGetPhysicalDeviceWin32PresentationSupportKHRPtr
  r <- vkGetPhysicalDeviceWin32PresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex)
  pure $ ((bool32ToBool r))



-- No documentation found for TopLevel "VkWin32SurfaceCreateInfoKHR"
data Win32SurfaceCreateInfoKHR = Win32SurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkWin32SurfaceCreateInfoKHR" "flags"
    flags :: Win32SurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkWin32SurfaceCreateInfoKHR" "hinstance"
    hinstance :: HINSTANCE
  , -- No documentation found for Nested "VkWin32SurfaceCreateInfoKHR" "hwnd"
    hwnd :: HWND
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Win32SurfaceCreateInfoKHR)
#endif
deriving instance Show Win32SurfaceCreateInfoKHR

instance ToCStruct Win32SurfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Win32SurfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Win32SurfaceCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr HINSTANCE)) (hinstance)
    poke ((p `plusPtr` 32 :: Ptr HWND)) (hwnd)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr HINSTANCE)) (zero)
    poke ((p `plusPtr` 32 :: Ptr HWND)) (zero)
    f

instance FromCStruct Win32SurfaceCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @Win32SurfaceCreateFlagsKHR ((p `plusPtr` 16 :: Ptr Win32SurfaceCreateFlagsKHR))
    hinstance <- peek @HINSTANCE ((p `plusPtr` 24 :: Ptr HINSTANCE))
    hwnd <- peek @HWND ((p `plusPtr` 32 :: Ptr HWND))
    pure $ Win32SurfaceCreateInfoKHR
             flags hinstance hwnd


instance Storable Win32SurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Win32SurfaceCreateInfoKHR where
  zero = Win32SurfaceCreateInfoKHR
           zero
           zero
           zero


-- No documentation found for TopLevel "VkWin32SurfaceCreateFlagsKHR"
newtype Win32SurfaceCreateFlagsKHR = Win32SurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameWin32SurfaceCreateFlagsKHR :: String
conNameWin32SurfaceCreateFlagsKHR = "Win32SurfaceCreateFlagsKHR"

enumPrefixWin32SurfaceCreateFlagsKHR :: String
enumPrefixWin32SurfaceCreateFlagsKHR = ""

showTableWin32SurfaceCreateFlagsKHR :: [(Win32SurfaceCreateFlagsKHR, String)]
showTableWin32SurfaceCreateFlagsKHR = []


instance Show Win32SurfaceCreateFlagsKHR where
showsPrec = enumShowsPrec enumPrefixWin32SurfaceCreateFlagsKHR
                          showTableWin32SurfaceCreateFlagsKHR
                          conNameWin32SurfaceCreateFlagsKHR
                          (\(Win32SurfaceCreateFlagsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read Win32SurfaceCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixWin32SurfaceCreateFlagsKHR
                          showTableWin32SurfaceCreateFlagsKHR
                          conNameWin32SurfaceCreateFlagsKHR
                          Win32SurfaceCreateFlagsKHR


type KHR_WIN32_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_SPEC_VERSION"
pattern KHR_WIN32_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_WIN32_SURFACE_SPEC_VERSION = 6


type KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"

-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_EXTENSION_NAME"
pattern KHR_WIN32_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"


type HINSTANCE = Ptr ()


type HWND = Ptr ()

