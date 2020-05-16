{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_win32_surface  ( createWin32SurfaceKHR
                                               , getPhysicalDeviceWin32PresentationSupportKHR
                                               , Win32SurfaceCreateInfoKHR(..)
                                               , Win32SurfaceCreateFlagsKHR(..)
                                               , KHR_WIN32_SURFACE_SPEC_VERSION
                                               , pattern KHR_WIN32_SURFACE_SPEC_VERSION
                                               , KHR_WIN32_SURFACE_EXTENSION_NAME
                                               , pattern KHR_WIN32_SURFACE_EXTENSION_NAME
                                               , SurfaceKHR(..)
                                               , HINSTANCE
                                               , HWND
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
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
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
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.BaseType (Bool32(..))
import Vulkan.Core10.BaseType (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Extensions.WSITypes (HINSTANCE)
import Vulkan.Extensions.WSITypes (HWND)
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
import Vulkan.Extensions.WSITypes (HINSTANCE)
import Vulkan.Extensions.WSITypes (HWND)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateWin32SurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr Win32SurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr Win32SurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateWin32SurfaceKHR - Create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object for an Win32 native window
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Win32SurfaceCreateInfoKHR' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pSurface@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.SurfaceKHR' handle
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
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR', 'Win32SurfaceCreateInfoKHR'
createWin32SurfaceKHR :: forall io
                       . (MonadIO io)
                      => -- | @instance@ is the instance to associate the surface with.
                         Instance
                      -> -- | @pCreateInfo@ is a pointer to a 'Win32SurfaceCreateInfoKHR' structure
                         -- containing parameters affecting the creation of the surface object.
                         Win32SurfaceCreateInfoKHR
                      -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                         -- surface object when there is no more specific allocator available (see
                         -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
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

-- | vkGetPhysicalDeviceWin32PresentationSupportKHR - query queue family
-- support for presentation on a Win32 display
--
-- = Description
--
-- This platform-specific function /can/ be called prior to creating a
-- surface.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceWin32PresentationSupportKHR :: forall io
                                              . (MonadIO io)
                                             => -- | @physicalDevice@ is the physical device.
                                                --
                                                -- @physicalDevice@ /must/ be a valid
                                                -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                PhysicalDevice
                                             -> -- | @queueFamilyIndex@ is the queue family index.
                                                --
                                                -- @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
                                                -- returned by
                                                -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
                                                -- for the given @physicalDevice@
                                                ("queueFamilyIndex" ::: Word32)
                                             -> io (Bool)
getPhysicalDeviceWin32PresentationSupportKHR physicalDevice queueFamilyIndex = liftIO $ do
  let vkGetPhysicalDeviceWin32PresentationSupportKHRPtr = pVkGetPhysicalDeviceWin32PresentationSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceWin32PresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceWin32PresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceWin32PresentationSupportKHR' = mkVkGetPhysicalDeviceWin32PresentationSupportKHR vkGetPhysicalDeviceWin32PresentationSupportKHRPtr
  r <- vkGetPhysicalDeviceWin32PresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex)
  pure $ ((bool32ToBool r))


-- | VkWin32SurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Win32 surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Win32SurfaceCreateFlagsKHR', 'createWin32SurfaceKHR'
data Win32SurfaceCreateInfoKHR = Win32SurfaceCreateInfoKHR
  { -- | @flags@ is reserved for future use.
    --
    -- @flags@ /must/ be @0@
    flags :: Win32SurfaceCreateFlagsKHR
  , -- | @hinstance@ is the Win32 'Vulkan.Extensions.WSITypes.HINSTANCE' for the
    -- window to associate the surface with.
    --
    -- @hinstance@ /must/ be a valid Win32
    -- 'Vulkan.Extensions.WSITypes.HINSTANCE'
    hinstance :: HINSTANCE
  , -- | @hwnd@ is the Win32 'Vulkan.Extensions.WSITypes.HWND' for the window to
    -- associate the surface with.
    --
    -- @hwnd@ /must/ be a valid Win32 'Vulkan.Extensions.WSITypes.HWND'
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


-- | VkWin32SurfaceCreateFlagsKHR - Reserved for future use
--
-- = Description
--
-- 'Win32SurfaceCreateFlagsKHR' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- 'Win32SurfaceCreateInfoKHR'
newtype Win32SurfaceCreateFlagsKHR = Win32SurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show Win32SurfaceCreateFlagsKHR where
  showsPrec p = \case
    Win32SurfaceCreateFlagsKHR x -> showParen (p >= 11) (showString "Win32SurfaceCreateFlagsKHR 0x" . showHex x)

instance Read Win32SurfaceCreateFlagsKHR where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "Win32SurfaceCreateFlagsKHR")
                       v <- step readPrec
                       pure (Win32SurfaceCreateFlagsKHR v)))


type KHR_WIN32_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_SPEC_VERSION"
pattern KHR_WIN32_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_WIN32_SURFACE_SPEC_VERSION = 6


type KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"

-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_EXTENSION_NAME"
pattern KHR_WIN32_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"

