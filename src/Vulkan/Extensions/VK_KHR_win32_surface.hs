{-# language CPP #-}
-- | = Name
--
-- VK_KHR_win32_surface - instance extension
--
-- == VK_KHR_win32_surface
--
-- [__Name String__]
--     @VK_KHR_win32_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     10
--
-- [__Revision__]
--     6
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@ to be enabled
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_win32_surface] @critsec%0A*Here describe the issue or question you have about the VK_KHR_win32_surface extension* >
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_win32_surface] @ianelliottus%0A*Here describe the issue or question you have about the VK_KHR_win32_surface extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-04-24
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Patrick Doane, Blizzard
--
--     -   Jason Ekstrand, Intel
--
--     -   Ian Elliott, LunarG
--
--     -   Courtney Goeltzenleuchter, LunarG
--
--     -   Jesse Hall, Google
--
--     -   James Jones, NVIDIA
--
--     -   Antoine Labour, Google
--
--     -   Jon Leech, Khronos
--
--     -   David Mao, AMD
--
--     -   Norbert Nopper, Freescale
--
--     -   Alon Or-bach, Samsung
--
--     -   Daniel Rakos, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Ray Smith, ARM
--
--     -   Jeff Vigil, Qualcomm
--
--     -   Chia-I Wu, LunarG
--
-- == Description
--
-- The @VK_KHR_win32_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to a
-- Win32 'HWND', as well as a query to determine support for rendering to
-- the windows desktop.
--
-- == New Commands
--
-- -   'createWin32SurfaceKHR'
--
-- -   'getPhysicalDeviceWin32PresentationSupportKHR'
--
-- == New Structures
--
-- -   'Win32SurfaceCreateInfoKHR'
--
-- == New Bitmasks
--
-- -   'Win32SurfaceCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_WIN32_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_WIN32_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Does Win32 need a way to query for compatibility between a particular
-- physical device and a specific screen? Compatibility between a physical
-- device and a window generally only depends on what screen the window is
-- on. However, there is not an obvious way to identify a screen without
-- already having a window on the screen.
--
-- __RESOLVED__: No. While it may be useful, there is not a clear way to do
-- this on Win32. However, a method was added to query support for
-- presenting to the windows desktop as a whole.
--
-- 2) If a native window object ('HWND') is used by one graphics API, and
-- then is later used by a different graphics API (one of which is Vulkan),
-- can these uses interfere with each other?
--
-- __RESOLVED__: Yes.
--
-- Uses of a window object by multiple graphics APIs results in undefined
-- behavior. Such behavior may succeed when using one Vulkan implementation
-- but fail when using a different Vulkan implementation. Potential
-- failures include:
--
-- -   Creating then destroying a flip presentation model DXGI swapchain on
--     a window object can prevent
--     'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR' from
--     succeeding on the same window object.
--
-- -   Creating then destroying a 'Vulkan.Extensions.Handles.SwapchainKHR'
--     on a window object can prevent creation of a bitblt model DXGI
--     swapchain on the same window object.
--
-- -   Creating then destroying a 'Vulkan.Extensions.Handles.SwapchainKHR'
--     on a window object can effectively @SetPixelFormat@ to a different
--     format than the format chosen by an OpenGL application.
--
-- -   Creating then destroying a 'Vulkan.Extensions.Handles.SwapchainKHR'
--     on a window object on one 'Vulkan.Core10.Handles.PhysicalDevice' can
--     prevent 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR' from
--     succeeding on the same window object, but on a different
--     'Vulkan.Core10.Handles.PhysicalDevice' that is associated with a
--     different Vulkan ICD.
--
-- In all cases the problem can be worked around by creating a new window
-- object.
--
-- Technical details include:
--
-- -   Creating a DXGI swapchain over a window object can alter the object
--     for the remainder of its lifetime. The alteration persists even
--     after the DXGI swapchain has been destroyed. This alteration can
--     make it impossible for a conformant Vulkan implementation to create
--     a 'Vulkan.Extensions.Handles.SwapchainKHR' over the same window
--     object. Mention of this alteration can be found in the remarks
--     section of the MSDN documentation for @DXGI_SWAP_EFFECT@.
--
-- -   Calling GDI’s @SetPixelFormat@ (needed by OpenGL’s WGL layer) on a
--     window object alters the object for the remainder of its lifetime.
--     The MSDN documentation for @SetPixelFormat@ explains that a window
--     object’s pixel format can be set only one time.
--
-- -   Creating a 'Vulkan.Extensions.Handles.SwapchainKHR' over a window
--     object can alter the object for its remaining lifetime. Either of
--     the above alterations may occur as a side effect of
--     'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
--
-- == Version History
--
-- -   Revision 1, 2015-09-23 (Jesse Hall)
--
--     -   Initial draft, based on the previous contents of
--         VK_EXT_KHR_swapchain (later renamed VK_EXT_KHR_surface).
--
-- -   Revision 2, 2015-10-02 (James Jones)
--
--     -   Added presentation support query for win32 desktops.
--
-- -   Revision 3, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_win32_surface to VK_KHR_win32_surface.
--
-- -   Revision 4, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to vkCreateWin32SurfaceKHR.
--
-- -   Revision 5, 2015-11-28 (Daniel Rakos)
--
--     -   Updated the surface create function to take a pCreateInfo
--         structure.
--
-- -   Revision 6, 2017-04-24 (Jeff Juliano)
--
--     -   Add issue 2 addressing reuse of a native window object in a
--         different Graphics API, or by a different Vulkan ICD.
--
-- == See Also
--
-- 'Win32SurfaceCreateFlagsKHR', 'Win32SurfaceCreateInfoKHR',
-- 'createWin32SurfaceKHR', 'getPhysicalDeviceWin32PresentationSupportKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_win32_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
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
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Handles (Instance(Instance))
import Vulkan.Dynamic (InstanceCmds(pVkCreateWin32SurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceWin32PresentationSupportKHR))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateWin32SurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr Win32SurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr Win32SurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateWin32SurfaceKHR - Create a VkSurfaceKHR object for an Win32
-- native window
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateWin32SurfaceKHR-instance-parameter# @instance@ /must/
--     be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateWin32SurfaceKHR-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'Win32SurfaceCreateInfoKHR'
--     structure
--
-- -   #VUID-vkCreateWin32SurfaceKHR-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateWin32SurfaceKHR-pSurface-parameter# @pSurface@ /must/
--     be a valid pointer to a 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_win32_surface VK_KHR_win32_surface>,
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
                         -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                         ("allocator" ::: Maybe AllocationCallbacks)
                      -> io (SurfaceKHR)
createWin32SurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateWin32SurfaceKHRPtr = pVkCreateWin32SurfaceKHR (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (vkCreateWin32SurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateWin32SurfaceKHR is null" Nothing Nothing
  let vkCreateWin32SurfaceKHR' = mkVkCreateWin32SurfaceKHR vkCreateWin32SurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateWin32SurfaceKHR" (vkCreateWin32SurfaceKHR'
                                                            (instanceHandle (instance'))
                                                            pCreateInfo
                                                            pAllocator
                                                            (pPSurface))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceWin32PresentationSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> IO Bool32

-- | vkGetPhysicalDeviceWin32PresentationSupportKHR - Query queue family
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_win32_surface VK_KHR_win32_surface>,
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceWin32PresentationSupportKHR :: forall io
                                              . (MonadIO io)
                                             => -- | @physicalDevice@ is the physical device.
                                                --
                                                -- #VUID-vkGetPhysicalDeviceWin32PresentationSupportKHR-physicalDevice-parameter#
                                                -- @physicalDevice@ /must/ be a valid
                                                -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                PhysicalDevice
                                             -> -- | @queueFamilyIndex@ is the queue family index.
                                                --
                                                -- #VUID-vkGetPhysicalDeviceWin32PresentationSupportKHR-queueFamilyIndex-01309#
                                                -- @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
                                                -- returned by
                                                -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
                                                -- for the given @physicalDevice@
                                                ("queueFamilyIndex" ::: Word32)
                                             -> io (Bool)
getPhysicalDeviceWin32PresentationSupportKHR physicalDevice
                                               queueFamilyIndex = liftIO $ do
  let vkGetPhysicalDeviceWin32PresentationSupportKHRPtr = pVkGetPhysicalDeviceWin32PresentationSupportKHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkGetPhysicalDeviceWin32PresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceWin32PresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceWin32PresentationSupportKHR' = mkVkGetPhysicalDeviceWin32PresentationSupportKHR vkGetPhysicalDeviceWin32PresentationSupportKHRPtr
  r <- traceAroundEvent "vkGetPhysicalDeviceWin32PresentationSupportKHR" (vkGetPhysicalDeviceWin32PresentationSupportKHR'
                                                                            (physicalDeviceHandle (physicalDevice))
                                                                            (queueFamilyIndex))
  pure $ ((bool32ToBool r))


-- | VkWin32SurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Win32 surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_win32_surface VK_KHR_win32_surface>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Win32SurfaceCreateFlagsKHR', 'createWin32SurfaceKHR'
data Win32SurfaceCreateInfoKHR = Win32SurfaceCreateInfoKHR
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkWin32SurfaceCreateInfoKHR-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: Win32SurfaceCreateFlagsKHR
  , -- | @hinstance@ is the Win32 'HINSTANCE' for the window to associate the
    -- surface with.
    --
    -- #VUID-VkWin32SurfaceCreateInfoKHR-hinstance-01307# @hinstance@ /must/ be
    -- a valid Win32 'HINSTANCE'
    hinstance :: HINSTANCE
  , -- | @hwnd@ is the Win32 'HWND' for the window to associate the surface with.
    --
    -- #VUID-VkWin32SurfaceCreateInfoKHR-hwnd-01308# @hwnd@ /must/ be a valid
    -- Win32 'HWND'
    hwnd :: HWND
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Win32SurfaceCreateInfoKHR)
#endif
deriving instance Show Win32SurfaceCreateInfoKHR

instance ToCStruct Win32SurfaceCreateInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_win32_surface VK_KHR_win32_surface>,
-- 'Win32SurfaceCreateInfoKHR'
newtype Win32SurfaceCreateFlagsKHR = Win32SurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameWin32SurfaceCreateFlagsKHR :: String
conNameWin32SurfaceCreateFlagsKHR = "Win32SurfaceCreateFlagsKHR"

enumPrefixWin32SurfaceCreateFlagsKHR :: String
enumPrefixWin32SurfaceCreateFlagsKHR = ""

showTableWin32SurfaceCreateFlagsKHR :: [(Win32SurfaceCreateFlagsKHR, String)]
showTableWin32SurfaceCreateFlagsKHR = []

instance Show Win32SurfaceCreateFlagsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixWin32SurfaceCreateFlagsKHR
      showTableWin32SurfaceCreateFlagsKHR
      conNameWin32SurfaceCreateFlagsKHR
      (\(Win32SurfaceCreateFlagsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read Win32SurfaceCreateFlagsKHR where
  readPrec =
    enumReadPrec
      enumPrefixWin32SurfaceCreateFlagsKHR
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

