{-# language CPP #-}
-- | = Name
--
-- VK_KHR_xcb_surface - instance extension
--
-- == VK_KHR_xcb_surface
--
-- [__Name String__]
--     @VK_KHR_xcb_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     6
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_xcb_surface] @critsec%0A*Here describe the issue or question you have about the VK_KHR_xcb_surface extension* >
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_xcb_surface] @ianelliottus%0A*Here describe the issue or question you have about the VK_KHR_xcb_surface extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2015-11-28
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
-- The @VK_KHR_xcb_surface@ extension is an instance extension. It provides
-- a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR' object
-- (defined by the @VK_KHR_surface@ extension) that refers to an X11
-- 'Vulkan.Extensions.VK_KHR_xlib_surface.Window', using the XCB
-- client-side library, as well as a query to determine support for
-- rendering via XCB.
--
-- == New Commands
--
-- -   'createXcbSurfaceKHR'
--
-- -   'getPhysicalDeviceXcbPresentationSupportKHR'
--
-- == New Structures
--
-- -   'XcbSurfaceCreateInfoKHR'
--
-- == New Bitmasks
--
-- -   'XcbSurfaceCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_XCB_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_XCB_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Does XCB need a way to query for compatibility between a particular
-- physical device and a specific screen? This would be a more general
-- query than
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR':
-- If it returned 'Vulkan.Core10.FundamentalTypes.TRUE', then the physical
-- device could be assumed to support presentation to any window on that
-- screen.
--
-- __RESOLVED__: Yes, this is needed for toolkits that want to create a
-- 'Vulkan.Core10.Handles.Device' before creating a window. To ensure the
-- query is reliable, it must be made against a particular X visual rather
-- than the screen in general.
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
--     -   Added presentation support query for an (xcb_connection_t*,
--         xcb_visualid_t) pair.
--
--     -   Removed “root” parameter from CreateXcbSurfaceKHR(), as it is
--         redundant when a window on the same screen is specified as well.
--
--     -   Adjusted wording of issue #1 and added agreed upon resolution.
--
-- -   Revision 3, 2015-10-14 (Ian Elliott)
--
--     -   Removed “root” parameter from CreateXcbSurfaceKHR() in one more
--         place.
--
-- -   Revision 4, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_xcb_surface to VK_KHR_xcb_surface.
--
-- -   Revision 5, 2015-10-23 (Daniel Rakos)
--
--     -   Added allocation callbacks to vkCreateXcbSurfaceKHR.
--
-- -   Revision 6, 2015-11-28 (Daniel Rakos)
--
--     -   Updated the surface create function to take a pCreateInfo
--         structure.
--
-- == See Also
--
-- 'XcbSurfaceCreateFlagsKHR', 'XcbSurfaceCreateInfoKHR',
-- 'createXcbSurfaceKHR', 'getPhysicalDeviceXcbPresentationSupportKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_xcb_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateXcbSurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceXcbPresentationSupportKHR))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateXcbSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr XcbSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr XcbSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateXcbSurfaceKHR - Create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object for a X11 window, using the XCB client-side library
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateXcbSurfaceKHR-instance-parameter# @instance@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateXcbSurfaceKHR-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'XcbSurfaceCreateInfoKHR'
--     structure
--
-- -   #VUID-vkCreateXcbSurfaceKHR-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateXcbSurfaceKHR-pSurface-parameter# @pSurface@ /must/ be
--     a valid pointer to a 'Vulkan.Extensions.Handles.SurfaceKHR' handle
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_xcb_surface VK_KHR_xcb_surface>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR', 'XcbSurfaceCreateInfoKHR'
createXcbSurfaceKHR :: forall io
                     . (MonadIO io)
                    => -- | @instance@ is the instance to associate the surface with.
                       Instance
                    -> -- | @pCreateInfo@ is a pointer to a 'XcbSurfaceCreateInfoKHR' structure
                       -- containing parameters affecting the creation of the surface object.
                       XcbSurfaceCreateInfoKHR
                    -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                       -- surface object when there is no more specific allocator available (see
                       -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io (SurfaceKHR)
createXcbSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateXcbSurfaceKHRPtr = pVkCreateXcbSurfaceKHR (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (vkCreateXcbSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateXcbSurfaceKHR is null" Nothing Nothing
  let vkCreateXcbSurfaceKHR' = mkVkCreateXcbSurfaceKHR vkCreateXcbSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateXcbSurfaceKHR" (vkCreateXcbSurfaceKHR'
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
  "dynamic" mkVkGetPhysicalDeviceXcbPresentationSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Xcb_connection_t -> Xcb_visualid_t -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Xcb_connection_t -> Xcb_visualid_t -> IO Bool32

-- | vkGetPhysicalDeviceXcbPresentationSupportKHR - Query physical device for
-- presentation to X11 server using XCB
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_xcb_surface VK_KHR_xcb_surface>,
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceXcbPresentationSupportKHR :: forall io
                                            . (MonadIO io)
                                           => -- | @physicalDevice@ is the physical device.
                                              --
                                              -- #VUID-vkGetPhysicalDeviceXcbPresentationSupportKHR-physicalDevice-parameter#
                                              -- @physicalDevice@ /must/ be a valid
                                              -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                              PhysicalDevice
                                           -> -- | @queueFamilyIndex@ is the queue family index.
                                              --
                                              -- #VUID-vkGetPhysicalDeviceXcbPresentationSupportKHR-queueFamilyIndex-01312#
                                              -- @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
                                              -- returned by
                                              -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
                                              -- for the given @physicalDevice@
                                              ("queueFamilyIndex" ::: Word32)
                                           -> -- | @connection@ is a pointer to an @xcb_connection_t@ to the X server.
                                              --
                                              -- #VUID-vkGetPhysicalDeviceXcbPresentationSupportKHR-connection-parameter#
                                              -- @connection@ /must/ be a valid pointer to an @xcb_connection_t@ value
                                              (Ptr Xcb_connection_t)
                                           -> -- | @visual_id@ is an X11 visual (@xcb_visualid_t@).
                                              ("visual_id" ::: Xcb_visualid_t)
                                           -> io (Bool)
getPhysicalDeviceXcbPresentationSupportKHR physicalDevice
                                             queueFamilyIndex
                                             connection
                                             visual_id = liftIO $ do
  let vkGetPhysicalDeviceXcbPresentationSupportKHRPtr = pVkGetPhysicalDeviceXcbPresentationSupportKHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkGetPhysicalDeviceXcbPresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceXcbPresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceXcbPresentationSupportKHR' = mkVkGetPhysicalDeviceXcbPresentationSupportKHR vkGetPhysicalDeviceXcbPresentationSupportKHRPtr
  r <- traceAroundEvent "vkGetPhysicalDeviceXcbPresentationSupportKHR" (vkGetPhysicalDeviceXcbPresentationSupportKHR'
                                                                          (physicalDeviceHandle (physicalDevice))
                                                                          (queueFamilyIndex)
                                                                          (connection)
                                                                          (visual_id))
  pure $ ((bool32ToBool r))


-- | VkXcbSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xcb surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_xcb_surface VK_KHR_xcb_surface>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'XcbSurfaceCreateFlagsKHR', 'createXcbSurfaceKHR'
data XcbSurfaceCreateInfoKHR = XcbSurfaceCreateInfoKHR
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkXcbSurfaceCreateInfoKHR-flags-zerobitmask# @flags@ /must/ be @0@
    flags :: XcbSurfaceCreateFlagsKHR
  , -- | @connection@ is a pointer to an @xcb_connection_t@ to the X server.
    --
    -- #VUID-VkXcbSurfaceCreateInfoKHR-connection-01310# @connection@ /must/
    -- point to a valid X11 @xcb_connection_t@
    connection :: Ptr Xcb_connection_t
  , -- | @window@ is the @xcb_window_t@ for the X11 window to associate the
    -- surface with.
    --
    -- #VUID-VkXcbSurfaceCreateInfoKHR-window-01311# @window@ /must/ be a valid
    -- X11 @xcb_window_t@
    window :: Xcb_window_t
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (XcbSurfaceCreateInfoKHR)
#endif
deriving instance Show XcbSurfaceCreateInfoKHR

instance ToCStruct XcbSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
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


-- | VkXcbSurfaceCreateFlagsKHR - Reserved for future use
--
-- = Description
--
-- 'XcbSurfaceCreateFlagsKHR' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_xcb_surface VK_KHR_xcb_surface>,
-- 'XcbSurfaceCreateInfoKHR'
newtype XcbSurfaceCreateFlagsKHR = XcbSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameXcbSurfaceCreateFlagsKHR :: String
conNameXcbSurfaceCreateFlagsKHR = "XcbSurfaceCreateFlagsKHR"

enumPrefixXcbSurfaceCreateFlagsKHR :: String
enumPrefixXcbSurfaceCreateFlagsKHR = ""

showTableXcbSurfaceCreateFlagsKHR :: [(XcbSurfaceCreateFlagsKHR, String)]
showTableXcbSurfaceCreateFlagsKHR = []

instance Show XcbSurfaceCreateFlagsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixXcbSurfaceCreateFlagsKHR
      showTableXcbSurfaceCreateFlagsKHR
      conNameXcbSurfaceCreateFlagsKHR
      (\(XcbSurfaceCreateFlagsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read XcbSurfaceCreateFlagsKHR where
  readPrec =
    enumReadPrec
      enumPrefixXcbSurfaceCreateFlagsKHR
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

