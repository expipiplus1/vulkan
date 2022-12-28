{-# language CPP #-}
-- | = Name
--
-- VK_KHR_xlib_surface - instance extension
--
-- == VK_KHR_xlib_surface
--
-- [__Name String__]
--     @VK_KHR_xlib_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     5
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_xlib_surface] @critsec%0A*Here describe the issue or question you have about the VK_KHR_xlib_surface extension* >
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_xlib_surface] @ianelliottus%0A*Here describe the issue or question you have about the VK_KHR_xlib_surface extension* >
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
-- The @VK_KHR_xlib_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to an X11
-- 'Window', using the Xlib client-side library, as well as a query to
-- determine support for rendering via Xlib.
--
-- == New Commands
--
-- -   'createXlibSurfaceKHR'
--
-- -   'getPhysicalDeviceXlibPresentationSupportKHR'
--
-- == New Structures
--
-- -   'XlibSurfaceCreateInfoKHR'
--
-- == New Bitmasks
--
-- -   'XlibSurfaceCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_XLIB_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_XLIB_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Does X11 need a way to query for compatibility between a particular
-- physical device and a specific screen? This would be a more general
-- query than
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR';
-- if it returned 'Vulkan.Core10.FundamentalTypes.TRUE', then the physical
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
--     -   Added presentation support query for (Display*, VisualID) pair.
--
--     -   Removed “root” parameter from CreateXlibSurfaceKHR(), as it is
--         redundant when a window on the same screen is specified as well.
--
--     -   Added appropriate X errors.
--
--     -   Adjusted wording of issue #1 and added agreed upon resolution.
--
-- -   Revision 3, 2015-10-14 (Ian Elliott)
--
--     -   Renamed this extension from VK_EXT_KHR_x11_surface to
--         VK_EXT_KHR_xlib_surface.
--
-- -   Revision 4, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_xlib_surface to VK_KHR_xlib_surface.
--
-- -   Revision 5, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to vkCreateXlibSurfaceKHR.
--
-- -   Revision 6, 2015-11-28 (Daniel Rakos)
--
--     -   Updated the surface create function to take a pCreateInfo
--         structure.
--
-- == See Also
--
-- 'XlibSurfaceCreateFlagsKHR', 'XlibSurfaceCreateInfoKHR',
-- 'createXlibSurfaceKHR', 'getPhysicalDeviceXlibPresentationSupportKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_xlib_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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
import Data.Word (Word64)
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateXlibSurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceXlibPresentationSupportKHR))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateXlibSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr XlibSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr XlibSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateXlibSurfaceKHR - Create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object for an X11 window, using the Xlib client-side library
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateXlibSurfaceKHR-instance-parameter# @instance@ /must/
--     be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateXlibSurfaceKHR-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'XlibSurfaceCreateInfoKHR'
--     structure
--
-- -   #VUID-vkCreateXlibSurfaceKHR-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateXlibSurfaceKHR-pSurface-parameter# @pSurface@ /must/
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_xlib_surface VK_KHR_xlib_surface>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR', 'XlibSurfaceCreateInfoKHR'
createXlibSurfaceKHR :: forall io
                      . (MonadIO io)
                     => -- | @instance@ is the instance to associate the surface with.
                        Instance
                     -> -- | @pCreateInfo@ is a pointer to a 'XlibSurfaceCreateInfoKHR' structure
                        -- containing the parameters affecting the creation of the surface object.
                        XlibSurfaceCreateInfoKHR
                     -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                        -- surface object when there is no more specific allocator available (see
                        -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io (SurfaceKHR)
createXlibSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateXlibSurfaceKHRPtr = pVkCreateXlibSurfaceKHR (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (vkCreateXlibSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateXlibSurfaceKHR is null" Nothing Nothing
  let vkCreateXlibSurfaceKHR' = mkVkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateXlibSurfaceKHR" (vkCreateXlibSurfaceKHR'
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
  "dynamic" mkVkGetPhysicalDeviceXlibPresentationSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Display -> VisualID -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Display -> VisualID -> IO Bool32

-- | vkGetPhysicalDeviceXlibPresentationSupportKHR - Query physical device
-- for presentation to X11 server using Xlib
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_xlib_surface VK_KHR_xlib_surface>,
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceXlibPresentationSupportKHR :: forall io
                                             . (MonadIO io)
                                            => -- | @physicalDevice@ is the physical device.
                                               --
                                               -- #VUID-vkGetPhysicalDeviceXlibPresentationSupportKHR-physicalDevice-parameter#
                                               -- @physicalDevice@ /must/ be a valid
                                               -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                               PhysicalDevice
                                            -> -- | @queueFamilyIndex@ is the queue family index.
                                               --
                                               -- #VUID-vkGetPhysicalDeviceXlibPresentationSupportKHR-queueFamilyIndex-01315#
                                               -- @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
                                               -- returned by
                                               -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
                                               -- for the given @physicalDevice@
                                               ("queueFamilyIndex" ::: Word32)
                                            -> -- | @dpy@ is a pointer to an Xlib 'Display' connection to the server.
                                               --
                                               -- #VUID-vkGetPhysicalDeviceXlibPresentationSupportKHR-dpy-parameter# @dpy@
                                               -- /must/ be a valid pointer to a 'Display' value
                                               ("dpy" ::: Ptr Display)
                                            -> -- No documentation found for Nested "vkGetPhysicalDeviceXlibPresentationSupportKHR" "visualID"
                                               VisualID
                                            -> io (Bool)
getPhysicalDeviceXlibPresentationSupportKHR physicalDevice
                                              queueFamilyIndex
                                              dpy
                                              visualID = liftIO $ do
  let vkGetPhysicalDeviceXlibPresentationSupportKHRPtr = pVkGetPhysicalDeviceXlibPresentationSupportKHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkGetPhysicalDeviceXlibPresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceXlibPresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceXlibPresentationSupportKHR' = mkVkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHRPtr
  r <- traceAroundEvent "vkGetPhysicalDeviceXlibPresentationSupportKHR" (vkGetPhysicalDeviceXlibPresentationSupportKHR'
                                                                           (physicalDeviceHandle (physicalDevice))
                                                                           (queueFamilyIndex)
                                                                           (dpy)
                                                                           (visualID))
  pure $ ((bool32ToBool r))


-- | VkXlibSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xlib surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_xlib_surface VK_KHR_xlib_surface>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'XlibSurfaceCreateFlagsKHR', 'createXlibSurfaceKHR'
data XlibSurfaceCreateInfoKHR = XlibSurfaceCreateInfoKHR
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkXlibSurfaceCreateInfoKHR-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: XlibSurfaceCreateFlagsKHR
  , -- | @dpy@ is a pointer to an Xlib 'Display' connection to the X server.
    --
    -- #VUID-VkXlibSurfaceCreateInfoKHR-dpy-01313# @dpy@ /must/ point to a
    -- valid Xlib 'Display'
    dpy :: Ptr Display
  , -- | @window@ is an Xlib 'Window' to associate the surface with.
    --
    -- #VUID-VkXlibSurfaceCreateInfoKHR-window-01314# @window@ /must/ be a
    -- valid Xlib 'Window'
    window :: Window
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (XlibSurfaceCreateInfoKHR)
#endif
deriving instance Show XlibSurfaceCreateInfoKHR

instance ToCStruct XlibSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
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


-- | VkXlibSurfaceCreateFlagsKHR - Reserved for future use
--
-- = Description
--
-- 'XlibSurfaceCreateFlagsKHR' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_xlib_surface VK_KHR_xlib_surface>,
-- 'XlibSurfaceCreateInfoKHR'
newtype XlibSurfaceCreateFlagsKHR = XlibSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameXlibSurfaceCreateFlagsKHR :: String
conNameXlibSurfaceCreateFlagsKHR = "XlibSurfaceCreateFlagsKHR"

enumPrefixXlibSurfaceCreateFlagsKHR :: String
enumPrefixXlibSurfaceCreateFlagsKHR = ""

showTableXlibSurfaceCreateFlagsKHR :: [(XlibSurfaceCreateFlagsKHR, String)]
showTableXlibSurfaceCreateFlagsKHR = []

instance Show XlibSurfaceCreateFlagsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixXlibSurfaceCreateFlagsKHR
      showTableXlibSurfaceCreateFlagsKHR
      conNameXlibSurfaceCreateFlagsKHR
      (\(XlibSurfaceCreateFlagsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read XlibSurfaceCreateFlagsKHR where
  readPrec =
    enumReadPrec
      enumPrefixXlibSurfaceCreateFlagsKHR
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

