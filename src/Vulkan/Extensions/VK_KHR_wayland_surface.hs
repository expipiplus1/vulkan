{-# language CPP #-}
-- | = Name
--
-- VK_KHR_wayland_surface - instance extension
--
-- == VK_KHR_wayland_surface
--
-- [__Name String__]
--     @VK_KHR_wayland_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     7
--
-- [__Revision__]
--     6
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_wayland_surface] @critsec%0A<<Here describe the issue or question you have about the VK_KHR_wayland_surface extension>> >
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_wayland_surface] @ianelliottus%0A<<Here describe the issue or question you have about the VK_KHR_wayland_surface extension>> >
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
-- The @VK_KHR_wayland_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to a
-- Wayland @wl_surface@, as well as a query to determine support for
-- rendering to a Wayland compositor.
--
-- == New Commands
--
-- -   'createWaylandSurfaceKHR'
--
-- -   'getPhysicalDeviceWaylandPresentationSupportKHR'
--
-- == New Structures
--
-- -   'WaylandSurfaceCreateInfoKHR'
--
-- == New Bitmasks
--
-- -   'WaylandSurfaceCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_WAYLAND_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_WAYLAND_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Does Wayland need a way to query for compatibility between a
-- particular physical device and a specific Wayland display? This would be
-- a more general query than
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR':
-- if the Wayland-specific query returned
-- 'Vulkan.Core10.FundamentalTypes.TRUE' for a
-- ('Vulkan.Core10.Handles.PhysicalDevice', @struct wl_display*@) pair,
-- then the physical device could be assumed to support presentation to any
-- 'Vulkan.Extensions.Handles.SurfaceKHR' for surfaces on the display.
--
-- __RESOLVED__: Yes. 'getPhysicalDeviceWaylandPresentationSupportKHR' was
-- added to address this issue.
--
-- 2) Should we require surfaces created with 'createWaylandSurfaceKHR' to
-- support the 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR'
-- present mode?
--
-- __RESOLVED__: Yes. Wayland is an inherently mailbox window system and
-- mailbox support is required for some Wayland compositor interactions to
-- work as expected. While handling these interactions may be possible with
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR', it is much
-- more difficult to do without deadlock and requiring all Wayland
-- applications to be able to support implementations which only support
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' would be an
-- onerous restriction on application developers.
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
--     -   Added vkGetPhysicalDeviceWaylandPresentationSupportKHR() to
--         resolve issue #1.
--
--     -   Adjusted wording of issue #1 to match the agreed-upon solution.
--
--     -   Renamed “window” parameters to “surface” to match Wayland
--         conventions.
--
-- -   Revision 3, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_wayland_surface to
--         VK_KHR_wayland_surface.
--
-- -   Revision 4, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to vkCreateWaylandSurfaceKHR.
--
-- -   Revision 5, 2015-11-28 (Daniel Rakos)
--
--     -   Updated the surface create function to take a pCreateInfo
--         structure.
--
-- -   Revision 6, 2017-02-08 (Jason Ekstrand)
--
--     -   Added the requirement that implementations support
--         'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR'.
--
--     -   Added wording about interactions between
--         'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' and the
--         Wayland requests sent to the compositor.
--
-- == See Also
--
-- 'WaylandSurfaceCreateFlagsKHR', 'WaylandSurfaceCreateInfoKHR',
-- 'createWaylandSurfaceKHR',
-- 'getPhysicalDeviceWaylandPresentationSupportKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_wayland_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Handles (Instance(Instance))
import Vulkan.Dynamic (InstanceCmds(pVkCreateWaylandSurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceWaylandPresentationSupportKHR))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateWaylandSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr WaylandSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr WaylandSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateWaylandSurfaceKHR - Create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object for a Wayland window
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateWaylandSurfaceKHR-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateWaylandSurfaceKHR-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'WaylandSurfaceCreateInfoKHR'
--     structure
--
-- -   #VUID-vkCreateWaylandSurfaceKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateWaylandSurfaceKHR-pSurface-parameter# @pSurface@
--     /must/ be a valid pointer to a
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_wayland_surface VK_KHR_wayland_surface>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR', 'WaylandSurfaceCreateInfoKHR'
createWaylandSurfaceKHR :: forall io
                         . (MonadIO io)
                        => -- | @instance@ is the instance to associate the surface with.
                           Instance
                        -> -- | @pCreateInfo@ is a pointer to a 'WaylandSurfaceCreateInfoKHR' structure
                           -- containing parameters affecting the creation of the surface object.
                           WaylandSurfaceCreateInfoKHR
                        -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                           -- surface object when there is no more specific allocator available (see
                           -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                           ("allocator" ::: Maybe AllocationCallbacks)
                        -> io (SurfaceKHR)
createWaylandSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateWaylandSurfaceKHRPtr = pVkCreateWaylandSurfaceKHR (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (vkCreateWaylandSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateWaylandSurfaceKHR is null" Nothing Nothing
  let vkCreateWaylandSurfaceKHR' = mkVkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateWaylandSurfaceKHR" (vkCreateWaylandSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceWaylandPresentationSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Wl_display -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Wl_display -> IO Bool32

-- | vkGetPhysicalDeviceWaylandPresentationSupportKHR - Query physical device
-- for presentation to Wayland
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_wayland_surface VK_KHR_wayland_surface>,
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceWaylandPresentationSupportKHR :: forall io
                                                . (MonadIO io)
                                               => -- | @physicalDevice@ is the physical device.
                                                  --
                                                  -- #VUID-vkGetPhysicalDeviceWaylandPresentationSupportKHR-physicalDevice-parameter#
                                                  -- @physicalDevice@ /must/ be a valid
                                                  -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                  PhysicalDevice
                                               -> -- | @queueFamilyIndex@ is the queue family index.
                                                  --
                                                  -- #VUID-vkGetPhysicalDeviceWaylandPresentationSupportKHR-queueFamilyIndex-01306#
                                                  -- @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
                                                  -- returned by
                                                  -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
                                                  -- for the given @physicalDevice@
                                                  ("queueFamilyIndex" ::: Word32)
                                               -> -- | @display@ is a pointer to the @wl_display@ associated with a Wayland
                                                  -- compositor.
                                                  --
                                                  -- #VUID-vkGetPhysicalDeviceWaylandPresentationSupportKHR-display-parameter#
                                                  -- @display@ /must/ be a valid pointer to a @wl_display@ value
                                                  (Ptr Wl_display)
                                               -> io (Bool)
getPhysicalDeviceWaylandPresentationSupportKHR physicalDevice queueFamilyIndex display = liftIO $ do
  let vkGetPhysicalDeviceWaylandPresentationSupportKHRPtr = pVkGetPhysicalDeviceWaylandPresentationSupportKHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkGetPhysicalDeviceWaylandPresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceWaylandPresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceWaylandPresentationSupportKHR' = mkVkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHRPtr
  r <- traceAroundEvent "vkGetPhysicalDeviceWaylandPresentationSupportKHR" (vkGetPhysicalDeviceWaylandPresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (display))
  pure $ ((bool32ToBool r))


-- | VkWaylandSurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created Wayland surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_wayland_surface VK_KHR_wayland_surface>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'WaylandSurfaceCreateFlagsKHR', 'createWaylandSurfaceKHR'
data WaylandSurfaceCreateInfoKHR = WaylandSurfaceCreateInfoKHR
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkWaylandSurfaceCreateInfoKHR-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: WaylandSurfaceCreateFlagsKHR
  , -- | @display@ and @surface@ are pointers to the Wayland @wl_display@ and
    -- @wl_surface@ to associate the surface with.
    --
    -- #VUID-VkWaylandSurfaceCreateInfoKHR-display-01304# @display@ /must/
    -- point to a valid Wayland @wl_display@
    display :: Ptr Wl_display
  , -- | #VUID-VkWaylandSurfaceCreateInfoKHR-surface-01305# @surface@ /must/
    -- point to a valid Wayland @wl_surface@
    surface :: Ptr Wl_surface
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WaylandSurfaceCreateInfoKHR)
#endif
deriving instance Show WaylandSurfaceCreateInfoKHR

instance ToCStruct WaylandSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
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


-- | VkWaylandSurfaceCreateFlagsKHR - Reserved for future use
--
-- = Description
--
-- 'WaylandSurfaceCreateFlagsKHR' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_wayland_surface VK_KHR_wayland_surface>,
-- 'WaylandSurfaceCreateInfoKHR'
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

