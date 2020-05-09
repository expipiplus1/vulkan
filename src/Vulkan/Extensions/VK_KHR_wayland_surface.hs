{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_wayland_surface  ( createWaylandSurfaceKHR
                                                 , getPhysicalDeviceWaylandPresentationSupportKHR
                                                 , WaylandSurfaceCreateInfoKHR(..)
                                                 , WaylandSurfaceCreateFlagsKHR(..)
                                                 , KHR_WAYLAND_SURFACE_SPEC_VERSION
                                                 , pattern KHR_WAYLAND_SURFACE_SPEC_VERSION
                                                 , KHR_WAYLAND_SURFACE_EXTENSION_NAME
                                                 , pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME
                                                 , SurfaceKHR(..)
                                                 , Wl_display
                                                 , Wl_surface
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
import Vulkan.Extensions.WSITypes (Wl_display)
import Vulkan.Extensions.WSITypes (Wl_surface)
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.WSITypes (Wl_display)
import Vulkan.Extensions.WSITypes (Wl_surface)
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
-- -   @instance@ /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'WaylandSurfaceCreateInfoKHR' structure
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
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceWaylandPresentationSupportKHR :: forall io
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
                                               -> -- | @display@ is a pointer to the @wl_display@ associated with a Wayland
                                                  -- compositor.
                                                  --
                                                  -- @display@ /must/ be a valid pointer to a @wl_display@ value
                                                  Ptr Wl_display
                                               -> io (Bool)
getPhysicalDeviceWaylandPresentationSupportKHR physicalDevice queueFamilyIndex display = liftIO $ do
  let vkGetPhysicalDeviceWaylandPresentationSupportKHRPtr = pVkGetPhysicalDeviceWaylandPresentationSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceWaylandPresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceWaylandPresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceWaylandPresentationSupportKHR' = mkVkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHRPtr
  r <- vkGetPhysicalDeviceWaylandPresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (display)
  pure $ ((bool32ToBool r))


-- | VkWaylandSurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created Wayland surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'WaylandSurfaceCreateFlagsKHR', 'createWaylandSurfaceKHR'
data WaylandSurfaceCreateInfoKHR = WaylandSurfaceCreateInfoKHR
  { -- | @flags@ is reserved for future use.
    --
    -- @flags@ /must/ be @0@
    flags :: WaylandSurfaceCreateFlagsKHR
  , -- | @display@ and @surface@ are pointers to the Wayland @wl_display@ and
    -- @wl_surface@ to associate the surface with.
    --
    -- @display@ /must/ point to a valid Wayland @wl_display@
    display :: Ptr Wl_display
  , -- | @surface@ /must/ point to a valid Wayland @wl_surface@
    surface :: Ptr Wl_surface
  }
  deriving (Typeable)
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
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show WaylandSurfaceCreateFlagsKHR where
  showsPrec p = \case
    WaylandSurfaceCreateFlagsKHR x -> showParen (p >= 11) (showString "WaylandSurfaceCreateFlagsKHR 0x" . showHex x)

instance Read WaylandSurfaceCreateFlagsKHR where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "WaylandSurfaceCreateFlagsKHR")
                       v <- step readPrec
                       pure (WaylandSurfaceCreateFlagsKHR v)))


type KHR_WAYLAND_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_SPEC_VERSION"
pattern KHR_WAYLAND_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_WAYLAND_SURFACE_SPEC_VERSION = 6


type KHR_WAYLAND_SURFACE_EXTENSION_NAME = "VK_KHR_wayland_surface"

-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME"
pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME = "VK_KHR_wayland_surface"

