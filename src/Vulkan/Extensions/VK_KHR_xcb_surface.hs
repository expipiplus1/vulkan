{-# language CPP #-}
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
                       -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
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

-- | vkGetPhysicalDeviceXcbPresentationSupportKHR - Query physical device for
-- presentation to X11 server using XCB
--
-- = Description
--
-- This platform-specific function /can/ be called prior to creating a
-- surface.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceXcbPresentationSupportKHR-queueFamilyIndex-01312#
--     @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     for the given @physicalDevice@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceXcbPresentationSupportKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceXcbPresentationSupportKHR-connection-parameter#
--     @connection@ /must/ be a valid pointer to an @xcb_connection_t@
--     value
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceXcbPresentationSupportKHR :: forall io
                                            . (MonadIO io)
                                           => -- | @physicalDevice@ is the physical device.
                                              PhysicalDevice
                                           -> -- | @queueFamilyIndex@ is the queue family index.
                                              ("queueFamilyIndex" ::: Word32)
                                           -> -- | @connection@ is a pointer to an @xcb_connection_t@ to the X server.
                                              (Ptr Xcb_connection_t)
                                           -> -- | @visual_id@ is an X11 visual (@xcb_visualid_t@).
                                              ("visual_id" ::: Xcb_visualid_t)
                                           -> io (Bool)
getPhysicalDeviceXcbPresentationSupportKHR physicalDevice queueFamilyIndex connection visual_id = liftIO $ do
  let vkGetPhysicalDeviceXcbPresentationSupportKHRPtr = pVkGetPhysicalDeviceXcbPresentationSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceXcbPresentationSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceXcbPresentationSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceXcbPresentationSupportKHR' = mkVkGetPhysicalDeviceXcbPresentationSupportKHR vkGetPhysicalDeviceXcbPresentationSupportKHRPtr
  r <- vkGetPhysicalDeviceXcbPresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (connection) (visual_id)
  pure $ ((bool32ToBool r))


-- | VkXcbSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xcb surface object
--
-- == Valid Usage
--
-- -   #VUID-VkXcbSurfaceCreateInfoKHR-connection-01310# @connection@
--     /must/ point to a valid X11 @xcb_connection_t@
--
-- -   #VUID-VkXcbSurfaceCreateInfoKHR-window-01311# @window@ /must/ be a
--     valid X11 @xcb_window_t@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkXcbSurfaceCreateInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR'
--
-- -   #VUID-VkXcbSurfaceCreateInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkXcbSurfaceCreateInfoKHR-flags-zerobitmask# @flags@ /must/ be
--     @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'XcbSurfaceCreateFlagsKHR', 'createXcbSurfaceKHR'
data XcbSurfaceCreateInfoKHR = XcbSurfaceCreateInfoKHR
  { -- | @flags@ is reserved for future use.
    flags :: XcbSurfaceCreateFlagsKHR
  , -- | @connection@ is a pointer to an @xcb_connection_t@ to the X server.
    connection :: Ptr Xcb_connection_t
  , -- | @window@ is the @xcb_window_t@ for the X11 window to associate the
    -- surface with.
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


-- | VkXcbSurfaceCreateFlagsKHR - Reserved for future use
--
-- = Description
--
-- 'XcbSurfaceCreateFlagsKHR' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'XcbSurfaceCreateInfoKHR'
newtype XcbSurfaceCreateFlagsKHR = XcbSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show XcbSurfaceCreateFlagsKHR where
  showsPrec p = \case
    XcbSurfaceCreateFlagsKHR x -> showParen (p >= 11) (showString "XcbSurfaceCreateFlagsKHR 0x" . showHex x)

instance Read XcbSurfaceCreateFlagsKHR where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "XcbSurfaceCreateFlagsKHR")
                       v <- step readPrec
                       pure (XcbSurfaceCreateFlagsKHR v)))


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

