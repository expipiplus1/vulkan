{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_xcb_surface  ( createXcbSurfaceKHR
                                                      , getPhysicalDeviceXcbPresentationSupportKHR
                                                      , XcbSurfaceCreateInfoKHR(..)
                                                      , XcbSurfaceCreateFlagsKHR(..)
                                                      , KHR_XCB_SURFACE_SPEC_VERSION
                                                      , pattern KHR_XCB_SURFACE_SPEC_VERSION
                                                      , KHR_XCB_SURFACE_EXTENSION_NAME
                                                      , pattern KHR_XCB_SURFACE_EXTENSION_NAME
                                                      , SurfaceKHR(..)
                                                      , Xcb_visualid_t
                                                      , Xcb_window_t
                                                      , Xcb_connection_t
                                                      ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.BaseType (Bool32(..))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Instance)
import Graphics.Vulkan.Core10.Handles (Instance(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkCreateXcbSurfaceKHR))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceXcbPresentationSupportKHR))
import Graphics.Vulkan.Core10.Handles (Instance_T)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Extensions.WSITypes (Xcb_connection_t)
import Graphics.Vulkan.Extensions.WSITypes (Xcb_visualid_t)
import Graphics.Vulkan.Extensions.WSITypes (Xcb_window_t)
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
import Graphics.Vulkan.Extensions.WSITypes (Xcb_connection_t)
import Graphics.Vulkan.Extensions.WSITypes (Xcb_visualid_t)
import Graphics.Vulkan.Extensions.WSITypes (Xcb_window_t)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateXcbSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr XcbSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr XcbSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateXcbSurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' object for a X11 window,
-- using the XCB client-side library
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to a 'XcbSurfaceCreateInfoKHR' structure
--     containing parameters affecting the creation of the surface object.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ is a pointer to a
--     'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' handle in which the
--     created surface object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Instance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'XcbSurfaceCreateInfoKHR' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pSurface@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Instance',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR',
-- 'XcbSurfaceCreateInfoKHR'
createXcbSurfaceKHR :: forall io . MonadIO io => Instance -> XcbSurfaceCreateInfoKHR -> ("allocator" ::: Maybe AllocationCallbacks) -> io (SurfaceKHR)
createXcbSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateXcbSurfaceKHR' = mkVkCreateXcbSurfaceKHR (pVkCreateXcbSurfaceKHR (instanceCmds (instance' :: Instance)))
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
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- -   @connection@ is a pointer to an @xcb_connection_t@ to the X server.
--
-- -   @visual_id@ is an X11 visual (@xcb_visualid_t@).
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
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceXcbPresentationSupportKHR :: forall io . MonadIO io => PhysicalDevice -> ("queueFamilyIndex" ::: Word32) -> Ptr Xcb_connection_t -> ("visual_id" ::: Xcb_visualid_t) -> io (Bool)
getPhysicalDeviceXcbPresentationSupportKHR physicalDevice queueFamilyIndex connection visual_id = liftIO $ do
  let vkGetPhysicalDeviceXcbPresentationSupportKHR' = mkVkGetPhysicalDeviceXcbPresentationSupportKHR (pVkGetPhysicalDeviceXcbPresentationSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice)))
  r <- vkGetPhysicalDeviceXcbPresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (connection) (visual_id)
  pure $ ((bool32ToBool r))


-- | VkXcbSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xcb surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'XcbSurfaceCreateFlagsKHR', 'createXcbSurfaceKHR'
data XcbSurfaceCreateInfoKHR = XcbSurfaceCreateInfoKHR
  { -- | @flags@ /must/ be @0@
    flags :: XcbSurfaceCreateFlagsKHR
  , -- | @connection@ /must/ point to a valid X11 @xcb_connection_t@.
    connection :: Ptr Xcb_connection_t
  , -- | @window@ /must/ be a valid X11 @xcb_window_t@.
    window :: Xcb_window_t
  }
  deriving (Typeable)
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

