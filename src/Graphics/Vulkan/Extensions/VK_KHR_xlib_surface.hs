{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_xlib_surface  ( createXlibSurfaceKHR
                                                       , getPhysicalDeviceXlibPresentationSupportKHR
                                                       , XlibSurfaceCreateInfoKHR(..)
                                                       , XlibSurfaceCreateFlagsKHR(..)
                                                       , KHR_XLIB_SURFACE_SPEC_VERSION
                                                       , pattern KHR_XLIB_SURFACE_SPEC_VERSION
                                                       , KHR_XLIB_SURFACE_EXTENSION_NAME
                                                       , pattern KHR_XLIB_SURFACE_EXTENSION_NAME
                                                       , SurfaceKHR(..)
                                                       , Display
                                                       , VisualID
                                                       , Window
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
import Graphics.Vulkan.Extensions.WSITypes (Display)
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Instance)
import Graphics.Vulkan.Core10.Handles (Instance(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkCreateXlibSurfaceKHR))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceXlibPresentationSupportKHR))
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
import Graphics.Vulkan.Extensions.WSITypes (VisualID)
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Extensions.WSITypes (Window)
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.WSITypes (Display)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
import Graphics.Vulkan.Extensions.WSITypes (VisualID)
import Graphics.Vulkan.Extensions.WSITypes (Window)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateXlibSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr XlibSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr XlibSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateXlibSurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' object for an X11
-- window, using the Xlib client-side library
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to a 'XlibSurfaceCreateInfoKHR' structure
--     containing the parameters affecting the creation of the surface
--     object.
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
--     'XlibSurfaceCreateInfoKHR' structure
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
-- 'XlibSurfaceCreateInfoKHR'
createXlibSurfaceKHR :: forall io . MonadIO io => Instance -> XlibSurfaceCreateInfoKHR -> ("allocator" ::: Maybe AllocationCallbacks) -> io (SurfaceKHR)
createXlibSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateXlibSurfaceKHR' = mkVkCreateXlibSurfaceKHR (pVkCreateXlibSurfaceKHR (instanceCmds (instance' :: Instance)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateXlibSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
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
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- -   @dpy@ is a pointer to an Xlib
--     'Graphics.Vulkan.Extensions.WSITypes.Display' connection to the
--     server.
--
-- -   @visualId@ is an X11 visual
--     ('Graphics.Vulkan.Extensions.WSITypes.VisualID').
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
getPhysicalDeviceXlibPresentationSupportKHR :: forall io . MonadIO io => PhysicalDevice -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> VisualID -> io (Bool)
getPhysicalDeviceXlibPresentationSupportKHR physicalDevice queueFamilyIndex dpy visualID = liftIO $ do
  let vkGetPhysicalDeviceXlibPresentationSupportKHR' = mkVkGetPhysicalDeviceXlibPresentationSupportKHR (pVkGetPhysicalDeviceXlibPresentationSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice)))
  r <- vkGetPhysicalDeviceXlibPresentationSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (dpy) (visualID)
  pure $ ((bool32ToBool r))


-- | VkXlibSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xlib surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'XlibSurfaceCreateFlagsKHR', 'createXlibSurfaceKHR'
data XlibSurfaceCreateInfoKHR = XlibSurfaceCreateInfoKHR
  { -- | @flags@ /must/ be @0@
    flags :: XlibSurfaceCreateFlagsKHR
  , -- | @dpy@ /must/ point to a valid Xlib
    -- 'Graphics.Vulkan.Extensions.WSITypes.Display'
    dpy :: Ptr Display
  , -- | @window@ /must/ be a valid Xlib
    -- 'Graphics.Vulkan.Extensions.WSITypes.Window'
    window :: Window
  }
  deriving (Typeable)
deriving instance Show XlibSurfaceCreateInfoKHR

instance ToCStruct XlibSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
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


-- No documentation found for TopLevel "VkXlibSurfaceCreateFlagsKHR"
newtype XlibSurfaceCreateFlagsKHR = XlibSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show XlibSurfaceCreateFlagsKHR where
  showsPrec p = \case
    XlibSurfaceCreateFlagsKHR x -> showParen (p >= 11) (showString "XlibSurfaceCreateFlagsKHR 0x" . showHex x)

instance Read XlibSurfaceCreateFlagsKHR where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "XlibSurfaceCreateFlagsKHR")
                       v <- step readPrec
                       pure (XlibSurfaceCreateFlagsKHR v)))


type KHR_XLIB_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_SPEC_VERSION"
pattern KHR_XLIB_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_XLIB_SURFACE_SPEC_VERSION = 6


type KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"

-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_EXTENSION_NAME"
pattern KHR_XLIB_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"

