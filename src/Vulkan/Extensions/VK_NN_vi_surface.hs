{-# language CPP #-}
module Vulkan.Extensions.VK_NN_vi_surface  ( createViSurfaceNN
                                           , ViSurfaceCreateInfoNN(..)
                                           , ViSurfaceCreateFlagsNN(..)
                                           , NN_VI_SURFACE_SPEC_VERSION
                                           , pattern NN_VI_SURFACE_SPEC_VERSION
                                           , NN_VI_SURFACE_EXTENSION_NAME
                                           , pattern NN_VI_SURFACE_EXTENSION_NAME
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
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateViSurfaceNN))
import Vulkan.Core10.Handles (Instance_T)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateViSurfaceNN
  :: FunPtr (Ptr Instance_T -> Ptr ViSurfaceCreateInfoNN -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr ViSurfaceCreateInfoNN -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateViSurfaceNN - Create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object for a VI layer
--
-- = Description
--
-- During the lifetime of a surface created using a particular
-- @nn@::@vi@::@NativeWindowHandle@, applications /must/ not attempt to
-- create another surface for the same @nn@::@vi@::@Layer@ or attempt to
-- connect to the same @nn@::@vi@::@Layer@ through other platform
-- mechanisms.
--
-- If the native window is created with a specified size, @currentExtent@
-- will reflect that size. In this case, applications should use the same
-- size for the swapchain’s @imageExtent@. Otherwise, the @currentExtent@
-- will have the special value (0xFFFFFFFF, 0xFFFFFFFF), indicating that
-- applications are expected to choose an appropriate size for the
-- swapchain’s @imageExtent@ (e.g., by matching the result of a call to
-- @nn@::@vi@::@GetDisplayResolution@).
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateViSurfaceNN-instance-parameter# @instance@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateViSurfaceNN-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'ViSurfaceCreateInfoNN'
--     structure
--
-- -   #VUID-vkCreateViSurfaceNN-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateViSurfaceNN-pSurface-parameter# @pSurface@ /must/ be a
--     valid pointer to a 'Vulkan.Extensions.Handles.SurfaceKHR' handle
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR', 'ViSurfaceCreateInfoNN'
createViSurfaceNN :: forall io
                   . (MonadIO io)
                  => -- | @instance@ is the instance with which to associate the surface.
                     Instance
                  -> -- | @pCreateInfo@ is a pointer to a 'ViSurfaceCreateInfoNN' structure
                     -- containing parameters affecting the creation of the surface object.
                     ViSurfaceCreateInfoNN
                  -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                     -- surface object when there is no more specific allocator available (see
                     -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (SurfaceKHR)
createViSurfaceNN instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateViSurfaceNNPtr = pVkCreateViSurfaceNN (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateViSurfaceNNPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateViSurfaceNN is null" Nothing Nothing
  let vkCreateViSurfaceNN' = mkVkCreateViSurfaceNN vkCreateViSurfaceNNPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateViSurfaceNN' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkViSurfaceCreateInfoNN - Structure specifying parameters of a newly
-- created VI surface object
--
-- == Valid Usage
--
-- -   #VUID-VkViSurfaceCreateInfoNN-window-01318# @window@ /must/ be a
--     valid @nn@::@vi@::@NativeWindowHandle@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkViSurfaceCreateInfoNN-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN'
--
-- -   #VUID-VkViSurfaceCreateInfoNN-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkViSurfaceCreateInfoNN-flags-zerobitmask# @flags@ /must/ be
--     @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'ViSurfaceCreateFlagsNN', 'createViSurfaceNN'
data ViSurfaceCreateInfoNN = ViSurfaceCreateInfoNN
  { -- | @flags@ is reserved for future use.
    flags :: ViSurfaceCreateFlagsNN
  , -- | @window@ is the @nn@::@vi@::@NativeWindowHandle@ for the
    -- @nn@::@vi@::@Layer@ with which to associate the surface.
    window :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViSurfaceCreateInfoNN)
#endif
deriving instance Show ViSurfaceCreateInfoNN

instance ToCStruct ViSurfaceCreateInfoNN where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViSurfaceCreateInfoNN{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViSurfaceCreateFlagsNN)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (window)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct ViSurfaceCreateInfoNN where
  peekCStruct p = do
    flags <- peek @ViSurfaceCreateFlagsNN ((p `plusPtr` 16 :: Ptr ViSurfaceCreateFlagsNN))
    window <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ ViSurfaceCreateInfoNN
             flags window

instance Storable ViSurfaceCreateInfoNN where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ViSurfaceCreateInfoNN where
  zero = ViSurfaceCreateInfoNN
           zero
           zero


-- | VkViSurfaceCreateFlagsNN - Reserved for future use
--
-- = Description
--
-- 'ViSurfaceCreateFlagsNN' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'ViSurfaceCreateInfoNN'
newtype ViSurfaceCreateFlagsNN = ViSurfaceCreateFlagsNN Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show ViSurfaceCreateFlagsNN where
  showsPrec p = \case
    ViSurfaceCreateFlagsNN x -> showParen (p >= 11) (showString "ViSurfaceCreateFlagsNN 0x" . showHex x)

instance Read ViSurfaceCreateFlagsNN where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "ViSurfaceCreateFlagsNN")
                       v <- step readPrec
                       pure (ViSurfaceCreateFlagsNN v)))


type NN_VI_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NN_VI_SURFACE_SPEC_VERSION"
pattern NN_VI_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern NN_VI_SURFACE_SPEC_VERSION = 1


type NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"

-- No documentation found for TopLevel "VK_NN_VI_SURFACE_EXTENSION_NAME"
pattern NN_VI_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"

