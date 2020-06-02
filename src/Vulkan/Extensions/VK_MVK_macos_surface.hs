{-# language CPP #-}
module Vulkan.Extensions.VK_MVK_macos_surface  ( createMacOSSurfaceMVK
                                               , MacOSSurfaceCreateInfoMVK(..)
                                               , MacOSSurfaceCreateFlagsMVK(..)
                                               , MVK_MACOS_SURFACE_SPEC_VERSION
                                               , pattern MVK_MACOS_SURFACE_SPEC_VERSION
                                               , MVK_MACOS_SURFACE_EXTENSION_NAME
                                               , pattern MVK_MACOS_SURFACE_EXTENSION_NAME
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateMacOSSurfaceMVK))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateMacOSSurfaceMVK
  :: FunPtr (Ptr Instance_T -> Ptr MacOSSurfaceCreateInfoMVK -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr MacOSSurfaceCreateInfoMVK -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateMacOSSurfaceMVK - Create a VkSurfaceKHR object for a macOS
-- NSView
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'MacOSSurfaceCreateInfoMVK' structure
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance', 'MacOSSurfaceCreateInfoMVK',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
createMacOSSurfaceMVK :: forall io
                       . (MonadIO io)
                      => -- | @instance@ is the instance with which to associate the surface.
                         Instance
                      -> -- | @pCreateInfo@ is a pointer to a 'MacOSSurfaceCreateInfoMVK' structure
                         -- containing parameters affecting the creation of the surface object.
                         MacOSSurfaceCreateInfoMVK
                      -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                         -- surface object when there is no more specific allocator available (see
                         -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                         ("allocator" ::: Maybe AllocationCallbacks)
                      -> io (SurfaceKHR)
createMacOSSurfaceMVK instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateMacOSSurfaceMVKPtr = pVkCreateMacOSSurfaceMVK (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateMacOSSurfaceMVKPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateMacOSSurfaceMVK is null" Nothing Nothing
  let vkCreateMacOSSurfaceMVK' = mkVkCreateMacOSSurfaceMVK vkCreateMacOSSurfaceMVKPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateMacOSSurfaceMVK' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkMacOSSurfaceCreateInfoMVK - Structure specifying parameters of a newly
-- created macOS surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'MacOSSurfaceCreateFlagsMVK',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createMacOSSurfaceMVK'
data MacOSSurfaceCreateInfoMVK = MacOSSurfaceCreateInfoMVK
  { -- | @flags@ is reserved for future use.
    --
    -- @flags@ /must/ be @0@
    flags :: MacOSSurfaceCreateFlagsMVK
  , -- | @pView@ is a reference to a @NSView@ object which will display this
    -- surface. This @NSView@ /must/ be backed by a @CALayer@ instance of type
    -- 'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer'.
    --
    -- @pView@ /must/ be a valid @NSView@ and /must/ be backed by a @CALayer@
    -- instance of type 'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer'
    view :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MacOSSurfaceCreateInfoMVK)
#endif
deriving instance Show MacOSSurfaceCreateInfoMVK

instance ToCStruct MacOSSurfaceCreateInfoMVK where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MacOSSurfaceCreateInfoMVK{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MacOSSurfaceCreateFlagsMVK)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (view)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct MacOSSurfaceCreateInfoMVK where
  peekCStruct p = do
    flags <- peek @MacOSSurfaceCreateFlagsMVK ((p `plusPtr` 16 :: Ptr MacOSSurfaceCreateFlagsMVK))
    pView <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ MacOSSurfaceCreateInfoMVK
             flags pView

instance Storable MacOSSurfaceCreateInfoMVK where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MacOSSurfaceCreateInfoMVK where
  zero = MacOSSurfaceCreateInfoMVK
           zero
           zero


-- No documentation found for TopLevel "VkMacOSSurfaceCreateFlagsMVK"
newtype MacOSSurfaceCreateFlagsMVK = MacOSSurfaceCreateFlagsMVK Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show MacOSSurfaceCreateFlagsMVK where
  showsPrec p = \case
    MacOSSurfaceCreateFlagsMVK x -> showParen (p >= 11) (showString "MacOSSurfaceCreateFlagsMVK 0x" . showHex x)

instance Read MacOSSurfaceCreateFlagsMVK where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "MacOSSurfaceCreateFlagsMVK")
                       v <- step readPrec
                       pure (MacOSSurfaceCreateFlagsMVK v)))


type MVK_MACOS_SURFACE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_SPEC_VERSION"
pattern MVK_MACOS_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern MVK_MACOS_SURFACE_SPEC_VERSION = 2


type MVK_MACOS_SURFACE_EXTENSION_NAME = "VK_MVK_macos_surface"

-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_EXTENSION_NAME"
pattern MVK_MACOS_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MVK_MACOS_SURFACE_EXTENSION_NAME = "VK_MVK_macos_surface"

