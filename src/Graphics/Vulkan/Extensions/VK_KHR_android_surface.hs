{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_android_surface  ( createAndroidSurfaceKHR
                                                          , AndroidSurfaceCreateInfoKHR(..)
                                                          , AndroidSurfaceCreateFlagsKHR(..)
                                                          , KHR_ANDROID_SURFACE_SPEC_VERSION
                                                          , pattern KHR_ANDROID_SURFACE_SPEC_VERSION
                                                          , KHR_ANDROID_SURFACE_EXTENSION_NAME
                                                          , pattern KHR_ANDROID_SURFACE_EXTENSION_NAME
                                                          , SurfaceKHR(..)
                                                          , ANativeWindow
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
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Extensions.WSITypes (ANativeWindow)
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Instance)
import Graphics.Vulkan.Core10.Handles (Instance(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkCreateAndroidSurfaceKHR))
import Graphics.Vulkan.Core10.Handles (Instance_T)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.WSITypes (ANativeWindow)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAndroidSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr AndroidSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr AndroidSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateAndroidSurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' object for an Android
-- native window
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to a 'AndroidSurfaceCreateInfoKHR'
--     structure containing parameters affecting the creation of the
--     surface object.
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
-- = Description
--
-- During the lifetime of a surface created using a particular
-- 'Graphics.Vulkan.Extensions.WSITypes.ANativeWindow' handle any attempts
-- to create another surface for the same
-- 'Graphics.Vulkan.Extensions.WSITypes.ANativeWindow' and any attempts to
-- connect to the same 'Graphics.Vulkan.Extensions.WSITypes.ANativeWindow'
-- through other platform mechanisms will fail.
--
-- Note
--
-- In particular, only one 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
-- /can/ exist at a time for a given window. Similarly, a native window
-- /cannot/ be used by both a
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' and @EGLSurface@
-- simultaneously.
--
-- If successful, 'createAndroidSurfaceKHR' increments the
-- 'Graphics.Vulkan.Extensions.WSITypes.ANativeWindow'’s reference count,
-- and 'Graphics.Vulkan.Extensions.VK_KHR_surface.destroySurfaceKHR' will
-- decrement it.
--
-- On Android, when a swapchain’s @imageExtent@ does not match the
-- surface’s @currentExtent@, the presentable images will be scaled to the
-- surface’s dimensions during presentation. @minImageExtent@ is (1,1), and
-- @maxImageExtent@ is the maximum image size supported by the consumer.
-- For the system compositor, @currentExtent@ is the window size (i.e. the
-- consumer’s preferred size).
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Instance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'AndroidSurfaceCreateInfoKHR' structure
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
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'AndroidSurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.Handles.Instance',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
createAndroidSurfaceKHR :: forall io . MonadIO io => Instance -> AndroidSurfaceCreateInfoKHR -> ("allocator" ::: Maybe AllocationCallbacks) -> io (SurfaceKHR)
createAndroidSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateAndroidSurfaceKHR' = mkVkCreateAndroidSurfaceKHR (pVkCreateAndroidSurfaceKHR (instanceCmds (instance' :: Instance)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateAndroidSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkAndroidSurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created Android surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AndroidSurfaceCreateFlagsKHR',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createAndroidSurfaceKHR'
data AndroidSurfaceCreateInfoKHR = AndroidSurfaceCreateInfoKHR
  { -- | @flags@ /must/ be @0@
    flags :: AndroidSurfaceCreateFlagsKHR
  , -- | @window@ /must/ point to a valid Android
    -- 'Graphics.Vulkan.Extensions.WSITypes.ANativeWindow'
    window :: Ptr ANativeWindow
  }
  deriving (Typeable)
deriving instance Show AndroidSurfaceCreateInfoKHR

instance ToCStruct AndroidSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AndroidSurfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AndroidSurfaceCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ANativeWindow))) (window)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ANativeWindow))) (zero)
    f

instance FromCStruct AndroidSurfaceCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @AndroidSurfaceCreateFlagsKHR ((p `plusPtr` 16 :: Ptr AndroidSurfaceCreateFlagsKHR))
    window <- peek @(Ptr ANativeWindow) ((p `plusPtr` 24 :: Ptr (Ptr ANativeWindow)))
    pure $ AndroidSurfaceCreateInfoKHR
             flags window

instance Storable AndroidSurfaceCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AndroidSurfaceCreateInfoKHR where
  zero = AndroidSurfaceCreateInfoKHR
           zero
           zero


-- No documentation found for TopLevel "VkAndroidSurfaceCreateFlagsKHR"
newtype AndroidSurfaceCreateFlagsKHR = AndroidSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show AndroidSurfaceCreateFlagsKHR where
  showsPrec p = \case
    AndroidSurfaceCreateFlagsKHR x -> showParen (p >= 11) (showString "AndroidSurfaceCreateFlagsKHR 0x" . showHex x)

instance Read AndroidSurfaceCreateFlagsKHR where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "AndroidSurfaceCreateFlagsKHR")
                       v <- step readPrec
                       pure (AndroidSurfaceCreateFlagsKHR v)))


type KHR_ANDROID_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_SPEC_VERSION"
pattern KHR_ANDROID_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_ANDROID_SURFACE_SPEC_VERSION = 6


type KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"

-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_EXTENSION_NAME"
pattern KHR_ANDROID_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"

