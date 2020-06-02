{-# language CPP #-}
module Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface  ( createImagePipeSurfaceFUCHSIA
                                                       , ImagePipeSurfaceCreateInfoFUCHSIA(..)
                                                       , ImagePipeSurfaceCreateFlagsFUCHSIA(..)
                                                       , FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
                                                       , pattern FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
                                                       , FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
                                                       , pattern FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
                                                       , Zx_handle_t
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateImagePipeSurfaceFUCHSIA))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImagePipeSurfaceFUCHSIA
  :: FunPtr (Ptr Instance_T -> Ptr ImagePipeSurfaceCreateInfoFUCHSIA -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr ImagePipeSurfaceCreateInfoFUCHSIA -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateImagePipeSurfaceFUCHSIA - Create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object for a Fuchsia ImagePipe
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'ImagePipeSurfaceCreateInfoFUCHSIA' structure
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
-- 'ImagePipeSurfaceCreateInfoFUCHSIA', 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
createImagePipeSurfaceFUCHSIA :: forall io
                               . (MonadIO io)
                              => -- | @instance@ is the instance to associate with the surface.
                                 Instance
                              -> -- | @pCreateInfo@ is a pointer to a 'ImagePipeSurfaceCreateInfoFUCHSIA'
                                 -- structure containing parameters affecting the creation of the surface
                                 -- object.
                                 ImagePipeSurfaceCreateInfoFUCHSIA
                              -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                                 -- surface object when there is no more specific allocator available (see
                                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io (SurfaceKHR)
createImagePipeSurfaceFUCHSIA instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateImagePipeSurfaceFUCHSIAPtr = pVkCreateImagePipeSurfaceFUCHSIA (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateImagePipeSurfaceFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateImagePipeSurfaceFUCHSIA is null" Nothing Nothing
  let vkCreateImagePipeSurfaceFUCHSIA' = mkVkCreateImagePipeSurfaceFUCHSIA vkCreateImagePipeSurfaceFUCHSIAPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateImagePipeSurfaceFUCHSIA' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkImagePipeSurfaceCreateInfoFUCHSIA - Structure specifying parameters of
-- a newly created ImagePipe surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ImagePipeSurfaceCreateFlagsFUCHSIA',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createImagePipeSurfaceFUCHSIA'
data ImagePipeSurfaceCreateInfoFUCHSIA = ImagePipeSurfaceCreateInfoFUCHSIA
  { -- | @flags@ is reserved for future use.
    --
    -- @flags@ /must/ be @0@
    flags :: ImagePipeSurfaceCreateFlagsFUCHSIA
  , -- | @imagePipeHandle@ is a @zx_handle_t@ referring to the ImagePipe to
    -- associate with the surface.
    --
    -- @imagePipeHandle@ /must/ be a valid @zx_handle_t@
    imagePipeHandle :: Zx_handle_t
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImagePipeSurfaceCreateInfoFUCHSIA)
#endif
deriving instance Show ImagePipeSurfaceCreateInfoFUCHSIA

instance ToCStruct ImagePipeSurfaceCreateInfoFUCHSIA where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImagePipeSurfaceCreateInfoFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImagePipeSurfaceCreateFlagsFUCHSIA)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Zx_handle_t)) (imagePipeHandle)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Zx_handle_t)) (zero)
    f

instance FromCStruct ImagePipeSurfaceCreateInfoFUCHSIA where
  peekCStruct p = do
    flags <- peek @ImagePipeSurfaceCreateFlagsFUCHSIA ((p `plusPtr` 16 :: Ptr ImagePipeSurfaceCreateFlagsFUCHSIA))
    imagePipeHandle <- peek @Zx_handle_t ((p `plusPtr` 20 :: Ptr Zx_handle_t))
    pure $ ImagePipeSurfaceCreateInfoFUCHSIA
             flags imagePipeHandle

instance Storable ImagePipeSurfaceCreateInfoFUCHSIA where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImagePipeSurfaceCreateInfoFUCHSIA where
  zero = ImagePipeSurfaceCreateInfoFUCHSIA
           zero
           zero


-- No documentation found for TopLevel "VkImagePipeSurfaceCreateFlagsFUCHSIA"
newtype ImagePipeSurfaceCreateFlagsFUCHSIA = ImagePipeSurfaceCreateFlagsFUCHSIA Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show ImagePipeSurfaceCreateFlagsFUCHSIA where
  showsPrec p = \case
    ImagePipeSurfaceCreateFlagsFUCHSIA x -> showParen (p >= 11) (showString "ImagePipeSurfaceCreateFlagsFUCHSIA 0x" . showHex x)

instance Read ImagePipeSurfaceCreateFlagsFUCHSIA where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "ImagePipeSurfaceCreateFlagsFUCHSIA")
                       v <- step readPrec
                       pure (ImagePipeSurfaceCreateFlagsFUCHSIA v)))


type FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION"
pattern FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION = 1


type FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME = "VK_FUCHSIA_imagepipe_surface"

-- No documentation found for TopLevel "VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME"
pattern FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME = "VK_FUCHSIA_imagepipe_surface"


type Zx_handle_t = Word32

