{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface  ( createImagePipeSurfaceFUCHSIA
                                                                , ImagePipeSurfaceCreateInfoFUCHSIA(..)
                                                                , ImagePipeSurfaceCreateFlagsFUCHSIA(..)
                                                                , FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
                                                                , pattern FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
                                                                , FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
                                                                , pattern FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
                                                                , SurfaceKHR(..)
                                                                , Zx_handle_t
                                                                ) where

import Control.Exception.Base (bracket)
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
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Instance)
import Graphics.Vulkan.Core10.Handles (Instance(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkCreateImagePipeSurfaceFUCHSIA))
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
import Graphics.Vulkan.Extensions.WSITypes (Zx_handle_t)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
import Graphics.Vulkan.Extensions.WSITypes (Zx_handle_t)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImagePipeSurfaceFUCHSIA
  :: FunPtr (Ptr Instance_T -> Ptr ImagePipeSurfaceCreateInfoFUCHSIA -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr ImagePipeSurfaceCreateInfoFUCHSIA -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateImagePipeSurfaceFUCHSIA - Create a
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' object for a Fuchsia
-- ImagePipe
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Instance' is the instance to
--     associate with the surface.
--
-- -   @pCreateInfo@ is a pointer to a 'ImagePipeSurfaceCreateInfoFUCHSIA'
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
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Instance' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Instance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'ImagePipeSurfaceCreateInfoFUCHSIA' structure
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
-- 'ImagePipeSurfaceCreateInfoFUCHSIA',
-- 'Graphics.Vulkan.Core10.Handles.Instance',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
createImagePipeSurfaceFUCHSIA :: Instance -> ImagePipeSurfaceCreateInfoFUCHSIA -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (SurfaceKHR)
createImagePipeSurfaceFUCHSIA instance' createInfo allocator = evalContT $ do
  let vkCreateImagePipeSurfaceFUCHSIA' = mkVkCreateImagePipeSurfaceFUCHSIA (pVkCreateImagePipeSurfaceFUCHSIA (instanceCmds (instance' :: Instance)))
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
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createImagePipeSurfaceFUCHSIA'
data ImagePipeSurfaceCreateInfoFUCHSIA = ImagePipeSurfaceCreateInfoFUCHSIA
  { -- | 'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be @0@
    flags :: ImagePipeSurfaceCreateFlagsFUCHSIA
  , -- | @imagePipeHandle@ /must/ be a valid
    -- 'Graphics.Vulkan.Extensions.WSITypes.Zx_handle_t'
    imagePipeHandle :: Zx_handle_t
  }
  deriving (Typeable)
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

