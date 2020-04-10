{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface  ( createStreamDescriptorSurfaceGGP
                                                                    , StreamDescriptorSurfaceCreateInfoGGP(..)
                                                                    , StreamDescriptorSurfaceCreateFlagsGGP(..)
                                                                    , GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
                                                                    , pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
                                                                    , GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
                                                                    , pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
                                                                    , SurfaceKHR(..)
                                                                    , GgpStreamDescriptor
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
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Extensions.WSITypes (GgpStreamDescriptor)
import Graphics.Vulkan.Core10.Handles (Instance)
import Graphics.Vulkan.Core10.Handles (Instance(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkCreateStreamDescriptorSurfaceGGP))
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
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.WSITypes (GgpStreamDescriptor)
import Graphics.Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateStreamDescriptorSurfaceGGP
  :: FunPtr (Ptr Instance_T -> Ptr StreamDescriptorSurfaceCreateInfoGGP -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr StreamDescriptorSurfaceCreateInfoGGP -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateStreamDescriptorSurfaceGGP - Create a
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR' object for a Google
-- Games Platform stream
--
-- = Parameters
--
-- -   @instance@ is the instance to associate with the surface.
--
-- -   @pCreateInfo@ is a pointer to a
--     'StreamDescriptorSurfaceCreateInfoGGP' structure containing
--     parameters that affect the creation of the surface object.
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
--     'StreamDescriptorSurfaceCreateInfoGGP' structure
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
-- 'Graphics.Vulkan.Core10.Handles.Instance',
-- 'StreamDescriptorSurfaceCreateInfoGGP',
-- 'Graphics.Vulkan.Extensions.Handles.SurfaceKHR'
createStreamDescriptorSurfaceGGP :: forall io . MonadIO io => Instance -> StreamDescriptorSurfaceCreateInfoGGP -> ("allocator" ::: Maybe AllocationCallbacks) -> io (SurfaceKHR)
createStreamDescriptorSurfaceGGP instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateStreamDescriptorSurfaceGGP' = mkVkCreateStreamDescriptorSurfaceGGP (pVkCreateStreamDescriptorSurfaceGGP (instanceCmds (instance' :: Instance)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateStreamDescriptorSurfaceGGP' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkStreamDescriptorSurfaceCreateInfoGGP - Structure specifying parameters
-- of a newly created Google Games Platform stream surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'StreamDescriptorSurfaceCreateFlagsGGP',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createStreamDescriptorSurfaceGGP'
data StreamDescriptorSurfaceCreateInfoGGP = StreamDescriptorSurfaceCreateInfoGGP
  { -- | @flags@ /must/ be @0@
    flags :: StreamDescriptorSurfaceCreateFlagsGGP
  , -- | @streamDescriptor@ /must/ be a valid
    -- 'Graphics.Vulkan.Extensions.WSITypes.GgpStreamDescriptor'
    streamDescriptor :: GgpStreamDescriptor
  }
  deriving (Typeable)
deriving instance Show StreamDescriptorSurfaceCreateInfoGGP

instance ToCStruct StreamDescriptorSurfaceCreateInfoGGP where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p StreamDescriptorSurfaceCreateInfoGGP{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr StreamDescriptorSurfaceCreateFlagsGGP)) (flags)
    poke ((p `plusPtr` 20 :: Ptr GgpStreamDescriptor)) (streamDescriptor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr GgpStreamDescriptor)) (zero)
    f

instance FromCStruct StreamDescriptorSurfaceCreateInfoGGP where
  peekCStruct p = do
    flags <- peek @StreamDescriptorSurfaceCreateFlagsGGP ((p `plusPtr` 16 :: Ptr StreamDescriptorSurfaceCreateFlagsGGP))
    streamDescriptor <- peek @GgpStreamDescriptor ((p `plusPtr` 20 :: Ptr GgpStreamDescriptor))
    pure $ StreamDescriptorSurfaceCreateInfoGGP
             flags streamDescriptor

instance Storable StreamDescriptorSurfaceCreateInfoGGP where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero StreamDescriptorSurfaceCreateInfoGGP where
  zero = StreamDescriptorSurfaceCreateInfoGGP
           zero
           zero


-- No documentation found for TopLevel "VkStreamDescriptorSurfaceCreateFlagsGGP"
newtype StreamDescriptorSurfaceCreateFlagsGGP = StreamDescriptorSurfaceCreateFlagsGGP Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show StreamDescriptorSurfaceCreateFlagsGGP where
  showsPrec p = \case
    StreamDescriptorSurfaceCreateFlagsGGP x -> showParen (p >= 11) (showString "StreamDescriptorSurfaceCreateFlagsGGP 0x" . showHex x)

instance Read StreamDescriptorSurfaceCreateFlagsGGP where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "StreamDescriptorSurfaceCreateFlagsGGP")
                       v <- step readPrec
                       pure (StreamDescriptorSurfaceCreateFlagsGGP v)))


type GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION"
pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION = 1


type GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME = "VK_GGP_stream_descriptor_surface"

-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME"
pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME = "VK_GGP_stream_descriptor_surface"

