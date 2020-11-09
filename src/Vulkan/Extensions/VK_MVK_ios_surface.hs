{-# language CPP #-}
module Vulkan.Extensions.VK_MVK_ios_surface  ( createIOSSurfaceMVK
                                             , IOSSurfaceCreateInfoMVK(..)
                                             , IOSSurfaceCreateFlagsMVK(..)
                                             , MVK_IOS_SURFACE_SPEC_VERSION
                                             , pattern MVK_IOS_SURFACE_SPEC_VERSION
                                             , MVK_IOS_SURFACE_EXTENSION_NAME
                                             , pattern MVK_IOS_SURFACE_EXTENSION_NAME
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateIOSSurfaceMVK))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIOSSurfaceMVK
  :: FunPtr (Ptr Instance_T -> Ptr IOSSurfaceCreateInfoMVK -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr IOSSurfaceCreateInfoMVK -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateIOSSurfaceMVK - Create a VkSurfaceKHR object for an iOS UIView
--
-- = Parameters
--
-- Note
--
-- The 'createIOSSurfaceMVK' function is considered deprecated and has been
-- superseded by
-- 'Vulkan.Extensions.VK_EXT_metal_surface.createMetalSurfaceEXT' from the
-- @VK_EXT_metal_surface@ extension.
--
-- = Description
--
-- -   @instance@ is the instance with which to associate the surface.
--
-- -   @pCreateInfo@ is a pointer to a 'IOSSurfaceCreateInfoMVK' structure
--     containing parameters affecting the creation of the surface object.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ is a pointer to a 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle in which the created surface object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateIOSSurfaceMVK-instance-parameter# @instance@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateIOSSurfaceMVK-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'IOSSurfaceCreateInfoMVK'
--     structure
--
-- -   #VUID-vkCreateIOSSurfaceMVK-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateIOSSurfaceMVK-pSurface-parameter# @pSurface@ /must/ be
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'IOSSurfaceCreateInfoMVK', 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
createIOSSurfaceMVK :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkCreateIOSSurfaceMVK" "instance"
                       Instance
                    -> -- No documentation found for Nested "vkCreateIOSSurfaceMVK" "pCreateInfo"
                       IOSSurfaceCreateInfoMVK
                    -> -- No documentation found for Nested "vkCreateIOSSurfaceMVK" "pAllocator"
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io (SurfaceKHR)
createIOSSurfaceMVK instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateIOSSurfaceMVKPtr = pVkCreateIOSSurfaceMVK (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateIOSSurfaceMVKPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateIOSSurfaceMVK is null" Nothing Nothing
  let vkCreateIOSSurfaceMVK' = mkVkCreateIOSSurfaceMVK vkCreateIOSSurfaceMVKPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateIOSSurfaceMVK' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkIOSSurfaceCreateInfoMVK - Structure specifying parameters of a newly
-- created iOS surface object
--
-- == Valid Usage
--
-- -   #VUID-VkIOSSurfaceCreateInfoMVK-pView-04143# If @pView@ is a
--     'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer' object, it
--     /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer'.
--
-- -   #VUID-VkIOSSurfaceCreateInfoMVK-pView-01316# If @pView@ is a
--     @UIView@ object, it /must/ be a valid @UIView@, /must/ be backed by
--     a @CALayer@ object of type
--     'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer', and
--     'createIOSSurfaceMVK' /must/ be called on the main thread.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIOSSurfaceCreateInfoMVK-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK'
--
-- -   #VUID-VkIOSSurfaceCreateInfoMVK-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkIOSSurfaceCreateInfoMVK-flags-zerobitmask# @flags@ /must/ be
--     @0@
--
-- = See Also
--
-- 'IOSSurfaceCreateFlagsMVK',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createIOSSurfaceMVK'
data IOSSurfaceCreateInfoMVK = IOSSurfaceCreateInfoMVK
  { -- | @flags@ is reserved for future use.
    flags :: IOSSurfaceCreateFlagsMVK
  , -- | @pView@ is a reference to either a
    -- 'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer' object or a
    -- @UIView@ object.
    view :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IOSSurfaceCreateInfoMVK)
#endif
deriving instance Show IOSSurfaceCreateInfoMVK

instance ToCStruct IOSSurfaceCreateInfoMVK where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IOSSurfaceCreateInfoMVK{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr IOSSurfaceCreateFlagsMVK)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (view)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct IOSSurfaceCreateInfoMVK where
  peekCStruct p = do
    flags <- peek @IOSSurfaceCreateFlagsMVK ((p `plusPtr` 16 :: Ptr IOSSurfaceCreateFlagsMVK))
    pView <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ IOSSurfaceCreateInfoMVK
             flags pView

instance Storable IOSSurfaceCreateInfoMVK where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IOSSurfaceCreateInfoMVK where
  zero = IOSSurfaceCreateInfoMVK
           zero
           zero


-- | VkIOSSurfaceCreateFlagsMVK - Reserved for future use
--
-- = Description
--
-- 'IOSSurfaceCreateFlagsMVK' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'IOSSurfaceCreateInfoMVK'
newtype IOSSurfaceCreateFlagsMVK = IOSSurfaceCreateFlagsMVK Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show IOSSurfaceCreateFlagsMVK where
  showsPrec p = \case
    IOSSurfaceCreateFlagsMVK x -> showParen (p >= 11) (showString "IOSSurfaceCreateFlagsMVK 0x" . showHex x)

instance Read IOSSurfaceCreateFlagsMVK where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "IOSSurfaceCreateFlagsMVK")
                       v <- step readPrec
                       pure (IOSSurfaceCreateFlagsMVK v)))


type MVK_IOS_SURFACE_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_SPEC_VERSION"
pattern MVK_IOS_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern MVK_IOS_SURFACE_SPEC_VERSION = 3


type MVK_IOS_SURFACE_EXTENSION_NAME = "VK_MVK_ios_surface"

-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_EXTENSION_NAME"
pattern MVK_IOS_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MVK_IOS_SURFACE_EXTENSION_NAME = "VK_MVK_ios_surface"

