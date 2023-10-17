{-# language CPP #-}
-- | = Name
--
-- VK_NN_vi_surface - instance extension
--
-- == VK_NN_vi_surface
--
-- [__Name String__]
--     @VK_NN_vi_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     63
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--
-- [__Contact__]
--
--     -   Mathias Heyer <<data:image/png;base64, GitLab>>mheyer
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mathias Heyer, NVIDIA
--
--     -   Michael Chock, NVIDIA
--
--     -   Yasuhiro Yoshioka, Nintendo
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- The @VK_NN_vi_surface@ extension is an instance extension. It provides a
-- mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR' object
-- (defined by the @VK_KHR_surface@ extension) associated with an
-- @nn@::@vi@::@Layer@.
--
-- == New Commands
--
-- -   'createViSurfaceNN'
--
-- == New Structures
--
-- -   'ViSurfaceCreateInfoNN'
--
-- == New Bitmasks
--
-- -   'ViSurfaceCreateFlagsNN'
--
-- == New Enum Constants
--
-- -   'NN_VI_SURFACE_EXTENSION_NAME'
--
-- -   'NN_VI_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN'
--
-- == Issues
--
-- 1) Does VI need a way to query for compatibility between a particular
-- physical device (and queue family?) and a specific VI display?
--
-- __RESOLVED__: No. It is currently always assumed that the device and
-- display will always be compatible.
--
-- 2) 'ViSurfaceCreateInfoNN'::@pWindow@ is intended to store an
-- @nn@::@vi@::@NativeWindowHandle@, but its declared type is a bare
-- @void*@ to store the window handle. Why the discrepancy?
--
-- __RESOLVED__: It is for C compatibility. The definition for the VI
-- native window handle type is defined inside the @nn@::@vi@ C++
-- namespace. This prevents its use in C source files.
-- @nn@::@vi@::@NativeWindowHandle@ is always defined to be @void*@, so
-- this extension uses @void*@ to match.
--
-- == Version History
--
-- -   Revision 1, 2016-12-2 (Michael Chock)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'ViSurfaceCreateFlagsNN', 'ViSurfaceCreateInfoNN', 'createViSurfaceNN'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NN_vi_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NN_vi_surface  ( createViSurfaceNN
                                           , ViSurfaceCreateInfoNN(..)
                                           , ViSurfaceCreateFlagsNN(..)
                                           , NN_VI_SURFACE_SPEC_VERSION
                                           , pattern NN_VI_SURFACE_SPEC_VERSION
                                           , NN_VI_SURFACE_EXTENSION_NAME
                                           , pattern NN_VI_SURFACE_EXTENSION_NAME
                                           , SurfaceKHR(..)
                                           ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
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
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Handles (Instance(Instance))
import Vulkan.Dynamic (InstanceCmds(pVkCreateViSurfaceNN))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Exception (VulkanException(..))
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NN_vi_surface VK_NN_vi_surface>,
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
                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (SurfaceKHR)
createViSurfaceNN instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateViSurfaceNNPtr = pVkCreateViSurfaceNN (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (vkCreateViSurfaceNNPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateViSurfaceNN is null" Nothing Nothing
  let vkCreateViSurfaceNN' = mkVkCreateViSurfaceNN vkCreateViSurfaceNNPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateViSurfaceNN" (vkCreateViSurfaceNN'
                                                        (instanceHandle (instance'))
                                                        pCreateInfo
                                                        pAllocator
                                                        (pPSurface))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkViSurfaceCreateInfoNN - Structure specifying parameters of a newly
-- created VI surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NN_vi_surface VK_NN_vi_surface>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'ViSurfaceCreateFlagsNN', 'createViSurfaceNN'
data ViSurfaceCreateInfoNN = ViSurfaceCreateInfoNN
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkViSurfaceCreateInfoNN-flags-zerobitmask# @flags@ /must/ be @0@
    flags :: ViSurfaceCreateFlagsNN
  , -- | @window@ is the @nn@::@vi@::@NativeWindowHandle@ for the
    -- @nn@::@vi@::@Layer@ with which to associate the surface.
    --
    -- #VUID-VkViSurfaceCreateInfoNN-window-01318# @window@ /must/ be a valid
    -- @nn@::@vi@::@NativeWindowHandle@
    window :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViSurfaceCreateInfoNN)
#endif
deriving instance Show ViSurfaceCreateInfoNN

instance ToCStruct ViSurfaceCreateInfoNN where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NN_vi_surface VK_NN_vi_surface>,
-- 'ViSurfaceCreateInfoNN'
newtype ViSurfaceCreateFlagsNN = ViSurfaceCreateFlagsNN Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameViSurfaceCreateFlagsNN :: String
conNameViSurfaceCreateFlagsNN = "ViSurfaceCreateFlagsNN"

enumPrefixViSurfaceCreateFlagsNN :: String
enumPrefixViSurfaceCreateFlagsNN = ""

showTableViSurfaceCreateFlagsNN :: [(ViSurfaceCreateFlagsNN, String)]
showTableViSurfaceCreateFlagsNN = []

instance Show ViSurfaceCreateFlagsNN where
  showsPrec =
    enumShowsPrec
      enumPrefixViSurfaceCreateFlagsNN
      showTableViSurfaceCreateFlagsNN
      conNameViSurfaceCreateFlagsNN
      (\(ViSurfaceCreateFlagsNN x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ViSurfaceCreateFlagsNN where
  readPrec =
    enumReadPrec
      enumPrefixViSurfaceCreateFlagsNN
      showTableViSurfaceCreateFlagsNN
      conNameViSurfaceCreateFlagsNN
      ViSurfaceCreateFlagsNN

type NN_VI_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NN_VI_SURFACE_SPEC_VERSION"
pattern NN_VI_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern NN_VI_SURFACE_SPEC_VERSION = 1


type NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"

-- No documentation found for TopLevel "VK_NN_VI_SURFACE_EXTENSION_NAME"
pattern NN_VI_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"

