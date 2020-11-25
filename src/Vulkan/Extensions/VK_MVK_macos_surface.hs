{-# language CPP #-}
-- | = Name
--
-- VK_MVK_macos_surface - instance extension
--
-- == VK_MVK_macos_surface
--
-- [__Name String__]
--     @VK_MVK_macos_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     124
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Deprecation state__]
--
--     -   /Deprecated/ by @VK_EXT_metal_surface@ extension
--
-- [__Contact__]
--
--     -   Bill Hollings
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_MVK_macos_surface:%20&body=@billhollings%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-31
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Bill Hollings, The Brenwill Workshop Ltd.
--
-- == Description
--
-- The @VK_MVK_macos_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) based on an @NSView@,
-- the native surface type of macOS, which is underpinned by a
-- 'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer', to support
-- rendering to the surface using Apple’s Metal framework.
--
-- == Deprecation by @VK_EXT_metal_surface@
--
-- The @VK_MVK_macos_surface@ extension is considered deprecated and has
-- been superseded by the @VK_EXT_metal_surface@ extension.
--
-- == New Commands
--
-- -   'createMacOSSurfaceMVK'
--
-- == New Structures
--
-- -   'MacOSSurfaceCreateInfoMVK'
--
-- == New Bitmasks
--
-- -   'MacOSSurfaceCreateFlagsMVK'
--
-- == New Enum Constants
--
-- -   'MVK_MACOS_SURFACE_EXTENSION_NAME'
--
-- -   'MVK_MACOS_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK'
--
-- == Version History
--
-- -   Revision 1, 2017-02-15 (Bill Hollings)
--
--     -   Initial draft.
--
-- -   Revision 2, 2017-02-24 (Bill Hollings)
--
--     -   Minor syntax fix to emphasize firm requirement for @NSView@ to
--         be backed by a
--         'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer'.
--
-- -   Revision 3, 2020-07-31 (Bill Hollings)
--
--     -   Update documentation on requirements for @NSView@.
--
--     -   Mark as deprecated by @VK_EXT_metal_surface@.
--
-- = See Also
--
-- 'MacOSSurfaceCreateFlagsMVK', 'MacOSSurfaceCreateInfoMVK',
-- 'createMacOSSurfaceMVK'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_MVK_macos_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_MVK_macos_surface  ( createMacOSSurfaceMVK
                                               , MacOSSurfaceCreateInfoMVK(..)
                                               , MacOSSurfaceCreateFlagsMVK(..)
                                               , MVK_MACOS_SURFACE_SPEC_VERSION
                                               , pattern MVK_MACOS_SURFACE_SPEC_VERSION
                                               , MVK_MACOS_SURFACE_EXTENSION_NAME
                                               , pattern MVK_MACOS_SURFACE_EXTENSION_NAME
                                               , SurfaceKHR(..)
                                               ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
-- = Parameters
--
-- Note
--
-- The 'createMacOSSurfaceMVK' function is considered deprecated and has
-- been superseded by
-- 'Vulkan.Extensions.VK_EXT_metal_surface.createMetalSurfaceEXT' from the
-- @VK_EXT_metal_surface@ extension.
--
-- = Description
--
-- -   @instance@ is the instance with which to associate the surface.
--
-- -   @pCreateInfo@ is a pointer to a 'MacOSSurfaceCreateInfoMVK'
--     structure containing parameters affecting the creation of the
--     surface object.
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
-- -   #VUID-vkCreateMacOSSurfaceMVK-instance-parameter# @instance@ /must/
--     be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateMacOSSurfaceMVK-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'MacOSSurfaceCreateInfoMVK'
--     structure
--
-- -   #VUID-vkCreateMacOSSurfaceMVK-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateMacOSSurfaceMVK-pSurface-parameter# @pSurface@ /must/
--     be a valid pointer to a 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
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
                      => -- No documentation found for Nested "vkCreateMacOSSurfaceMVK" "instance"
                         Instance
                      -> -- No documentation found for Nested "vkCreateMacOSSurfaceMVK" "pCreateInfo"
                         MacOSSurfaceCreateInfoMVK
                      -> -- No documentation found for Nested "vkCreateMacOSSurfaceMVK" "pAllocator"
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
-- == Valid Usage
--
-- -   #VUID-VkMacOSSurfaceCreateInfoMVK-pView-04144# If @pView@ is a
--     'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer' object, it
--     /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer'.
--
-- -   #VUID-VkMacOSSurfaceCreateInfoMVK-pView-01317# If @pView@ is an
--     @NSView@ object, it /must/ be a valid @NSView@, /must/ be backed by
--     a @CALayer@ object of type
--     'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer', and
--     'createMacOSSurfaceMVK' /must/ be called on the main thread.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMacOSSurfaceCreateInfoMVK-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK'
--
-- -   #VUID-VkMacOSSurfaceCreateInfoMVK-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkMacOSSurfaceCreateInfoMVK-flags-zerobitmask# @flags@ /must/
--     be @0@
--
-- = See Also
--
-- 'MacOSSurfaceCreateFlagsMVK',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createMacOSSurfaceMVK'
data MacOSSurfaceCreateInfoMVK = MacOSSurfaceCreateInfoMVK
  { -- | @flags@ is reserved for future use.
    flags :: MacOSSurfaceCreateFlagsMVK
  , -- | @pView@ is a reference to either a
    -- 'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer' object or an
    -- @NSView@ object.
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


-- | VkMacOSSurfaceCreateFlagsMVK - Reserved for future use
--
-- = Description
--
-- 'MacOSSurfaceCreateFlagsMVK' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- 'MacOSSurfaceCreateInfoMVK'
newtype MacOSSurfaceCreateFlagsMVK = MacOSSurfaceCreateFlagsMVK Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameMacOSSurfaceCreateFlagsMVK :: String
conNameMacOSSurfaceCreateFlagsMVK = "MacOSSurfaceCreateFlagsMVK"

enumPrefixMacOSSurfaceCreateFlagsMVK :: String
enumPrefixMacOSSurfaceCreateFlagsMVK = ""

showTableMacOSSurfaceCreateFlagsMVK :: [(MacOSSurfaceCreateFlagsMVK, String)]
showTableMacOSSurfaceCreateFlagsMVK = []

instance Show MacOSSurfaceCreateFlagsMVK where
  showsPrec = enumShowsPrec enumPrefixMacOSSurfaceCreateFlagsMVK
                            showTableMacOSSurfaceCreateFlagsMVK
                            conNameMacOSSurfaceCreateFlagsMVK
                            (\(MacOSSurfaceCreateFlagsMVK x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read MacOSSurfaceCreateFlagsMVK where
  readPrec = enumReadPrec enumPrefixMacOSSurfaceCreateFlagsMVK
                          showTableMacOSSurfaceCreateFlagsMVK
                          conNameMacOSSurfaceCreateFlagsMVK
                          MacOSSurfaceCreateFlagsMVK


type MVK_MACOS_SURFACE_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_SPEC_VERSION"
pattern MVK_MACOS_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern MVK_MACOS_SURFACE_SPEC_VERSION = 3


type MVK_MACOS_SURFACE_EXTENSION_NAME = "VK_MVK_macos_surface"

-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_EXTENSION_NAME"
pattern MVK_MACOS_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MVK_MACOS_SURFACE_EXTENSION_NAME = "VK_MVK_macos_surface"

