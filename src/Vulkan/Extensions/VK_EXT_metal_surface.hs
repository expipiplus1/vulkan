{-# language CPP #-}
-- | = Name
--
-- VK_EXT_metal_surface - instance extension
--
-- == VK_EXT_metal_surface
--
-- [__Name String__]
--     @VK_EXT_metal_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     218
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Contact__]
--
--     -   Dzmitry Malyshau
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_metal_surface:%20&body=@kvark%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Dzmitry Malyshau, Mozilla Corp.
--
-- == Description
--
-- The @VK_EXT_metal_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) from 'CAMetalLayer',
-- which is the native rendering surface of Apple’s Metal framework.
--
-- == New Base Types
--
-- -   'CAMetalLayer'
--
-- == New Commands
--
-- -   'createMetalSurfaceEXT'
--
-- == New Structures
--
-- -   'MetalSurfaceCreateInfoEXT'
--
-- == New Bitmasks
--
-- -   'MetalSurfaceCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_METAL_SURFACE_EXTENSION_NAME'
--
-- -   'EXT_METAL_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-10-01 (Dzmitry Malyshau)
--
--     -   Initial version
--
-- = See Also
--
-- 'CAMetalLayer', 'MetalSurfaceCreateFlagsEXT',
-- 'MetalSurfaceCreateInfoEXT', 'createMetalSurfaceEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_metal_surface  ( createMetalSurfaceEXT
                                               , MetalSurfaceCreateInfoEXT(..)
                                               , MetalSurfaceCreateFlagsEXT(..)
                                               , EXT_METAL_SURFACE_SPEC_VERSION
                                               , pattern EXT_METAL_SURFACE_SPEC_VERSION
                                               , EXT_METAL_SURFACE_EXTENSION_NAME
                                               , pattern EXT_METAL_SURFACE_EXTENSION_NAME
                                               , CAMetalLayer
                                               , SurfaceKHR(..)
                                               ) where

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
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateMetalSurfaceEXT))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateMetalSurfaceEXT
  :: FunPtr (Ptr Instance_T -> Ptr MetalSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr MetalSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateMetalSurfaceEXT - Create a VkSurfaceKHR object for CAMetalLayer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateMetalSurfaceEXT-instance-parameter# @instance@ /must/
--     be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateMetalSurfaceEXT-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'MetalSurfaceCreateInfoEXT'
--     structure
--
-- -   #VUID-vkCreateMetalSurfaceEXT-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateMetalSurfaceEXT-pSurface-parameter# @pSurface@ /must/
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
-- 'Vulkan.Core10.Handles.Instance', 'MetalSurfaceCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
createMetalSurfaceEXT :: forall io
                       . (MonadIO io)
                      => -- | @instance@ is the instance with which to associate the surface.
                         Instance
                      -> -- | @pCreateInfo@ is a pointer to a 'MetalSurfaceCreateInfoEXT' structure
                         -- specifying parameters affecting the creation of the surface object.
                         MetalSurfaceCreateInfoEXT
                      -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                         -- surface object when there is no more specific allocator available (see
                         -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                         ("allocator" ::: Maybe AllocationCallbacks)
                      -> io (SurfaceKHR)
createMetalSurfaceEXT instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateMetalSurfaceEXTPtr = pVkCreateMetalSurfaceEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateMetalSurfaceEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateMetalSurfaceEXT is null" Nothing Nothing
  let vkCreateMetalSurfaceEXT' = mkVkCreateMetalSurfaceEXT vkCreateMetalSurfaceEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateMetalSurfaceEXT" (vkCreateMetalSurfaceEXT' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkMetalSurfaceCreateInfoEXT - Structure specifying parameters of a newly
-- created Metal surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'MetalSurfaceCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createMetalSurfaceEXT'
data MetalSurfaceCreateInfoEXT = MetalSurfaceCreateInfoEXT
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkMetalSurfaceCreateInfoEXT-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: MetalSurfaceCreateFlagsEXT
  , -- | @pLayer@ is a reference to a 'CAMetalLayer' object representing a
    -- renderable surface.
    layer :: Ptr CAMetalLayer
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MetalSurfaceCreateInfoEXT)
#endif
deriving instance Show MetalSurfaceCreateInfoEXT

instance ToCStruct MetalSurfaceCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MetalSurfaceCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MetalSurfaceCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr CAMetalLayer))) (layer)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr CAMetalLayer))) (zero)
    f

instance FromCStruct MetalSurfaceCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @MetalSurfaceCreateFlagsEXT ((p `plusPtr` 16 :: Ptr MetalSurfaceCreateFlagsEXT))
    pLayer <- peek @(Ptr CAMetalLayer) ((p `plusPtr` 24 :: Ptr (Ptr CAMetalLayer)))
    pure $ MetalSurfaceCreateInfoEXT
             flags pLayer

instance Storable MetalSurfaceCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MetalSurfaceCreateInfoEXT where
  zero = MetalSurfaceCreateInfoEXT
           zero
           zero


-- | VkMetalSurfaceCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'MetalSurfaceCreateFlagsEXT' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- 'MetalSurfaceCreateInfoEXT'
newtype MetalSurfaceCreateFlagsEXT = MetalSurfaceCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameMetalSurfaceCreateFlagsEXT :: String
conNameMetalSurfaceCreateFlagsEXT = "MetalSurfaceCreateFlagsEXT"

enumPrefixMetalSurfaceCreateFlagsEXT :: String
enumPrefixMetalSurfaceCreateFlagsEXT = ""

showTableMetalSurfaceCreateFlagsEXT :: [(MetalSurfaceCreateFlagsEXT, String)]
showTableMetalSurfaceCreateFlagsEXT = []

instance Show MetalSurfaceCreateFlagsEXT where
  showsPrec = enumShowsPrec enumPrefixMetalSurfaceCreateFlagsEXT
                            showTableMetalSurfaceCreateFlagsEXT
                            conNameMetalSurfaceCreateFlagsEXT
                            (\(MetalSurfaceCreateFlagsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read MetalSurfaceCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixMetalSurfaceCreateFlagsEXT
                          showTableMetalSurfaceCreateFlagsEXT
                          conNameMetalSurfaceCreateFlagsEXT
                          MetalSurfaceCreateFlagsEXT


type EXT_METAL_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_METAL_SURFACE_SPEC_VERSION"
pattern EXT_METAL_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_METAL_SURFACE_SPEC_VERSION = 1


type EXT_METAL_SURFACE_EXTENSION_NAME = "VK_EXT_metal_surface"

-- No documentation found for TopLevel "VK_EXT_METAL_SURFACE_EXTENSION_NAME"
pattern EXT_METAL_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_METAL_SURFACE_EXTENSION_NAME = "VK_EXT_metal_surface"


data CAMetalLayer

