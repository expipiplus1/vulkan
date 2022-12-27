{-# language CPP #-}
-- | = Name
--
-- VK_GGP_stream_descriptor_surface - instance extension
--
-- == VK_GGP_stream_descriptor_surface
--
-- [__Name String__]
--     @VK_GGP_stream_descriptor_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     50
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@ to be enabled
--
-- [__Contact__]
--
--     -   Jean-Francois Roy
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_GGP_stream_descriptor_surface] @jfroy%0A*Here describe the issue or question you have about the VK_GGP_stream_descriptor_surface extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jean-Francois Roy, Google
--
--     -   Brad Grantham, Google
--
--     -   Connor Smith, Google
--
--     -   Cort Stratton, Google
--
--     -   Hai Nguyen, Google
--
--     -   Ian Elliott, Google
--
--     -   Jesse Hall, Google
--
--     -   Jim Ray, Google
--
--     -   Katherine Wu, Google
--
--     -   Kaye Mason, Google
--
--     -   Kuangye Guo, Google
--
--     -   Mark Segal, Google
--
--     -   Nicholas Vining, Google
--
--     -   Paul Lalonde, Google
--
--     -   Richard O’Grady, Google
--
-- == Description
--
-- The @VK_GGP_stream_descriptor_surface@ extension is an instance
-- extension. It provides a mechanism to create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object (defined by the
-- @VK_KHR_surface@ extension) that refers to a Google Games Platform
-- 'GgpStreamDescriptor'.
--
-- == New Commands
--
-- -   'createStreamDescriptorSurfaceGGP'
--
-- == New Structures
--
-- -   'StreamDescriptorSurfaceCreateInfoGGP'
--
-- == New Bitmasks
--
-- -   'StreamDescriptorSurfaceCreateFlagsGGP'
--
-- == New Enum Constants
--
-- -   'GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME'
--
-- -   'GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP'
--
-- == Version History
--
-- -   Revision 1, 2018-11-26 (Jean-Francois Roy)
--
--     -   Initial revision.
--
-- == See Also
--
-- 'StreamDescriptorSurfaceCreateFlagsGGP',
-- 'StreamDescriptorSurfaceCreateInfoGGP',
-- 'createStreamDescriptorSurfaceGGP'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_GGP_stream_descriptor_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GGP_stream_descriptor_surface  ( createStreamDescriptorSurfaceGGP
                                                           , StreamDescriptorSurfaceCreateInfoGGP(..)
                                                           , StreamDescriptorSurfaceCreateFlagsGGP(..)
                                                           , GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
                                                           , pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION
                                                           , GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
                                                           , pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME
                                                           , GgpStreamDescriptor
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
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Bits (Bits)
import GHC.Bits (FiniteBits)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Handles (Instance(Instance))
import Vulkan.Dynamic (InstanceCmds(pVkCreateStreamDescriptorSurfaceGGP))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateStreamDescriptorSurfaceGGP
  :: FunPtr (Ptr Instance_T -> Ptr StreamDescriptorSurfaceCreateInfoGGP -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr StreamDescriptorSurfaceCreateInfoGGP -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateStreamDescriptorSurfaceGGP - Create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object for a Google Games
-- Platform stream
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateStreamDescriptorSurfaceGGP-instance-parameter#
--     @instance@ /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateStreamDescriptorSurfaceGGP-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'StreamDescriptorSurfaceCreateInfoGGP' structure
--
-- -   #VUID-vkCreateStreamDescriptorSurfaceGGP-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateStreamDescriptorSurfaceGGP-pSurface-parameter#
--     @pSurface@ /must/ be a valid pointer to a
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_GGP_stream_descriptor_surface VK_GGP_stream_descriptor_surface>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance',
-- 'StreamDescriptorSurfaceCreateInfoGGP',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
createStreamDescriptorSurfaceGGP :: forall io
                                  . (MonadIO io)
                                 => -- | @instance@ is the instance to associate with the surface.
                                    Instance
                                 -> -- | @pCreateInfo@ is a pointer to a 'StreamDescriptorSurfaceCreateInfoGGP'
                                    -- structure containing parameters that affect the creation of the surface
                                    -- object.
                                    StreamDescriptorSurfaceCreateInfoGGP
                                 -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                                    -- surface object when there is no more specific allocator available (see
                                    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                                    ("allocator" ::: Maybe AllocationCallbacks)
                                 -> io (SurfaceKHR)
createStreamDescriptorSurfaceGGP instance'
                                   createInfo
                                   allocator = liftIO . evalContT $ do
  let vkCreateStreamDescriptorSurfaceGGPPtr = pVkCreateStreamDescriptorSurfaceGGP (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (vkCreateStreamDescriptorSurfaceGGPPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateStreamDescriptorSurfaceGGP is null" Nothing Nothing
  let vkCreateStreamDescriptorSurfaceGGP' = mkVkCreateStreamDescriptorSurfaceGGP vkCreateStreamDescriptorSurfaceGGPPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateStreamDescriptorSurfaceGGP" (vkCreateStreamDescriptorSurfaceGGP'
                                                                       (instanceHandle (instance'))
                                                                       pCreateInfo
                                                                       pAllocator
                                                                       (pPSurface))
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_GGP_stream_descriptor_surface VK_GGP_stream_descriptor_surface>,
-- 'StreamDescriptorSurfaceCreateFlagsGGP',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createStreamDescriptorSurfaceGGP'
data StreamDescriptorSurfaceCreateInfoGGP = StreamDescriptorSurfaceCreateInfoGGP
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkStreamDescriptorSurfaceCreateInfoGGP-flags-zerobitmask# @flags@
    -- /must/ be @0@
    flags :: StreamDescriptorSurfaceCreateFlagsGGP
  , -- | @streamDescriptor@ is a 'GgpStreamDescriptor' referring to the GGP
    -- stream descriptor to associate with the surface.
    --
    -- #VUID-VkStreamDescriptorSurfaceCreateInfoGGP-streamDescriptor-02681#
    -- @streamDescriptor@ /must/ be a valid 'GgpStreamDescriptor'
    streamDescriptor :: GgpStreamDescriptor
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (StreamDescriptorSurfaceCreateInfoGGP)
#endif
deriving instance Show StreamDescriptorSurfaceCreateInfoGGP

instance ToCStruct StreamDescriptorSurfaceCreateInfoGGP where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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


-- | VkStreamDescriptorSurfaceCreateFlagsGGP - Reserved for future use
--
-- = Description
--
-- 'StreamDescriptorSurfaceCreateFlagsGGP' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_GGP_stream_descriptor_surface VK_GGP_stream_descriptor_surface>,
-- 'StreamDescriptorSurfaceCreateInfoGGP'
newtype StreamDescriptorSurfaceCreateFlagsGGP = StreamDescriptorSurfaceCreateFlagsGGP Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameStreamDescriptorSurfaceCreateFlagsGGP :: String
conNameStreamDescriptorSurfaceCreateFlagsGGP = "StreamDescriptorSurfaceCreateFlagsGGP"

enumPrefixStreamDescriptorSurfaceCreateFlagsGGP :: String
enumPrefixStreamDescriptorSurfaceCreateFlagsGGP = ""

showTableStreamDescriptorSurfaceCreateFlagsGGP :: [(StreamDescriptorSurfaceCreateFlagsGGP, String)]
showTableStreamDescriptorSurfaceCreateFlagsGGP = []

instance Show StreamDescriptorSurfaceCreateFlagsGGP where
  showsPrec =
    enumShowsPrec
      enumPrefixStreamDescriptorSurfaceCreateFlagsGGP
      showTableStreamDescriptorSurfaceCreateFlagsGGP
      conNameStreamDescriptorSurfaceCreateFlagsGGP
      (\(StreamDescriptorSurfaceCreateFlagsGGP x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read StreamDescriptorSurfaceCreateFlagsGGP where
  readPrec =
    enumReadPrec
      enumPrefixStreamDescriptorSurfaceCreateFlagsGGP
      showTableStreamDescriptorSurfaceCreateFlagsGGP
      conNameStreamDescriptorSurfaceCreateFlagsGGP
      StreamDescriptorSurfaceCreateFlagsGGP

type GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION"
pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION = 1


type GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME = "VK_GGP_stream_descriptor_surface"

-- No documentation found for TopLevel "VK_GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME"
pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME = "VK_GGP_stream_descriptor_surface"


type GgpStreamDescriptor = Word32

