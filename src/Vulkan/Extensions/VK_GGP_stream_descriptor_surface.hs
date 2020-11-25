{-# language CPP #-}
-- No documentation found for Chapter "VK_GGP_stream_descriptor_surface"
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateStreamDescriptorSurfaceGGP))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateStreamDescriptorSurfaceGGP
  :: FunPtr (Ptr Instance_T -> Ptr StreamDescriptorSurfaceCreateInfoGGP -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr StreamDescriptorSurfaceCreateInfoGGP -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateStreamDescriptorSurfaceGGP"
createStreamDescriptorSurfaceGGP :: forall io
                                  . (MonadIO io)
                                 => -- No documentation found for Nested "vkCreateStreamDescriptorSurfaceGGP" "instance"
                                    Instance
                                 -> -- No documentation found for Nested "vkCreateStreamDescriptorSurfaceGGP" "pCreateInfo"
                                    StreamDescriptorSurfaceCreateInfoGGP
                                 -> -- No documentation found for Nested "vkCreateStreamDescriptorSurfaceGGP" "pAllocator"
                                    ("allocator" ::: Maybe AllocationCallbacks)
                                 -> io (SurfaceKHR)
createStreamDescriptorSurfaceGGP instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateStreamDescriptorSurfaceGGPPtr = pVkCreateStreamDescriptorSurfaceGGP (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateStreamDescriptorSurfaceGGPPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateStreamDescriptorSurfaceGGP is null" Nothing Nothing
  let vkCreateStreamDescriptorSurfaceGGP' = mkVkCreateStreamDescriptorSurfaceGGP vkCreateStreamDescriptorSurfaceGGPPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateStreamDescriptorSurfaceGGP' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)



-- No documentation found for TopLevel "VkStreamDescriptorSurfaceCreateInfoGGP"
data StreamDescriptorSurfaceCreateInfoGGP = StreamDescriptorSurfaceCreateInfoGGP
  { -- No documentation found for Nested "VkStreamDescriptorSurfaceCreateInfoGGP" "flags"
    flags :: StreamDescriptorSurfaceCreateFlagsGGP
  , -- No documentation found for Nested "VkStreamDescriptorSurfaceCreateInfoGGP" "streamDescriptor"
    streamDescriptor :: GgpStreamDescriptor
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (StreamDescriptorSurfaceCreateInfoGGP)
#endif
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
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameStreamDescriptorSurfaceCreateFlagsGGP :: String
conNameStreamDescriptorSurfaceCreateFlagsGGP = "StreamDescriptorSurfaceCreateFlagsGGP"

enumPrefixStreamDescriptorSurfaceCreateFlagsGGP :: String
enumPrefixStreamDescriptorSurfaceCreateFlagsGGP = ""

showTableStreamDescriptorSurfaceCreateFlagsGGP :: [(StreamDescriptorSurfaceCreateFlagsGGP, String)]
showTableStreamDescriptorSurfaceCreateFlagsGGP = []


instance Show StreamDescriptorSurfaceCreateFlagsGGP where
showsPrec = enumShowsPrec enumPrefixStreamDescriptorSurfaceCreateFlagsGGP
                          showTableStreamDescriptorSurfaceCreateFlagsGGP
                          conNameStreamDescriptorSurfaceCreateFlagsGGP
                          (\(StreamDescriptorSurfaceCreateFlagsGGP x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read StreamDescriptorSurfaceCreateFlagsGGP where
  readPrec = enumReadPrec enumPrefixStreamDescriptorSurfaceCreateFlagsGGP
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

