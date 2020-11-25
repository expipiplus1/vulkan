{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_metal_surface"
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateMetalSurfaceEXT))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateMetalSurfaceEXT
  :: FunPtr (Ptr Instance_T -> Ptr MetalSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr MetalSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateMetalSurfaceEXT"
createMetalSurfaceEXT :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkCreateMetalSurfaceEXT" "instance"
                         Instance
                      -> -- No documentation found for Nested "vkCreateMetalSurfaceEXT" "pCreateInfo"
                         MetalSurfaceCreateInfoEXT
                      -> -- No documentation found for Nested "vkCreateMetalSurfaceEXT" "pAllocator"
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
  r <- lift $ vkCreateMetalSurfaceEXT' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)



-- No documentation found for TopLevel "VkMetalSurfaceCreateInfoEXT"
data MetalSurfaceCreateInfoEXT = MetalSurfaceCreateInfoEXT
  { -- No documentation found for Nested "VkMetalSurfaceCreateInfoEXT" "flags"
    flags :: MetalSurfaceCreateFlagsEXT
  , -- No documentation found for Nested "VkMetalSurfaceCreateInfoEXT" "pLayer"
    layer :: Ptr CAMetalLayer
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MetalSurfaceCreateInfoEXT)
#endif
deriving instance Show MetalSurfaceCreateInfoEXT

instance ToCStruct MetalSurfaceCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
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


-- No documentation found for TopLevel "VkMetalSurfaceCreateFlagsEXT"
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

