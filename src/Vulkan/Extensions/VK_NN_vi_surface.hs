{-# language CPP #-}
-- No documentation found for Chapter "VK_NN_vi_surface"
module Vulkan.Extensions.VK_NN_vi_surface  ( createViSurfaceNN
                                           , ViSurfaceCreateInfoNN(..)
                                           , ViSurfaceCreateFlagsNN(..)
                                           , NN_VI_SURFACE_SPEC_VERSION
                                           , pattern NN_VI_SURFACE_SPEC_VERSION
                                           , NN_VI_SURFACE_EXTENSION_NAME
                                           , pattern NN_VI_SURFACE_EXTENSION_NAME
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateViSurfaceNN))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateViSurfaceNN
  :: FunPtr (Ptr Instance_T -> Ptr ViSurfaceCreateInfoNN -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr ViSurfaceCreateInfoNN -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateViSurfaceNN"
createViSurfaceNN :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCreateViSurfaceNN" "instance"
                     Instance
                  -> -- No documentation found for Nested "vkCreateViSurfaceNN" "pCreateInfo"
                     ViSurfaceCreateInfoNN
                  -> -- No documentation found for Nested "vkCreateViSurfaceNN" "pAllocator"
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (SurfaceKHR)
createViSurfaceNN instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateViSurfaceNNPtr = pVkCreateViSurfaceNN (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateViSurfaceNNPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateViSurfaceNN is null" Nothing Nothing
  let vkCreateViSurfaceNN' = mkVkCreateViSurfaceNN vkCreateViSurfaceNNPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateViSurfaceNN' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)



-- No documentation found for TopLevel "VkViSurfaceCreateInfoNN"
data ViSurfaceCreateInfoNN = ViSurfaceCreateInfoNN
  { -- No documentation found for Nested "VkViSurfaceCreateInfoNN" "flags"
    flags :: ViSurfaceCreateFlagsNN
  , -- No documentation found for Nested "VkViSurfaceCreateInfoNN" "window"
    window :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViSurfaceCreateInfoNN)
#endif
deriving instance Show ViSurfaceCreateInfoNN

instance ToCStruct ViSurfaceCreateInfoNN where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
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


-- No documentation found for TopLevel "VkViSurfaceCreateFlagsNN"
newtype ViSurfaceCreateFlagsNN = ViSurfaceCreateFlagsNN Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameViSurfaceCreateFlagsNN :: String
conNameViSurfaceCreateFlagsNN = "ViSurfaceCreateFlagsNN"

enumPrefixViSurfaceCreateFlagsNN :: String
enumPrefixViSurfaceCreateFlagsNN = ""

showTableViSurfaceCreateFlagsNN :: [(ViSurfaceCreateFlagsNN, String)]
showTableViSurfaceCreateFlagsNN = []


instance Show ViSurfaceCreateFlagsNN where
showsPrec = enumShowsPrec enumPrefixViSurfaceCreateFlagsNN
                          showTableViSurfaceCreateFlagsNN
                          conNameViSurfaceCreateFlagsNN
                          (\(ViSurfaceCreateFlagsNN x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ViSurfaceCreateFlagsNN where
  readPrec = enumReadPrec enumPrefixViSurfaceCreateFlagsNN
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

