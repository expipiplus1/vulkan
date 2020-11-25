{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_android_surface"
module Vulkan.Extensions.VK_KHR_android_surface  ( createAndroidSurfaceKHR
                                                 , AndroidSurfaceCreateInfoKHR(..)
                                                 , AndroidSurfaceCreateFlagsKHR(..)
                                                 , KHR_ANDROID_SURFACE_SPEC_VERSION
                                                 , pattern KHR_ANDROID_SURFACE_SPEC_VERSION
                                                 , KHR_ANDROID_SURFACE_EXTENSION_NAME
                                                 , pattern KHR_ANDROID_SURFACE_EXTENSION_NAME
                                                 , ANativeWindow
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateAndroidSurfaceKHR))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAndroidSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr AndroidSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr AndroidSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateAndroidSurfaceKHR"
createAndroidSurfaceKHR :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCreateAndroidSurfaceKHR" "instance"
                           Instance
                        -> -- No documentation found for Nested "vkCreateAndroidSurfaceKHR" "pCreateInfo"
                           AndroidSurfaceCreateInfoKHR
                        -> -- No documentation found for Nested "vkCreateAndroidSurfaceKHR" "pAllocator"
                           ("allocator" ::: Maybe AllocationCallbacks)
                        -> io (SurfaceKHR)
createAndroidSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateAndroidSurfaceKHRPtr = pVkCreateAndroidSurfaceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateAndroidSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateAndroidSurfaceKHR is null" Nothing Nothing
  let vkCreateAndroidSurfaceKHR' = mkVkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateAndroidSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)



-- No documentation found for TopLevel "VkAndroidSurfaceCreateInfoKHR"
data AndroidSurfaceCreateInfoKHR = AndroidSurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkAndroidSurfaceCreateInfoKHR" "flags"
    flags :: AndroidSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkAndroidSurfaceCreateInfoKHR" "window"
    window :: Ptr ANativeWindow
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AndroidSurfaceCreateInfoKHR)
#endif
deriving instance Show AndroidSurfaceCreateInfoKHR

instance ToCStruct AndroidSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AndroidSurfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AndroidSurfaceCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ANativeWindow))) (window)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ANativeWindow))) (zero)
    f

instance FromCStruct AndroidSurfaceCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @AndroidSurfaceCreateFlagsKHR ((p `plusPtr` 16 :: Ptr AndroidSurfaceCreateFlagsKHR))
    window <- peek @(Ptr ANativeWindow) ((p `plusPtr` 24 :: Ptr (Ptr ANativeWindow)))
    pure $ AndroidSurfaceCreateInfoKHR
             flags window


instance Storable AndroidSurfaceCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AndroidSurfaceCreateInfoKHR where
  zero = AndroidSurfaceCreateInfoKHR
           zero
           zero


-- No documentation found for TopLevel "VkAndroidSurfaceCreateFlagsKHR"
newtype AndroidSurfaceCreateFlagsKHR = AndroidSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameAndroidSurfaceCreateFlagsKHR :: String
conNameAndroidSurfaceCreateFlagsKHR = "AndroidSurfaceCreateFlagsKHR"

enumPrefixAndroidSurfaceCreateFlagsKHR :: String
enumPrefixAndroidSurfaceCreateFlagsKHR = ""

showTableAndroidSurfaceCreateFlagsKHR :: [(AndroidSurfaceCreateFlagsKHR, String)]
showTableAndroidSurfaceCreateFlagsKHR = []


instance Show AndroidSurfaceCreateFlagsKHR where
showsPrec = enumShowsPrec enumPrefixAndroidSurfaceCreateFlagsKHR
                          showTableAndroidSurfaceCreateFlagsKHR
                          conNameAndroidSurfaceCreateFlagsKHR
                          (\(AndroidSurfaceCreateFlagsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read AndroidSurfaceCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixAndroidSurfaceCreateFlagsKHR
                          showTableAndroidSurfaceCreateFlagsKHR
                          conNameAndroidSurfaceCreateFlagsKHR
                          AndroidSurfaceCreateFlagsKHR


type KHR_ANDROID_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_SPEC_VERSION"
pattern KHR_ANDROID_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_ANDROID_SURFACE_SPEC_VERSION = 6


type KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"

-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_EXTENSION_NAME"
pattern KHR_ANDROID_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"


data ANativeWindow

