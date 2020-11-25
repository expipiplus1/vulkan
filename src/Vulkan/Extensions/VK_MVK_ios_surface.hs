{-# language CPP #-}
-- No documentation found for Chapter "VK_MVK_ios_surface"
module Vulkan.Extensions.VK_MVK_ios_surface  ( createIOSSurfaceMVK
                                             , IOSSurfaceCreateInfoMVK(..)
                                             , IOSSurfaceCreateFlagsMVK(..)
                                             , MVK_IOS_SURFACE_SPEC_VERSION
                                             , pattern MVK_IOS_SURFACE_SPEC_VERSION
                                             , MVK_IOS_SURFACE_EXTENSION_NAME
                                             , pattern MVK_IOS_SURFACE_EXTENSION_NAME
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

-- No documentation found for TopLevel "vkCreateIOSSurfaceMVK"
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



-- No documentation found for TopLevel "VkIOSSurfaceCreateInfoMVK"
data IOSSurfaceCreateInfoMVK = IOSSurfaceCreateInfoMVK
  { -- No documentation found for Nested "VkIOSSurfaceCreateInfoMVK" "flags"
    flags :: IOSSurfaceCreateFlagsMVK
  , -- No documentation found for Nested "VkIOSSurfaceCreateInfoMVK" "pView"
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


-- No documentation found for TopLevel "VkIOSSurfaceCreateFlagsMVK"
newtype IOSSurfaceCreateFlagsMVK = IOSSurfaceCreateFlagsMVK Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameIOSSurfaceCreateFlagsMVK :: String
conNameIOSSurfaceCreateFlagsMVK = "IOSSurfaceCreateFlagsMVK"

enumPrefixIOSSurfaceCreateFlagsMVK :: String
enumPrefixIOSSurfaceCreateFlagsMVK = ""

showTableIOSSurfaceCreateFlagsMVK :: [(IOSSurfaceCreateFlagsMVK, String)]
showTableIOSSurfaceCreateFlagsMVK = []


instance Show IOSSurfaceCreateFlagsMVK where
showsPrec = enumShowsPrec enumPrefixIOSSurfaceCreateFlagsMVK
                          showTableIOSSurfaceCreateFlagsMVK
                          conNameIOSSurfaceCreateFlagsMVK
                          (\(IOSSurfaceCreateFlagsMVK x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read IOSSurfaceCreateFlagsMVK where
  readPrec = enumReadPrec enumPrefixIOSSurfaceCreateFlagsMVK
                          showTableIOSSurfaceCreateFlagsMVK
                          conNameIOSSurfaceCreateFlagsMVK
                          IOSSurfaceCreateFlagsMVK


type MVK_IOS_SURFACE_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_SPEC_VERSION"
pattern MVK_IOS_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern MVK_IOS_SURFACE_SPEC_VERSION = 3


type MVK_IOS_SURFACE_EXTENSION_NAME = "VK_MVK_ios_surface"

-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_EXTENSION_NAME"
pattern MVK_IOS_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MVK_IOS_SURFACE_EXTENSION_NAME = "VK_MVK_ios_surface"

