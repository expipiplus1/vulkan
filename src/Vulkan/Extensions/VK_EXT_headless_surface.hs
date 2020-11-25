{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_headless_surface"
module Vulkan.Extensions.VK_EXT_headless_surface  ( createHeadlessSurfaceEXT
                                                  , HeadlessSurfaceCreateInfoEXT(..)
                                                  , HeadlessSurfaceCreateFlagsEXT(..)
                                                  , EXT_HEADLESS_SURFACE_SPEC_VERSION
                                                  , pattern EXT_HEADLESS_SURFACE_SPEC_VERSION
                                                  , EXT_HEADLESS_SURFACE_EXTENSION_NAME
                                                  , pattern EXT_HEADLESS_SURFACE_EXTENSION_NAME
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
import Vulkan.Dynamic (InstanceCmds(pVkCreateHeadlessSurfaceEXT))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateHeadlessSurfaceEXT
  :: FunPtr (Ptr Instance_T -> Ptr HeadlessSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr HeadlessSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateHeadlessSurfaceEXT"
createHeadlessSurfaceEXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCreateHeadlessSurfaceEXT" "instance"
                            Instance
                         -> -- No documentation found for Nested "vkCreateHeadlessSurfaceEXT" "pCreateInfo"
                            HeadlessSurfaceCreateInfoEXT
                         -> -- No documentation found for Nested "vkCreateHeadlessSurfaceEXT" "pAllocator"
                            ("allocator" ::: Maybe AllocationCallbacks)
                         -> io (SurfaceKHR)
createHeadlessSurfaceEXT instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateHeadlessSurfaceEXTPtr = pVkCreateHeadlessSurfaceEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateHeadlessSurfaceEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateHeadlessSurfaceEXT is null" Nothing Nothing
  let vkCreateHeadlessSurfaceEXT' = mkVkCreateHeadlessSurfaceEXT vkCreateHeadlessSurfaceEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateHeadlessSurfaceEXT' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)



-- No documentation found for TopLevel "VkHeadlessSurfaceCreateInfoEXT"
data HeadlessSurfaceCreateInfoEXT = HeadlessSurfaceCreateInfoEXT
  { -- No documentation found for Nested "VkHeadlessSurfaceCreateInfoEXT" "flags"
    flags :: HeadlessSurfaceCreateFlagsEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HeadlessSurfaceCreateInfoEXT)
#endif
deriving instance Show HeadlessSurfaceCreateInfoEXT

instance ToCStruct HeadlessSurfaceCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HeadlessSurfaceCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HeadlessSurfaceCreateFlagsEXT)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct HeadlessSurfaceCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @HeadlessSurfaceCreateFlagsEXT ((p `plusPtr` 16 :: Ptr HeadlessSurfaceCreateFlagsEXT))
    pure $ HeadlessSurfaceCreateInfoEXT
             flags


instance Storable HeadlessSurfaceCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HeadlessSurfaceCreateInfoEXT where
  zero = HeadlessSurfaceCreateInfoEXT
           zero


-- No documentation found for TopLevel "VkHeadlessSurfaceCreateFlagsEXT"
newtype HeadlessSurfaceCreateFlagsEXT = HeadlessSurfaceCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameHeadlessSurfaceCreateFlagsEXT :: String
conNameHeadlessSurfaceCreateFlagsEXT = "HeadlessSurfaceCreateFlagsEXT"

enumPrefixHeadlessSurfaceCreateFlagsEXT :: String
enumPrefixHeadlessSurfaceCreateFlagsEXT = ""

showTableHeadlessSurfaceCreateFlagsEXT :: [(HeadlessSurfaceCreateFlagsEXT, String)]
showTableHeadlessSurfaceCreateFlagsEXT = []


instance Show HeadlessSurfaceCreateFlagsEXT where
showsPrec = enumShowsPrec enumPrefixHeadlessSurfaceCreateFlagsEXT
                          showTableHeadlessSurfaceCreateFlagsEXT
                          conNameHeadlessSurfaceCreateFlagsEXT
                          (\(HeadlessSurfaceCreateFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read HeadlessSurfaceCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixHeadlessSurfaceCreateFlagsEXT
                          showTableHeadlessSurfaceCreateFlagsEXT
                          conNameHeadlessSurfaceCreateFlagsEXT
                          HeadlessSurfaceCreateFlagsEXT


type EXT_HEADLESS_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_HEADLESS_SURFACE_SPEC_VERSION"
pattern EXT_HEADLESS_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_HEADLESS_SURFACE_SPEC_VERSION = 1


type EXT_HEADLESS_SURFACE_EXTENSION_NAME = "VK_EXT_headless_surface"

-- No documentation found for TopLevel "VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME"
pattern EXT_HEADLESS_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_HEADLESS_SURFACE_EXTENSION_NAME = "VK_EXT_headless_surface"

