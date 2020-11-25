{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_directfb_surface"
module Vulkan.Extensions.VK_EXT_directfb_surface  ( createDirectFBSurfaceEXT
                                                  , getPhysicalDeviceDirectFBPresentationSupportEXT
                                                  , DirectFBSurfaceCreateInfoEXT(..)
                                                  , DirectFBSurfaceCreateFlagsEXT(..)
                                                  , EXT_DIRECTFB_SURFACE_SPEC_VERSION
                                                  , pattern EXT_DIRECTFB_SURFACE_SPEC_VERSION
                                                  , EXT_DIRECTFB_SURFACE_EXTENSION_NAME
                                                  , pattern EXT_DIRECTFB_SURFACE_EXTENSION_NAME
                                                  , IDirectFB
                                                  , IDirectFBSurface
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDirectFBSurfaceEXT))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDirectFBPresentationSupportEXT))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDirectFBSurfaceEXT
  :: FunPtr (Ptr Instance_T -> Ptr DirectFBSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr DirectFBSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateDirectFBSurfaceEXT"
createDirectFBSurfaceEXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCreateDirectFBSurfaceEXT" "instance"
                            Instance
                         -> -- No documentation found for Nested "vkCreateDirectFBSurfaceEXT" "pCreateInfo"
                            DirectFBSurfaceCreateInfoEXT
                         -> -- No documentation found for Nested "vkCreateDirectFBSurfaceEXT" "pAllocator"
                            ("allocator" ::: Maybe AllocationCallbacks)
                         -> io (SurfaceKHR)
createDirectFBSurfaceEXT instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateDirectFBSurfaceEXTPtr = pVkCreateDirectFBSurfaceEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateDirectFBSurfaceEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDirectFBSurfaceEXT is null" Nothing Nothing
  let vkCreateDirectFBSurfaceEXT' = mkVkCreateDirectFBSurfaceEXT vkCreateDirectFBSurfaceEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateDirectFBSurfaceEXT' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDirectFBPresentationSupportEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr IDirectFB -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> Ptr IDirectFB -> IO Bool32

-- No documentation found for TopLevel "vkGetPhysicalDeviceDirectFBPresentationSupportEXT"
getPhysicalDeviceDirectFBPresentationSupportEXT :: forall io
                                                 . (MonadIO io)
                                                => -- No documentation found for Nested "vkGetPhysicalDeviceDirectFBPresentationSupportEXT" "physicalDevice"
                                                   PhysicalDevice
                                                -> -- No documentation found for Nested "vkGetPhysicalDeviceDirectFBPresentationSupportEXT" "queueFamilyIndex"
                                                   ("queueFamilyIndex" ::: Word32)
                                                -> -- No documentation found for Nested "vkGetPhysicalDeviceDirectFBPresentationSupportEXT" "dfb"
                                                   ("dfb" ::: Ptr IDirectFB)
                                                -> io (Bool)
getPhysicalDeviceDirectFBPresentationSupportEXT physicalDevice queueFamilyIndex dfb = liftIO $ do
  let vkGetPhysicalDeviceDirectFBPresentationSupportEXTPtr = pVkGetPhysicalDeviceDirectFBPresentationSupportEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceDirectFBPresentationSupportEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDirectFBPresentationSupportEXT is null" Nothing Nothing
  let vkGetPhysicalDeviceDirectFBPresentationSupportEXT' = mkVkGetPhysicalDeviceDirectFBPresentationSupportEXT vkGetPhysicalDeviceDirectFBPresentationSupportEXTPtr
  r <- vkGetPhysicalDeviceDirectFBPresentationSupportEXT' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (dfb)
  pure $ ((bool32ToBool r))



-- No documentation found for TopLevel "VkDirectFBSurfaceCreateInfoEXT"
data DirectFBSurfaceCreateInfoEXT = DirectFBSurfaceCreateInfoEXT
  { -- No documentation found for Nested "VkDirectFBSurfaceCreateInfoEXT" "flags"
    flags :: DirectFBSurfaceCreateFlagsEXT
  , -- No documentation found for Nested "VkDirectFBSurfaceCreateInfoEXT" "dfb"
    dfb :: Ptr IDirectFB
  , -- No documentation found for Nested "VkDirectFBSurfaceCreateInfoEXT" "surface"
    surface :: Ptr IDirectFBSurface
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DirectFBSurfaceCreateInfoEXT)
#endif
deriving instance Show DirectFBSurfaceCreateInfoEXT

instance ToCStruct DirectFBSurfaceCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DirectFBSurfaceCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DirectFBSurfaceCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr IDirectFB))) (dfb)
    poke ((p `plusPtr` 32 :: Ptr (Ptr IDirectFBSurface))) (surface)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr IDirectFB))) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr IDirectFBSurface))) (zero)
    f

instance FromCStruct DirectFBSurfaceCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @DirectFBSurfaceCreateFlagsEXT ((p `plusPtr` 16 :: Ptr DirectFBSurfaceCreateFlagsEXT))
    dfb <- peek @(Ptr IDirectFB) ((p `plusPtr` 24 :: Ptr (Ptr IDirectFB)))
    surface <- peek @(Ptr IDirectFBSurface) ((p `plusPtr` 32 :: Ptr (Ptr IDirectFBSurface)))
    pure $ DirectFBSurfaceCreateInfoEXT
             flags dfb surface


instance Storable DirectFBSurfaceCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DirectFBSurfaceCreateInfoEXT where
  zero = DirectFBSurfaceCreateInfoEXT
           zero
           zero
           zero


-- No documentation found for TopLevel "VkDirectFBSurfaceCreateFlagsEXT"
newtype DirectFBSurfaceCreateFlagsEXT = DirectFBSurfaceCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDirectFBSurfaceCreateFlagsEXT :: String
conNameDirectFBSurfaceCreateFlagsEXT = "DirectFBSurfaceCreateFlagsEXT"

enumPrefixDirectFBSurfaceCreateFlagsEXT :: String
enumPrefixDirectFBSurfaceCreateFlagsEXT = ""

showTableDirectFBSurfaceCreateFlagsEXT :: [(DirectFBSurfaceCreateFlagsEXT, String)]
showTableDirectFBSurfaceCreateFlagsEXT = []


instance Show DirectFBSurfaceCreateFlagsEXT where
showsPrec = enumShowsPrec enumPrefixDirectFBSurfaceCreateFlagsEXT
                          showTableDirectFBSurfaceCreateFlagsEXT
                          conNameDirectFBSurfaceCreateFlagsEXT
                          (\(DirectFBSurfaceCreateFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DirectFBSurfaceCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixDirectFBSurfaceCreateFlagsEXT
                          showTableDirectFBSurfaceCreateFlagsEXT
                          conNameDirectFBSurfaceCreateFlagsEXT
                          DirectFBSurfaceCreateFlagsEXT


type EXT_DIRECTFB_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DIRECTFB_SURFACE_SPEC_VERSION"
pattern EXT_DIRECTFB_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DIRECTFB_SURFACE_SPEC_VERSION = 1


type EXT_DIRECTFB_SURFACE_EXTENSION_NAME = "VK_EXT_directfb_surface"

-- No documentation found for TopLevel "VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME"
pattern EXT_DIRECTFB_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DIRECTFB_SURFACE_EXTENSION_NAME = "VK_EXT_directfb_surface"


data IDirectFB


data IDirectFBSurface

