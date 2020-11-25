{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_display_surface_counter"
module Vulkan.Extensions.VK_EXT_display_surface_counter  ( getPhysicalDeviceSurfaceCapabilities2EXT
                                                         , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
                                                         , pattern SURFACE_COUNTER_VBLANK_EXT
                                                         , SurfaceCapabilities2EXT(..)
                                                         , SurfaceCounterFlagsEXT
                                                         , SurfaceCounterFlagBitsEXT( SURFACE_COUNTER_VBLANK_BIT_EXT
                                                                                    , ..
                                                                                    )
                                                         , EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
                                                         , pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
                                                         , EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
                                                         , pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
                                                         , SurfaceKHR(..)
                                                         , CompositeAlphaFlagBitsKHR(..)
                                                         , CompositeAlphaFlagsKHR
                                                         , SurfaceTransformFlagBitsKHR(..)
                                                         , SurfaceTransformFlagsKHR
                                                         ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagsKHR)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceCapabilities2EXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagsKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilities2EXT
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilities2EXT -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilities2EXT -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceCapabilities2EXT"
getPhysicalDeviceSurfaceCapabilities2EXT :: forall io
                                          . (MonadIO io)
                                         => -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceCapabilities2EXT" "physicalDevice"
                                            PhysicalDevice
                                         -> -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceCapabilities2EXT" "surface"
                                            SurfaceKHR
                                         -> io (SurfaceCapabilities2EXT)
getPhysicalDeviceSurfaceCapabilities2EXT physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceCapabilities2EXTPtr = pVkGetPhysicalDeviceSurfaceCapabilities2EXT (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceCapabilities2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceCapabilities2EXT is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceCapabilities2EXT' = mkVkGetPhysicalDeviceSurfaceCapabilities2EXT vkGetPhysicalDeviceSurfaceCapabilities2EXTPtr
  pPSurfaceCapabilities <- ContT (withZeroCStruct @SurfaceCapabilities2EXT)
  r <- lift $ vkGetPhysicalDeviceSurfaceCapabilities2EXT' (physicalDeviceHandle (physicalDevice)) (surface) (pPSurfaceCapabilities)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceCapabilities <- lift $ peekCStruct @SurfaceCapabilities2EXT pPSurfaceCapabilities
  pure $ (pSurfaceCapabilities)


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT"
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT = STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT


-- No documentation found for TopLevel "VK_SURFACE_COUNTER_VBLANK_EXT"
pattern SURFACE_COUNTER_VBLANK_EXT = SURFACE_COUNTER_VBLANK_BIT_EXT



-- No documentation found for TopLevel "VkSurfaceCapabilities2EXT"
data SurfaceCapabilities2EXT = SurfaceCapabilities2EXT
  { -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "minImageCount"
    minImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "maxImageCount"
    maxImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "currentExtent"
    currentExtent :: Extent2D
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "minImageExtent"
    minImageExtent :: Extent2D
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "maxImageExtent"
    maxImageExtent :: Extent2D
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "maxImageArrayLayers"
    maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedTransforms"
    supportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "currentTransform"
    currentTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedCompositeAlpha"
    supportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedUsageFlags"
    supportedUsageFlags :: ImageUsageFlags
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedSurfaceCounters"
    supportedSurfaceCounters :: SurfaceCounterFlagsEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilities2EXT)
#endif
deriving instance Show SurfaceCapabilities2EXT

instance ToCStruct SurfaceCapabilities2EXT where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilities2EXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (minImageCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxImageCount)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (currentExtent)
    poke ((p `plusPtr` 32 :: Ptr Extent2D)) (minImageExtent)
    poke ((p `plusPtr` 40 :: Ptr Extent2D)) (maxImageExtent)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxImageArrayLayers)
    poke ((p `plusPtr` 52 :: Ptr SurfaceTransformFlagsKHR)) (supportedTransforms)
    poke ((p `plusPtr` 56 :: Ptr SurfaceTransformFlagBitsKHR)) (currentTransform)
    poke ((p `plusPtr` 60 :: Ptr CompositeAlphaFlagsKHR)) (supportedCompositeAlpha)
    poke ((p `plusPtr` 64 :: Ptr ImageUsageFlags)) (supportedUsageFlags)
    poke ((p `plusPtr` 68 :: Ptr SurfaceCounterFlagsEXT)) (supportedSurfaceCounters)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    f

instance FromCStruct SurfaceCapabilities2EXT where
  peekCStruct p = do
    minImageCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxImageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    currentExtent <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    minImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 32 :: Ptr Extent2D))
    maxImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 40 :: Ptr Extent2D))
    maxImageArrayLayers <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    supportedTransforms <- peek @SurfaceTransformFlagsKHR ((p `plusPtr` 52 :: Ptr SurfaceTransformFlagsKHR))
    currentTransform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 56 :: Ptr SurfaceTransformFlagBitsKHR))
    supportedCompositeAlpha <- peek @CompositeAlphaFlagsKHR ((p `plusPtr` 60 :: Ptr CompositeAlphaFlagsKHR))
    supportedUsageFlags <- peek @ImageUsageFlags ((p `plusPtr` 64 :: Ptr ImageUsageFlags))
    supportedSurfaceCounters <- peek @SurfaceCounterFlagsEXT ((p `plusPtr` 68 :: Ptr SurfaceCounterFlagsEXT))
    pure $ SurfaceCapabilities2EXT
             minImageCount maxImageCount currentExtent minImageExtent maxImageExtent maxImageArrayLayers supportedTransforms currentTransform supportedCompositeAlpha supportedUsageFlags supportedSurfaceCounters


instance Storable SurfaceCapabilities2EXT where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceCapabilities2EXT where
  zero = SurfaceCapabilities2EXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT

-- No documentation found for TopLevel "VkSurfaceCounterFlagBitsEXT"
newtype SurfaceCounterFlagBitsEXT = SurfaceCounterFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSurfaceCounterFlagBitsEXT" "VK_SURFACE_COUNTER_VBLANK_BIT_EXT"
pattern SURFACE_COUNTER_VBLANK_BIT_EXT = SurfaceCounterFlagBitsEXT 0x00000001

conNameSurfaceCounterFlagBitsEXT :: String
conNameSurfaceCounterFlagBitsEXT = "SurfaceCounterFlagBitsEXT"

enumPrefixSurfaceCounterFlagBitsEXT :: String
enumPrefixSurfaceCounterFlagBitsEXT = "SURFACE_COUNTER_VBLANK_BIT_EXT"

showTableSurfaceCounterFlagBitsEXT :: [(SurfaceCounterFlagBitsEXT, String)]
showTableSurfaceCounterFlagBitsEXT = [(SURFACE_COUNTER_VBLANK_BIT_EXT, "")]


instance Show SurfaceCounterFlagBitsEXT where
showsPrec = enumShowsPrec enumPrefixSurfaceCounterFlagBitsEXT
                          showTableSurfaceCounterFlagBitsEXT
                          conNameSurfaceCounterFlagBitsEXT
                          (\(SurfaceCounterFlagBitsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read SurfaceCounterFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixSurfaceCounterFlagBitsEXT
                          showTableSurfaceCounterFlagBitsEXT
                          conNameSurfaceCounterFlagBitsEXT
                          SurfaceCounterFlagBitsEXT


type EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION"
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1


type EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = "VK_EXT_display_surface_counter"

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME"
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = "VK_EXT_display_surface_counter"

