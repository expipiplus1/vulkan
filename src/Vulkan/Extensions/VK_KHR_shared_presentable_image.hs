{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_shared_presentable_image"
module Vulkan.Extensions.VK_KHR_shared_presentable_image  ( getSwapchainStatusKHR
                                                          , SharedPresentSurfaceCapabilitiesKHR(..)
                                                          , KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
                                                          , pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
                                                          , KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
                                                          , pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
                                                          , SwapchainKHR(..)
                                                          , PresentModeKHR(..)
                                                          ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.IO.Class (MonadIO)
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
import Data.Kind (Type)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetSwapchainStatusKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainStatusKHR
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> IO Result) -> Ptr Device_T -> SwapchainKHR -> IO Result

-- No documentation found for TopLevel "vkGetSwapchainStatusKHR"
getSwapchainStatusKHR :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkGetSwapchainStatusKHR" "device"
                         Device
                      -> -- No documentation found for Nested "vkGetSwapchainStatusKHR" "swapchain"
                         SwapchainKHR
                      -> io (Result)
getSwapchainStatusKHR device swapchain = liftIO $ do
  let vkGetSwapchainStatusKHRPtr = pVkGetSwapchainStatusKHR (deviceCmds (device :: Device))
  unless (vkGetSwapchainStatusKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSwapchainStatusKHR is null" Nothing Nothing
  let vkGetSwapchainStatusKHR' = mkVkGetSwapchainStatusKHR vkGetSwapchainStatusKHRPtr
  r <- vkGetSwapchainStatusKHR' (deviceHandle (device)) (swapchain)
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)



-- No documentation found for TopLevel "VkSharedPresentSurfaceCapabilitiesKHR"
data SharedPresentSurfaceCapabilitiesKHR = SharedPresentSurfaceCapabilitiesKHR
  { -- No documentation found for Nested "VkSharedPresentSurfaceCapabilitiesKHR" "sharedPresentSupportedUsageFlags"
    sharedPresentSupportedUsageFlags :: ImageUsageFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SharedPresentSurfaceCapabilitiesKHR)
#endif
deriving instance Show SharedPresentSurfaceCapabilitiesKHR

instance ToCStruct SharedPresentSurfaceCapabilitiesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SharedPresentSurfaceCapabilitiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageUsageFlags)) (sharedPresentSupportedUsageFlags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SharedPresentSurfaceCapabilitiesKHR where
  peekCStruct p = do
    sharedPresentSupportedUsageFlags <- peek @ImageUsageFlags ((p `plusPtr` 16 :: Ptr ImageUsageFlags))
    pure $ SharedPresentSurfaceCapabilitiesKHR
             sharedPresentSupportedUsageFlags


instance Storable SharedPresentSurfaceCapabilitiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SharedPresentSurfaceCapabilitiesKHR where
  zero = SharedPresentSurfaceCapabilitiesKHR
           zero


type KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION"
pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1


type KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = "VK_KHR_shared_presentable_image"

-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME"
pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = "VK_KHR_shared_presentable_image"

