{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_display_swapchain"
module Vulkan.Extensions.VK_KHR_display_swapchain  ( createSharedSwapchainsKHR
                                                   , DisplayPresentInfoKHR(..)
                                                   , KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
                                                   , pattern KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
                                                   , KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
                                                   , pattern KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
                                                   , SurfaceKHR(..)
                                                   , SwapchainKHR(..)
                                                   , SwapchainCreateInfoKHR(..)
                                                   , PresentModeKHR(..)
                                                   , ColorSpaceKHR(..)
                                                   , CompositeAlphaFlagBitsKHR(..)
                                                   , CompositeAlphaFlagsKHR
                                                   , SurfaceTransformFlagBitsKHR(..)
                                                   , SurfaceTransformFlagsKHR
                                                   , SwapchainCreateFlagBitsKHR(..)
                                                   , SwapchainCreateFlagsKHR
                                                   ) where

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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateSharedSwapchainsKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateInfoKHR)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateInfoKHR(..))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSharedSwapchainsKHR
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct SwapchainCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr SwapchainKHR -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct SwapchainCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr SwapchainKHR -> IO Result

-- No documentation found for TopLevel "vkCreateSharedSwapchainsKHR"
createSharedSwapchainsKHR :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkCreateSharedSwapchainsKHR" "device"
                             Device
                          -> -- No documentation found for Nested "vkCreateSharedSwapchainsKHR" "pCreateInfos"
                             ("createInfos" ::: Vector (SomeStruct SwapchainCreateInfoKHR))
                          -> -- No documentation found for Nested "vkCreateSharedSwapchainsKHR" "pAllocator"
                             ("allocator" ::: Maybe AllocationCallbacks)
                          -> io (("swapchains" ::: Vector SwapchainKHR))
createSharedSwapchainsKHR device createInfos allocator = liftIO . evalContT $ do
  let vkCreateSharedSwapchainsKHRPtr = pVkCreateSharedSwapchainsKHR (deviceCmds (device :: Device))
  lift $ unless (vkCreateSharedSwapchainsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateSharedSwapchainsKHR is null" Nothing Nothing
  let vkCreateSharedSwapchainsKHR' = mkVkCreateSharedSwapchainsKHR vkCreateSharedSwapchainsKHRPtr
  pPCreateInfos <- ContT $ allocaBytesAligned @(SwapchainCreateInfoKHR _) ((Data.Vector.length (createInfos)) * 104) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (104 * (i)) :: Ptr (SwapchainCreateInfoKHR _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSwapchains <- ContT $ bracket (callocBytes @SwapchainKHR ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ vkCreateSharedSwapchainsKHR' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (forgetExtensions (pPCreateInfos)) pAllocator (pPSwapchains)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSwapchains <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @SwapchainKHR ((pPSwapchains `advancePtrBytes` (8 * (i)) :: Ptr SwapchainKHR)))
  pure $ (pSwapchains)



-- No documentation found for TopLevel "VkDisplayPresentInfoKHR"
data DisplayPresentInfoKHR = DisplayPresentInfoKHR
  { -- No documentation found for Nested "VkDisplayPresentInfoKHR" "srcRect"
    srcRect :: Rect2D
  , -- No documentation found for Nested "VkDisplayPresentInfoKHR" "dstRect"
    dstRect :: Rect2D
  , -- No documentation found for Nested "VkDisplayPresentInfoKHR" "persistent"
    persistent :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPresentInfoKHR)
#endif
deriving instance Show DisplayPresentInfoKHR

instance ToCStruct DisplayPresentInfoKHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPresentInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Rect2D)) (srcRect)
    poke ((p `plusPtr` 32 :: Ptr Rect2D)) (dstRect)
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (persistent))
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Rect2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Rect2D)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DisplayPresentInfoKHR where
  peekCStruct p = do
    srcRect <- peekCStruct @Rect2D ((p `plusPtr` 16 :: Ptr Rect2D))
    dstRect <- peekCStruct @Rect2D ((p `plusPtr` 32 :: Ptr Rect2D))
    persistent <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    pure $ DisplayPresentInfoKHR
             srcRect dstRect (bool32ToBool persistent)


instance Storable DisplayPresentInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPresentInfoKHR where
  zero = DisplayPresentInfoKHR
           zero
           zero
           zero


type KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 10

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION"
pattern KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 10


type KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_display_swapchain"

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME"
pattern KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_display_swapchain"

