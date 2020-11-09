{-# language CPP #-}
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
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
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

-- | vkCreateSharedSwapchainsKHR - Create multiple swapchains that share
-- presentable images
--
-- = Description
--
-- 'createSharedSwapchainsKHR' is similar to
-- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR', except that it
-- takes an array of
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR' structures,
-- and returns an array of swapchain objects.
--
-- The swapchain creation parameters that affect the properties and number
-- of presentable images /must/ match between all the swapchains. If the
-- displays used by any of the swapchains do not use the same presentable
-- image layout or are incompatible in a way that prevents sharing images,
-- swapchain creation will fail with the result code
-- 'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_DISPLAY_KHR'. If any
-- error occurs, no swapchains will be created. Images presented to
-- multiple swapchains /must/ be re-acquired from all of them before
-- transitioning away from
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR'. After
-- destroying one or more of the swapchains, the remaining swapchains and
-- the presentable images /can/ continue to be used.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateSharedSwapchainsKHR-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateSharedSwapchainsKHR-pCreateInfos-parameter#
--     @pCreateInfos@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
--     structures
--
-- -   #VUID-vkCreateSharedSwapchainsKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateSharedSwapchainsKHR-pSwapchains-parameter#
--     @pSwapchains@ /must/ be a valid pointer to an array of
--     @swapchainCount@ 'Vulkan.Extensions.Handles.SwapchainKHR' handles
--
-- -   #VUID-vkCreateSharedSwapchainsKHR-swapchainCount-arraylength#
--     @swapchainCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @pCreateInfos@[].surface /must/ be externally
--     synchronized
--
-- -   Host access to @pCreateInfos@[].oldSwapchain /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_DISPLAY_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
createSharedSwapchainsKHR :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the device to create the swapchains for.
                             Device
                          -> -- | @pCreateInfos@ is a pointer to an array of
                             -- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR' structures
                             -- specifying the parameters of the created swapchains.
                             ("createInfos" ::: Vector (SomeStruct SwapchainCreateInfoKHR))
                          -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                             -- swapchain objects when there is no more specific allocator available
                             -- (see
                             -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
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


-- | VkDisplayPresentInfoKHR - Structure describing parameters of a queue
-- presentation to a swapchain
--
-- = Description
--
-- If the extent of the @srcRect@ and @dstRect@ are not equal, the
-- presented pixels will be scaled accordingly.
--
-- == Valid Usage
--
-- -   #VUID-VkDisplayPresentInfoKHR-srcRect-01257# @srcRect@ /must/
--     specify a rectangular region that is a subset of the image being
--     presented
--
-- -   #VUID-VkDisplayPresentInfoKHR-dstRect-01258# @dstRect@ /must/
--     specify a rectangular region that is a subset of the @visibleRegion@
--     parameter of the display mode the swapchain being presented uses
--
-- -   #VUID-VkDisplayPresentInfoKHR-persistentContent-01259# If the
--     @persistentContent@ member of the
--     'Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_display.getPhysicalDeviceDisplayPropertiesKHR'
--     for the display the present operation targets then @persistent@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDisplayPresentInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DisplayPresentInfoKHR = DisplayPresentInfoKHR
  { -- | @srcRect@ is a rectangular region of pixels to present. It /must/ be a
    -- subset of the image being presented. If 'DisplayPresentInfoKHR' is not
    -- specified, this region will be assumed to be the entire presentable
    -- image.
    srcRect :: Rect2D
  , -- | @dstRect@ is a rectangular region within the visible region of the
    -- swapchain’s display mode. If 'DisplayPresentInfoKHR' is not specified,
    -- this region will be assumed to be the entire visible region of the
    -- visible region of the swapchain’s mode. If the specified rectangle is a
    -- subset of the display mode’s visible region, content from display planes
    -- below the swapchain’s plane will be visible outside the rectangle. If
    -- there are no planes below the swapchain’s, the area outside the
    -- specified rectangle will be black. If portions of the specified
    -- rectangle are outside of the display’s visible region, pixels mapping
    -- only to those portions of the rectangle will be discarded.
    dstRect :: Rect2D
  , -- | @persistent@: If this is 'Vulkan.Core10.FundamentalTypes.TRUE', the
    -- display engine will enable buffered mode on displays that support it.
    -- This allows the display engine to stop sending content to the display
    -- until a new image is presented. The display will instead maintain a copy
    -- of the last presented image. This allows less power to be used, but
    -- /may/ increase presentation latency. If 'DisplayPresentInfoKHR' is not
    -- specified, persistent mode will not be used.
    persistent :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPresentInfoKHR)
#endif
deriving instance Show DisplayPresentInfoKHR

instance ToCStruct DisplayPresentInfoKHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPresentInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Rect2D)) (srcRect) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr Rect2D)) (dstRect) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (persistent))
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Rect2D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr Rect2D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance FromCStruct DisplayPresentInfoKHR where
  peekCStruct p = do
    srcRect <- peekCStruct @Rect2D ((p `plusPtr` 16 :: Ptr Rect2D))
    dstRect <- peekCStruct @Rect2D ((p `plusPtr` 32 :: Ptr Rect2D))
    persistent <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    pure $ DisplayPresentInfoKHR
             srcRect dstRect (bool32ToBool persistent)

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

