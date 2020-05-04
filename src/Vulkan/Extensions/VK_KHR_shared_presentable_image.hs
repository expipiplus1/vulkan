{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_shared_presentable_image  ( getSwapchainStatusKHR
                                                          , SharedPresentSurfaceCapabilitiesKHR(..)
                                                          , KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
                                                          , pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
                                                          , KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
                                                          , pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
                                                          , SwapchainKHR(..)
                                                          , PresentModeKHR(..)
                                                          ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
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

-- | vkGetSwapchainStatusKHR - Get a swapchain’s status
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to query.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.SwapchainKHR'
getSwapchainStatusKHR :: forall io . MonadIO io => Device -> SwapchainKHR -> io (Result)
getSwapchainStatusKHR device swapchain = liftIO $ do
  let vkGetSwapchainStatusKHR' = mkVkGetSwapchainStatusKHR (pVkGetSwapchainStatusKHR (deviceCmds (device :: Device)))
  r <- vkGetSwapchainStatusKHR' (deviceHandle (device)) (swapchain)
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


-- | VkSharedPresentSurfaceCapabilitiesKHR - structure describing
-- capabilities of a surface for shared presentation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SharedPresentSurfaceCapabilitiesKHR = SharedPresentSurfaceCapabilitiesKHR
  { -- | @sharedPresentSupportedUsageFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' representing
    -- the ways the application /can/ use the shared presentable image from a
    -- swapchain created with 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR'
    -- set to
    -- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
    -- or
    -- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR'
    -- for the surface on the specified device.
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
    -- /must/ be included in the set but implementations /may/ support
    -- additional usages.
    sharedPresentSupportedUsageFlags :: ImageUsageFlags }
  deriving (Typeable)
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

