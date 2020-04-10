{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image  ( getSwapchainStatusKHR
                                                                   , SharedPresentSurfaceCapabilitiesKHR(..)
                                                                   , PresentModeKHR( PRESENT_MODE_IMMEDIATE_KHR
                                                                                   , PRESENT_MODE_MAILBOX_KHR
                                                                                   , PRESENT_MODE_FIFO_KHR
                                                                                   , PRESENT_MODE_FIFO_RELAXED_KHR
                                                                                   , PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
                                                                                   , PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
                                                                                   , ..
                                                                                   )
                                                                   , KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
                                                                   , pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
                                                                   , KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
                                                                   , pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
                                                                   , SwapchainKHR(..)
                                                                   ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetSwapchainStatusKHR))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR)
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR(..))
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
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @swapchain@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Extensions.Handles.SwapchainKHR'
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
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data SharedPresentSurfaceCapabilitiesKHR = SharedPresentSurfaceCapabilitiesKHR
  { -- | @sharedPresentSupportedUsageFlags@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits'
    -- representing the ways the application /can/ use the shared presentable
    -- image from a swapchain created with 'PresentModeKHR' set to
    -- 'PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR' or
    -- 'PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR' for the surface on the
    -- specified device.
    -- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
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


-- | VkPresentModeKHR - presentation mode supported for a surface
--
-- = Description
--
-- The supported
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' of
-- the presentable images of a swapchain created for a surface /may/ differ
-- depending on the presentation mode, and can be determined as per the
-- table below:
--
-- +----------------------------------------------+-------------------------------------------------------------------------------------------+
-- | Presentation mode                            | Image usage flags                                                                         |
-- +==============================================+===========================================================================================+
-- | 'PRESENT_MODE_IMMEDIATE_KHR'                 | 'Graphics.Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@supportedUsageFlags@ |
-- +----------------------------------------------+-------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_MAILBOX_KHR'                   | 'Graphics.Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@supportedUsageFlags@ |
-- +----------------------------------------------+-------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_FIFO_KHR'                      | 'Graphics.Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@supportedUsageFlags@ |
-- +----------------------------------------------+-------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_FIFO_RELAXED_KHR'              | 'Graphics.Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@supportedUsageFlags@ |
-- +----------------------------------------------+-------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'     | 'SharedPresentSurfaceCapabilitiesKHR'::@sharedPresentSupportedUsageFlags@                 |
-- +----------------------------------------------+-------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR' | 'SharedPresentSurfaceCapabilitiesKHR'::@sharedPresentSupportedUsageFlags@                 |
-- +----------------------------------------------+-------------------------------------------------------------------------------------------+
--
-- Presentable image usage queries
--
-- Note
--
-- For reference, the mode indicated by 'PRESENT_MODE_FIFO_KHR' is
-- equivalent to the behavior of {wgl|glX|egl}SwapBuffers with a swap
-- interval of 1, while the mode indicated by
-- 'PRESENT_MODE_FIFO_RELAXED_KHR' is equivalent to the behavior of
-- {wgl|glX}SwapBuffers with a swap interval of -1 (from the
-- {WGL|GLX}_EXT_swap_control_tear extensions).
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive.getPhysicalDeviceSurfacePresentModes2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR'
newtype PresentModeKHR = PresentModeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PRESENT_MODE_IMMEDIATE_KHR' specifies that the presentation engine does
-- not wait for a vertical blanking period to update the current image,
-- meaning this mode /may/ result in visible tearing. No internal queuing
-- of presentation requests is needed, as the requests are applied
-- immediately.
pattern PRESENT_MODE_IMMEDIATE_KHR = PresentModeKHR 0
-- | 'PRESENT_MODE_MAILBOX_KHR' specifies that the presentation engine waits
-- for the next vertical blanking period to update the current image.
-- Tearing /cannot/ be observed. An internal single-entry queue is used to
-- hold pending presentation requests. If the queue is full when a new
-- presentation request is received, the new request replaces the existing
-- entry, and any images associated with the prior entry become available
-- for re-use by the application. One request is removed from the queue and
-- processed during each vertical blanking period in which the queue is
-- non-empty.
pattern PRESENT_MODE_MAILBOX_KHR = PresentModeKHR 1
-- | 'PRESENT_MODE_FIFO_KHR' specifies that the presentation engine waits for
-- the next vertical blanking period to update the current image. Tearing
-- /cannot/ be observed. An internal queue is used to hold pending
-- presentation requests. New requests are appended to the end of the
-- queue, and one request is removed from the beginning of the queue and
-- processed during each vertical blanking period in which the queue is
-- non-empty. This is the only value of @presentMode@ that is /required/ to
-- be supported.
pattern PRESENT_MODE_FIFO_KHR = PresentModeKHR 2
-- | 'PRESENT_MODE_FIFO_RELAXED_KHR' specifies that the presentation engine
-- generally waits for the next vertical blanking period to update the
-- current image. If a vertical blanking period has already passed since
-- the last update of the current image then the presentation engine does
-- not wait for another vertical blanking period for the update, meaning
-- this mode /may/ result in visible tearing in this case. This mode is
-- useful for reducing visual stutter with an application that will mostly
-- present a new image before the next vertical blanking period, but may
-- occasionally be late, and present a new image just after the next
-- vertical blanking period. An internal queue is used to hold pending
-- presentation requests. New requests are appended to the end of the
-- queue, and one request is removed from the beginning of the queue and
-- processed during or after each vertical blanking period in which the
-- queue is non-empty.
pattern PRESENT_MODE_FIFO_RELAXED_KHR = PresentModeKHR 3
-- | 'PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR' specifies that the
-- presentation engine and application have concurrent access to a single
-- image, which is referred to as a /shared presentable image/. The
-- presentation engine periodically updates the current image on its
-- regular refresh cycle. The application is only required to make one
-- initial presentation request, after which the presentation engine /must/
-- update the current image without any need for further presentation
-- requests. The application /can/ indicate the image contents have been
-- updated by making a presentation request, but this does not guarantee
-- the timing of when it will be updated. This mode /may/ result in visible
-- tearing if rendering to the image is not timed correctly.
pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = PresentModeKHR 1000111001
-- | 'PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR' specifies that the presentation
-- engine and application have concurrent access to a single image, which
-- is referred to as a /shared presentable image/. The presentation engine
-- is only required to update the current image after a new presentation
-- request is received. Therefore the application /must/ make a
-- presentation request whenever an update is required. However, the
-- presentation engine /may/ update the current image at any point, meaning
-- this mode /may/ result in visible tearing.
pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR = PresentModeKHR 1000111000
{-# complete PRESENT_MODE_IMMEDIATE_KHR,
             PRESENT_MODE_MAILBOX_KHR,
             PRESENT_MODE_FIFO_KHR,
             PRESENT_MODE_FIFO_RELAXED_KHR,
             PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR,
             PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR :: PresentModeKHR #-}

instance Show PresentModeKHR where
  showsPrec p = \case
    PRESENT_MODE_IMMEDIATE_KHR -> showString "PRESENT_MODE_IMMEDIATE_KHR"
    PRESENT_MODE_MAILBOX_KHR -> showString "PRESENT_MODE_MAILBOX_KHR"
    PRESENT_MODE_FIFO_KHR -> showString "PRESENT_MODE_FIFO_KHR"
    PRESENT_MODE_FIFO_RELAXED_KHR -> showString "PRESENT_MODE_FIFO_RELAXED_KHR"
    PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR -> showString "PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR"
    PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR -> showString "PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR"
    PresentModeKHR x -> showParen (p >= 11) (showString "PresentModeKHR " . showsPrec 11 x)

instance Read PresentModeKHR where
  readPrec = parens (choose [("PRESENT_MODE_IMMEDIATE_KHR", pure PRESENT_MODE_IMMEDIATE_KHR)
                            , ("PRESENT_MODE_MAILBOX_KHR", pure PRESENT_MODE_MAILBOX_KHR)
                            , ("PRESENT_MODE_FIFO_KHR", pure PRESENT_MODE_FIFO_KHR)
                            , ("PRESENT_MODE_FIFO_RELAXED_KHR", pure PRESENT_MODE_FIFO_RELAXED_KHR)
                            , ("PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR", pure PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR)
                            , ("PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR", pure PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "PresentModeKHR")
                       v <- step readPrec
                       pure (PresentModeKHR v)))


type KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION"
pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1


type KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = "VK_KHR_shared_presentable_image"

-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME"
pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = "VK_KHR_shared_presentable_image"

