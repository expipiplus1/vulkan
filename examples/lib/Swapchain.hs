{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-| Swapchain creation, recreation, and the small helper for catching
swapchain-out-of-date exceptions thrown elsewhere.
-}
module Swapchain
  ( Swapchain (..)
  , allocSwapchain
  , recreateSwapchain
  , threwSwapchainError
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Either
import Data.Foldable (for_, traverse_)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import RefCounted (RefCounted, newRefCounted, releaseRefCounted)
import UnliftIO.Exception (throwString, tryJust)
import VkResources (VkResources (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception (VulkanException (..))
import Vulkan.Extensions.VK_KHR_surface as SurfaceCapabilitiesKHR (SurfaceCapabilitiesKHR (..))
import Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import qualified Vulkan.Extensions.VK_KHR_swapchain as KHR
import qualified Vulkan.Utils.Framebuffer as Framebuffer
import Vulkan.Utils.Misc ((.&&.))
import Vulkan.Zero (zero)

data Swapchain = Swapchain
  { sSwapchain :: KHR.SwapchainKHR
  , sSurface :: KHR.SurfaceKHR
  , sFormat :: KHR.SurfaceFormatKHR
  , sExtent :: Vk.Extent2D
  , sPresentMode :: KHR.PresentModeKHR
  , sImages :: Vector Vk.Image
  , sImageViews :: Vector Vk.ImageView
  , sRelease :: RefCounted
  -- ^ Held until no in-flight frame still uses this swapchain.
  }
  deriving (Generic)

----------------------------------------------------------------
-- Allocate / recreate
----------------------------------------------------------------

-- | Allocate a new swapchain plus its image views.
allocSwapchain
  :: (MonadUnliftIO m, MonadResource m)
  => VkResources
  -> KHR.SwapchainKHR
  -- ^ Previous swapchain ('NULL_HANDLE' for first)
  -> Vk.Extent2D
  -- ^ Fallback size when the surface lets us pick
  -> KHR.SurfaceKHR
  -> m Swapchain
allocSwapchain vr oldSwapchain windowSize surface = do
  (sSwapchain, sFormat, sExtent, sPresentMode, swapchainKey) <-
    createSwapchain vr oldSwapchain windowSize surface

  (_, sImages) <- KHR.getSwapchainImagesKHR (vrDevice vr) sSwapchain
  (imageViewKeys, sImageViews) <-
    fmap V.unzip . V.forM sImages $ \image ->
      Framebuffer.createImageView
        (vrDevice vr)
        (SurfaceFormatKHR.format sFormat)
        image

  -- Released by the next 'recreateSwapchain' (when frames stop using it).
  sRelease <- newRefCounted $ do
    traverse_ release imageViewKeys
    release swapchainKey

  pure Swapchain{sSurface = surface, ..}

{- | Build a new swapchain at a new size, dropping the reference to the old
one so its resources can be released once in-flight frames complete.
-}
recreateSwapchain
  :: (MonadUnliftIO m, MonadResource m)
  => VkResources
  -> Vk.Extent2D
  -- ^ New window size
  -> Swapchain
  -> m Swapchain
recreateSwapchain vr newSize old = do
  fresh <- allocSwapchain vr (sSwapchain old) newSize (sSurface old)
  releaseRefCounted (sRelease old)
  pure fresh

----------------------------------------------------------------
-- Internals
----------------------------------------------------------------

createSwapchain
  :: (MonadUnliftIO m, MonadResource m)
  => VkResources
  -> KHR.SwapchainKHR
  -> Vk.Extent2D
  -> KHR.SurfaceKHR
  -> m (KHR.SwapchainKHR, SurfaceFormatKHR, Vk.Extent2D, KHR.PresentModeKHR, ReleaseKey)
createSwapchain vr oldSwapchain explicitSize surf = do
  let
    phys = vrPhysicalDevice vr
    dev = vrDevice vr

  surfaceCaps <- KHR.getPhysicalDeviceSurfaceCapabilitiesKHR phys surf

  -- Sanity-check that the surface advertises the usages we need.
  for_ requiredUsageFlags $ \f ->
    unless (supportedUsageFlags surfaceCaps .&&. f) $
      throwString ("Surface images do not support " <> show f)

  -- Pick a present mode in our preference order.
  (_, availablePresentModes) <- KHR.getPhysicalDeviceSurfacePresentModesKHR phys surf
  presentMode <-
    case filter (`V.elem` availablePresentModes) desiredPresentModes of
      [] -> throwString "Unable to find a suitable present mode for swapchain"
      x : _ -> pure x

  -- Pick a surface format. Vulkan guarantees at least one.
  (_, availableFormats) <- KHR.getPhysicalDeviceSurfaceFormatsKHR phys surf
  surfaceFormat <- selectSurfaceFormat phys availableFormats

  -- Use the surface's reported extent unless it tells us we can pick.
  let imageExtent =
        case currentExtent (surfaceCaps :: SurfaceCapabilitiesKHR) of
          Vk.Extent2D w h | w == maxBound, h == maxBound -> explicitSize
          e -> e

  let imageCount =
        let
          limit = case maxImageCount (surfaceCaps :: SurfaceCapabilitiesKHR) of
            0 -> maxBound
            n -> n
          buffer = 1 -- request one extra to avoid waiting on the driver
          desired = buffer + SurfaceCapabilitiesKHR.minImageCount surfaceCaps
        in
          min limit desired

  compositeAlphaMode <-
    if KHR.COMPOSITE_ALPHA_OPAQUE_BIT_KHR .&&. supportedCompositeAlpha surfaceCaps
      then pure KHR.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
      else throwString "Surface doesn't support COMPOSITE_ALPHA_OPAQUE_BIT_KHR"

  let swapchainCreateInfo =
        KHR.SwapchainCreateInfoKHR
          { surface = surf
          , next = ()
          , flags = zero
          , queueFamilyIndices = mempty
          , minImageCount = imageCount
          , imageFormat = SurfaceFormatKHR.format surfaceFormat
          , imageColorSpace = colorSpace surfaceFormat
          , imageExtent = imageExtent
          , imageArrayLayers = 1
          , imageUsage = foldr (.|.) zero requiredUsageFlags
          , imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE
          , preTransform = SurfaceCapabilitiesKHR.currentTransform surfaceCaps
          , compositeAlpha = compositeAlphaMode
          , presentMode = presentMode
          , clipped = True
          , oldSwapchain = oldSwapchain
          }

  (key, swapchain) <- KHR.withSwapchainKHR dev swapchainCreateInfo Nothing allocate

  pure (swapchain, surfaceFormat, imageExtent, presentMode, key)

----------------------------------------------------------------
-- Format selection
----------------------------------------------------------------

{- | Prefer formats whose 'optimalTilingFeatures' satisfy
'requiredFormatFeatures'; SRGB formats typically omit
'FORMAT_FEATURE_STORAGE_IMAGE_BIT' and would otherwise cause
@vkCreateSwapchainKHR@ to fail.
-}
selectSurfaceFormat
  :: (MonadIO m) => Vk.PhysicalDevice -> Vector SurfaceFormatKHR -> m SurfaceFormatKHR
selectSurfaceFormat phys fmts = do
  good <- V.filterM suitable fmts
  pure $ if V.null good then V.head fmts else V.head good
  where
    suitable f = do
      props <- Vk.getPhysicalDeviceFormatProperties phys (SurfaceFormatKHR.format f)
      pure $ all (Vk.optimalTilingFeatures props .&&.) requiredFormatFeatures

----------------------------------------------------------------
-- Specifications
----------------------------------------------------------------

-- | Catch an 'ERROR_OUT_OF_DATE_KHR' exception and return 'True' when caught.
threwSwapchainError :: (MonadUnliftIO f) => f b -> f Bool
threwSwapchainError = fmap isLeft . tryJust swapchainError
  where
    swapchainError = \case
      VulkanException e@Vk.ERROR_OUT_OF_DATE_KHR -> Just e
      -- TODO: handle ERROR_SURFACE_LOST_KHR too
      VulkanException _ -> Nothing

-- | Present-mode preference, best first.
desiredPresentModes :: [KHR.PresentModeKHR]
desiredPresentModes =
  [ KHR.PRESENT_MODE_FIFO_RELAXED_KHR
  , KHR.PRESENT_MODE_FIFO_KHR
  , KHR.PRESENT_MODE_IMMEDIATE_KHR
  ]

-- | Image usages every swapchain image must support.
requiredUsageFlags :: [Vk.ImageUsageFlagBits]
requiredUsageFlags =
  [ Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
  , Vk.IMAGE_USAGE_STORAGE_BIT
  ]

-- | Format feature flags the chosen surface format must support.
requiredFormatFeatures :: [Vk.FormatFeatureFlagBits]
requiredFormatFeatures =
  [ Vk.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
  , Vk.FORMAT_FEATURE_STORAGE_IMAGE_BIT
  ]
