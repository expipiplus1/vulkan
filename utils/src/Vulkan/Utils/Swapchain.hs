{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-| Swapchain creation, recreation, and the small helper for catching
swapchain-out-of-date exceptions thrown elsewhere.

Opinionated choices (storage-image usage, FIFO_RELAXED preference, surface
format selection) are exposed via 'SwapchainConfig'. 'defaultSwapchainConfig'
gives a color-attachment-only swapchain prefering FIFO_RELAXED then FIFO;
compute-shader callers add @IMAGE_USAGE_STORAGE_BIT@ etc.
-}
module Vulkan.Utils.Swapchain
  ( Swapchain (..)
  , SwapchainConfig (..)
  , defaultSwapchainConfig
  , allocateSwapchain
  , recreateSwapchain
  , threwSwapchainError
  ) where

import Control.Exception (throwIO, tryJust)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Either (isLeft)
import Data.Foldable (for_, traverse_)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreTypeCreateInfo (..), pattern SEMAPHORE_TYPE_BINARY)
import Vulkan.Exception (VulkanException (..))
import Vulkan.Extensions.VK_KHR_surface as SurfaceCapabilitiesKHR (SurfaceCapabilitiesKHR (..))
import Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import qualified Vulkan.Extensions.VK_KHR_swapchain as KHR
import Vulkan.Utils.Misc ((.&&.))
import Vulkan.Utils.RefCounted (RefCounted, newRefCounted, releaseRefCounted)
import Vulkan.Zero (zero)

----------------------------------------------------------------
-- Config
----------------------------------------------------------------

{- | Opinionated knobs for swapchain creation. Use 'defaultSwapchainConfig' as
a starting point and override the bits you care about.
-}
data SwapchainConfig = SwapchainConfig
  { scRequiredUsageFlags :: [Vk.ImageUsageFlagBits]
  {- ^ Image usages every swapchain image must support. Default:
  @[IMAGE_USAGE_COLOR_ATTACHMENT_BIT]@. Compute-shader callers add
  @IMAGE_USAGE_STORAGE_BIT@.
  -}
  , scRequiredFormatFeatures :: [Vk.FormatFeatureFlagBits]
  {- ^ Format-feature flags the chosen surface format's optimal tiling
  must satisfy. Default: @[]@. Set @FORMAT_FEATURE_STORAGE_IMAGE_BIT@ if
  using @IMAGE_USAGE_STORAGE_BIT@ — SRGB formats typically omit it.
  -}
  , scDesiredPresentModes :: [KHR.PresentModeKHR]
  {- ^ Present-mode preference, best first. Default:
  @[FIFO_RELAXED, FIFO]@. The driver-guaranteed @FIFO@ is the safe
  fallback. Add @IMMEDIATE@ or @MAILBOX@ if your scheduler can tolerate
  them.
  -}
  , scSurfaceFormatPreferences :: [KHR.SurfaceFormatKHR -> Bool]
  {- ^ Surface-format preference predicates, best first. For each predicate
  in order, the first format that matches both the predicate AND the
  feature requirements wins. If no preference matches, falls back to the
  first feature-satisfying format, then to the head. Default: @[]@.
  -}
  }
  deriving (Generic)

-- | Sensible defaults: color-attachment swapchain, FIFO_RELAXED preferred.
defaultSwapchainConfig :: SwapchainConfig
defaultSwapchainConfig =
  SwapchainConfig
    { scRequiredUsageFlags = [Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT]
    , scRequiredFormatFeatures = []
    , scDesiredPresentModes =
        [ KHR.PRESENT_MODE_FIFO_RELAXED_KHR
        , KHR.PRESENT_MODE_FIFO_KHR
        ]
    , scSurfaceFormatPreferences = []
    }

----------------------------------------------------------------
-- Swapchain
----------------------------------------------------------------

data Swapchain = Swapchain
  { sSwapchain :: KHR.SwapchainKHR
  , sSurface :: KHR.SurfaceKHR
  , sFormat :: KHR.SurfaceFormatKHR
  , sExtent :: Vk.Extent2D
  , sPresentMode :: KHR.PresentModeKHR
  , sImages :: Vector Vk.Image
  , sImageViews :: Vector Vk.ImageView
  , sRenderFinished :: Vector Vk.Semaphore
  {- ^ Per-image present-wait binary semaphore, indexed by the acquired image
  index (@length == length sImages@). A frame's submit signals
  @sRenderFinished ! imageIndex@ and the present waits on it; reusing it is
  safe only once that image is re-acquired, which is why it lives here (per
  image) rather than in the per-frame 'RecycledResources'. Freed by 'sRelease'.
  -}
  , sRelease :: RefCounted
  -- ^ Held until no in-flight frame still uses this swapchain.
  , sConfig :: SwapchainConfig
  -- ^ Retained so 'recreateSwapchain' can re-apply the same knobs.
  }
  deriving (Generic)

----------------------------------------------------------------
-- Allocate / recreate
----------------------------------------------------------------

-- | Allocate a new swapchain plus its image views.
allocateSwapchain
  :: (MonadResource m)
  => Vk.PhysicalDevice
  -> Vk.Device
  -> SwapchainConfig
  -> KHR.SwapchainKHR
  -- ^ Previous swapchain ('NULL_HANDLE' for first)
  -> Vk.Extent2D
  -- ^ Fallback size when the surface lets us pick
  -> KHR.SurfaceKHR
  -> m Swapchain
allocateSwapchain phys dev cfg oldSwapchain windowSize surface = do
  (sSwapchain, sFormat, sExtent, sPresentMode, swapchainKey) <-
    allocateSwapchainEx phys dev cfg oldSwapchain windowSize surface

  (_, sImages) <- KHR.getSwapchainImagesKHR dev sSwapchain
  (imageViewKeys, sImageViews) <-
    fmap V.unzip . V.forM sImages $ \image ->
      allocateImageView dev (SurfaceFormatKHR.format sFormat) image

  -- One present-wait binary semaphore per swapchain image, indexed by the
  -- acquired image index (see 'sRenderFinished').
  (renderFinishedKeys, sRenderFinished) <-
    fmap V.unzip . V.forM sImages $ \_image ->
      Vk.withSemaphore
        dev
        (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())
        Nothing
        allocate

  -- Released by the next 'recreateSwapchain' (when frames stop using it).
  sRelease <- newRefCounted $ do
    traverse_ release renderFinishedKeys
    traverse_ release imageViewKeys
    release swapchainKey

  pure Swapchain{sSurface = surface, sConfig = cfg, ..}

{- | Build a new swapchain at a new size, dropping the reference to the old
one so its resources can be released once in-flight frames complete.
-}
recreateSwapchain
  :: (MonadResource m)
  => Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.Extent2D
  -- ^ New window size
  -> Swapchain
  -> m Swapchain
recreateSwapchain phys dev newSize old = do
  fresh <- allocateSwapchain phys dev (sConfig old) (sSwapchain old) newSize (sSurface old)
  releaseRefCounted (sRelease old)
  pure fresh

----------------------------------------------------------------
-- Internals
----------------------------------------------------------------

allocateSwapchainEx
  :: (MonadResource m)
  => Vk.PhysicalDevice
  -> Vk.Device
  -> SwapchainConfig
  -> KHR.SwapchainKHR
  -> Vk.Extent2D
  -> KHR.SurfaceKHR
  -> m (KHR.SwapchainKHR, SurfaceFormatKHR, Vk.Extent2D, KHR.PresentModeKHR, ReleaseKey)
allocateSwapchainEx phys dev cfg oldSwapchain explicitSize surf = do
  surfaceCaps <- KHR.getPhysicalDeviceSurfaceCapabilitiesKHR phys surf

  -- Sanity-check that the surface advertises the usages we need.
  for_ (scRequiredUsageFlags cfg) $ \f ->
    unless (supportedUsageFlags surfaceCaps .&&. f) $
      liftIO . throwIO . userError $
        "Surface images do not support " <> show f

  -- Pick a present mode in our preference order.
  (_, availablePresentModes) <- KHR.getPhysicalDeviceSurfacePresentModesKHR phys surf
  presentMode <-
    case filter (`V.elem` availablePresentModes) (scDesiredPresentModes cfg) of
      [] -> liftIO . throwIO . userError $ "Unable to find a suitable present mode for swapchain"
      x : _ -> pure x

  -- Pick a surface format. Vulkan guarantees at least one.
  (_, availableFormats) <- KHR.getPhysicalDeviceSurfaceFormatsKHR phys surf
  surfaceFormat <- selectSurfaceFormat phys cfg availableFormats

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
      else liftIO . throwIO . userError $ "Surface doesn't support COMPOSITE_ALPHA_OPAQUE_BIT_KHR"

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
          , imageUsage = foldr (.|.) zero (scRequiredUsageFlags cfg)
          , imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE
          , preTransform = SurfaceCapabilitiesKHR.currentTransform surfaceCaps
          , compositeAlpha = compositeAlphaMode
          , presentMode = presentMode
          , clipped = True
          , oldSwapchain = oldSwapchain
          }

  (key, swapchain) <- KHR.withSwapchainKHR dev swapchainCreateInfo Nothing allocate

  pure (swapchain, surfaceFormat, imageExtent, presentMode, key)

-- | 2D color image view covering the whole image.
allocateImageView
  :: (MonadResource m)
  => Vk.Device
  -> Vk.Format
  -> Vk.Image
  -> m (ReleaseKey, Vk.ImageView)
allocateImageView dev format image =
  Vk.withImageView dev imageViewCreateInfo Nothing allocate
  where
    imageViewCreateInfo =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format = format
        , Vk.components =
            zero
              { Vk.r = Vk.COMPONENT_SWIZZLE_IDENTITY
              , Vk.g = Vk.COMPONENT_SWIZZLE_IDENTITY
              , Vk.b = Vk.COMPONENT_SWIZZLE_IDENTITY
              , Vk.a = Vk.COMPONENT_SWIZZLE_IDENTITY
              }
        , Vk.subresourceRange =
            zero
              { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
              , Vk.baseMipLevel = 0
              , Vk.levelCount = 1
              , Vk.baseArrayLayer = 0
              , Vk.layerCount = 1
              }
        }

----------------------------------------------------------------
-- Format selection
----------------------------------------------------------------

{- | Prefer formats whose 'optimalTilingFeatures' satisfy
'scRequiredFormatFeatures' and additionally match one of
'scSurfaceFormatPreferences' (best preference first). Falls back to the
first feature-satisfying format, then to the head if all else fails.
-}
selectSurfaceFormat
  :: (MonadIO m)
  => Vk.PhysicalDevice
  -> SwapchainConfig
  -> Vector SurfaceFormatKHR
  -> m SurfaceFormatKHR
selectSurfaceFormat phys cfg fmts = do
  good <- V.filterM featuresOK fmts
  let fallback = if V.null good then V.head fmts else V.head good
  pure $ pickPreference (scSurfaceFormatPreferences cfg) good fallback
  where
    featuresOK f = do
      props <- Vk.getPhysicalDeviceFormatProperties phys (SurfaceFormatKHR.format f)
      pure $ all (Vk.optimalTilingFeatures props .&&.) (scRequiredFormatFeatures cfg)

    pickPreference [] _ fallback = fallback
    pickPreference (p : ps) good fallback =
      case V.find p good of
        Just f -> f
        Nothing -> pickPreference ps good fallback

----------------------------------------------------------------
-- Specifications
----------------------------------------------------------------

-- | Catch an 'ERROR_OUT_OF_DATE_KHR' exception and return 'True' when caught.
threwSwapchainError :: IO b -> IO Bool
threwSwapchainError = fmap isLeft . tryJust swapchainError
  where
    swapchainError = \case
      VulkanException e@Vk.ERROR_OUT_OF_DATE_KHR -> Just e
      VulkanException _ -> Nothing
