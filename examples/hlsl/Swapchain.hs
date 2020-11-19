module Swapchain
  ( SwapchainInfo(..)
  , SwapchainResources(..)
  , allocSwapchainResources
  , recreateSwapchainResources
  , threwSwapchainError
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Either
import           Data.Foldable                  ( for_
                                                , traverse_
                                                )
import           Data.Ord                       ( comparing )
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Framebuffer
import           MonadVulkan
import           RefCounted
import           RenderPass
import qualified SDL
import qualified SDL.Video.Vulkan              as SDL
import           UnliftIO.Exception             ( throwString
                                                , tryJust
                                                )
import           Vulkan.Core10
import           Vulkan.Exception
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Utils.Misc
import           Vulkan.Zero

data SwapchainInfo = SwapchainInfo
  { siSwapchain           :: SwapchainKHR
  , siSwapchainReleaseKey :: ReleaseKey
  , siPresentMode         :: PresentModeKHR
  , siSurfaceFormat       :: SurfaceFormatKHR
  , siImageExtent         :: Extent2D
  , siSurface             :: SurfaceKHR
  }

data SwapchainResources = SwapchainResources
  { srInfo         :: SwapchainInfo
  , srFramebuffers :: Vector Framebuffer
  , srImageViews   :: Vector ImageView
  , srImages       :: Vector Image
  , srRenderPass   :: RenderPass
  , srRelease      :: RefCounted
  }

----------------------------------------------------------------
-- All the resources which depend on the swapchain
----------------------------------------------------------------

-- | Allocate everything which depends on the swapchain
allocSwapchainResources
  :: SwapchainKHR
  -- ^ Previous swapchain, can be NULL_HANDLE
  -> Extent2D
  -- ^ If the swapchain size determines the surface size, use this size
  -> SurfaceKHR
  -> V SwapchainResources
allocSwapchainResources oldSwapchain windowSize surface = do
  info@SwapchainInfo {..} <- createSwapchain oldSwapchain windowSize surface

  -- TODO: cache this, it's probably not going to change
  (renderPassKey, srRenderPass) <- RenderPass.createRenderPass
    (format (siSurfaceFormat :: SurfaceFormatKHR))

  -- Get all the swapchain images, and create views for them
  (_            , swapchainImages) <- getSwapchainImagesKHR' siSwapchain
  (imageViewKeys, imageViews     ) <-
    fmap V.unzip . V.forM swapchainImages $ \image ->
      Framebuffer.createImageView
        (format (siSurfaceFormat :: SurfaceFormatKHR))
        image

  -- Also create a framebuffer for each one
  (framebufferKeys, framebuffers) <-
    fmap V.unzip . V.forM imageViews $ \imageView ->
      Framebuffer.createFramebuffer srRenderPass imageView siImageExtent

  -- This refcount is released in 'recreateSwapchainResources'
  releaseResources <- newRefCounted $ do
    traverse_ release framebufferKeys
    traverse_ release imageViewKeys
    release renderPassKey
    release siSwapchainReleaseKey

  pure $ SwapchainResources info
                            framebuffers
                            imageViews
                            swapchainImages
                            srRenderPass
                            releaseResources

recreateSwapchainResources
  :: SDL.Window
  -> SwapchainResources
  -- ^ The reference to these resources will be dropped
  -> V SwapchainResources
recreateSwapchainResources win oldResources = do
  SDL.V2 width height <- SDL.vkGetDrawableSize win
  let oldSwapchain = siSwapchain . srInfo $ oldResources
      oldSurface   = siSurface . srInfo $ oldResources
  r <- allocSwapchainResources
    oldSwapchain
    (Extent2D (fromIntegral width) (fromIntegral height))
    oldSurface
  releaseRefCounted (srRelease oldResources)
  pure r

----------------------------------------------------------------
-- Creating the actual swapchain
----------------------------------------------------------------

-- | Create a swapchain from a 'SurfaceKHR'
createSwapchain
  :: SwapchainKHR
  -- ^ Old swapchain, can be NULL_HANDLE
  -> Extent2D
  -- ^ If the swapchain size determines the surface size, use this size
  -> SurfaceKHR
  -> V SwapchainInfo
createSwapchain oldSwapchain explicitSize surf = do
  surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR' surf

  -- Check flags
  for_ requiredUsageFlags $ \f ->
    unless (supportedUsageFlags surfaceCaps .&&. f)
      $ throwString ("Surface images do not support " <> show f)

  -- Select a present mode
  (_, availablePresentModes) <- getPhysicalDeviceSurfacePresentModesKHR' surf
  presentMode                <-
    case filter (`V.elem` availablePresentModes) desiredPresentModes of
      [] -> throwString "Unable to find a suitable present mode for swapchain"
      x : _ -> pure x

  -- Select a surface format
  -- getPhysicalDeviceSurfaceFormatsKHR doesn't return an empty list
  (_, availableFormats) <- getPhysicalDeviceSurfaceFormatsKHR' surf
  let surfaceFormat = selectSurfaceFormat availableFormats

  -- Calculate the extent
  let imageExtent =
        case currentExtent (surfaceCaps :: SurfaceCapabilitiesKHR) of
          Extent2D w h | w == maxBound, h == maxBound -> explicitSize
          e -> e

  let
    imageCount =
      let
        limit = case maxImageCount (surfaceCaps :: SurfaceCapabilitiesKHR) of
          0 -> maxBound
          n -> n
        -- Request one additional image to prevent us having to wait for
        -- the driver to finish
        buffer = 1
        desired =
          buffer + minImageCount (surfaceCaps :: SurfaceCapabilitiesKHR)
      in
        min limit desired

  compositeAlphaMode <-
    if COMPOSITE_ALPHA_OPAQUE_BIT_KHR .&&. supportedCompositeAlpha surfaceCaps
      then pure COMPOSITE_ALPHA_OPAQUE_BIT_KHR
      else throwString "Surface doesn't support COMPOSITE_ALPHA_OPAQUE_BIT_KHR"

  let
    swapchainCreateInfo = SwapchainCreateInfoKHR
      { surface            = surf
      , next               = ()
      , flags              = zero
      , queueFamilyIndices = mempty -- No need to specify when not using concurrent access
      , minImageCount      = imageCount
      , imageFormat        = format (surfaceFormat :: SurfaceFormatKHR)
      , imageColorSpace    = colorSpace surfaceFormat
      , imageExtent        = imageExtent
      , imageArrayLayers   = 1
      , imageUsage         = foldr (.|.) zero requiredUsageFlags
      , imageSharingMode   = SHARING_MODE_EXCLUSIVE
      , preTransform = currentTransform (surfaceCaps :: SurfaceCapabilitiesKHR)
      , compositeAlpha     = compositeAlphaMode
      , presentMode        = presentMode
      , clipped            = True
      , oldSwapchain       = oldSwapchain
      }

  (key, swapchain) <- withSwapchainKHR' swapchainCreateInfo

  pure $ SwapchainInfo swapchain key presentMode surfaceFormat imageExtent surf

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Catch an ERROR_OUT_OF_DATE_KHR exception and return 'True' if that happened
threwSwapchainError :: V a -> V Bool
threwSwapchainError = fmap isLeft . tryJust swapchainError
 where
  swapchainError = \case
    VulkanException e@ERROR_OUT_OF_DATE_KHR -> Just e
    -- TODO handle this case
    -- VulkanException e@ERROR_SURFACE_LOST_KHR -> Just e
    VulkanException _                       -> Nothing

----------------------------------------------------------------
-- Specifications
----------------------------------------------------------------

-- The vector passed will have at least one element
selectSurfaceFormat :: Vector SurfaceFormatKHR -> SurfaceFormatKHR
selectSurfaceFormat = V.maximumBy (comparing surfaceFormatScore)
 where
  -- An ordered list of formats to choose for the swapchain images, if none
  -- match then the first available format will be chosen.
  surfaceFormatScore :: SurfaceFormatKHR -> Int
  surfaceFormatScore = \case
    _ -> 0

-- | An ordered list of the present mode to be chosen for the swapchain.
desiredPresentModes :: [PresentModeKHR]
desiredPresentModes =
  [PRESENT_MODE_MAILBOX_KHR, PRESENT_MODE_FIFO_KHR, PRESENT_MODE_IMMEDIATE_KHR]

-- | The images in the swapchain must support these flags.
requiredUsageFlags :: [ImageUsageFlagBits]
requiredUsageFlags = [IMAGE_USAGE_COLOR_ATTACHMENT_BIT]
