module Swapchain
  ( createSwapchain
  , threwSwapchainError
  , recreateSwapchain
  , allocSwapchainResources
  ) where

import           Control.Monad                  ( unless )
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Either                    ( isLeft )
import           Data.Foldable                  ( traverse_ )
import qualified Data.Vector                   as V
import           RefCounted
import qualified SDL
import qualified SDL.Video.Vulkan              as SDL
import           Say
import           UnliftIO.Exception             ( throwString
                                                , tryJust
                                                )
import           Vulkan.Core10           hiding ( createFramebuffer
                                                , createImageView
                                                )
import           Vulkan.Exception
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Zero

import           Frame
import           Framebuffer
import           MonadVulkan
import           Pipeline

-- | Create a swapchain from a surface
createSwapchain
  :: SwapchainKHR
  -- ^ Old swapchain, can be NULL_HANDLE
  -> Extent2D
  -- ^ If the swapchain size determines the surface size, use this size
  -> SurfaceKHR
  -> V (ReleaseKey, SwapchainKHR, SurfaceFormatKHR, Extent2D)
createSwapchain oldSwapchain explicitSize surf = do
  surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR' surf

  unless (supportedUsageFlags surfaceCaps .&&. IMAGE_USAGE_STORAGE_BIT)
    $ throwString "Surface images do not support IMAGE_USAGE_STORAGE_BIT"
  unless (supportedUsageFlags surfaceCaps .&&. IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
    $ throwString
        "Surface images do not support IMAGE_USAGE_COLOR_ATTACHMENT_BIT"

  (_, availablePresentModes) <- getPhysicalDeviceSurfacePresentModesKHR' surf
  let desiredPresentModes =
        [ PRESENT_MODE_MAILBOX_KHR
        , PRESENT_MODE_FIFO_KHR
        , PRESENT_MODE_IMMEDIATE_KHR
        ]
  presentMode <-
    case filter (`V.elem` availablePresentModes) desiredPresentModes of
      [] -> throwString "Unable to find a suitable present mode for swapchain"
      x : _ -> pure x
  sayErrString $ "Using present mode " <> show presentMode

  (_, availableFormats) <- getPhysicalDeviceSurfaceFormatsKHR' surf
  let desiredFormats = []
      surfaceFormat  = case filter (`V.elem` availableFormats) desiredFormats of
        -- Use the first available format if we don't have our desired one
        []    -> V.head availableFormats
        x : _ -> x
  sayErrString $ "Using surface format " <> show surfaceFormat

  let imageExtent =
        case currentExtent (surfaceCaps :: SurfaceCapabilitiesKHR) of
          Extent2D w h | w == maxBound, h == maxBound -> explicitSize
          e -> e

  let
    swapchainCreateInfo = zero
      { surface          = surf
      , minImageCount    = minImageCount (surfaceCaps :: SurfaceCapabilitiesKHR)
                             + 1
      , imageFormat      = (format :: SurfaceFormatKHR -> Format) surfaceFormat
      , imageColorSpace  = colorSpace surfaceFormat
      , imageExtent      = imageExtent
      , imageArrayLayers = 1
      , imageUsage       = IMAGE_USAGE_STORAGE_BIT
                             .|. IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      , imageSharingMode = SHARING_MODE_EXCLUSIVE
      , preTransform = currentTransform (surfaceCaps :: SurfaceCapabilitiesKHR)
      , compositeAlpha   = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
      , presentMode      = presentMode
      , clipped          = True
      , oldSwapchain     = oldSwapchain
      }

  (key, swapchain) <- withSwapchainKHR' swapchainCreateInfo
  pure (key, swapchain, surfaceFormat, imageExtent)


----------------------------------------------------------------
-- Utils for recreating a swapchain
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

-- |
recreateSwapchain :: Frame -> V Frame
recreateSwapchain f@Frame {..} = do
  SDL.V2 width height <- SDL.vkGetDrawableSize fWindow
  (swapchain, imageExtent, framebuffers, imageViews, images, newFormat, releaseSwapchain) <-
    allocSwapchainResources
      (Extent2D (fromIntegral width) (fromIntegral height))
      fSwapchain
      fSurface

  unless (newFormat == fSwapchainFormat)
    $ throwString "New swapchain has a different (unhandled) format"

  releaseRefCounted fReleaseSwapchain

  pure f { fSwapchain        = swapchain
         , fImageExtent      = imageExtent
         , fFramebuffers     = (framebuffers V.!) . fromIntegral
         , fImages           = (images V.!) . fromIntegral
         , fImageViews       = (imageViews V.!) . fromIntegral
         , fReleaseSwapchain = releaseSwapchain
         }

allocSwapchainResources
  :: Extent2D
  -> SwapchainKHR
  -- ^ Previous swapchain, can be NULL_HANDLE
  -> SurfaceKHR
  -> V
       ( SwapchainKHR
       , Extent2D
       , V.Vector Framebuffer
       , V.Vector ImageView
       , V.Vector Image
       , Format
       , RefCounted
       )
allocSwapchainResources windowSize oldSwapchain surface = do
  (swapchainKey, swapchain, surfaceFormat, imageExtent) <- createSwapchain
    oldSwapchain
    windowSize
    surface

  (renderPassKey, renderPass) <- Pipeline.createRenderPass
    (format (surfaceFormat :: SurfaceFormatKHR))
  (_            , swapchainImages) <- getSwapchainImagesKHR' swapchain
  (imageViewKeys, imageViews     ) <-
    fmap V.unzip . V.forM swapchainImages $ \image ->
      createImageView (format (surfaceFormat :: SurfaceFormatKHR)) image

  (framebufferKeys, framebuffers) <-
    fmap V.unzip . V.forM imageViews $ \imageView ->
      createFramebuffer renderPass imageView imageExtent

  releaseSwapchain <- newRefCounted $ do
    traverse_ release framebufferKeys
    traverse_ release imageViewKeys
    release renderPassKey
    release swapchainKey

  pure
    ( swapchain
    , imageExtent
    , framebuffers
    , imageViews
    , swapchainImages
    , format (surfaceFormat :: SurfaceFormatKHR)
    , releaseSwapchain
    )

----------------------------------------------------------------
-- Bit utils
----------------------------------------------------------------

infixl 4 .&&.
(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
