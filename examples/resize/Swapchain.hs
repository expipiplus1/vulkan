module Swapchain
  where

import           Control.Exception.Safe
import           Control.Monad.Trans.Resource
import qualified Data.Vector                   as V
import           Say
import           Vulkan.Core10
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Zero

import           MonadVulkan

-- Create a swapchain from an image
createSwapchain
  :: SwapchainKHR
  -- ^ Old swapchain, can be NULL_HANDLE
  -> Extent2D
  -- ^ If the swapchain size determines the surface size, use this size
  -> SurfaceKHR
  -> V (ReleaseKey, SwapchainKHR, SurfaceFormatKHR, Extent2D)
createSwapchain oldSwapchain explicitSize surf = do
  surfaceCaps                <- getPhysicalDeviceSurfaceCapabilitiesKHR' surf

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
      , imageUsage       = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      , imageSharingMode = SHARING_MODE_EXCLUSIVE
      , preTransform = currentTransform (surfaceCaps :: SurfaceCapabilitiesKHR)
      , compositeAlpha   = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
      , presentMode      = presentMode
      , clipped          = True
      , oldSwapchain     = oldSwapchain
      }

  (key, swapchain) <- withSwapchainKHR' swapchainCreateInfo
  pure (key, swapchain, surfaceFormat, imageExtent)
