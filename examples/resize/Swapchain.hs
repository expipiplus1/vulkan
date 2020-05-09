module Swapchain
  where

import           Control.Exception.Safe
import qualified Data.Vector                   as V
import           Vulkan.Core10
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Zero

import           MonadVulkan

-- Create a swapchain from an image
createSwapchain
  :: Extent2D
  -- ^ If the swapchain size determines the surface size, use this size
  -> SurfaceKHR
  -> V (SwapchainKHR, SurfaceFormatKHR, Extent2D)
createSwapchain explicitSize surf = do
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

  (_, availableFormats) <- getPhysicalDeviceSurfaceFormatsKHR' surf
  let desiredFormats = []
      surfaceFormat  = case filter (`V.elem` availableFormats) desiredFormats of
        -- Use the first available format if we don't have our desired one
        []    -> V.head availableFormats
        x : _ -> x

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
      }

  (_, swapchain) <- withSwapchainKHR' swapchainCreateInfo
  pure (swapchain, surfaceFormat, imageExtent)
