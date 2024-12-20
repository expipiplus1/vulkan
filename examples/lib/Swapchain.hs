{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Swapchain
  ( SwapchainInfo(..)
  , SwapchainResources(..)
  , allocSwapchainResources
  , recreateSwapchainResources
  , threwSwapchainError
  ) where

import           AutoApply
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
import           GHC.Generics                   ( Generic )
import           HasVulkan
import           InstrumentDecs
import           Language.Haskell.TH            ( nameBase )
import           NoThunks.Class
import           Orphans                        ( )
import           RefCounted
import qualified SDL
import qualified SDL.Video.Vulkan              as SDL
import           UnliftIO.Exception             ( throwString
                                                , tryJust
                                                )
import           Vulkan.Core10
import           Vulkan.Exception
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_surface as SurfaceCapabilitiesKHR (SurfaceCapabilitiesKHR(..))
import           Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR(..))
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Utils.Misc
import           Vulkan.Zero

instrumentDecs (Just . init . nameBase) =<< autoapplyDecs
  (<> "'")
  [ 'getDevice
  , 'getPhysicalDevice
  , 'getInstance
  , 'getAllocator
  , 'noAllocationCallbacks
  , 'noPipelineCache
  ]
  [ 'allocate ]
  [ 'getSwapchainImagesKHR
  , 'getPhysicalDeviceSurfaceCapabilitiesKHR
  , 'getPhysicalDeviceSurfacePresentModesKHR
  , 'getPhysicalDeviceSurfaceFormatsKHR
  , 'withSwapchainKHR
  ]

data SwapchainInfo = SwapchainInfo
  { siSwapchain           :: SwapchainKHR
  , siSwapchainReleaseKey :: ReleaseKey
  , siPresentMode         :: PresentModeKHR
  , siSurfaceFormat       :: SurfaceFormatKHR
  , siImageExtent         :: Extent2D
  , siSurface             :: SurfaceKHR
  }
  deriving (Generic, NoThunks)

data SwapchainResources = SwapchainResources
  { srInfo       :: SwapchainInfo
  , srImageViews :: Vector ImageView
  , srImages     :: Vector Image
  , srRelease    :: RefCounted
  }
  deriving (Generic, NoThunks)

----------------------------------------------------------------
-- All the resources which depend on the swapchain
----------------------------------------------------------------

-- | Allocate everything which depends on the swapchain
allocSwapchainResources
  :: (MonadUnliftIO m, MonadResource m, HasVulkan m)
  => SwapchainKHR
  -- ^ Previous swapchain, can be NULL_HANDLE
  -> Extent2D
  -- ^ If the swapchain size determines the surface size, use this size
  -> SurfaceKHR
  -> m SwapchainResources
allocSwapchainResources oldSwapchain windowSize surface = do
  info@SwapchainInfo {..} <- createSwapchain oldSwapchain windowSize surface

  -- Get all the swapchain images, and create views for them
  (_, swapchainImages) <- getSwapchainImagesKHR' siSwapchain
  (imageViewKeys, imageViews) <-
    fmap V.unzip . V.forM swapchainImages $ \image ->
      Framebuffer.createImageView
        (SurfaceFormatKHR.format siSurfaceFormat)
        image

  -- This refcount is released in 'recreateSwapchainResources'
  releaseResources <- newRefCounted $ do
    traverse_ release imageViewKeys
    release siSwapchainReleaseKey

  pure $ SwapchainResources info imageViews swapchainImages releaseResources

recreateSwapchainResources
  :: (MonadUnliftIO m, MonadResource m, HasVulkan m)
  => SDL.Window
  -> SwapchainResources
  -- ^ The reference to these resources will be dropped
  -> m SwapchainResources
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
  :: (MonadUnliftIO m, MonadResource m, HasVulkan m)
  => SwapchainKHR
  -- ^ Old swapchain, can be NULL_HANDLE
  -> Extent2D
  -- ^ If the swapchain size determines the surface size, use this size
  -> SurfaceKHR
  -> m SwapchainInfo
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
          buffer + SurfaceCapabilitiesKHR.minImageCount surfaceCaps
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
      , imageFormat        = SurfaceFormatKHR.format surfaceFormat
      , imageColorSpace    = colorSpace surfaceFormat
      , imageExtent        = imageExtent
      , imageArrayLayers   = 1
      , imageUsage         = foldr (.|.) zero requiredUsageFlags
      , imageSharingMode   = SHARING_MODE_EXCLUSIVE
      , preTransform       = SurfaceCapabilitiesKHR.currentTransform surfaceCaps
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
threwSwapchainError :: MonadUnliftIO f => f b -> f Bool
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
  [ PRESENT_MODE_FIFO_RELAXED_KHR
  , PRESENT_MODE_FIFO_KHR --  ^ This will always be present
  , PRESENT_MODE_IMMEDIATE_KHR --  ^ Keep this here for easy swapping for testing
  ]

-- | The images in the swapchain must support these flags.
requiredUsageFlags :: [ImageUsageFlagBits]
requiredUsageFlags =
  [IMAGE_USAGE_COLOR_ATTACHMENT_BIT, IMAGE_USAGE_STORAGE_BIT]
