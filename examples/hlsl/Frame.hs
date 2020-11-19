module Frame
  where

import qualified SDL
import Data.Word
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainKHR)
import Swapchain
import qualified SDL.Video.Vulkan as SDL

-- | All the information required to render a single frame, parameterized by
-- the type of previous frames.
data Frame f = Frame
  { fIndex         :: Word64 -- ^ Which number frame is this
    -- SDL things
  , fWindow        :: SDL.Window
    -- Vulkan things
  , fSurface       :: SurfaceKHR
  , fSwapchainInfo :: SwapchainInfo
  -- , fSwapchainFormat         :: Format
  -- , fRenderPass              :: RenderPass
  -- , fImageExtent             :: Extent2D
  -- , fImageAvailableSemaphore :: Semaphore
  -- , fRenderFinishedSemaphore :: Semaphore
  -- , fPipeline                :: Pipeline
  -- , fJuliaPipeline           :: Pipeline
  -- , fJuliaPipelineLayout     :: PipelineLayout
  -- , fJuliaDescriptorSets     :: Word32 -> DescriptorSet
  -- , fImages                  :: Word32 -> Image
  -- , fImageViews              :: Word32 -> ImageView
  -- , fFramebuffers            :: Word32 -> Framebuffer
  }

initialFrame fWindow fSurface = do
  let fPreviousFrame = ()
      fIndex         = 0
  SDL.V2 width height <- SDL.vkGetDrawableSize fWindow
  let windowSize = Extent2D (fromIntegral width) (fromIntegral height)
  fSwapchainInfo <- createSwapchain NULL_HANDLE windowSize fSurface
  pure Frame { .. }

