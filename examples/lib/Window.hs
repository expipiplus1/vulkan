module Window
  ( VulkanWindow(..)
  ) where

import           Data.Word                      ( Word32 )
import qualified Data.Vector                   as V
import           Vulkan.Core10                  ( Device
                                                , Extent2D
                                                , Format
                                                , ImageView
                                                , Queue
                                                )
import           Vulkan.Extensions.VK_KHR_surface
                                                ( SurfaceKHR )
import           Vulkan.Extensions.VK_KHR_swapchain
                                                ( SwapchainKHR )

data VulkanWindow w = VulkanWindow
  { vwWindow                   :: w
  , vwDevice                   :: Device
  , vwSurface                  :: SurfaceKHR
  , vwSwapchain                :: SwapchainKHR
  , vwExtent                   :: Extent2D
  , vwFormat                   :: Format
  , vwImageViews               :: V.Vector ImageView
  , vwGraphicsQueue            :: Queue
  , vwGraphicsQueueFamilyIndex :: Word32
  , vwPresentQueue             :: Queue
  }
