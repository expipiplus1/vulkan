{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text)
import qualified TriangleDynamic
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PhysicalDeviceDynamicRenderingFeatures)
import qualified Vulkan.Utils.Init.GLFW.Window as Window
import qualified Vulkan.Utils.Requirements.TH as U
import Vulkan.Utils.Swapchain (defaultSwapchainConfig)
import Vulkan.Zero (zero)
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = runResourceT $ do
  Window.withGLFW
  window <- Window.createWindow appName windowWidth windowHeight
  Window.showWindow window
  (vc, _vma, initialSC) <-
    withWindowedVk
      WindowedConfig
        { appName = appName
        , instanceReqs = []
        , deviceReqs =
            [U.reqs|
              VK_KHR_dynamic_rendering
              PhysicalDeviceDynamicRenderingFeatures.dynamicRendering
            |]
        , vmaFlags = zero
        , swapchainConfig = defaultSwapchainConfig
        }
      (Window.glfwAdapter window)
  TriangleDynamic.runTriangle
    vc
    initialSC
    (Window.drawableSize window)
    (Window.shouldQuit window)

appName :: Text
appName = "Haskell Vulkan dynamic-rendering triangle (GLFW)"

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600
