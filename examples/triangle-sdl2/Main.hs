module Main where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text)
import qualified Triangle
import qualified Vulkan.Utils.Init.SDL2.Window as Window
import Vulkan.Utils.Swapchain (defaultSwapchainConfig)
import Vulkan.Zero (zero)
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = runResourceT $ do
  Window.withSDL
  window <- Window.createWindow appName windowWidth windowHeight
  Window.showWindow window
  (vc, _vma, initialSC) <-
    withWindowedVk
      WindowedConfig
        { appName = appName
        , instanceReqs = []
        , deviceReqs = []
        , vmaFlags = zero
        , swapchainConfig = defaultSwapchainConfig
        }
      (Window.sdl2Adapter window)
  Triangle.runTriangle
    vc
    initialSC
    (Window.drawableSize window)
    (Window.shouldQuit window)

appName :: Text
appName = "Haskell Vulkan triangle example"

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600
