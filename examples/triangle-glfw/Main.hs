module Main where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text)
import qualified Triangle
import Vulkan.Zero (zero)
import qualified Window.GLFW as Window
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = runResourceT $ do
  Window.withGLFW
  window <- Window.createWindow appName windowWidth windowHeight
  (vr, initialSC) <-
    withWindowedVk
      WindowedConfig
        { wcAppName = appName
        , wcInstanceReqs = []
        , wcDeviceReqs = []
        , wcVmaFlags = zero
        }
      (Window.glfwAdapter window)
  Window.showWindow window
  Triangle.runTriangle
    vr
    initialSC
    (Window.drawableSize window)
    (Window.shouldQuit window)

appName :: Text
appName = "Haskell Vulkan triangle example (GLFW)"

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600
