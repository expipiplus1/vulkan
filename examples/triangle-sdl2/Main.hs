{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.String (IsString)
import Data.Text.Encoding (decodeUtf8)
import Frame
  ( frameDeviceRequirements
  , frameInstanceRequirements
  )
import InitDevice (withDevice)
import Say
import Swapchain (allocSwapchain)
import qualified Triangle
import VkResources (mkVkResources)
import qualified Vma
import qualified Vulkan.Core10 as Vk
import Vulkan.Requirement (DeviceRequirement (..))
import qualified Vulkan.Utils.Init.SDL2 as Init
import Vulkan.Zero (zero)
import qualified Window.SDL2 as Window

main :: IO ()
main = runResourceT $ do
  Window.withSDL
  window <- Window.createWindow appName windowWidth windowHeight
  inst <-
    Init.withInstance
      window
      (Just zero{Vk.applicationName = Just appName, Vk.apiVersion = Vk.API_VERSION_1_0})
      frameInstanceRequirements
      []
  surface <- Init.withSurface inst window
  let deviceReqs =
        [ RequireDeviceExtension Nothing e minBound
        | e <- Init.getRequiredDeviceExtensions
        ]
          ++ frameDeviceRequirements
  (phys, dev, qs) <- withDevice inst surface deviceReqs
  vma <- Vma.createVMA zero Vk.API_VERSION_1_0 inst phys dev
  props <- Vk.getPhysicalDeviceProperties phys
  sayErr $ "Using device: " <> decodeUtf8 (Vk.deviceName props)

  vr <- liftIO $ mkVkResources inst phys dev vma qs

  initialSize <- Window.drawableSize window
  initialSC <- allocSwapchain vr Vk.NULL_HANDLE initialSize surface

  Window.showWindow window
  Triangle.runTriangle vr initialSC (Window.drawableSize window) (Window.shouldQuit Window.NoLimit)

appName :: (IsString a) => a
appName = "Haskell Vulkan triangle example"

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600
