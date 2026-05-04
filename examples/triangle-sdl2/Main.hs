{-# LANGUAGE OverloadedLists #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.String                    ( IsString )
import           Data.Text.Encoding             ( decodeUtf8 )
import           InitDevice                     ( withGraphicsPresentDevice )
import           Say
import qualified Triangle
import qualified Vma
import           VkResources                    ( Queues(..)
                                                , mkVkResources
                                                )
import           Vulkan.Core10
import           Vulkan.Requirement             ( DeviceRequirement(..) )
import           Vulkan.Zero                    ( zero )
import qualified Vulkan.Utils.Init.SDL2        as Init
import           Frame                          ( frameDeviceRequirements
                                                , frameInstanceRequirements
                                                )
import           Swapchain                      ( allocSwapchain )
import qualified Window.SDL2                   as Window

main :: IO ()
main = runResourceT $ do
  Window.withSDL
  window <- Window.createWindow appName windowWidth windowHeight
  inst   <- Init.withInstance
    window
    (Just zero { applicationName = Just appName, apiVersion = API_VERSION_1_0 })
    frameInstanceRequirements
    []
  surface              <- Init.withSurface inst window
  let deviceReqs =
        [ RequireDeviceExtension Nothing e minBound
        | e <- Init.getRequiredDeviceExtensions
        ] ++ frameDeviceRequirements
  (phys, dev, qfi, gQueue) <-
    withGraphicsPresentDevice inst surface deviceReqs
  vma                  <- Vma.createVMA zero API_VERSION_1_0 inst phys dev
  props                <- getPhysicalDeviceProperties phys
  sayErr $ "Using device: " <> decodeUtf8 (deviceName props)

  let qs = Queues (qfi, gQueue)
  vr <- liftIO $ mkVkResources inst phys dev vma qs

  initialSize <- Window.drawableSize window
  initialSC   <- allocSwapchain vr NULL_HANDLE initialSize surface

  Window.showWindow window
  Triangle.runTriangle vr initialSC (Window.drawableSize window) (Window.shouldQuit Window.NoLimit)

appName :: IsString a => a
appName = "Haskell Vulkan triangle example"

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600
