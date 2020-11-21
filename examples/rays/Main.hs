module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Frame
import           Init
import           MonadFrame
import           MonadVulkan
import           Render
import           SDL                            ( showWindow
                                                , time
                                                )
import           Swapchain                      ( threwSwapchainError )
import           Utils
import           Window

main :: IO ()
main = runResourceT $ do
  --
  -- Initialization
  --
  withSDL
  win                        <- createWindow "Vulkan ⚡ Haskell" 1280 720
  inst                       <- Init.createInstance win
  (phys, pdi, dev, qs, surf) <- Init.createDevice inst win
  vma   <- createVMA inst phys dev

  --
  -- Go
  --
  start <- SDL.time @Double
  let reportFPS f = do
        end <- SDL.time
        let frames = fIndex f
            mean   = realToFrac frames / (end - start)
        liftIO $ putStrLn $ "Average: " <> show mean

  let rtInfo = pdiRTInfo pdi

  let frame f = do
        shouldQuit >>= \case
          True -> do
            reportFPS f
            pure Nothing
          False -> Just <$> do
            needsNewSwapchain <- threwSwapchainError (runFrame f renderFrame)
            advanceFrame needsNewSwapchain f

  runV inst phys rtInfo dev qs vma $ do
    initial <- initialFrame win surf
    showWindow win
    loopJust frame initial
