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
import           Utils
import           Window

main :: IO ()
main = runResourceT $ do
  --
  -- Initialization
  --
  withSDL
  win                   <- createWindow "Vulkan 🚀 Haskell" 1280 720
  inst                  <- Init.createInstance win
  (phys, dev, qs, surf) <- Init.createDevice inst win
  vma                   <- createVMA inst phys dev
  commandPools          <- Init.createCommandPools dev
                                                   numConcurrentFrames
                                                   (fst . graphicsQueue $ qs)

  --
  -- Go
  --
  start <- SDL.time @Double
  let frame f = do
        shouldQuit >>= \case
          True -> do
            end <- SDL.time
            let frames = fIndex f
                mean   = realToFrac frames / (end - start)
            liftIO $ putStrLn $ "Average: " <> show mean
            pure Nothing
          False -> Just <$> do
            runFrame f renderFrame
            advanceFrame f

  runV inst phys dev qs commandPools vma $ do
    initial <- initialFrame win surf
    showWindow win
    loopJust frame initial
