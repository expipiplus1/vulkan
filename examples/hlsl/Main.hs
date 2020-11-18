module Main where

import           Control.Monad.Trans.Resource

import           Control.Monad
import           Init
import           MonadVulkan
import qualified SDL
import           Window

main :: IO ()
main = runResourceT $ do
  --
  -- Initialization
  --
  withSDL
  win                        <- createWindow "Vulkan 🚀 Haskell" 1280 720
  inst                       <- Init.createInstance win
  (phys, dev, Queues (i, q)) <- Init.createDevice inst win
  vma                        <- createVMA inst phys dev

  --
  -- Go
  --
  let commandPools = mempty
  runV inst phys dev q i commandPools vma $ do
    SDL.showWindow win
    loop shouldQuit

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

loop :: Monad m => m Bool -> m ()
loop m = do
  q <- m
  unless q $ loop m
