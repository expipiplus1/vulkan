module Main where

import           Control.Monad.Trans.Resource
import           Data.Foldable                  ( for_ )
import           Say

import           Control.Monad
import           Control.Monad.IO.Class
import           Init
import           MonadVulkan
import qualified SDL
import           System.Exit                    ( exitFailure )
import           Vulkan.Core10                  ( Device
                                                , Instance
                                                )
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
  requireCommands inst dev
  vma <- createVMA inst phys dev

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

requireCommands :: MonadIO f => Instance -> Device -> f ()
requireCommands inst dev = case checkCommands inst dev of
  [] -> pure ()
  xs -> do
    for_ xs $ \n -> sayErr ("Failed to load function pointer for: " <> n)
    liftIO exitFailure

loop :: Monad m => m Bool -> m ()
loop m = do
  q <- m
  unless q $ loop m
