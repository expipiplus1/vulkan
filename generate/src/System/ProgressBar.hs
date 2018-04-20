module System.ProgressBar
  ( withProgress
  ) where

import Control.Concurrent.Async.Pool
import System.Console.AsciiProgress (Options(..), displayConsoleRegions,
                                     def, newProgressBar, tick)

-- | Traverse concurrently with several threads while showing a progress bar
withProgress
  :: Traversable t
  => Int
  -- ^ Number of threads to run
  -> (a -> IO b)
  -> t a
  -> IO (t b)
withProgress numThreads f t = displayConsoleRegions $ do
  let numElements = length t
  pg <- newProgressBar def { pgTotal = fromIntegral numElements }
  withTaskGroup numThreads $ \g -> mapTasks g ((\x -> f x <* tick pg) <$> t)
