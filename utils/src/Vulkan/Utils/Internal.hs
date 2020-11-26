module Vulkan.Utils.Internal where

import           Control.Monad.IO.Class
import           GHC.IO                         ( throwIO )
import           GHC.IO.Exception               ( IOErrorType(..)
                                                , IOException(..)
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

----------------------------------------------------------------
-- Internal utils
----------------------------------------------------------------

unsatisfiedConstraints :: String -> IO a
unsatisfiedConstraints message =
  throwIO $ IOError Nothing UnsatisfiedConstraints "" message Nothing Nothing

noSuchThing :: String -> IO a
noSuchThing message =
  throwIO $ IOError Nothing NoSuchThing "" message Nothing Nothing

sayErr :: MonadIO m => String -> m ()
sayErr = liftIO . hPutStrLn stderr
