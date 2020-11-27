module Vulkan.Utils.Internal where

import           Control.Monad.IO.Class
import           GHC.IO                         ( throwIO )
import           GHC.IO.Exception               ( IOErrorType(..)
                                                , IOException(..)
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import Language.Haskell.TH.Quote

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

badQQ :: String -> QuasiQuoter
badQQ name = QuasiQuoter (bad "expression")
                         (bad "pattern")
                         (bad "type")
                         (bad "declaration")
 where
  bad :: String -> a
  bad context =
    error $ "Can't use " <> name <> " quote in a " <> context <> " context"
