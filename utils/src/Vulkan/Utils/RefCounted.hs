{-| Lightweight reference counter that runs a release action when the count
hits zero. Useful for keeping a Vulkan object alive across an unknown number
of in-flight frames — bump the count when a frame starts using it, drop the
count when the frame retires, and the object is destroyed promptly after the
last frame finishes.
-}
module Vulkan.Utils.RefCounted
  ( RefCounted
  , newRefCounted
  , releaseRefCounted
  , takeRefCounted
  , resourceTRefCount
  ) where

import Control.Exception (mask, throwIO)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, allocate_)
import Data.IORef
import GHC.IO.Exception (IOErrorType (UserError), IOException (IOError))

-- | A 'RefCounted' will perform the specified action when the count reaches 0
data RefCounted = RefCounted
  { rcCount :: IORef Int
  , rcAction :: IO ()
  }

-- | Create a counter with a value of 1
newRefCounted :: (MonadIO m) => IO () -> m RefCounted
newRefCounted rcAction = do
  rcCount <- liftIO $ newIORef 1
  pure RefCounted{..}

{- | Decrement the value, the action will be run promptly and in
this thread if the counter reached 0.
-}
releaseRefCounted :: (MonadIO m) => RefCounted -> m ()
releaseRefCounted RefCounted{..} = liftIO $ mask $ \_ ->
  atomicModifyIORef' rcCount (\c -> (pred c, pred c)) >>= \case
    0 -> rcAction
    n
      | n < 0 ->
          liftIO . throwIO $
            IOError
              Nothing
              UserError
              ""
              "Ref counted value decremented below 0"
              Nothing
              Nothing
    _ -> pure ()

-- | Increment the counter by 1
takeRefCounted :: (MonadIO m) => RefCounted -> m ()
takeRefCounted RefCounted{..} =
  liftIO $ atomicModifyIORef' rcCount (\c -> (succ c, ()))

-- | Hold a reference for the duration of the 'MonadResource' action
resourceTRefCount :: (MonadResource f) => RefCounted -> f ()
resourceTRefCount r = void $ allocate_ (takeRefCounted r) (releaseRefCounted r)
