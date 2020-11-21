{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Cache
  ( Cache
  , Token
  , newCache
  , newCacheR
  , cachedCreate
  , cachedCreateR
  )
where

import           Control.Concurrent.MVar
import           Data.IORef
import           Data.Primitive.Unlifted.Weak.IO
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Primitive.Unlifted.Class  ( PrimUnlifted )
import           Control.Monad                  ( join )
import           UnliftIO.Exception             ( mask )
import           UnliftIO                       ( MonadUnliftIO )
import           Data.Functor                   ( void )
import           Control.Monad.Trans.Resource   ( ReleaseKey )
import qualified Control.Monad.Trans.Resource  as R
import           Data.Bifunctor                 ( Bifunctor(first) )

data Cache m k v = Cache
  { cacheCreate :: k -> m (IO (), v)
    -- ^ Action to create new cache elements
  , cacheMap    :: MVar (Map k (UnliftedWeak (MVar (IO (), Token v, v))))
    -- ^ A map containing weak pointers to (release action, touch token, value)
  }

-- | Create a cache
newCache :: MonadIO n => (k -> m (IO (), v)) -> n (Cache m k v)
newCache c = liftIO $ Cache c <$> newMVar Map.empty

-- | Create a cache which creates objects with a 'ReleaseKey' instead of a
-- release action.
newCacheR
  :: (Functor m, MonadIO n) => (k -> m (ReleaseKey, v)) -> n (Cache m k v)
newCacheR c = liftIO $ Cache (fmap (first R.release) . c) <$> newMVar Map.empty

newtype Token a = Token { unToken :: IORef () }
  deriving newtype PrimUnlifted

-- | Keep the object referred to by this token alive until at least now
--
-- Note that if the object has been explicitly released before now this won't
-- do anything.
touchToken :: MonadIO m => Token v -> m ()
touchToken = liftIO . touchUnlifted . unToken

-- | The same as cachedCreate but doesn't return the destroy action.
--
-- This is intended to be used when using a monad, m, which performs its own
-- resource tracking where the destroy action would be unnecessary.
--
-- Keep in mind that the cache has no visibility into when the user generated
-- destroy action is called, so it's possible to destroy objects from under the
-- nose of the cache. To avoid this, make sure not to use the cache after one
-- of these destructors has been called, i.e. don't return the 'Cache' out of
-- 'runResourceT'
cachedCreateR :: (MonadUnliftIO m, Ord k) => Cache m k b -> k -> m (Token b, b)
cachedCreateR c k = do
  (_, t, r) <- cachedCreate c k
  pure (t, r)

-- | Fetch an element from a cache, or create a new one and insert it.
--
-- If the element is already in the cache it's returned immediately, otherwise
-- it's created and inserted.
--
-- Elements are kept in the cache using weak pointers, so if they only exist in
-- the cache then they may be collected and deallocated (not promptly).
--
-- Note that there is no reference counting, so manually releasing a resource
-- (using the returned release action) better be done when all other users of
-- it are done.
--
-- If you need your resources to be deallocated without fail then make sure you
-- use a monad, @m@, which supports that, such as the 'ResourceT' monad.
cachedCreate
  :: forall m k v
   . (MonadUnliftIO m, Ord k)
  => Cache m k v
  -> k
  -- ^ The key to lookup
  -> m (IO (), Token v, v)
  -- ^
  -- ( An action which can be called to release this resource immediately (this
  --   may be called more than once)
  -- , A token to pass to 'touchToken' to indicate that the value must be alive
  --   until at least this point (can be called with no effect after the
  --   release action)
  -- , the created value
  -- )
cachedCreate Cache {..} k = do
  let insert m = do
        -- Tie the knot for the release action. The release action is required
        -- when we create the weak pointer, however we don't want to have to
        -- create it now as we have taken the Map MVar.
        --
        -- Create an empty action here in case there's an exception between
        -- creating the finalizer and setting the release action.
        releaseRef <- newMVar (pure ())
        let runRelease = join . readMVar $ releaseRef

        v <- newEmptyMVar
        i <- newToken
        w <- mkWeakFromUnliftedToUnlifted i v (Just runRelease)
        let m' = Map.insert k w m

        -- Return the new map, along with an action to create the object and
        -- put it in the map
        pure . (m', ) $ do
          -- If we have a new one:
          -- - Create the object
          -- - Create a weak pointer with the release action as finalizer
          -- - Put it into the MVar
          a <- mask $ \_ -> do
            (release, a) <- cacheCreate k
            void . liftIO $ swapMVar releaseRef release
            -- Make sure the Weak stays alive until at least it's been given
            -- the correct release action.
            touchToken i
            pure a
          let r = (finalizeUnlifted w, i, a)
          liftIO $ putMVar v r
          pure r

  -- This takes the MVar and returns an action to run after, do this so we
  -- spend as little time as possible locking the cache.
  join . liftIO . modifyMVar cacheMap $ \m -> do
    case Map.lookup k m of
      -- It's never been in the map, make a new one
      Nothing -> insert m
      Just w  -> deRefUnliftedWeak w >>= \case
        -- It was in the map but has been released, make a new one
        Nothing -> insert m
        -- It's in the map and still alive, return it without modifying the map
        Just v  -> pure (m, liftIO $ readMVar v)

newToken :: IO (Token v)
newToken = Token <$> newIORef ()

-- test :: IO ()
-- test = do
--   runResourceT $ do
--     c <- newCacheR $ \k -> allocate (sayShow ("create", k) >> pure k)
--                                     (\l -> (sayShow ("destroy", l)))
--     say "new cache"

--     (releaseTwo, two, _) <- cachedCreate c 2
--     say "created 2"

--     liftIO performGC
--     liftIO $ threadDelay 1e6
--     liftIO performGC

--     (releaseThree, three, _) <- cachedCreate c 3
--     say "created 3"

--     liftIO performGC
--     liftIO $ threadDelay 1e6
--     liftIO performGC
--     liftIO $ threadDelay 1e6
--     liftIO performGC

--     say "release 2"
--     say "released 2"

--     (releaseTwo2, two2, _) <- cachedCreate c 2
--     say "created another 2"

--     say "touching"
--     touchToken two
--     say "touched"

--   say "end"
