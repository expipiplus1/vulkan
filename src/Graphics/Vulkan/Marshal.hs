{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}

module Graphics.Vulkan.Marshal
  (
  ) where

import           Data.Vector           as V
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

withVec
  :: forall a b d
   . Storable b
  => (forall c . a -> (b -> IO c) -> IO c)
  -> Vector a
  -> (Ptr b -> IO d)
  -> IO d
withVec alloc v cont = allocaArray (V.length v) $ \p ->
  let go :: Int -> ((b -> IO d) -> IO d) -> IO d -> IO d
      go index with complete = with (\b -> pokeElemOff p index b *> complete)
  in  ifoldr go (cont p) (fmap alloc v)
