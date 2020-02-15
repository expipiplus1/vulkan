{-# language DeriveFunctor #-}
{-# language RecordWildCards #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}
{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language DerivingVia #-}
{-# language ApplicativeDo #-}
{-# language RecursiveDo #-}
{-# language StrictData #-}

module Store where

-- import Control.Monad.State.Strict
-- import Control.Monad.Reader
import Data.Bifunctor (first, second)
import Foreign.Ptr
import GHC.IO
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Semigroup
import Data.Monoid
import Data.Bits
import Data.IORef
import Control.Exception
import Control.Monad.Fix
import Control.Monad(void)

import Data.STRef
import Control.Monad.ST

----------------------------------------------------------------
-- Store is an Applicative for poking values
----------------------------------------------------------------

data Store a where
  Store :: { storeSize  :: !Int
             -- ^ The size of this allocation
           , storeAlign :: !Int
             -- ^ The alignment of this allocation, must be a power of 2
           , storePoke  :: Ptr b -> IO a
             -- ^ An action storing something in the allocated region
           }
        -> Store a
  deriving (Semigroup, Monoid) via (Ap Store a)

deriving instance Functor Store

instance Applicative Store where
  {-# inline pure #-}
  pure x = Store 0 1 (const (pure x))
  {-# inline (<*>) #-}
  Store s1 a1 p1 <*> Store s2 a2 p2 =
    let o = alignUp s1 a2
        s = o + s2
        a = max a1 a2
    in  Store s a (\mem -> p1 mem <*> p2 (mem `plusPtr` o))


type WriteState = (Int, Int, IO ())

newtype Write a = Write { unWrite :: forall s. Ptr () -> STRef s WriteState -> ST s a }

instance Functor Write where
  fmap f (Write x) = Write $ \(~p) s -> f <$> x p s

instance Applicative Write where
  {-# inline pure #-}
  pure x = Write (\(~_) _ -> pure x)
  {-# inline (<*>) #-}
  Write f <*> Write x = Write $ \(~p) s -> f p s <*> x p s

instance Monad Write where
  {-# inline (>>=) #-}
  Write x >>= f = Write $ \(~p) s -> do
    let ~x' = x p s
    Write f' <- f <$> x'
    f' p s

instance MonadFix Write where
  {-# inline mfix #-}
  mfix f = Write $ \(~p) s -> fixST (\x -> unWrite (f x) p s)

{-# inline unsafeFixIOCont #-}
unsafeFixIOCont :: ((a -> IO ()) -> a -> IO b) -> IO b
unsafeFixIOCont f = do
  ref <- newIORef (throw NonTermination)
  ~ans <- unsafeDupableInterleaveIO (readIORef ref)
  f (writeIORef ref) ans

{-# inline ask #-}
ask :: Write (Ptr ())
ask = Write $ \(~p) _ -> pure p

{-# inline get #-}
get :: Write WriteState
get = Write $ \(~_) r -> readSTRef r

{-# inline set #-}
set :: WriteState -> Write ()
set s = Write $ \(~_) r -> writeSTRef r s

{-# inline [2] write #-}
write :: Int -> Int -> (Ptr a -> IO ()) -> Write (Ptr a)
write storeSize storeAlign storePoke = do
  ~mem                <- ask
  (size, align, poke) <- get
  let padded  = alignUp size storeAlign
      size'   = padded + storeSize
      align'  = max align storeAlign
      ~offset = mem `plusPtr` padded
      poke'   = poke >> storePoke offset
  set (size', align', poke')
  pure offset

{-# inline alignUp #-}
alignUp :: Int -> Int -> Int
alignUp offset alignment = (offset + (alignment - 1)) .&. (-alignment)

{-# inline renderWrite #-}
renderWrite :: Write a -> IO a
renderWrite w = unsafeFixIOCont $ \set (~mem) -> do
  ~ref <- stToIO $ newSTRef (0, 1, pure ())
  ~r <- stToIO $ unWrite w mem ref
  ~(size, align, go) <- stToIO $ readSTRef ref
  allocaBytesAligned
    size
    align
    (\(~mem) -> do
      set mem
      go
    )
  pure r

data Example = Example Int (Maybe Example2) (Maybe Example2)

data Example2 = Example2 Float

pokeExample :: Example -> IO ()
pokeExample (Example i m m2) = renderWrite $ do
  t  <- maybe (pure nullPtr) (write 4 4 . (\(Example2 e) -> (`poke` e))) m
  t2 <- maybe (pure nullPtr) (write 4 4 . (\(Example2 e) -> (`poke` e))) m2
  write 4 4 (`poke` i)
  write 8 8 (`poke` t)
  write 8 8 (`poke` t2)
  -- t <- write $ Store 4 4 (`poke` m)
  pure ()

bar :: Write ()
bar = do
  ~a <- write 4 4 (\p -> print p)
  ~b <- write 4 4 (\p -> print a)
  pure ()

-- baz :: Write
baz = mfix $ \(~a) -> write 4 4 (\p -> print a)

bazz :: Write ()
bazz = do
  rec ~a <- write 4 4 (\(~p) -> print a)
  pure ()
