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

type WriteState = (Int, Int, IO ())

-- | Write is a lazy Reader and strict State Monad.
newtype Write a = Write { unWrite :: Ptr () -> WriteState -> (WriteState, a) }

instance Functor Write where
  fmap f (Write x) = Write $ \ ~p s ->
    let (s', x') = x p s
    in (s', f x')

instance Applicative Write where
  -- {-# inline pure #-}
  pure x = Write (\(~_) s -> (s, x))
  -- {-# inline (<*>) #-}
  Write f <*> Write x = Write $ \(~p) s ->
    let (s' , f') = f p s
        (s'', x') = x p s'
    in  (s'', f' x')

instance Monad Write where
  -- {-# inline (>>=) #-}
  Write x >>= f = Write $ \(~p) s ->
    let ~ ~(s', x') = x p s
        ~ ~(Write f') = f x'
    in  f' p s'

instance MonadFix Write where
  {-# inline mfix #-}
  mfix f = Write $ \(~p) s -> let ~ ~(~s', ~r) = unWrite (f r) p s in (s', r)

{-# inline unsafeFixIOCont #-}
unsafeFixIOCont :: ((a -> IO ()) -> a -> IO b) -> IO b
unsafeFixIOCont f = do
  ref <- newIORef (throw NonTermination)
  ~ans <- unsafeDupableInterleaveIO (readIORef ref)
  f (writeIORef ref) ans

{-# inline ask #-}
ask :: Write (Ptr ())
ask = Write $ \(~p) s -> (s, p)

{-# inline get #-}
get :: Write WriteState
get = Write $ \(~_) s -> (s, s)

{-# inline set #-}
set :: WriteState -> Write ()
set s = Write $ \(~_) _ -> (s, ())

{-# inline write #-}
write :: Int -> Int -> (Ptr a -> IO ()) -> Write (Ptr a)
write storeSize storeAlign storePoke = do
  ~mem                 <- ask
  (size, align, poke) <- get
  let padded = alignUp size storeAlign
      size'  = padded + storeSize
      align' = max align storeAlign
      ~offset = mem `plusPtr` padded
      poke'  = poke >> storePoke offset
  set (size', align', poke')
  pure offset

{-# inline alignUp #-}
alignUp :: Int -> Int -> Int
alignUp offset alignment = (offset + (alignment - 1)) .&. (-alignment)

{-# inline renderWrite #-}
renderWrite :: Write a -> IO a
renderWrite w = unsafeFixIOCont $ \set (~mem) -> do
  let ((size, align, go), r) = unWrite w mem (0, 1, pure ())
  allocaBytesAligned
    size
    align
    (\(~mem) -> do
      set mem
      go
    )
  pure r

data Example = Example Int (Maybe Example2) (Maybe Example2)
                           (Maybe Example2) (Maybe Example2)

data Example2 = Example2 Float

nullWrite = nullPtr <$ write 0 1 (const (pure ()))

pokeExample :: Example -> IO ()
pokeExample (Example i m1 m2 m3 m4) = renderWrite $ do
  t1 <- maybe nullWrite (write 4 4 . (\(Example2 e) -> (`poke` e))) m1
  t2 <- maybe nullWrite (write 4 4 . (\(Example2 e) -> (`poke` e))) m1
  t3 <- maybe nullWrite (write 4 4 . (\(Example2 e) -> (`poke` e))) m1
  t4 <- maybe nullWrite (write 4 4 . (\(Example2 e) -> (`poke` e))) m1
  -- t1 <- maybe (pure nullPtr) (write 4 4 . (\(Example2 e) -> (`poke` e))) m1
  -- t2 <- maybe (pure nullPtr) (write 4 4 . (\(Example2 e) -> (`poke` e))) m2
  -- t3 <- maybe (pure nullPtr) (write 4 4 . (\(Example2 e) -> (`poke` e))) m3
  -- t4 <- maybe (pure nullPtr) (write 4 4 . (\(Example2 e) -> (`poke` e))) m4
  write 4 4 (`poke` i)
  write 8 8 (`poke` t1)
  write 8 8 (`poke` t2)
  write 8 8 (`poke` t3)
  write 8 8 (`poke` t4)
  pure ()

--
-- 1: RHS size: {terms: 211, types: 320, coercions: 83, joins: 0/0}
-- 2: RHS size: {terms: 557, types: 761, coercions: 214, joins: 0/0}
-- 3: RHS size: {terms: 1,314, types: 1,775, coercions: 500, joins: 1/1}
-- 4: RHS size: {terms: 2,638, types: 3,539, coercions: 994, joins: 4/4}
--

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
