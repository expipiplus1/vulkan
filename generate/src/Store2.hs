{-# language DeriveFunctor #-}
{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language DerivingVia #-}
{-# language ApplicativeDo #-}
{-# language RecursiveDo #-}

module Store2 where

-- import Control.Monad.State.Lazy
import Data.Bifunctor (first, second)
import Control.Monad.Fix
import Foreign.Ptr
import GHC.IO
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Semigroup
import Data.Monoid
import Data.Bits
import Data.IORef
import Control.Exception


-- {-# inline alignUp #-}
-- alignUp :: Int -> Int -> Int
-- alignUp offset alignment = (offset + (alignment - 1)) .&. (-alignment)


-- {-# inline unsafeFixIOCont #-}
-- unsafeFixIOCont :: ((a -> IO ()) -> a -> IO b) -> IO b
-- unsafeFixIOCont f = do
--   ref <- newIORef (throw NonTermination)
--   ~ans <- unsafeDupableInterleaveIO (readIORef ref)
--   f (writeIORef ref) ans

-- {-# inline write #-}
-- write :: Int -> Int -> (Ptr a -> IO ()) -> Write (Ptr a)
-- write storeSize storeAlign storePoke = do
--   ~mem                 <- ask
--   ~(~size, ~align, ~poke) <- get
--   let ~padded = alignUp size storeAlign
--       ~size'  = padded + storeSize
--       ~align' = max align storeAlign
--       ~offset = mem `plusPtr` padded
--       ~poke'  = poke >> storePoke offset
--   set (size', align', poke')
--   pure offset


-- {-# inline ask #-}
-- ask :: Write (Ptr ())
-- ask = Write $ \p s -> (s, p)

-- {-# inline get #-}
-- get :: Write WriteState
-- get = Write $ \_ s -> (s, s)

-- {-# inline set #-}
-- set :: WriteState -> Write ()
-- set s = Write $ \_ _ -> (s, ())

-- type WriteState = (Int, Int, IO ())

-- newtype Write a = Write { unWrite :: Ptr () -> WriteState -> (WriteState, a) }
--   deriving (Functor)

-- instance Applicative Write where
--   {-# inline pure #-}
--   pure x = Write (\_ s -> (s, x))
--   {-# inline (<*>) #-}
--   Write f <*> Write x = Write $ \p s ->
--     let (s' , f') = f p s
--         (s'', x') = x p s'
--     in  (s'', f' x')

-- instance Monad Write where
--   {-# inline (>>=) #-}
--   Write x >>= f = Write $ \p s ->
--     let (s', x') = x p s
--         Write f' = f x'
--     in  f' p s'

-- instance MonadFix Write where
--   {-# inline mfix #-}
--   mfix f = Write $ \p s -> let (s', r) = unWrite (f r) p s in (s', r)

-- {-# inline renderWrite #-}
-- renderWrite :: Write a -> IO a
-- renderWrite ~w = unsafeFixIOCont $ \(~set) (~mem) -> do
--   let ~(~(size, ~align, ~go), ~r) = unWrite w mem (0, 1, pure ())
--   allocaBytesAligned
--     size
--     align
--     (\(~mem) -> do
--       set mem
--       -- go
--     )
--   pure r


-- -- unsafeFixIOCont :: ((a -> IO ()) -> a -> IO b) -> IO b
-- -- unsafeFixIOCont f = do
-- --   ~ref <- newIORef (throw NonTermination)
-- --   ~ans <- unsafeDupableInterleaveIO (readIORef ref)
-- --   f (writeIORef ref) ans

-- -- newtype Write a = Write { unWrite :: State (Ptr (), Store ()) a }
-- --   deriving (Functor, Applicative, Monad, MonadFix, MonadState (Ptr(), Store ()))

-- -- data Store a where
-- --   Store :: { storeSize  :: ~Int
-- --              -- ^ The size of this allocation
-- --            , storePoke  :: ~(Ptr b -> IO a)
-- --              -- ^ An action storing something in the allocated region
-- --            }
-- --         -> Store a
-- --   deriving (Semigroup, Monoid) via (Ap Store a)

-- -- deriving instance Functor Store

-- -- instance Applicative Store where
-- --   pure ~x = Store 0 (const (pure x))
-- --   Store ~s1 ~p1 <*> Store ~s2 ~p2 =
-- --     let ~o = alignUp s1 1
-- --         ~s = o + s2
-- --     in  Store 0 (\(~mem) -> p1 mem <*> p2 (mem `plusPtr` o))

-- -- {-# inline alignUp #-}
-- -- alignUp :: Int -> Int -> Int
-- -- alignUp ~offset ~alignment = (offset + (alignment - 1)) .&. (-alignment)

-- -- store :: Store a -> (Int,  Ptr () -> IO a)
-- -- store (Store ~s ~w) = (s, w . castPtr)


-- -- write :: Store () -> Write (Ptr ())
-- -- write s = do
-- --   ~(~p, ~e) <- get
-- --   modify (second (<> s))
-- --   pure (p `plusPtr` alignUp (storeSize e) 1)

-- -- bar :: Write ()
-- -- bar = mdo
-- --   ~a <- write $ Store 4 (\(~p) -> print a)
-- --   pure ()

-- -- renderWrite :: Write a -> IO a
-- -- renderWrite ~w = unsafeFixIOCont $ \(~set) (~mem) -> do
-- --   let ~(~r   , ~(~_, ~s)) = runState (unWrite w) (mem, mempty)
-- --       ~(~size, ~go      ) = store s
-- --   print size
-- --     set mem
-- --     go mem
-- --   pure r

