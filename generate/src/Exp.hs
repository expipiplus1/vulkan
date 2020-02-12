{-# language StrictData #-}
{-# language LambdaCase #-}

module Store where

import Foreign.Ptr
import Foreign.Storable
import Data.Bits
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

{-# inline unsafeFixIOCont #-}
unsafeFixIOCont :: ((a -> IO ()) -> a -> IO b) -> IO b
unsafeFixIOCont f = do
  ref <- newIORef (throw NonTermination)
  ~ans <- unsafeDupableInterleaveIO (readIORef ref)
  f (writeIORef ref) ans

-- | Write is a lazy Reader and strict State Monad.
-- newtype Write a = Write { unWrite :: Ptr () -> WriteState -> (WriteState, a) }
newtype Write a = Write { unWrite :: Ptr () -> Int -> Int -> IO () -> (Int, Int, IO (), a) }

data WriteState = WriteState Int Int (IO ())

instance Functor Write where
  fmap f (Write x) = Write $ \ ~p a b c ->
    let (a',b',c', x') = x p a b c
    in (a',b',c', f x')

instance Applicative Write where
  pure x = Write (\(~_) a b c -> (a, b, c, x))
  Write f <*> Write x = Write $ \(~p) a b c ->
    let (a',b',c' , f') = f p a b c
        (a'', b'', c'', x') = x p a' b' c'
    in  (a'', b'', c'', f' x')

instance Monad Write where
  {-# noinline (>>=) #-}
  Write x >>= f = Write $ \(~p) a b c ->
    let (a', b', c', x') = x p a b c
        Write f' = f x'
    in  f' p a' b' c'

ask :: Write (Ptr ())
ask = Write $ \(~p) a b c -> (a, b, c, p)

get :: Write WriteState
get = Write $ \(~_) a b c -> (a, b, c, WriteState a b c)

set :: WriteState -> Write ()
set (WriteState a b c) = Write $ \(~_) _ _ _ -> (a, b, c, ())

write :: Int -> Int -> (Ptr a -> IO ()) -> Write (Ptr a)
write storeSize storeAlign storePoke = do
  ~mem                 <- ask
  WriteState size align poke <- get
  let padded = alignUp size storeAlign
      size'  = padded + storeSize
      align' = max align storeAlign
      ~offset = mem `plusPtr` padded
      poke'  = poke >> storePoke offset
  set (WriteState size' align' poke')
  pure offset

writeMaybe :: Int -> Int -> Maybe (Ptr a -> IO ()) -> Write (Ptr a)
writeMaybe size align = \case
  Nothing -> pure nullPtr
  Just p  -> write size align p

alignUp :: Int -> Int -> Int
alignUp offset alignment = (offset + (alignment - 1)) .&. (-alignment)

-- The size of $wbad with different numbers of the 'maybe' lines uncommented
--
-- 0:  RHS size: {terms: 35     , types: 56     , coercions: 3    , joins: 0/0}
-- 1:  RHS size: {terms: 116    , types: 130    , coercions: 8    , joins: 1/3}
-- 2:  RHS size: {terms: 280    , types: 284    , coercions: 18   , joins: 3/10}
-- 3:  RHS size: {terms: 630    , types: 622    , coercions: 38   , joins: 7/24}
-- 4:  RHS size: {terms: 1,298  , types: 1,297  , coercions: 73   , joins: 15/49}
-- 5:  RHS size: {terms: 2,504  , types: 2,512  , coercions: 250  , joins: 14/92}
-- ...
-- 10: RHS size: {terms: 17,587 , types: 19,502 , coercions: 875  , joins: 295/690}
-- ...
-- 20: RHS size: {terms: 137,157, types: 153,577, coercions: 6,750, joins: 2,490/5,380}

{-# inline renderWrite #-}
renderWrite :: Write a -> IO a
renderWrite w = unsafeFixIOCont $ \set (~mem) -> do
  let (size, align, go, r) = unWrite w mem 0 1 (pure ())
  allocaBytesAligned
    size
    align
    (\(~mem) -> do
      set mem
      go
    )
  pure r

-- runWrite w mem = unWrite w mem (0, 1, pure())

nullWrite :: Write (Ptr a)
nullWrite = nullPtr <$ write 0 1 (const (pure ()))

{-# noinline maybe' #-}
maybe' = maybe

maybeWrite :: Int -> Int -> Maybe (Ptr a -> IO ()) -> Write (Ptr a)
maybeWrite s a = \case
  Nothing -> pure nullPtr
  Just x -> write s a x

-- bad :: [Int] -> Ptr () -> IO ()
bad :: [Maybe Int] -> IO ()
bad [m1 , m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20]
  = renderWrite $ do
    -- write 4 4 (flip poke (m1 :: Int))
    -- write 4 4 (flip poke m2)
    -- write 4 4 (flip poke m3)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m1)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m2)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m3)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m4)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m5)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m6)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m7)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m8)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m9)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m10)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m11)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m12)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m13)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m14)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m15)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m16)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m17)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m18)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m19)
    maybeWrite 4 4 ((\e -> (`poke` e)) <$> m20)
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m1
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m2
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m3
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m4
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m5
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m6
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m7
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m8
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m9
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m10
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m11
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m12
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m13
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m14
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m15
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m16
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m17
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m18
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m19
    -- maybe' (pure nullPtr) (write 4 4 . (\e -> (`poke` e))) m20
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m1
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m2
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m3
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m4
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m5
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m6
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m7
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m8
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m9
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m10
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m11
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m12
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m13
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m14
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m15
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m16
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m17
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m18
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m19
    -- maybe (pure nullPtr) (write 4 4 . (flip poke)) m20
    -- writeMaybe 4 4 (flip poke <$> m4)
    -- writeMaybe 4 4 (flip poke <$> m5)
    -- writeMaybe 4 4 (flip poke <$> m6)
    -- writeMaybe 4 4 (flip poke <$> m7)
    -- writeMaybe 4 4 (flip poke <$> m8)
    -- writeMaybe 4 4 (flip poke <$> m9)
    -- writeMaybe 4 4 (flip poke <$> m10)
    -- writeMaybe 4 4 (flip poke <$> m11)
    -- writeMaybe 4 4 (flip poke <$> m12)
    -- writeMaybe 4 4 (flip poke <$> m13)
    -- writeMaybe 4 4 (flip poke <$> m14)
    -- writeMaybe 4 4 (flip poke <$> m15)
    -- writeMaybe 4 4 (flip poke <$> m16)
    -- writeMaybe 4 4 (flip poke <$> m17)
    -- writeMaybe 4 4 (flip poke <$> m18)
    -- writeMaybe 4 4 (flip poke <$> m19)
    -- writeMaybe 4 4 (flip poke <$> m20)
    pure ()

