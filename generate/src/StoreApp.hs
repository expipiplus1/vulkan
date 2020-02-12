{-# language DeriveFunctor #-}
{-# language StandaloneDeriving #-}
{-# language GADTs #-}
{-# language DerivingVia #-}
{-# language Strict #-}

module Store where

import Foreign.Ptr
import Data.Monoid
import Data.Bits

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

{-# inline alignUp #-}
alignUp :: Int -> Int -> Int
alignUp offset alignment = (offset + (alignment - 1)) .&. (-alignment)
