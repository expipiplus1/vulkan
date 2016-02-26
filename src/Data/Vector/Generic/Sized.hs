{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module Data.Vector.Generic.Sized
    ( Vec
      -- * Construction
    , fromVector
    , replicate
    , singleton
    , generate
      -- * Elimination
    , length
    , index
    , head
    , last
      -- * Extract subsets
    , tail
    , init
    , take
    , drop
      -- * Mapping
    , map
      -- * Folding
    , foldl'
    , foldl1'
    ) where

import qualified Data.Vector.Generic as VG
import GHC.TypeLits
import Data.Proxy
import Control.DeepSeq
import Foreign.Storable
import Prelude hiding (replicate, singleton, head, last,
                       tail, init, map, length, drop, take)

newtype Vec v (n :: Nat) a = Vec (v a)
                           deriving (Show, Eq, Ord, Foldable, Storable,
                                     Monoid, NFData)

fromVector :: forall a v (n :: Nat). (KnownNat n, VG.Vector v a)
           => v a -> Maybe (Vec v n a)
fromVector v
  | n' == fromIntegral (VG.length v) = Just (Vec v)
  | otherwise                        = Nothing
  where n' = natVal (Proxy :: Proxy n)

singleton :: forall a v. (VG.Vector v a)
          => a -> Vec v 1 a
singleton a = Vec (VG.singleton a)

generate :: forall (n :: Nat) a v. (VG.Vector v a, KnownNat n)
         => Proxy n -> (Int -> a) -> Vec v n a
generate n f = Vec (VG.generate (fromIntegral $ natVal n) f)

withVector :: forall a b v (n :: Nat). (VG.Vector v a, VG.Vector v b)
           => (v a -> v b) -> Vec v n a -> Vec v n b
withVector f (Vec v) = Vec (f v)

index :: forall (m :: Nat) a v (n :: Nat). (KnownNat n, KnownNat m, VG.Vector v a)
      => Proxy m -> Vec v (m+n) a -> a
index i (Vec v) = v VG.! fromIntegral (natVal i)

take :: forall (m :: Nat) a v (n :: Nat). (KnownNat n, KnownNat m, VG.Vector v a)
      => Proxy m -> Vec v (m+n) a -> Vec v m a
take i (Vec v) = Vec (VG.take (fromIntegral $ natVal i) v)

drop :: forall (m :: Nat) a v (n :: Nat). (KnownNat n, KnownNat m, VG.Vector v a)
      => Proxy m -> Vec v (m+n) a -> Vec v n a
drop i (Vec v) = Vec (VG.drop (fromIntegral $ natVal i) v)

length :: forall a v (n :: Nat). (VG.Vector v a)
     => Vec v n a -> Int
length (Vec v) = VG.length v

head :: forall a v (n :: Nat). (VG.Vector v a)
     => Vec v (n+1) a -> a
head (Vec v) = VG.head v

last :: forall a v (n :: Nat). (VG.Vector v a)
     => Vec v (n+1) a -> a
last (Vec v) = VG.last v

tail :: forall a v (n :: Nat). (VG.Vector v a)
     => Vec v (n+1) a -> Vec v n a
tail (Vec v) = Vec (VG.tail v)

init :: forall a v (n :: Nat). (VG.Vector v a)
     => Vec v (n+1) a -> Vec v n a
init (Vec v) = Vec (VG.init v)

replicate :: forall a v (n :: Nat). (VG.Vector v a, KnownNat n)
          => Proxy n -> a -> Vec v n a
replicate n a = Vec (VG.replicate (fromIntegral $ natVal n) a)

map :: forall a b v (n :: Nat). (VG.Vector v a, VG.Vector v b)
    => (a -> b) -> Vec v n a -> Vec v n b
map f = withVector (VG.map f)

foldl' :: forall a b v (n :: Nat). (VG.Vector v a, VG.Vector v b)
       => (a -> b -> a) -> a -> Vec v n b -> a
foldl' f z (Vec v) = VG.foldl' f z v

foldl1' :: forall a v (n :: Nat). (VG.Vector v a)
       => (a -> a -> a) -> Vec v (n+1) a -> a
foldl1' f (Vec v) = VG.foldl1' f v
