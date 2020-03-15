module Data.Vector.Extra
  ( module Data.Vector
  , uncons
  , pattern Empty
  , pattern (:<|)
  , pattern Singleton
  ) where

import           Prelude
import           Data.Vector
import qualified Data.Vector                   as V

{-# complete Empty, (:<|) #-}

pattern Empty :: Vector a
pattern Empty <- (V.null -> True) where Empty = V.empty

uncons :: Vector a -> Maybe (a, Vector a)
uncons Empty = Nothing
uncons v   = Just (V.unsafeHead v, V.unsafeTail v)

pattern (:<|)  :: a -> Vector a -> Vector a
pattern x :<| xs <- (uncons -> Just (x, xs))
  where (:<|) = V.cons
infixr :<|

pattern Singleton :: a -> Vector a
pattern Singleton x <- (uncons -> Just (x, Empty))
  where Singleton = V.singleton

