{-# language CPP #-}
-- No documentation found for Chapter "Extends"
module OpenXR.CStruct.Extends  ( BaseInStructure
                               , BaseOutStructure
                               , Extendss
                               , PeekChain
                               , PokeChain
                               , Chain
                               ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Constraint)
import Data.Kind (Type)

data BaseInStructure

instance ToCStruct BaseInStructure
instance Show BaseInStructure

instance FromCStruct BaseInStructure


data BaseOutStructure

instance ToCStruct BaseOutStructure
instance Show BaseOutStructure

instance FromCStruct BaseOutStructure


class PeekChain (xs :: [Type])
class PokeChain (xs :: [Type])
type family ExtendsWith (p :: [Type] -> Type) (x :: Type) :: () where ..

-- | We don't really need constraint units produced by `ExtendsWith`, so this type
-- family will ensure that it would reduce and drop the result
--
-- That will result in less overhead because `Extendss` reduces into a single
-- contraint unit `()` instead of cons-list `((), ((), ()))` produced by `(,)`
type family ReportUnsolved (a :: ()) (b :: Constraint) :: Constraint where
  ReportUnsolved '() b = b

type family Extendss (p :: [Type] -> Type) (xs :: [Type]) :: Constraint where
  Extendss p '[]      = ()
  Extendss p (x : xs) = ExtendsWith p x `ReportUnsolved` Extendss p xs
type Extends p a = ExtendsWith p a ~ '()
type family Chain (xs :: [a]) = (r :: a) | r -> xs where
  Chain '[]    = ()
  Chain (x:xs) = (x, Chain xs)

