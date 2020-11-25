{-# language CPP #-}
-- No documentation found for Chapter "Extends"
module Vulkan.CStruct.Extends  ( BaseInStructure
                               , BaseOutStructure
                               , Extendss
                               , PeekChain
                               , PokeChain
                               , Chain
                               ) where

import Data.Kind (Constraint)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
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
type family Extends (p :: [Type] -> Type) (x :: Type) :: Constraint
type family Extendss (p :: [Type] -> Type) (xs :: [Type]) :: Constraint where
  Extendss p '[]      = ()
  Extendss p (x : xs) = (Extends p x, Extendss p xs)
type family Chain (xs :: [a]) = (r :: a) | r -> xs where
  Chain '[]    = ()
  Chain (x:xs) = (x, Chain xs)

