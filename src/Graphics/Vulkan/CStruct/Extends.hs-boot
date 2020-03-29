{-# language CPP #-}
module Graphics.Vulkan.CStruct.Extends  ( BaseInStructure
                                        , BaseOutStructure
                                        , PeekChain
                                        , PokeChain
                                        , Chain
                                        ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
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
type family Chain (xs :: [a]) = (r :: a) | r -> xs where
  Chain '[]    = ()
  Chain (x:xs) = (x, Chain xs)

