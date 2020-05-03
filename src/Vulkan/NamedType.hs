{-# language CPP #-}
module Vulkan.NamedType  ((:::)) where



-- | Annotate a type with a name
type (name :: k) ::: a = a

