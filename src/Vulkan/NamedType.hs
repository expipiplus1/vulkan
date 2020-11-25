{-# language CPP #-}
-- No documentation found for Chapter "NamedType"
module Vulkan.NamedType  ((:::)) where



-- | Annotate a type with a name
type (name :: k) ::: a = a

