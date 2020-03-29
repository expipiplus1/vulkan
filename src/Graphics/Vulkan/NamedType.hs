{-# language CPP #-}
module Graphics.Vulkan.NamedType  ((:::)) where



-- | Annotate a type with a name
type (name :: k) ::: a = a

