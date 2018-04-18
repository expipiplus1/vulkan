{-# language Strict #-}
{-# language CPP #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.NamedType
  ( (:::)
  ) where







-- | Annotate a type with a name
type (name :: k) ::: a = a
