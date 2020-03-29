module Render.Type.Preserve
  ( Preserve(..)
  )
where

data Preserve
  = DoNotPreserve
    -- ^ Use more idiomatic haskell types
  | DoPreserve
    -- ^ Use the types from Foreign.C.Types
  | DoLower
    -- ^ Use the types from Foreign.C.Types and lower arrays to pointers
