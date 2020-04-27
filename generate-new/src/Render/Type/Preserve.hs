module Render.Type.Preserve where

import           Language.Haskell.TH            ( Type )
import           Polysemy

data Preserve
  = DoNotPreserve
    -- ^ Use more idiomatic haskell types
  | DoPreserve
    -- ^ Use the types from Foreign.C.Types
  | DoLower
    -- ^ Use the types from Foreign.C.Types and lower arrays to pointers

data ExtensibleStructStyle r
  = Applied (Sem r Type)
  -- ^ A variable or hole of kind @[Type]@ to apply to the extensible struct
  | Wrapped
  -- ^ Structs should be wrapped in @SomeStruct@
