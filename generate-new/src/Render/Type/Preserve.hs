module Render.Type.Preserve where

import           Language.Haskell.TH            ( Name
                                                , Type
                                                )
import           Prelude (Show)

data Preserve
  = DoNotPreserve
    -- ^ Use more idiomatic haskell types
  | DoPreserve
    -- ^ Use the types from Foreign.C.Types
  | DoLower
    -- ^ Use the types from Foreign.C.Types and lower arrays to pointers

data ExtensibleStructStyle r
  = Unwrapped
  -- ^ A variable is applied to the extensible struct
  --
  -- Inheriting types are represented just by a var
  | UnwrappedHole
  -- ^ A hole is applied to extensible structs
  --
  -- Inheriting structs are just left as holes
  | Wrapped
  -- ^ Structs should be wrapped in @SomeStruct@

data ConstrainedVar
  = Extends {cVarBase :: Type, cVarName :: Name}
  | Inherits {cVarBase :: Type, cVarName :: Name}
  | Unconstrained {cVarName :: Name}
  deriving (Show)
