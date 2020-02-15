module Haskell
  where

import Relude

data HType
  = Con Text
  | App HType HType

infixl `App`
infixr ~>

(~>) :: HType -> HType -> HType
a ~> b = Con "->" `App` a `App` b

