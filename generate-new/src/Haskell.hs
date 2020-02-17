module Haskell
  ( Type(..)
  , renderType
  , pattern (:@)
  , mkName
  , (~>)
  )
  where

import           Relude                  hiding ( Type, group )
import           Language.Haskell.TH
import           Data.Text.Prettyprint.Doc
import           Data.Generics.Uniplate.Data

renderType :: Type -> Doc ()
renderType =
  group -- All on one line, to work around brittany #277
    . pretty
    . pprint
    . removeModules
  where removeModules = transformBi (mkName . nameBase)

pattern (:@) :: Type -> Type -> Type
pattern a :@ b = AppT a b

(~>) :: Type -> Type -> Type
a ~> b = ArrowT :@ a :@ b
