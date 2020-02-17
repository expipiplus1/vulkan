module Haskell
  ( Type(..)
  , Name
  , renderType
  , pattern (:@)
  , mkName
  , (~>)
  )
  where

import           Relude                  hiding ( Type
                                                , group
                                                , State
                                                )
import           Language.Haskell.TH
import           Data.Text.Prettyprint.Doc
import           Data.Generics.Uniplate.Data
import           Polysemy
import           Polysemy.State

import           Render.Element

renderType :: MemberWithError (State RenderElement) r => Type -> Sem r (Doc ())
renderType t = do
  traverse_ tellImport [ Import n False | n <- childrenBi t ]
  pure
    . group -- All on one line, to work around brittany #277
    . pretty
    . pprint
    . removeModules
    $ t
  where removeModules = transformBi (mkName . nameBase)

pattern (:@) :: Type -> Type -> Type
pattern a :@ b = AppT a b

(~>) :: Type -> Type -> Type
a ~> b = ArrowT :@ a :@ b
