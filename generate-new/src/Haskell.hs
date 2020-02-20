module Haskell
  ( Type(..)
  , Name
  , renderType
  , renderTypeHighPrec
  , pattern (:@)
  , typeName
  , (~>)
  , module Haskell.Name
  )
where

import           Relude                  hiding ( Type
                                                , group
                                                , State
                                                , Reader
                                                , ask
                                                )
import           Language.Haskell.TH
import           Data.Text.Prettyprint.Doc
import           Data.Generics.Uniplate.Data
import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Data.Char                      ( isLower )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Prelude                        ( head )

import           Render.Element
import           Render.Element.Write
import           Haskell.Name

typeName :: Text -> Name
typeName = mkName . T.unpack

renderType
  :: ( MemberWithError (State RenderElement) r
     , MemberWithError (Reader RenderParams) r
     )
  => Type
  -> Sem r (Doc ())
renderType t = do
  RenderParams {..} <- ask
  let removeModules = transformBi
        (\n ->
          if V.elem n alwaysQualifiedNames then n else mkName . nameBase $ n
        )
  traverse_
    tellImport
    [ n | n <- childrenBi t, not (isLower (Prelude.head (nameBase n))) ] -- TODO: do this properly
  pure
    . group -- All on one line, to work around brittany #277
    . pretty
    . pprint
    . removeModules
    $ t

-- TODO, do this properly lol
renderTypeHighPrec
  :: ( MemberWithError (State RenderElement) r
     , MemberWithError (Reader RenderParams) r
     )
  => Type
  -> Sem r (Doc ())
renderTypeHighPrec = \case
  t@(ConT _) -> renderType t
  t          -> parens <$> renderType t

pattern (:@) :: Type -> Type -> Type
pattern a :@ b = AppT a b
infixl 2 :@

infixr 1 ~>
(~>) :: Type -> Type -> Type
a ~> b = ArrowT :@ a :@ b
