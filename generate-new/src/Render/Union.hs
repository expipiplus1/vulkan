module Render.Union
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , State
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import           Polysemy.State

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.Type

renderUnion
  :: (HasErr r, Member (Reader RenderParams) r) => Union -> Sem r RenderElement
renderUnion Struct {..} = context sName $ do
  RenderParams {..} <- ask
  genRe ("union " <> sName) $ do
    let n = mkTyName sName
    ms <- traverseV (renderUnionMember sName) sMembers
    tellExport (EData n)
    tellDoc $ "data" <+> pretty n <> line <> indent
      2
      (vsep $ zipWith (<+>) ("=" : repeat "|") (toList ms))

renderUnionMember
  :: ( HasErr r
     , MemberWithError (Reader RenderParams) r
     , MemberWithError (State RenderElement) r
     )
  => Text
  -- ^ union type name
  -> StructMember
  -> Sem r (Doc ())
renderUnionMember tyName StructMember {..} = do
  RenderParams {..} <- ask
  let n = mkConName tyName smName
  tDoc <- renderTypeHighPrec =<< cToHsType DoPreserve smType
  pure $ pretty n <+> tDoc

