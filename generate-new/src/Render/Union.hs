{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.Union
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , State
                                                )
import           Text.InterpolatedString.Perl6.Unindented
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import           Polysemy.State
import qualified Data.Vector                   as V

import           Spec.Parse
import           Haskell                       as H
import           Marshal
import           Error
import           Render.Utils
import           Render.Element
import           Render.Type
import           Render.Scheme

renderUnion
  :: (HasErr r, Member (Reader RenderParams) r) => Union -> Sem r RenderElement
renderUnion Struct {..} = context sName $ do
  RenderParams {..} <- ask
  genRe ("union " <> sName) $ do
    let n = mkTyName sName
    ms <- traverseV renderUnionMember sMembers
    tellExport (EData n)
    tellDoc $ "data" <+> pretty n <> line <> indent
      2
      (vsep $ zipWith (<+>) ("=" : repeat "|") (toList ms))

renderUnionMember
  :: ( HasErr r
     , MemberWithError (Reader RenderParams) r
     , MemberWithError (State RenderElement) r
     )
  => StructMember
  -> Sem r (Doc ())
renderUnionMember StructMember {..} = do
  RenderParams {..} <- ask
  let n = mkConName smName
  tDoc <- renderType =<< cToHsType DoPreserve smType
  pure $ pretty n <+> tDoc

