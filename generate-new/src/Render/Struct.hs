{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.Struct
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

renderStruct
  :: (HasErr r, Member (Reader RenderParams) r)
  => MarshaledStruct
  -> Sem r RenderElement
renderStruct s@MarshaledStruct {..} = do
  RenderParams {..} <- ask
  genRe ("struct " <> msName) $ do
    let n = mkTyName msName
    ms <- V.mapMaybe id <$> traverseV renderStructMember msMembers
    tellExport (EData n)
    tellDoc [qqi|
        data {n} = {mkConName msName msName}
          {braceList ms}
        |]
    toCStructInstance s

renderStructMember
  :: ( HasErr r
     , MemberWithError (Reader RenderParams) r
     , MemberWithError (State RenderElement) r
     )
  => MarshaledStructMember
  -> Sem r (Maybe (Doc ()))
renderStructMember MarshaledStructMember {..} = do
  let StructMember {..} = msmStructMember
  RenderParams {..} <- ask
  m                 <- schemeType msmScheme
  traverse
    (\t -> do
      tDoc <- renderType t
      pure [qqi|{mkMemberName smName} :: {tDoc}|]
    )
    m

----------------------------------------------------------------
-- Marshaling
----------------------------------------------------------------

toCStructInstance
  :: ( HasErr r
     , MemberWithError (Reader RenderParams) r
     , MemberWithError (State RenderElement) r
     )
  => MarshaledStruct
  -> Sem r ()
toCStructInstance MarshaledStruct {..} = do
  RenderParams {..} <- ask
  tellImportWithAll (TyConName "ToCStruct")
  let n = mkTyName msName
  tellDoc $ "instance ToCStruct" <+> pretty n <+> "where" <> line <> indent
    2
    (vsep ["withCStruct =" <+> "undefined", "pokeCStruct =" <+> "undefined"])
