{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.Struct
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , State
                                                , Type
                                                )
import           Text.InterpolatedString.Perl6.Unindented
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Error
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

import           Spec.Parse
import           Haskell                       as H
import           Marshal
import           Error
import           Render.Utils
import           Render.Element
import           Render.Type
import           Render.Scheme
import           Render.SpecInfo
import           Render.Poke

renderStruct
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
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
    renderStoreInstances s

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

addrVar :: Doc ()
addrVar = "p"

valueVar :: Doc ()
valueVar = "poked"

renderStoreInstances
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => MarshaledStruct
  -> Sem r ()
renderStoreInstances ms@MarshaledStruct {..} = do
  RenderParams {..} <- ask
  pokes <- forV msMembers $ \m@MarshaledStructMember {..} -> do
    p <- getPoke msmStructMember msmScheme
    let
      p' =
        (\(Unassigned f) -> Assigned $ do
            t <- cToHsType DoPreserve (smType msmStructMember)
            let member = mkMemberName (smName msmStructMember)
            tellImport 'plusPtr
            tDoc <- renderTypeHighPrec (ConT (typeName msName))
            f
              t
              (AddrDoc
                (parens $ addrVar <+> "`plusPtr`" <+> viaShow
                  (smOffset msmStructMember)
                )
              )
              (ValueDoc
                (parens $ pretty member <+> parens (valueVar <+> "::" <+> tDoc))
              )
          )
          <$> p
    pure p'
  when (all isIOPoke pokes) $ do
    storableInstance ms
  toCStructInstance ms pokes

storableInstance
  :: ( HasErr r
     , MemberWithError (Reader RenderParams) r
     , MemberWithError (State RenderElement) r
     )
  => MarshaledStruct
  -> Sem r ()
storableInstance MarshaledStruct{..} = do
  RenderParams{..} <- ask
  let n = mkTyName msName
  -- Some member names clash with storable members "alignment" for instance
  tellQualImportWithAll ''Storable
  tellImport ''Storable
  tellDoc [qqi|
    instance Storable {n} where
      sizeOf ~_ = {sSize msStruct}
      alignment ~_ = {sAlignment msStruct}
      peek = undefined
      poke ptr poked = pokeCStruct ptr poked (pure ())
  |]

toCStructInstance
  :: ( HasErr r
     , MemberWithError (Reader RenderParams) r
     , MemberWithError (State RenderElement) r
     )
  => MarshaledStruct
  -> Vector AssignedPoke
  -> Sem r ()
toCStructInstance MarshaledStruct {..} pokes = do
  RenderParams {..} <- ask
  tellImportWithAll (TyConName "ToCStruct")
  let n           = mkTyName msName
      Struct {..} = msStruct
  tellImport 'allocaBytesAligned
  pokeDoc <- renderPokesInIO pokes
  tellDoc $ "instance ToCStruct" <+> pretty n <+> "where" <> line <> indent
    2
    (vsep
      [ "withCStruct x f = allocaBytesAligned"
      <+> viaShow sSize
      <+> viaShow sAlignment
      <+> "$ \\p -> pokeCStruct p x (f p)"
      , "pokeCStruct" <+> addrVar <+> valueVar <+> "=" <+> pokeDoc
      ]
    )

