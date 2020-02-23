{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.Struct
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , runReader
                                                , lift
                                                , State
                                                , Type
                                                )
import           Text.InterpolatedString.Perl6.Unindented
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.Map                      as Map

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
import           Render.Peek

renderStruct
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => MarshaledStruct AStruct
  -> Sem r RenderElement
renderStruct s@MarshaledStruct {..} = context msName $ do
  RenderParams {..} <- ask
  genRe ("struct " <> msName) $ do
    let n = mkTyName msName
    ms <- V.mapMaybe id <$> traverseV renderStructMember msMembers
    tellExport (EData n)
    tellDoc [qqi|
        data {n} = {mkConName msName msName}
          {braceList ms}
        |]
    memberMap <- sequenceV $ Map.fromList
      [ ( smName (msmStructMember m)
        , (\v -> SiblingInfo v (msmScheme m)) <$> memberValue m
        )
      | m <- V.toList msMembers
      ]
    let lookupMember :: Text -> Maybe (SiblingInfo StructMember)
        lookupMember = (`Map.lookup` memberMap)
    runReader lookupMember $ renderStoreInstances s

renderStructMember
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
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

renderStoreInstances
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSpecInfo r
     , HasSiblingInfo StructMember r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
renderStoreInstances ms@MarshaledStruct {..} = do
  RenderParams {..} <- ask
  pokes             <- forV msMembers $ \m@MarshaledStructMember {..} -> do
    p <- getPoke msmStructMember msmScheme
    let p' =
          (\(Unassigned f) -> Assigned $ do
              t <- cToHsType DoPreserve (smType msmStructMember)
              tellImport 'plusPtr
              mVal <- ValueDoc <$> memberValue m
              f
                t
                (AddrDoc
                  (parens $ addrVar <+> "`plusPtr`" <+> viaShow
                    (smOffset msmStructMember)
                  )
                )
                mVal
            )
            <$> p
    pure p'

  toCStructInstance ms pokes

  let isDiscriminated u =
        sName u `elem` (udUnionType <$> toList unionDiscriminators)
  descendentUnions <- filter (not . isDiscriminated) <$> containsUnion msName
  when (null descendentUnions) $ do
    fromCStructInstance ms
    when (all isIOPoke pokes) $ storableInstance ms

-- TODO: Make this calculate the type locally and tell the depends
-- TODO: Don't clutter the code with the type on the record if the accessor is
-- not ambiguous
memberValue
  :: HasRenderParams r => MarshaledStructMember -> Sem r (Doc ())
memberValue MarshaledStructMember {..} = do
  RenderParams {..} <- ask
  let m = mkMemberName (smName msmStructMember)
  pure $ pretty m

storableInstance
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
storableInstance MarshaledStruct{..} = do
  RenderParams{..} <- ask
  let n = mkTyName msName
  -- Some member names clash with storable members "alignment" for instance
  tellImportWithAll ''Storable
  tellImport ''Storable
  tellDoc [qqi|
    instance Storable {n} where
      sizeOf ~_ = {sSize msStruct}
      alignment ~_ = {sAlignment msStruct}
      peek = peekCStruct
      poke ptr poked = pokeCStruct ptr poked (pure ())
  |]

toCStructInstance
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSiblingInfo StructMember r
     )
  => MarshaledStruct AStruct
  -> Vector (AssignedPoke StructMember)
  -> Sem r ()
toCStructInstance MarshaledStruct {..} pokes = do
  RenderParams {..} <- ask
  tellImportWithAll (TyConName "ToCStruct")
  let n           = mkTyName msName
      con         = mkConName msName msName
      Struct {..} = msStruct
      structT     = ConT (typeName n)
      aVar        = mkVar "a"
  tellImport 'allocaBytesAligned
  pokeDoc         <- renderPokesInIO pokes
  pokeCStructTDoc <- renderType
    (ConT ''Ptr :@ structT ~> structT ~> ConT ''IO :@ aVar ~> ConT ''IO :@ aVar)
  tellDoc $ "instance ToCStruct" <+> pretty n <+> "where" <> line <> indent
    2
    (vsep
      [ "withCStruct x f = allocaBytesAligned"
      <+> viaShow sSize
      <+> viaShow sAlignment
      <+> "$ \\p -> pokeCStruct p x (f p)"
      , "pokeCStruct ::" <+> pokeCStructTDoc
      , "pokeCStruct" <+> addrVar <+> pretty con <> "{..} =" <+> pokeDoc
      ]
    )

fromCStructInstance
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
fromCStructInstance MarshaledStruct {..} = do
  RenderParams {..} <- ask
  tellImportWithAll (TyConName "FromCStruct")
  let
    n           = mkTyName msName
    con         = mkConName msName msName
    Struct {..} = msStruct
    structT     = ConT (typeName n)
    offset o tDoc =
      AddrDoc . parens $ addrVar <+> "`plusPtr`" <+> viaShow o <+> "::" <+> tDoc
  tellImport 'plusPtr
  peekDocs <- catMaybes <$> sequenceV
    [ context (smName msmStructMember) $ do
        hTy  <- cToHsType DoPreserve (smType msmStructMember)
        tDoc <- renderType (ConT ''Ptr :@ hTy)
        renderPeekStmt (mkMemberName . smName)
                       msmStructMember
                       (offset (smOffset msmStructMember) tDoc)
                       msmScheme
    | MarshaledStructMember {..} <- V.toList msMembers
    ]
  peekCStructTDoc <- renderType (ConT ''Ptr :@ structT ~> ConT ''IO :@ structT)
  tellDoc $ "instance FromCStruct" <+> pretty n <+> "where" <> line <> indent
    2
    (vsep
      [ "peekCStruct ::" <+> peekCStructTDoc
      , "peekCStruct" <+> addrVar <+> "=" <+> doBlock
        (peekDocs <> ["pure" <+> pretty con <+> "{..}"])
      ]
    )

