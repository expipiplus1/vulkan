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
                                                , Handle
                                                )
import           Text.InterpolatedString.Perl6.Unindented
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.Map                      as Map
import           Data.List.Extra                ( nubOrd )

import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Control.Exception              ( bracket )

import           CType
import           Error
import           Haskell                       as H
import           Marshal
import           Render.Element
import           Render.Peek
import           Render.Poke             hiding ( Pure
                                                , Inline(..)
                                                )
import           Render.Scheme
import           Render.Stmts
import           Render.SpecInfo
import           Render.Type
import           Render.Utils
import           Spec.Parse

renderStruct
  :: (HasErr r, HasRenderParams r, HasSpecInfo r, Member Fixpoint r)
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
    pure ()

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
     , Member Fixpoint r
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
    pure (isUnivalued msmScheme, p')

  toCStructInstance ms pokes

  let isDiscriminated u =
        sName u `elem` (udUnionType <$> toList unionDiscriminators)
  descendentUnions <- filter (not . isDiscriminated) <$> containsUnion msName
  containsDispatchable <- containsDispatchableHandle msStruct
  when (null descendentUnions) $ do
    fromCStruct ms

    -- TODO, this doesn't allow for pure or chained pokes (without contt)
    unless (any (containsContTPoke . snd) pokes || containsDispatchable)
      $ storableInstance ms

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
     , HasSpecInfo r
     )
  => MarshaledStruct AStruct
  -> Vector (Bool, AssignedPoke StructMember)
  -> Sem r ()
toCStructInstance m@MarshaledStruct {..} pokes = do
  RenderParams {..} <- ask
  tellImportWithAll (TyConName "ToCStruct")
  let n           = mkTyName msName
      con         = mkConName msName msName
      Struct {..} = msStruct
      structT     = ConT (typeName n)
      aVar        = mkVar "a"
  tellImport 'allocaBytesAligned
  pokeDoc         <- renderPokesWithRunContT (snd <$> pokes)
  pokeCStructTDoc <- renderType
    (ConT ''Ptr :@ structT ~> structT ~> ConT ''IO :@ aVar ~> ConT ''IO :@ aVar)
  zero <- withZeroCStructDecl m (snd <$> V.filter fst pokes)
  tellDoc $ "instance ToCStruct" <+> pretty n <+> "where" <> line <> indent
    2
    (vsep
      [ "withCStruct x f = allocaBytesAligned"
      <+> viaShow sSize
      <+> viaShow sAlignment
      <+> "$ \\p -> pokeCStruct p x (f p)"
      , "pokeCStruct ::" <+> pokeCStructTDoc
      , "pokeCStruct" <+> addrVar <+> pretty con <> "{..} =" <+> pokeDoc
      , zero
      ]
    )

fromCStruct
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , Member Fixpoint r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
fromCStruct m@MarshaledStruct {..} = do
  dispatchableHandles msStruct >>= \case
    [] -> fromCStructInstance m
    hs -> fromCStructFunction m hs

fromCStructInstance
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , Member Fixpoint r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
fromCStructInstance m@MarshaledStruct {..} = do
  RenderParams {..} <- ask
  tellImportWithAll (TyConName "FromCStruct")
  let n           = mkTyName msName
      Struct {..} = msStruct
      structT     = ConT (typeName n)
  tellImport 'plusPtr
  peekCStructTDoc <- renderType (ConT ''Ptr :@ structT ~> ConT ''IO :@ structT)
  peekStmts       <- peekCStructBody m
  tellDoc $ "instance FromCStruct" <+> pretty n <+> "where" <> line <> indent
    2
    (vsep
      [ "peekCStruct ::" <+> peekCStructTDoc
      , "peekCStruct" <+> addrVar <+> "=" <+> peekStmts
      ]
    )

fromCStructFunction
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , Member Fixpoint r
     )
  => MarshaledStruct AStruct
  -> [Handle]
  -> Sem r ()
fromCStructFunction m@MarshaledStruct {..} handles = do
  RenderParams {..} <- ask
  tellImportWithAll (TyConName "FromCStruct")
  let n            = mkTyName msName
      Struct {..}  = msStruct
      structT      = ConT (typeName n)
      funName      = "peekCStruct" <> n
      handleLevels = nubOrd (hLevel <$> handles)
  handleCommandTypes <- forV handleLevels $ \case
    NoHandleLevel -> throw "Dispatchable handle with no level" -- TODO: use types for this
    Device        -> pure $ ConT (typeName "DeviceCmds")
    Instance      -> pure $ ConT (typeName "InstanceCmds")
  handleCommandNames <- case handleLevels of
    [_] -> pure ["cmds"]
    _   -> throw "TODO: fromCStruct with multiple cmd levels"
  peekCStructTDoc <- renderType
    (foldr (~>)
           (ConT ''IO :@ structT)
           (handleCommandTypes <> [ConT ''Ptr :@ structT])
    )
  peekStmts <- peekCStructBody m
  tellExport (ETerm funName)
  tellDoc $ vsep
    [ "-- |"
    <+> pretty n
    <+> "contains dispatchable handles, for which the function"
    , "-- pointer record would be inaccessible if this was an instance of @fromCStruct@"
    , pretty funName <+> "::" <+> peekCStructTDoc
    , pretty funName
    <+> sep handleCommandNames
    <+> addrVar
    <+> "="
    <+> peekStmts
    ]


peekCStructBody
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , Member Fixpoint r
     )
  => MarshaledStruct AStruct
  -> Sem r (Doc ())
peekCStructBody MarshaledStruct {..} = do
  RenderParams {..} <- ask
  let
    con         = mkConName msName msName
    Struct {..} = msStruct
    offset o tDoc =
      AddrDoc . parens $ addrVar <+> "`plusPtr`" <+> viaShow o <+> "::" <+> tDoc
  renderStmtsIO $ do
    memberRefs <- forV msMembers $ \MarshaledStructMember {..} -> do
      pure ()
    stmt $ do
      -- traverse_ (void . use) memberRefs
      pure $ StmtResult Nothing Nothing $ Pure
        DoInline
        (pretty con <> "{..}" :: Doc ())


--   peekDocs <- catMaybes <$> sequenceV
--     [ context (smName msmStructMember) $ do
--         hTy  <- cToHsType DoPreserve (smType msmStructMember)
--         tDoc <- renderType (ConT ''Ptr :@ hTy)
--         renderPeekStmt (mkMemberName . smName)
--                        msmStructMember
--                        (offset (smOffset msmStructMember) tDoc)
--                        msmScheme
--     | MarshaledStructMember {..} <- V.toList msMembers
--     ]

withZeroCStructDecl
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     )
  => MarshaledStruct AStruct
  -> Vector (AssignedPoke StructMember)
  -> Sem r (Doc ())
withZeroCStructDecl MarshaledStruct {..} pokes = do
  RenderParams {..} <- ask
  let n           = mkTyName msName
      Struct {..} = msStruct
      structT     = ConT (typeName n)
  withZeroCStructTDoc <-
    let retVar = VarT (typeName "b")
    in  renderType
          ((ConT ''Ptr :@ structT ~> ConT ''IO :@ retVar) ~> ConT ''IO :@ retVar
          )
  tellImport 'bracket
  tellImport 'callocBytes
  tellImport 'free
  pokeDoc <- renderPokesInIO
    (pokes `V.snoc` IOPoke (Assigned $ pure ("f" <+> addrVar)))
  pure $ vsep
    [ "withZeroCStruct ::" <+> withZeroCStructTDoc
    , "withZeroCStruct f = bracket (callocBytes"
    <+> viaShow sSize
    <>  ") free $ \\"
    <>  addrVar
    <+> "->"
    <+> pokeDoc
    ]

