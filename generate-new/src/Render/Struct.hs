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
import           Data.Text.Extra                ( upperCaseFirst )
import           Polysemy
import           Polysemy.NonDet
import           Polysemy.Reader
import qualified Data.Vector                   as V
import qualified Data.Map                      as Map
import           Data.List.Extra                ( nubOrd )

import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Control.Exception              ( bracket )
import           Control.Monad.Trans.Cont       ( evalContT )

import           Error
import           Haskell                       as H
import           Marshal
import           Marshal.Scheme
import           Render.Element
import           Render.Peek
import           Render.Stmts.Poke
import           Render.Scheme
import           Render.Stmts
import           Render.SpecInfo
import           Render.Type
import           Render.Utils
import           Spec.Parse

renderStruct
  :: (HasErr r, HasRenderParams r, HasSpecInfo r, HasStmts r)
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
          deriving (Show)
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

addrVar :: Text
addrVar = "p"

contVar :: Text
contVar = "f"

renderStoreInstances
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSpecInfo r
     , HasSiblingInfo StructMember r
     , HasStmts r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
renderStoreInstances ms@MarshaledStruct {..} = do
  RenderParams {..} <- ask

  pokes             <- renderPokes (const True) (IOAction $ pretty contVar) ms

  toCStructInstance ms pokes

  let isDiscriminated u =
        sName u `elem` (udUnionType <$> toList unionDiscriminators)
  descendentUnions <- filter (not . isDiscriminated) <$> containsUnion msName

  when (null descendentUnions) $ do
    fromCStruct ms

    -- Render a Storable instance if it's safe to do so
    containsDispatchable <- containsDispatchableHandle msStruct
    case pokes of
      IOStmts _ | not containsDispatchable -> storableInstance ms
      _ -> pure ()

  zeroInstanceDecl ms

-- | We use RecordWildCards, so just use the member name here
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
     , HasStmts r
     )
  => MarshaledStruct AStruct
  -> RenderedStmts (Doc ())
  -> Sem r ()
toCStructInstance m@MarshaledStruct {..} pokeValue = do
  RenderParams {..} <- ask
  tellImportWithAll (TyConName "ToCStruct")
  let n           = mkTyName msName
      con         = mkConName msName msName
      Struct {..} = msStruct
      structT     = ConT (typeName n)
      aVar        = mkVar "a"
  tellImport 'allocaBytesAligned
  pokeCStructTDoc <- renderType
    (ConT ''Ptr :@ structT ~> structT ~> ConT ''IO :@ aVar ~> ConT ''IO :@ aVar)
  zero    <- withZeroCStructDecl m
  pokeDoc <- case pokeValue of
    ContTStmts d -> do
      tellImport 'evalContT
      pure $ "evalContT $" <+> d
    IOStmts d -> pure d
  tellDoc $ "instance ToCStruct" <+> pretty n <+> "where" <> line <> indent
    2
    (vsep
      [ "withCStruct x f = allocaBytesAligned"
      <+> viaShow sSize
      <+> viaShow sAlignment
      <+> "$ \\p -> pokeCStruct p x (f p)"
      , "pokeCStruct ::" <+> pokeCStructTDoc
      , "pokeCStruct"
      <+> pretty addrVar
      <+> pretty con
      <>  "{..}"
      <+> pretty contVar
      <+> "="
      <+> pokeDoc
      , zero
      ]
    )

fromCStruct
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , HasStmts r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
fromCStruct m@MarshaledStruct {..} = dispatchableHandles msStruct >>= \case
  [] -> fromCStructInstance m
  hs -> fromCStructFunction m hs

fromCStructInstance
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , HasStmts r
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
      , "peekCStruct" <+> pretty addrVar <+> "=" <+> peekStmts
      ]
    )

fromCStructFunction
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , HasStmts r
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
    <+> pretty addrVar
    <+> "="
    <+> peekStmts
    ]


peekCStructBody
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , HasStmts r
     )
  => MarshaledStruct AStruct
  -> Sem r (Doc ())
peekCStructBody MarshaledStruct {..} = do
  RenderParams {..} <- ask
  let con         = mkConName msName msName
      Struct {..} = msStruct
      offset o tDoc =
        AddrDoc
          .   parens
          $   pretty addrVar
          <+> "`plusPtr`"
          <+> viaShow o
          <+> "::"
          <+> tDoc
      forbiddenNames = fromList []
  renderStmtsIO forbiddenNames $ do
    cmdsRef <- pureStmt (CmdsDoc "cmds")
    nameRef "cmds" cmdsRef
    memberRefs <-
      fmap (V.mapMaybe id) . forV msMembers $ \MarshaledStructMember {..} ->
        context (smName msmStructMember) $ do

          hTy <- cToHsType DoPreserve (smType msmStructMember)

          fmap (isElided msmScheme, ) <$> do

            addr <- stmt (Just (ConT ''Ptr :@ hTy)) Nothing $ do
              tDoc <- renderType (ConT ''Ptr :@ hTy)
              pure $ Pure InlineOnce (offset (smOffset msmStructMember) tDoc)

            p <- peekStmt msmStructMember addr msmScheme
            for_ p (nameRef (smName msmStructMember))
            pure p


    stmt Nothing Nothing $ do
      memberDocs <- traverse use [ r | (e, r) <- toList memberRefs, not e ]
      pure $ Pure InlineOnce $ pretty con <+> align
        (sep (unValueDoc <$> memberDocs))

withZeroCStructDecl
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , HasStmts r
     )
  => MarshaledStruct AStruct
  -> Sem r (Doc ())
withZeroCStructDecl ms@MarshaledStruct {..} = do
  RenderParams {..} <- ask

  pokeDoc           <-
    renderPokes (isUnivalued . msmScheme)
                (IOAction $ pretty contVar <+> pretty addrVar)
                ms
      >>= \case
            ContTStmts d -> do
              tellImport 'evalContT
              pure $ "evalContT $" <+> d
            IOStmts d -> pure d

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
  pure $ vsep
    [ "withZeroCStruct ::" <+> withZeroCStructTDoc
    , "withZeroCStruct f = bracket (callocBytes"
    <+> viaShow sSize
    <>  ") free $ \\"
    <>  pretty addrVar
    <+> "->"
    <+> pokeDoc
    ]

zeroInstanceDecl
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , HasStmts r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
zeroInstanceDecl MarshaledStruct {..} = do
  RenderParams {..} <- ask
  let n   = mkTyName msName
      con = mkConName msName msName
  zeroMembers <- catMaybes . toList <$> forV msMembers (zeroScheme . msmScheme)
  tellImportWithAll (TyConName "Zero")
  tellDoc $ "instance Zero" <+> pretty n <+> "where" <> line <> indent
    2
    -- (vsep ["zero =" <+> pretty con <> line <> indent 2 (vsep zeroMembers)])
    (vsep ["zero = undefined"])

zeroScheme
  :: MarshalScheme a
  -> Sem r (Maybe (Doc ()))
zeroScheme = runNonDetMaybe . go
 where
  go = \case
    Unit              -> pure "()"
    Preserve _        -> pure "zero"
    Normal   _        -> pure "zero"
    ElidedLength _ _  -> empty
    ElidedUnivalued _ -> empty
    ElidedVoid        -> empty
    VoidPtr           -> pure "zero"
    ByteString        -> pure "mempty"
    Maybe        _    -> pure "Nothing"
    Vector       _    -> pure "mempty"
    EitherWord32 _    -> pure $ parens "Left 0"
    Tupled n s        -> do
      z <- go s
      pure $ tupled (replicate (fromIntegral n) z)
    Returned   _ -> empty
    InOutCount _ -> empty

renderPokes
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSiblingInfo StructMember r
     , HasSpecInfo r
     , HasStmts r
     )
  => (MarshaledStructMember -> Bool)
  -- ^ A predicate for which members we should poke
  -> Value (Doc ())
  -- ^ The value to return at the end
  -> MarshaledStruct AStruct
  -> Sem r (RenderedStmts (Doc ()))
renderPokes p end MarshaledStruct {..} = do
  let forbiddenNames = fromList
        (contVar : addrVar : toList (smName . msmStructMember <$> msMembers))
  renderStmts forbiddenNames $ do
    -- Make and name all the values first so that they can all be referred to
    -- by name.
    values <- forV msMembers $ \m@MarshaledStructMember {..} -> do
      ty <- schemeType msmScheme
      v  <-
        stmt ty (Just (smName msmStructMember))
        $   Pure AlwaysInline
        .   ValueDoc
        <$> memberValue m
      nameRef (smName msmStructMember) v
      pure v
    ps <-
      fmap (V.mapMaybe id)
      . forV (V.zip values msMembers)
      $ \(value, msm@MarshaledStructMember {..}) -> if p msm
          then Just <$> do
            addr <-
              stmt Nothing
                   (Just ("p" <> upperCaseFirst (smName msmStructMember)))
              . fmap (Pure InlineOnce . AddrDoc)
              $ do
                  pTyDoc <- renderType . (ConT ''Ptr :@) =<< cToHsType
                    DoPreserve
                    (smType msmStructMember)
                  tellImport 'plusPtr
                  pure $ parens
                    (   pretty addrVar
                    <+> "`plusPtr`"
                    <+> viaShow (smOffset msmStructMember)
                    <+> "::"
                    <+> pTyDoc
                    )
            getPokeIndirect msmStructMember msmScheme value addr
          else pure Nothing
    stmt Nothing Nothing $ do
      traverse_ after ps
      pure end
