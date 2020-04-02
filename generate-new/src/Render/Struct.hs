{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.Struct
  where

import           Relude                  hiding ( Handle )
import           Text.InterpolatedString.Perl6.Unindented
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                ( upperCaseFirst )
import           Polysemy
import           Polysemy.Input
import qualified Data.Vector.Extra             as V
import qualified Data.Map                      as Map
import           Data.List.Extra                ( nubOrd )
import           Language.Haskell.TH            ( mkName )

import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Control.Monad.Trans.Cont       ( evalContT )

import           Error
import           Data.Typeable
import           Haskell                       as H
                                         hiding ( Type )
import           CType
import           Marshal
import           Marshal.Scheme
import           Marshal.Scheme.Zero
import           Render.Element
import           Render.Peek
import           Render.Stmts.Poke
import           Render.Scheme
import           Render.Stmts
import           Render.SpecInfo
import           Render.Type
import           Render.Utils
import           Render.Names
import           Spec.Parse
import           Bespoke

renderStruct
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     )
  => MarshaledStruct AStruct
  -> Sem r RenderElement
renderStruct s@MarshaledStruct {..} = context (unCName msName) $ do
  RenderParams {..} <- input
  genRe ("struct " <> unCName msName) $ do
    let n = mkTyName msName
    ms <- V.mapMaybe id <$> traverseV (renderStructMember msName) msMembers
    tellImport ''Type
    showStub <- showInstanceStub tellImport msStruct
    let childVar = if hasChildren msStruct
          then " (" <> pretty structChainVar <+> ":: [Type])"
          else ""
        derivingDecl = "deriving" <+> showStub
    tellExport (EData n)
    tellBoot $ do
      tellExport (EType n)
      tellImport ''Type
      tellDoc $ vsep
        (  [ "type role" <+> pretty n <+> "nominal" | hasChildren msStruct ]
        <> ["data" <+> pretty n <> childVar]
        )
    tellImport ''Typeable
    tellDocWithHaddock $ \getDoc -> [qqi|
        {getDoc (TopLevel msName)}
        data {n}{childVar} = {mkConName msName msName}
          {braceList' (($ getDoc) <$> ms)}
          deriving (Typeable)
        {derivingDecl}
        |]
    memberMap <- sequenceV $ Map.fromList
      [ ( smName (msmStructMember m)
        , (\v -> SiblingInfo v (msmScheme m)) <$> recordWildCardsMemberVal m
        )
      | m <- V.toList msMembers
      ]
    renderExtensibleInstance s
    let lookupMember :: CName -> Maybe (SiblingInfo StructMember)
        lookupMember = (`Map.lookup` memberMap)
    runInputConst lookupMember $ renderStoreInstances s
    pure ()

renderStructMember
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => CName
  -> MarshaledStructMember
  -> Sem r (Maybe ((Documentee -> Doc ()) -> Doc ()))
renderStructMember sName MarshaledStructMember {..} = do
  let StructMember {..} = msmStructMember
  RenderParams {..} <- input
  m                 <- schemeType msmScheme
  traverse
    (\t -> do
      tDoc <- renderType t
      pure $ \getDoc -> align
        (vsep
          [ getDoc (Nested sName smName)
          , pretty (mkMemberName smName) <+> "::" <+> tDoc
          ]
        )
    )
    m

----------------------------------------------------------------
-- Extending
----------------------------------------------------------------

renderExtensibleInstance
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => MarshaledStruct AStruct
  -> Sem r ()
renderExtensibleInstance MarshaledStruct {..} = do
  RenderParams {..} <- input
  let n   = mkTyName (sName msStruct)
      con = mkConName (sName msStruct) (sName msStruct)
  unless (V.null (sExtendedBy msStruct)) $ do
    tellImportWithAll (TyConName "Extensible")
    tellImport (TyConName "Extends")
    tellImport ''Typeable
    tellImportWith ''(:~:) 'Refl
    tellImport 'eqT
    matches <- forV (sExtendedBy msStruct) $ \childName -> do
      child <- note "Unable to find extending struct" =<< getStruct childName
      childTDoc <- renderTypeHighPrecSource $ if V.null (sExtendedBy child)
        then ConT (typeName (mkTyName childName))
        else ConT (typeName (mkTyName childName)) :@ PromotedNilT
      pure $ "| Just Refl <- eqT @e @" <> childTDoc <+> "= Just f"
    let
      noMatch = "| otherwise = Nothing"
      cases   = toList matches ++ [noMatch]
      structTypes =
        [ v
        | MarshaledStructMember { msmStructMember = StructMember { smName = "sType" }, msmScheme = ElidedUnivalued v } <-
          toList msMembers
        ]
    structType <- case structTypes of
      []  -> throw "Unable to find type enum of extensible struct"
      [v] -> pure v
      vs  -> throw $ "Found multiple struct type enums " <> show vs
    tellDoc $ "instance Extensible" <+> pretty n <+> "where" <> line <> indent
      2
      (vsep
        [ "extensibleType =" <+> pretty (mkPatternName (CName structType))
        , "setNext x next = x{next = next}"
        , "getNext" <+> pretty con <> "{..} = next"
        , "extends :: forall e b proxy. Typeable e => proxy e -> (Extends"
        <+> pretty n
        <+> "e => b) -> Maybe b"
        , "extends _ f" <> line <> indent 2 (vsep cases)
        ]
      )

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
     , HasRenderedNames r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
renderStoreInstances ms@MarshaledStruct {..} = do
  RenderParams {..} <- input

  tellBoot $ tellDoc . vsep =<< sequenceV
    [ toCStructInstanceStub tellSourceImport msStruct
    , showInstanceStub tellSourceImport msStruct
    ]

  pokes <- renderPokes (fmap Just . recordWildCardsMemberVal)
                       (IOAction $ pretty contVar)
                       ms

  toCStructInstance ms pokes

  let isDiscriminated u =
        sName u `elem` (udUnionType <$> toList unionDiscriminators)
  descendentUnions <- filter (not . isDiscriminated) <$> containsUnion msName

  when (null descendentUnions) $ do
    fromCStructInstance ms

    -- Render a Storable instance if it's safe to do so
    case pokes of
      IOStmts _ -> storableInstance ms
      _         -> pure ()

  zeroInstanceDecl ms

storableInstance
  :: (HasErr r, HasRenderParams r, HasRenderElem r)
  => MarshaledStruct AStruct
  -> Sem r ()
storableInstance MarshaledStruct {..} = do
  RenderParams {..} <- input
  let n = mkTyName msName
  -- Some member names clash with storable members "alignment" for instance
  tellImport ''Storable
  tellQualImportWithAll ''Storable
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
     , HasRenderedNames r
     )
  => MarshaledStruct AStruct
  -> RenderedStmts (Doc ())
  -> Sem r ()
toCStructInstance m@MarshaledStruct {..} pokeValue = do
  RenderParams {..} <- input
  tellImportWithAll (TyConName "ToCStruct")
  let con         = mkConName msName msName
      Struct {..} = msStruct
  tellImport 'allocaBytesAligned
  zero    <- pokeZeroCStructDecl m
  pokeDoc <- case pokeValue of
    ContTStmts d -> do
      tellImport 'evalContT
      pure $ "evalContT $" <+> d
    IOStmts d -> pure d
  (size, alignment) <- getTypeSize (TypeName msName)
  let unpack = if all (isElided . msmScheme) msMembers then "" else "{..}"
  stub <- toCStructInstanceStub tellImport msStruct
  tellDoc $ (stub <+> "where") <> line <> indent
    2
    (vsep
      [ "withCStruct x f = allocaBytesAligned"
      <+> viaShow sSize
      <+> viaShow sAlignment
      <+> "$ \\p -> pokeCStruct p x (f p)"
      , "pokeCStruct"
      <+> pretty addrVar
      <+> pretty con
      <>  unpack
      <+> pretty contVar
      <+> "="
      <+> pokeDoc
      , "cStructSize =" <+> viaShow size
      , "cStructAlignment =" <+> viaShow alignment
      , zero
      ]
    )

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
  RenderParams {..} <- input
  tellImportWithAll (TyConName "FromCStruct")
  tellImport 'plusPtr
  peekStmts <- peekCStructBody m
  stub      <- fromCStructInstanceStub tellImport msStruct
  tellBoot $ tellDoc =<< fromCStructInstanceStub tellSourceImport msStruct
  let addrVar' =
        if V.all (isElided . msmScheme) msMembers then "_" else addrVar
  tellDoc $ (stub <+> "where") <> line <> indent
    2
    (vsep ["peekCStruct" <+> pretty addrVar' <+> "=" <+> peekStmts])

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
  RenderParams {..} <- input
  tellImportWithAll (TyConName "FromCStruct")
  let n           = mkTyName msName
      Struct {..} = msStruct
      structT     = if hasChildren msStruct
        then ConT (typeName n) :@ VarT (mkName structChainVar)
        else ConT (typeName n)
      funName      = TermName $ "peekCStruct" <> unName n
      handleLevels = nubOrd (hLevel <$> handles)
  addContext <- if hasChildren msStruct
    then do
      tellImport (TyConName "PeekChain")
      pure $ ForallT
        []
        [ConT (mkName "PeekChain") :@ VarT (mkName structChainVar)]
    else pure id
  handleCommandTypes <- forV handleLevels $ \case
    NoHandleLevel -> throw "Dispatchable handle with no level" -- TODO: use types for this
    Device        -> pure $ ConT (typeName (TyConName "DeviceCmds"))
    Instance      -> pure $ ConT (typeName (TyConName "InstanceCmds"))
  handleCommandNames <- case handleLevels of
    [_] -> pure ["cmds"]
    _   -> throw "TODO: fromCStruct with multiple cmd levels"
  peekCStructTDoc <- renderType . addContext $ foldr
    (~>)
    (ConT ''IO :@ structT)
    (handleCommandTypes <> [ConT ''Ptr :@ structT])

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
  RenderParams {..} <- input
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
    memberRefs <-
      fmap (V.mapMaybe id) . forV msMembers $ \MarshaledStructMember {..} ->
        context (unCName $ smName msmStructMember) $ do

          hTy <- cToHsType DoPreserve (smType msmStructMember)

          fmap (isElided msmScheme, ) <$> do

            addr <- stmt (Just (ConT ''Ptr :@ hTy)) Nothing $ do
              tDoc <- renderType (ConT ''Ptr :@ hTy)
              pure $ Pure InlineOnce (offset (smOffset msmStructMember) tDoc)

            p <- peekStmt msmStructMember addr msmScheme
            for_ p (nameRef (unCName $ smName msmStructMember))
            pure p


    stmt Nothing Nothing $ do
      memberDocs <- traverse use [ r | (e, r) <- toList memberRefs, not e ]
      pure $ Pure InlineOnce $ align
        (pretty con <> line <> indent 2 (sep (unValueDoc <$> memberDocs)))

pokeZeroCStructDecl
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , HasStmts r
     , HasRenderedNames r
     )
  => MarshaledStruct AStruct
  -> Sem r (Doc ())
pokeZeroCStructDecl ms@MarshaledStruct {..} = do
  RenderParams {..} <- input

  let replaceWithZeroChainPoke m = case msmScheme m of
        Custom s@(CustomScheme "Chain" _ _ _ _) ->
          m { msmScheme = Custom s { csDirectPoke = const zeroNextPointer } }
        _ -> m
      ms' = ms { msMembers = replaceWithZeroChainPoke <$> msMembers }
  pokeDoc <- renderPokes zeroMemberVal (IOAction $ pretty contVar) ms' >>= \case
    ContTStmts d -> do
      tellImport 'evalContT
      pure $ "evalContT $" <+> d
    IOStmts d -> pure d

  let Struct {..} = msStruct
  addrVar' <- bool "_" addrVar . V.any isJust <$> forV msMembers zeroMemberVal
  pure
    $   "pokeZeroCStruct"
    <+> pretty addrVar'
    <+> pretty contVar
    <+> "="
    <+> pokeDoc

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
  RenderParams {..} <- input
  let n    = mkTyName msName
      con  = mkConName msName msName
      head = if hasChildren msStruct
        then " " <> pretty structChainVar <> " ~ '[] =>"
        else ""
      tDoc = if hasChildren msStruct
        then parens (pretty n <+> pretty structChainVar)
        else pretty n
  zeroMembers <- catMaybes . toList <$> forV msMembers (zeroScheme . msmScheme)
  tellImportWithAll (TyConName "Zero")
  tellDoc $ "instance" <> head <+> "Zero" <+> tDoc <+> "where" <> line <> indent
    2
    (vsep
      ["zero =" <+> align (pretty con <> line <> indent 2 (vsep zeroMembers))]
    )

renderPokes
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSiblingInfo StructMember r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     )
  => (MarshaledStructMember -> Sem r (Maybe (Doc ())))
  -- ^ A predicate for which members we should poke
  -> Value (Doc ())
  -- ^ The value to return at the end
  -> MarshaledStruct AStruct
  -> Sem r (RenderedStmts (Doc ()))
renderPokes memberDoc end MarshaledStruct {..} = do
  let forbiddenNames = fromList
        (contVar : addrVar : toList
          (unCName . smName . msmStructMember <$> msMembers)
        )
  renderStmts forbiddenNames $ do
    -- Make and name all the values first so that they can all be referred to
    -- by name.
    values <- forV msMembers $ \m -> raise (memberDoc m) >>= \case
      Nothing -> pure Nothing
      Just d  -> Just <$> memberVal m d
    ps <-
      fmap (V.mapMaybe id)
      . forV (V.zip values msMembers)
      $ \(valueMaybe, MarshaledStructMember {..}) -> case valueMaybe of
          Just value -> Just <$> do
            addr <-
              stmt
                Nothing
                (Just ("p" <> upperCaseFirst (unCName $ smName msmStructMember))
                )
              . fmap (Pure InlineOnce . AddrDoc)
              $ do
                  pTyDoc <- renderType . (ConT ''Ptr :@) =<< cToHsTypeWithHoles
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
          Nothing -> pure Nothing
    stmt Nothing Nothing $ do
      traverse_ after ps
      pure end

memberVal
  :: (HasErr r, HasSpecInfo r, HasRenderParams r)
  => MarshaledStructMember
  -> Doc ()
  -> Stmt s r (Ref s ValueDoc)
memberVal MarshaledStructMember {..} doc = do
  ty <- schemeType msmScheme
  v  <-
    stmt ty (Just . unCName . smName $ msmStructMember)
    . pure
    . Pure AlwaysInline
    . ValueDoc
    $ doc
  nameRef (unCName $ smName msmStructMember) v
  pure v

-- | We use RecordWildCards, so just use the member name here
recordWildCardsMemberVal
  :: HasRenderParams r => MarshaledStructMember -> Sem r (Doc ())
recordWildCardsMemberVal MarshaledStructMember {..} = do
  RenderParams {..} <- input
  let m = mkMemberName (smName msmStructMember)
  pure $ pretty m

zeroMemberVal
  :: (HasRenderElem r, HasRenderParams r)
  => MarshaledStructMember
  -> Sem r (Maybe (Doc ()))
  -- ^ Returns nothing if this is just 0 bytes and doesn't need poking
zeroMemberVal MarshaledStructMember {..} = case msmScheme of
  ElidedUnivalued _ ->
    pure $ Just "error \"This should never appear in the generated source\""
  _ | True V.:<| _ <- smIsOptional msmStructMember -> pure Nothing
  s -> zeroScheme s

----------------------------------------------------------------
-- Instance decls
----------------------------------------------------------------

fromCStructInstanceStub
  :: (HasRenderParams r, HasRenderElem r)
  => (HName -> Sem r ())
  -> Struct
  -> Sem r (Doc ())
fromCStructInstanceStub tellSourceImport s = do
  RenderParams {..} <- input
  let n    = mkTyName (sName s)
      tDoc = if hasChildren s
        then parens (pretty n <+> pretty structChainVar)
        else pretty n
  head <- if hasChildren s
    then do
      tellSourceImport (TyConName "PeekChain")
      pure $ " PeekChain" <+> pretty structChainVar <+> "=>"
    else pure ""
  tellImport (TyConName "FromCStruct")
  pure $ "instance" <> head <+> "FromCStruct" <+> tDoc

toCStructInstanceStub
  :: (HasRenderParams r, HasRenderElem r)
  => (HName -> Sem r ())
  -> Struct
  -> Sem r (Doc ())
toCStructInstanceStub tellSourceImport s = do
  RenderParams {..} <- input
  let n    = mkTyName (sName s)
      tDoc = if hasChildren s
        then parens (pretty n <+> pretty structChainVar)
        else pretty n
  head <- if hasChildren s
    then do
      tellSourceImport (TyConName "PokeChain")
      pure $ " PokeChain" <+> pretty structChainVar <+> "=>"
    else pure ""
  tellImport (TyConName "ToCStruct")
  pure $ "instance" <> head <+> "ToCStruct" <+> tDoc

showInstanceStub
  :: (HasRenderParams r, HasRenderElem r)
  => (HName -> Sem r ())
  -> Struct
  -> Sem r (Doc ())
showInstanceStub tellSourceImport s = do
  RenderParams {..} <- input
  let n = mkTyName (sName s)
  if hasChildren s
    then do
      tellSourceImport (TyConName "Chain")
      pure $ "instance Show (Chain es) => Show (" <> pretty n <+> "es)"
    else pure $ "instance Show" <+> pretty n


----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

hasChildren :: Struct -> Bool
hasChildren = not . V.null . sExtendedBy
