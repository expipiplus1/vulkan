{-# LANGUAGE NamedFieldPuns #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.Struct where

import qualified Data.Map                      as Map
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector.Extra             as V
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( Handle )
import           Text.InterpolatedString.Perl6.Unindented

import           Control.Monad.Trans.Cont       ( evalContT )
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

import           Bespoke
import           CType
import           Control.Exception              ( IOException
                                                , throwIO
                                                )
import           Data.Char                      ( isUpper )
import           Data.Traversable
import           Data.Typeable
import           Error
import           Haskell                       as H
                                         hiding ( Type )
import           Marshal
import           Marshal.Scheme
import           Marshal.Scheme.Zero
import           Render.Element
import           Render.Names
import           Render.Peek
import           Render.Scheme
import           Render.SpecInfo
import           Render.State
import           Render.Stmts
import           Render.Stmts.Poke
import           Render.Type
import           Render.Utils
import           Spec.Types
import           System.IO.Error                ( IOErrorType )

renderStruct
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     , HasRenderState r
     )
  => MarshaledStruct AStruct
  -> Sem r RenderElement
renderStruct s@MarshaledStruct {..} = context (unCName msName) $ do
  RenderParams {..} <- input
  genRe ("struct " <> unCName msName) $ do
    let n = mkTyName msName
    ms <- V.mapMaybe id <$> traverseV (renderStructMember msName) msMembers
    tellImport ''Type
    tellImport ''Generic
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
    simple <- isSimpleStruct s
    let derivedInstances = ["Typeable"] <> [ "Eq" | simple ]
    tellDocWithHaddock $ \getDoc -> [qqi|
        {getDoc (TopLevel msName)}
        data {n}{childVar} = {mkConName msName msName}
          {braceList' (($ getDoc) <$> ms)}
          deriving {tupled derivedInstances}
        #if defined(GENERIC_INSTANCES)
        deriving instance Generic ({n}{childVar})
        #endif
        {derivingDecl}
        |]
    memberMap <- sequenceV $ Map.fromList
      [ ( smName (msmStructMember m)
        , (\v -> SiblingInfo v (msmScheme m))
          <$> recordWildCardsMemberVal msName m
        )
      | m <- V.toList msMembers
      ]
    renderExtensibleInstance s
    renderInheritableClass s
    renderInheritableParentInstance s
    renderInheritableInstance s
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
  m                 <- schemeTypeNegative msmScheme
  traverse
    (\t -> do
      tDoc <- renderType t
      pure $ \getDoc -> align
        (vsep
          [ getDoc (Nested sName smName)
          , pretty (mkMemberName sName smName) <+> "::" <+> tDoc
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
  let n          = mkTyName (sName msStruct)
      con        = mkConName (sName msStruct) (sName msStruct)
      isExtended = not (V.null (sExtendedBy msStruct))
  when isExtended $ do
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
    let noMatch = "| otherwise = Nothing"
        cases   = toList matches ++ [noMatch]
    tellDoc $ "instance Extensible" <+> pretty n <+> "where" <> line <> indent
      2
      (vsep
        [ "extensibleTypeName =" <+> dquotes (pretty n)
        , "setNext x next = x{next = next}"
        , "getNext" <+> pretty con <> "{..} = next"
        , "extends :: forall e b proxy. Typeable e => proxy e -> (Extends"
        <+> pretty n
        <+> "e => b) -> Maybe b"
        , "extends _ f" <> line <> indent 2 (vsep cases)
        ]
      )

----------------------------------------------------------------
-- Inheriting
----------------------------------------------------------------

renderInheritableClass
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => MarshaledStruct AStruct
  -> Sem r ()
renderInheritableClass MarshaledStruct {..} | null (sInheritedBy msStruct) =
  pure ()
renderInheritableClass MarshaledStruct {..} = context "inheritable class" $ do
  RenderParams {..} <- input
  className         <- inheritClassName msName
  let t = mkTyName msName
  tellExport (EClass className)
  if null (sExtendedBy msStruct)
    then tellDoc [qqi|
      class ToCStruct a => {className} a where
        to{t} :: a -> {t}
    |]
    else tellDoc [qqi|
      class ToCStruct a => {className} a where
        to{t} :: a -> {t} '[]
    |]

renderInheritableParentInstance
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => MarshaledStruct AStruct
  -> Sem r ()
renderInheritableParentInstance MarshaledStruct {..}
  | null (sInheritedBy msStruct) = pure ()
  | otherwise = do
    RenderParams {..} <- input
    let structType = mkTyName "XrStructureType"
    let t          = mkTyName msName
    let h = if null (sExtendedBy msStruct)
          then pretty t
          else parens (pretty t <+> "'[]")
    tellImportWithAll (TyConName "Inheritable")
    tellImport 'castPtr
    tellImportWithAll (TyConName "SomeChild")
    tellImport 'throwIO
    tellImportWithAll ''IOException
    tellImportWithAll ''IOErrorType
    tellImport structType
    cases <- fmap toList . for (sInheritedBy msStruct) $ \cName -> do
      getStruct cName >>= \case
        Nothing    -> throw "Unable to find child struct"
        Just child -> do
          let cHName = mkTyName (sName child)
          tellSourceImport cHName
          e <- mkPatternName <$> getStructTypeTag child
          tellImport e
          pure
            [qqi|{e} -> SomeChild <$> peekCStruct (castPtr @(SomeChild {h}) @{cHName} p)|]
    tellDoc [qqi|
      instance Inheritable {h} where
        peekSomeCChild :: Ptr (SomeChild {h}) -> IO (SomeChild {h})
        peekSomeCChild p = do
          ty <- peek @StructureType (castPtr @(SomeChild {h}) @{pretty structType} p)
          case ty of
      {indent 6 (vsep cases)}
            c -> throwIO $
              IOError
                Nothing
                InvalidArgument
                "peekSomeCChild"
                ("Illegal struct inheritance of {t} with " <> show c)
                Nothing
                Nothing
    |]

getStructTypeTag
  :: (HasSpecInfo r, HasRenderParams r, HasErr r) => Struct -> Sem r CName
getStructTypeTag Struct {..} = do
  RenderParams {..} <- input
  estt              <- maybe (throw "No extensible structure type tag type")
                             pure
                             extensibleStructTypeType

  case sMembers V.!? 0 of
    Just (StructMember smName (TypeName estt') vals _ _ _)
      | Just smName == extensibleStructTypeMemberName, estt' == estt -> case
          vals
        of
          V.Singleton typeEnum -> pure (CName typeEnum)
          _                    -> throw "Multiple values for sType"
    _ -> throw $ "Unable to find sType member in " <> show sName

renderInheritableInstance
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => MarshaledStruct AStruct
  -> Sem r ()
renderInheritableInstance ms@MarshaledStruct {..} =
  for_ (sInherits msStruct) $ \parentName -> do
    RenderParams {..} <- input
    parent <- maybe (throw "couldn't find parent") pure =<< getStruct parentName
    let hasExtensibleParent = not (null (sExtendedBy parent))
        omitDots            = length (sMembers parent) <= 2
    className <- inheritClassName parentName
    let t  = mkTyName msName
        c  = mkConName msName msName
        p  = mkTyName parentName
        pc = mkConName parentName parentName
    tellImportWithAll className
    tellImportWithAll p
    typeEnumValue <- mkPatternName <$> getStructTypeEnumValue ms
    tellImport typeEnumValue
    if
      | hasExtensibleParent -> tellDoc [qqi|
        instance {className} {t} where
          to{p} {c}\{..} = {pc}\{type' = {typeEnumValue}, next = (), ..}
      |]
      | omitDots -> tellDoc [qqi|
        instance {className} {t} where
          to{p} {c}\{} = {pc}\{type' = {typeEnumValue}}
      |]
      | otherwise -> tellDoc [qqi|
        instance {className} {t} where
          to{p} {c}\{..} = {pc}\{type' = {typeEnumValue}, ..}
      |]

inheritClassName :: (HasErr r, HasRenderParams r) => CName -> Sem r HName
inheritClassName parent = do
  RenderParams {..} <- input
  nameBase <- maybe (throw "Name didn't end in BaseHeader")
                    (pure . unName . mkTyName . CName)
                    (underVendor (T.dropSuffix "BaseHeader") (unCName parent))
  pure $ TyConName ("Is" <> nameBase)

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
     , HasRenderState r
     )
  => MarshaledStruct AStruct
  -> Sem r ()
renderStoreInstances ms@MarshaledStruct {..} = do
  RenderParams {..} <- input

  tellBoot $ tellDoc . vsep =<< sequenceV
    [ toCStructInstanceStub tellSourceImport msStruct
    , showInstanceStub tellSourceImport msStruct
    ]

  pokes <- renderPokes (fmap Just . recordWildCardsMemberVal msName)
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
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasRenderState r)
  => MarshaledStruct AStruct
  -> Sem r ()
storableInstance MarshaledStruct {..} = do
  RenderParams {..} <- input
  let n = mkTyName msName
  -- Some member names clash with storable members "alignment" for instance
  tellImport ''Storable
  tellQualImportWithAll ''Storable
  declareStorable n
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
     , HasRenderState r
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
peekCStructBody MarshaledStruct {..} = context "peekCStruct" $ do
  RenderParams {..} <- input
  let con = mkConName msName msName
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
      fmap (V.mapMaybe id) . forV msMembers $ \MarshaledStructMember {..} -> do

        -- Safe to unBitfield here as we're making it into a pointer type
        hPtrTy <- (ConT ''Ptr :@) <$> cToHsTypeWithHoles
          DoPreserve
          (unBitfield (smType msmStructMember))

        fmap (isElided msmScheme, ) <$> do

          addr <- stmt (Just hPtrTy) Nothing $ do
            tDoc <- renderType hPtrTy
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
     , HasRenderState r
     )
  => MarshaledStruct AStruct
  -> Sem r (Doc ())
pokeZeroCStructDecl ms@MarshaledStruct {..}
  | Just a <- bespokeZeroCStruct msName = a
  | otherwise = context "ZeroCStruct" $ do
    let replaceWithZeroChainPoke m = case msmScheme m of
          Custom s@(CustomScheme "Chain" _ _ _ _ _) -> m
            { msmScheme = Custom s
                            { csDirectPoke = APoke $ const zeroNextPointer
                            }
            }
          _ -> m
        ms' = ms { msMembers = replaceWithZeroChainPoke <$> msMembers }
    pokeDoc <-
      renderPokes zeroMemberVal (IOAction $ pretty contVar) ms' >>= \case
        ContTStmts d -> do
          tellImport 'evalContT
          pure $ "evalContT $" <+> d
        IOStmts d -> pure d

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
zeroInstanceDecl MarshaledStruct {..}
  | Just a <- bespokeZeroInstances msName = a
  | otherwise = do
    RenderParams {..} <- input
    let n    = mkTyName msName
        con  = mkConName msName msName
        head = if hasChildren msStruct
          then " " <> pretty structChainVar <> " ~ '[] =>"
          else ""
        tDoc = if hasChildren msStruct
          then parens (pretty n <+> pretty structChainVar)
          else pretty n
    zeroMembers <- catMaybes . toList <$> forV msMembers
                                               (zeroScheme . msmScheme)
    tellImportWithAll (TyConName "Zero")
    tellDoc
      $   "instance"
      <>  head
      <+> "Zero"
      <+> tDoc
      <+> "where"
      <>  line
      <>  indent
            2
            (vsep
              [ "zero ="
                  <+> align (pretty con <> line <> indent 2 (vsep zeroMembers))
              ]
            )

renderPokes
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSiblingInfo StructMember r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     , HasRenderState r
     )
  => (MarshaledStructMember -> Sem r (Maybe (Doc ())))
  -- ^ A predicate for which members we should poke
  -> Value (Doc ())
  -- ^ The value to return at the end
  -> MarshaledStruct AStruct
  -> Sem r (RenderedStmts (Doc ()))
renderPokes memberDoc end MarshaledStruct {..} = context "render pokes" $ do
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
                (Just
                  ("p" <> T.upperCaseFirst (unCName $ smName msmStructMember))
                )
              . fmap (Pure InlineOnce . AddrDoc)
              $ do
                  -- Safe to unBitfield here as we're making it into a pointer
                  pTyDoc <- renderType . (ConT ''Ptr :@) =<< cToHsTypeWithHoles
                    DoPreserve
                    (unBitfield (smType msmStructMember))
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
  ty <- schemeTypeNegative msmScheme
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
  :: HasRenderParams r => CName -> MarshaledStructMember -> Sem r (Doc ())
recordWildCardsMemberVal structName MarshaledStructMember {..} = do
  RenderParams {..} <- input
  let m = mkMemberName structName (smName msmStructMember)
  pure $ pretty m

zeroMemberVal
  :: (HasRenderElem r, HasRenderParams r, HasErr r)
  => MarshaledStructMember
  -> Sem r (Maybe (Doc ()))
  -- ^ Returns nothing if this is just 0 bytes and doesn't need poking
zeroMemberVal MarshaledStructMember {..} = case msmScheme of
  ElidedUnivalued _ ->
    pure $ Just "error \"This should never appear in the generated source\""
  s | True V.:<| _ <- smIsOptional msmStructMember, not (isCustom s) ->
    pure Nothing
  Custom CustomScheme { csZeroIsZero = True } -> pure Nothing
  Length{} -> pure Nothing
  Vector{} -> pure Nothing
  s        -> zeroScheme s

isCustom :: MarshalScheme a -> Bool
isCustom = \case
  Custom       _ -> True
  ElidedCustom _ -> True
  _              -> False

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
      tellSourceImport (TyConName "Extendss")
      pure
        $   " "
        <>  tupled
              [ "Extendss" <+> pretty n <+> pretty structChainVar
              , "PeekChain" <+> pretty structChainVar
              ]
        <+> "=>"
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
      tellSourceImport (TyConName "Extendss")
      pure
        $   " "
        <>  tupled
              [ "Extendss" <+> pretty n <+> pretty structChainVar
              , "PokeChain" <+> pretty structChainVar
              ]
        <+> "=>"
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

isSimpleStruct :: HasSpecInfo r => MarshaledStruct a -> Sem r Bool
isSimpleStruct = allM (isSimple . msmScheme) . msMembers

hasChildren :: Struct -> Bool
hasChildren = not . V.null . sExtendedBy

underVendor :: Functor f => (Text -> f Text) -> Text -> f Text
underVendor f t =
  let vendor = T.takeWhileEnd isUpper t
      inside = T.dropEnd (T.length vendor) t
  in  (<> vendor) <$> f inside

getStructTypeEnumValue
  :: (HasErr r, HasRenderParams r) => MarshaledStruct a -> Sem r CName
getStructTypeEnumValue MarshaledStruct {..} = do
  RenderParams {..} <- input
  let
    structTypes =
      [ v
      | MarshaledStructMember { msmStructMember = StructMember { smName }, msmScheme = ElidedUnivalued v } <-
        toList msMembers
      , Just smName == extensibleStructTypeMemberName
      ]
  case structTypes of
    []  -> throw "Unable to find type enum of extensible struct"
    [v] -> pure (CName v)
    vs  -> throw $ "Found multiple struct type enums " <> show vs
