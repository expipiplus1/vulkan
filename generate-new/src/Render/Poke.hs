{-# language DeriveFunctor #-}
{-# language TypeApplications #-}
module Render.Poke
  where

import           Relude                  hiding ( Type
                                                , ask
                                                , asks
                                                , last
                                                , init
                                                , Const
                                                , Reader
                                                )
import           Data.List                      ( last
                                                , init
                                                )
import           Data.Text.Prettyprint.Doc
import qualified Data.List.NonEmpty            as NE
import           Polysemy
import           Polysemy.NonDet         hiding ( Empty )
import           Polysemy.Reader
import           Polysemy.Fail
import           Data.Vector.Extra              ( Vector
                                                , pattern Empty
                                                )
import qualified Data.Text.Extra               as T

import qualified Data.Vector                   as V
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Utils
import           Foreign.Marshal.Array
import           Foreign.Marshal.Alloc
import           Control.Monad.Trans.Cont       ( ContT
                                                , runContT
                                                )
import qualified Data.ByteString               as BS
import           GHC.IO.Exception
import           Control.Exception              ( throwIO )

import           Error
import           Marshal.Scheme
import           Render.Element
import           Render.SpecInfo
import           Render.Type
import           CType                         as C
import           Marshal.Marshalable
import           Haskell
import           Spec.Types


type P s a
  =  forall r
   . ( HasErr r
     , HasRenderElem r
     , HasRenderParams r
     , HasSpecInfo r
     , HasSiblingInfo s r
       -- Needed to find the names of vectors to get their lengths
       -- TODO: This should return a Maybe (Sem q ValueDoc) where q HasRenderElem
     )
  => Sem r a

data Poke a
  = Pure Inline a -- Not a poke, should only appear on the left of a ChainedPoke
  | IOPoke a
  | ContTPoke a
  | ChainedPoke ValueDoc (Poke a) [Poke (ValueDoc -> a)]
    -- The first poke produces a result, and the name of that is fed into the
    -- second poke, looks familiar...
  deriving (Functor)

data Inline = DoInline | DoNotInline

newtype AddrDoc = AddrDoc (Doc ())
  deriving Show
newtype ValueDoc = ValueDoc { unValueDoc :: Doc () }
  deriving Show

newtype Unassigned a = Unassigned (Type -> AddrDoc -> ValueDoc -> P a (Doc ()))
newtype Direct a = Direct (Type -> ValueDoc -> P a (Doc ()))
newtype Assigned a = Assigned (P a (Doc ()))
type UnassignedPoke a = Poke (Unassigned a)
type DirectPoke a = Poke (Direct a)
type AssignedPoke a = Poke (Assigned a)

data SiblingInfo a = SiblingInfo
  { siReferrer :: Doc ()
    -- ^ How to refer to this sibling in code
  , siScheme :: MarshalScheme a
    -- ^ What type is this sibling
  }

type HasSiblingInfo a r = Member (Reader (Text -> Maybe (SiblingInfo a))) r

getSiblingInfo
  :: forall a r. (HasErr r, HasSiblingInfo a r) => Text -> Sem r (SiblingInfo a)
getSiblingInfo n = note ("Unable to find info for: " <> n) =<< asks ($ n)

containsContTPoke :: Poke a -> Bool
containsContTPoke = \case
  Pure _ _           -> False
  IOPoke    _        -> False
  ContTPoke _        -> True
  ChainedPoke _ p ps -> containsContTPoke p || any containsContTPoke ps

-- TODO: Make this into getPokeDirect which returns a poke whose value is the
-- pointed to element. This can then be poked into a value (via storable) if
-- necessary.
getPoke
  :: ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => a
  -> MarshalScheme a
  -> Sem r (UnassignedPoke a)
getPoke a = getPoke' (type' a) (name a)

getPokeDirect
  :: ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => a
  -> MarshalScheme a
  -> Sem r (DirectPoke a)
getPokeDirect a s =
  note ("Unable to get direct poke for " <> show a <> " from " <> show s)
    =<< getPokeDirect' (type' a) (name a) s

getPoke'
  :: ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => CType
  -- ^ The type we are translating to, i.e. the C-side type
  -> Text
  -- ^ The name of the thing being poked
  -> MarshalScheme a
  -> Sem r (UnassignedPoke a)
getPoke' toType valueName s = do
  getPokeDirect' toType valueName s >>= \case
    Just directPoke -> pure $ ChainedPoke
      (ValueDoc (pretty ("p" <> valueName)))
      (directToUnassigned <$> directPoke)
      [intermediateStorablePoke]
    Nothing -> case s of
      Unit            -> throw "Getting poke for a unit member"
      Normal from     -> normalPokeIndirect toType from
      ByteString      -> byteStringPokeIndirect toType
      Vector fromElem -> case toType of
        Array NonConst (SymbolicArraySize n) toElem ->
          vectorToFixedArray valueName n fromElem toElem
        t -> throw $ "Unhandled Vector conversion to: " <> show t
      Tupled n fromElem -> case toType of
        Array _ (NumericArraySize n') toElem | n == n' ->
          tupleToFixedArray valueName n fromElem toElem
        t ->
          throw $ "Unhandled Tupled " <> show n <> " conversion to: " <> show t
      scheme -> throw ("Unhandled scheme: " <> show scheme)

getPokeDirect'
  :: ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => CType
  -- ^ The type we are translating to, i.e. the C-side type
  -> Text
  -- ^ The name of the thing being poked
  -> MarshalScheme a
  -> Sem r (Maybe (DirectPoke a))
getPokeDirect' toType valueName s = do
  RenderParams {..} <- ask
  case s of
    Unit                  -> throw "Getting poke for a unit member"
    Normal          from  -> normalPoke toType from
    ElidedUnivalued value -> do
      let vName = mkPatternName value
      pure . Just $ constPoke DoInline $ do
        tellImport (ConName vName)
        pure (pretty vName)
    ElidedLength os rs -> Just <$> elidedLengthPoke os rs
    ByteString         -> byteStringPoke toType
    Maybe ByteString   -> Just <$> maybeByteStringPoke toType
    Maybe _            -> Just <$> maybePoke toType
    VoidPtr            -> pure . Just $ idPoke
    Vector fromElem    -> case toType of
      Array NonConst (SymbolicArraySize _) _ -> pure $ Nothing
      Ptr Const toElem -> Just <$> vectorToPointer valueName fromElem toElem
      t                -> throw $ "Unhandled Vector conversion to: " <> show t
    Tupled n fromElem -> case toType of
      Ptr _ toElem -> do
        let a = ContTPoke $ Direct $ \_ _ -> do
              tellImport 'allocaArray
              tyDoc <- renderTypeHighPrec =<< cToHsType DoPreserve toElem
              pure $ "ContT $ allocaArray @" <> tyDoc <+> viaShow n
        u <- tupleToFixedArray valueName n fromElem toElem
        pure . Just $ ChainedPoke
          (ValueDoc ("p" <> pretty (T.upperCaseFirst valueName)))
          a
          [ u <&> \(Unassigned f) (ValueDoc p) ->
            Direct $ \ty v -> f ty (AddrDoc p) v
          , Pure DoInline $ \(ValueDoc v) -> Direct $ \_ _ -> pure v
          ]
      _ -> pure Nothing
    EitherWord32 fromElem -> case toType of
      Ptr Const toElem -> Just <$> eitherLengthVec valueName fromElem toElem
      t -> throw $ "Unhandled EitherWord32 conversion to: " <> show t
    Preserve fromType | fromType == toType -> pure . Just $ idPoke
    _ -> pure Nothing

-- TODO, find Pures with the same RHS and combine them
resolveChainedPoke
  :: forall a r
   . (HasErr r, HasRenderParams r, HasRenderElem r)
  => AssignedPoke a
  -> Sem r [AssignedPoke a]
resolveChainedPoke = \case
  ChainedPoke _            initialPoke []             -> pure [initialPoke]
  ChainedPoke intermediate initialPoke dependentPokes -> do
    initialPokeDocs <- resolveChainedPoke initialPoke
    let priorPokes = init initialPokeDocs
        resultPoke = last initialPokeDocs
    let
      (applyResult, resultAssignment) = case resultPoke of
        Pure DoInline (Assigned d) ->
          ( resolveChainedPoke . fmap
            (\case
              f -> Assigned $ do
                i <- d
                case f (ValueDoc (parens i)) of
                  Assigned x -> x
            )
          , Nothing
          )
        r ->
          ( resolveChainedPoke . fmap ($ intermediate)
          , Just
            $ let op = case r of
                    Pure _ _ -> "=" :: Doc ()
                    _        -> "<-"
              in
                (\(Assigned d) ->
                    Assigned $ ((unValueDoc intermediate <+> op) <+>) <$> d
                  )
                  <$> r
          )

    dependentPokes' <- traverseV applyResult dependentPokes

    pure $ priorPokes <> maybeToList resultAssignment <> concat dependentPokes'

  p -> pure [p]

-- | Renders to a function of type @IO a -> IO a@
renderPokesWithRunContT
  :: Vector (AssignedPoke a)
  -> P a (Doc ())
renderPokesWithRunContT pokes = do
  renderedContT <- renderPokesContT pokes
  tellImport 'runContT
  pure $ "(. const) . runContT $" <+> renderedContT

renderPokesInIO :: Vector (AssignedPoke a) -> P a (Doc ())
renderPokesInIO pokes = do
  -- TODO: do this with types
  when (any containsContTPoke pokes)
    $ throw "Trying to render pokes containing ContT in IO"
  unchained <- concat . toList <$> traverse resolveChainedPoke pokes
  let pureDocs = [ d | Pure _ (Assigned d) <- toList unchained ]
      ioDocs   = [ d | IOPoke (Assigned d) <- toList unchained ]
  pureStmts <- if null pureDocs
    then pure []
    else pure . letBlock <$> sequenceV pureDocs
  ioStmts <- sequenceV ioDocs
  pure $ doBlock (pureStmts <> ioStmts)

renderPokesContT :: Vector (AssignedPoke a) -> P a (Doc ())
renderPokesContT = renderPokesContTRet Nothing

renderPokesContTRet :: Maybe (Doc ()) -> Vector (AssignedPoke a) -> P a (Doc ())
renderPokesContTRet ret pokes = do
  unchained <- concat . toList <$> traverse resolveChainedPoke pokes
  let pureDocs  = [ d | Pure _ (Assigned d) <- toList unchained ]
      contTDocs = [ d | ContTPoke (Assigned d) <- toList unchained ]
      ioDocs    = [ d | IOPoke (Assigned d) <- toList unchained ]
  pureStmts <- if null pureDocs
    then pure []
    else pure . letBlock <$> sequenceV pureDocs
  contTStmts <- sequenceV contTDocs
  ioStmt     <- if null ioDocs
    then pure Nothing
    else Just <$> do
      ioStmts <- sequenceV ioDocs
      tellImport 'liftIO
      pure ["liftIO $" <+> doBlock (ioStmts <> toList ret)]
  pure $ case ioStmt of
    Nothing ->
      doBlock (pureStmts <> contTStmts <> toList ret)
    Just ioStmt' ->
      doBlock (pureStmts <> contTStmts <> ioStmt')

letBlock :: [Doc ()] -> Doc ()
letBlock  = \case
  []   -> mempty
  lets -> "let" <+> align (vsep lets)

doBlock :: [Doc ()] -> Doc ()
doBlock = \case
  []    -> "pure ()"
  [s]   -> s
  stmts -> "do" <> line <> indent 2 (vsep stmts)

----------------------------------------------------------------
-- Pokes
----------------------------------------------------------------

{- HLINT ignore -}

normalPoke
  :: (HasErr r, HasSpecInfo r)
  => CType
  -> CType
  -> Sem r (Maybe (DirectPoke a))
normalPoke to from = runNonDet (asum [same, pointed, pointerStruct])
 where
  same = failToNonDet $ do
    guard (from == to)
    case to of
      TypeName n -> do
        guard . isNothing =<< getStruct n
        guard . isNothing =<< getUnion n
      _ -> pure ()
    pure $ idPoke

  pointed = failToNonDet $ do
    Ptr Const to' <- pure to
    guard (from == to')
    case to' of
      TypeName n -> do
        guard . isNothing =<< getStruct n
        guard . isNothing =<< getUnion n
      _ -> pure ()
    pure $ contTPokeDirect $ \ty (ValueDoc value) -> do
      tellImport 'with
      tDoc <- renderTypeHighPrec ty
      pure $ "with @" <> tDoc <+> value

  pointerStruct = failToNonDet $ do
    Ptr Const (TypeName toName) <- pure to
    TypeName fromName           <- pure from
    guard (toName == fromName)
    Just _ <- liftA2 (<|>)
                     (void <$> getStruct toName)
                     (void <$> getUnion toName)
    pure $ contTPokeDirect $ \_  (ValueDoc value) -> do
      tellImportWithAll (TyConName "ToCStruct")
      pure $ "withCStruct" <+> value

normalPokeIndirect
  :: (HasErr r, HasSpecInfo r)
  => CType
  -> CType
  -> Sem r (UnassignedPoke a)
normalPokeIndirect to from = runNonDet inlineStruct >>= \case
  Nothing -> throw ("Unhandled " <> show from <> " conversion to: " <> show to)
  Just ps -> pure ps
 where
  inlineStruct = failToNonDet $ do
    TypeName n <- pure to
    Just     _ <- liftA2 (<|>) (void <$> getStruct n) (void <$> getUnion n)
    pure $ contTPoke $ \_ (AddrDoc addr) (ValueDoc value) -> do
      tellImportWithAll (TyConName "ToCStruct")
      pure $ "pokeCStruct" <+> addr <+> value <+> ". ($ ())"

elidedLengthPoke
  :: forall a r
   . (HasErr r, HasSpecInfo r, Marshalable a, HasSiblingInfo a r, Show a)
  => Vector a
  -> Vector a
  -> Sem r (DirectPoke a)
elidedLengthPoke Empty Empty = throw "No vectors to get length from"
elidedLengthPoke os    rs    = do
  let
    inline = if V.length os + V.length rs == 1 then DoInline else DoNotInline
    getLength v = do
      SiblingInfo {..} <- getSiblingInfo @a (name v)
      (name v, ) <$> case siScheme of
        Vector _ -> pure $ constPoke inline $ do
          tellQualImport 'V.length
          pure $ "fromIntegral . Data.Vector.length $" <+> siReferrer
        EitherWord32 _ -> pure $ constPoke inline $ do
          tellImport 'either
          tellQualImport 'V.length
          pure $ "either id (fromIntegral . Data.Vector.length)" <+> siReferrer
        Maybe (Vector _) -> pure $ constPoke inline $ do
          tellQualImport 'V.length
          tellQualImport 'maybe
          pure $ "maybe 0 (fromIntegral . Data.Vector.length)" <+> siReferrer
        s ->
          throw
            ("Don't know how to get length for vector with scheme: " <> show s)
  optionalLengths <- traverseV getLength os
  requiredLengths <- traverseV getLength rs
  let
    allLengths                     = requiredLengths <> optionalLengths
    (firstVecName, firstVecLength) = V.head allLengths
    assertSamePoke
      :: Bool -> (Text, Poke (Direct a)) -> Poke (ValueDoc -> Direct a)
    assertSamePoke isOptVector (n, p) = ChainedPoke
      (ValueDoc (pretty (n <> "Length")))
      (const <$> p)
      [ IOPoke $ \(ValueDoc otherVecLength) (ValueDoc firstVecLength') ->
          Direct $ \_ _ -> do
            tellImport 'throwIO
            tellImportWithAll ''IOException
            tellImportWithAll ''IOErrorType
            tellImport 'unless
            orNull <- if isOptVector
              then do
                tellQualImport 'V.null
                pure $ " ||" <+> otherVecLength <+> "== 0"
              else pure mempty
            let err :: Text
                err =
                  n <> " and " <> firstVecName <> " must have the same length"
            pure
              $   "unless"
              <+> parens
                    (otherVecLength <+> "==" <+> firstVecLength' <> orNull)
              <+> "$"
              <>  line
              <>  indent
                    2
                    (   "throwIO $ IOError Nothing InvalidArgument"
                    <+> viaShow ("" :: Text) -- TODO: function name
                    <+> viaShow err
                    <+> "Nothing Nothing"
                    )
      ]
  pure $ ChainedPoke
    (ValueDoc (pretty (firstVecName <> "Length")))
    firstVecLength
    (  V.toList
    .  V.tail
    $  (assertSamePoke False <$> requiredLengths)
    <> (assertSamePoke True <$> optionalLengths)
    <> V.singleton idPokeIntermediate
    )

byteStringPoke
  :: (HasErr r, HasSpecInfo r) => CType -> Sem r (Maybe (DirectPoke a))
byteStringPoke = \case
  Ptr Const Char -> pure . Just $ contTPokeDirect $ \_ (ValueDoc value) -> do
    tellImport 'BS.useAsCString
    pure $ "useAsCString" <+> value

  _ -> pure Nothing

byteStringPokeIndirect
  :: (HasErr r, HasSpecInfo r) => CType -> Sem r (UnassignedPoke a)
byteStringPokeIndirect = \case
  Array _ (SymbolicArraySize n) toElem ->
    pure $ ioPoke $ \_ (AddrDoc addr) (ValueDoc value) -> do
      f <- case toElem of
        Char -> do
          let fn = "pokeFixedLengthNullTerminatedByteString"
          tellImport (TermName fn)
          pure fn
        _ -> do
          let fn = "pokeFixedLengthByteString"
          tellImport (TermName fn)
          pure fn
      tellImport (ConName n)
      pure $ pretty f <+> pretty n <+> addr <+> value

  t -> throw $ "Unhandled ByteString conversion to: " <> show t

maybeByteStringPoke
  :: (HasErr r, HasSpecInfo r) => CType -> Sem r (DirectPoke a)
maybeByteStringPoke = \case
  Ptr Const Char ->
    pure
      $ (contTPokeDirect $ \_ (ValueDoc value) -> do
          tellImport 'BS.useAsCString
          tellImport 'maybeWith
          pure $ "maybeWith useAsCString" <+> value
        )
  t -> throw $ "Unhandled Maybe Bytestring conversion to: " <> show t

maybePoke :: (HasErr r, HasSpecInfo r) => CType -> Sem r (DirectPoke a)
maybePoke = \case
  Ptr Const t@(TypeName n) -> getStruct n >>= \case
    Nothing -> throw $ "Unhandled Maybe conversion to: " <> show t
    Just _ ->
      pure
        $ (contTPokeDirect $ \_ (ValueDoc value) -> do
            tellImportWithAll (TyConName "ToCStruct")
            tellImport 'maybeWith
            pure $ "maybeWith withCStruct" <+> value
          )
  _ -> pure $ ChainedPoke
    (ValueDoc "m")
    (Pure DoInline $ Direct $ \_ (ValueDoc v) -> do
      tellImport 'fromMaybe
      tellImportWithAll (TyConName "Zero")
      pure . parens $ "fromMaybe zero" <+> v
    )
    [idPokeIntermediate]


elemPokeAndSize
  :: forall r a
   . ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => Text
  -> MarshalScheme a
  -> CType
  -> Sem
       r
       (Maybe Int, Unassigned a -> UnassignedPoke a, Unassigned a)
elemPokeAndSize vecName fromElem toElem = do
  sizeMaybe <- case toElem of
    TypeName n -> do
      s <- fmap sSize <$> getStruct n
      u <- fmap sSize <$> getUnion n
      pure (s <|> u)
    _ -> pure Nothing
  chainedElem                 <- getPoke' toElem (vecName <> "Elem") fromElem

  (pokeConstructor, elemPoke) <- case chainedElem of
    Pure _ _           -> throw "Pure poke as vector element?"
    IOPoke    elemPoke -> pure (IOPoke, elemPoke)
    ContTPoke elemPoke -> pure (ContTPoke, elemPoke)
    p@ChainedPoke{} ->
      let inContT  = containsContTPoke $ p
          elemPoke = Unassigned $ \ty addr value -> do
            let assigned =
                  (\(Unassigned f) -> Assigned @a $ f ty addr value) <$> p
            unchained <- resolveChainedPoke assigned
            (bool renderPokesInIO renderPokesContT inContT)
              (V.fromList . toList $ unchained)
      in  pure (bool IOPoke ContTPoke inContT, elemPoke)

  pure (sizeMaybe, pokeConstructor, elemPoke)

vectorToArray
  :: forall r a
   . ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => Text
  -> MarshalScheme a
  -> CType
  -> Sem r (UnassignedPoke a)
vectorToArray vecName fromElem toElem = do
  (sizeMaybe, pokeConstructor, Unassigned elemPoke) <- elemPokeAndSize
    vecName
    fromElem
    toElem

  pure $ pokeConstructor $ Unassigned $ \_ (AddrDoc addr) (ValueDoc value) -> do
    let elemValue = ValueDoc "v"
    elemAddr <- case sizeMaybe of
      Nothing -> do
        tellImport 'advancePtr
        pure $ AddrDoc (parens (addr <+> "`advancePtr` i"))
      Just elemSize -> do
        tellImport 'plusPtr
        pure
          $ AddrDoc
              (parens
                (addr <+> "`plusPtr`" <+> parens (viaShow elemSize <+> "* i"))
              )
    tellQualImport 'V.imapM_
    toElemHType <- cToHsType DoPreserve toElem
    elemPokeDoc <- elemPoke toElemHType elemAddr elemValue
    pure
      $   "Data.Vector.imapM_"
      <+> parens ("\\i v ->" <+> elemPokeDoc)
      <+> value

vectorToFixedArray
  :: forall r a
   . ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => Text
  -> Text
  -> MarshalScheme a
  -> CType
  -> Sem r (UnassignedPoke a)
vectorToFixedArray vecName arrayLength fromElem toElem = do
  -- TODO: Error on truncation! Easy to implement with ChainedPoke
  toArray <- vectorToArray vecName fromElem toElem
  pure $ mapPokeValue
    (\(ValueDoc v) -> do
      tellImport (ConName arrayLength)
      tellQualImport 'V.take
      pure . ValueDoc $ parens ("Data.Vector.take" <+> pretty arrayLength <+> v)
    )
    toArray

tupleToFixedArray
  :: forall r a
   . ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => Text
  -> Word
  -> MarshalScheme a
  -> CType
  -> Sem r (UnassignedPoke a)
tupleToFixedArray vecName arrayLength fromElem toElem = do
  (sizeMaybe, pokeConstructor, elemPoke) <- elemPokeAndSize vecName
                                                            fromElem
                                                            toElem

  toElemH    <- cToHsType DoPreserve toElem
  structSize <- case toElem of
    TypeName n -> do
      s <- fmap sSize <$> getStruct n
      u <- fmap sSize <$> getUnion n
      pure (s <|> u)
    _ -> pure Nothing

  pure $ pokeConstructor $ Unassigned $ \_ (AddrDoc addr) (ValueDoc value) -> do
    let
      precalaulated = do
        structSize'     <- structSize
        addrWord :: Int <- readMaybe (show addr)
        pure $ pure (\n -> AddrDoc (show (addrWord + n * structSize')))
      dynamic = case structSize of
        Just elemSize -> do
          tellImport 'plusPtr
          pure $ \n -> AddrDoc
            (parens
              (addr <+> "`plusPtr`" <+> parens
                (viaShow elemSize <+> "*" <+> viaShow n)
              )
            )
        Nothing -> do
          tellImport 'advancePtr
          pure (\n -> AddrDoc (parens (addr <+> "`advancePtr`" <+> viaShow n)))
    getAddr <- fromMaybe dynamic precalaulated
    let elemName n = "e" <> viaShow n
        pokeElem n = pokeConstructor elemPoke <&> \(Unassigned f) ->
          Assigned @a $ f toElemH (getAddr n) (ValueDoc (elemName n))
    ps <- renderPokesContT (V.generate (fromIntegral arrayLength) pokeElem)
    pure $ "case" <+> value <+> "of" <> line <> indent
      2
      (tupled [ elemName n | n <- [0 .. arrayLength - 1] ] <+> "->" <+> ps)


vectorToPointer
  :: forall r a
   . ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => Text
  -> MarshalScheme a
  -> CType
  -> Sem r (DirectPoke a)
vectorToPointer vecName fromElem toElem = do
  (alloc, writeElems) <- vectorToPointerPokes vecName fromElem toElem
  pure $ ChainedPoke
    (ValueDoc $ "p" <> pretty vecName)
    alloc
    [writeElems, Pure DoInline $ \(ValueDoc v) -> Direct $ \_ _ -> pure v]

vectorToPointerPokes
  :: forall r a
   . ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => Text
  -> MarshalScheme a
  -> CType
  -> Sem r (DirectPoke a, Poke (ValueDoc -> Direct a))
  -- ^ (alloc, write elems)
vectorToPointerPokes vecName fromElem toElem = do
  toArray   <- vectorToArray vecName fromElem toElem
  sizeMaybe <- case toElem of
    TypeName n -> do
      s <- fmap (sSize &&& sAlignment) <$> getStruct n
      u <- fmap (sSize &&& sAlignment) <$> getUnion n
      pure $ s <|> u
    _ -> pure Nothing
  pure
    ( contTPokeDirect $ \_ (ValueDoc value) -> do
      tellQualImport 'V.length
      elemTyDoc <- renderTypeHighPrec =<< cToHsType DoPreserve toElem
      let vecLengthDoc = "Data.Vector.length" <+> value
      case sizeMaybe of
        Nothing -> do
          -- Use a storable instance
          tellImport 'allocaArray
          pure $ "allocaArray @" <> elemTyDoc <+> parens vecLengthDoc
        Just (elemSize, elemAlignment) -> do
          -- Size things explicitly
          tellImport 'allocaBytesAligned
          pure
            $   "allocaBytesAligned @"
            <>  elemTyDoc
            <+> parens (viaShow elemSize <+> "*" <+> vecLengthDoc)
            <+> viaShow elemAlignment
    , (\(Unassigned p) (ValueDoc addr) ->
        Direct $ \ty value -> p ty (AddrDoc addr) value
      )
      <$> toArray
    )

eitherLengthVec
  :: forall r a
   . ( Show a
     , Marshalable a
     , HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => Text
  -> MarshalScheme a
  -> CType
  -> Sem r (DirectPoke a)
eitherLengthVec vecName fromElem toElem = do
  (alloc, writeElems) <- vectorToPointerPokes vecName fromElem toElem
  let
    allocAndWrite = ContTPoke $ Direct $ \ty (ValueDoc value) -> do
      tellImport 'nullPtr
      let
        vecVar                = "vec"
        ptrVar                = "pVec"
        allocAndWriteChained  = ChainedPoke (ValueDoc ptrVar) alloc [writeElems]
        allocAndWriteAssigned = allocAndWriteChained
          <&> \(Direct f) -> Assigned @a (f ty (ValueDoc vecVar))
      allocAndWriteDoc <- renderPokesContTRet
        (Just ("pure" <+> ptrVar))
        (V.singleton allocAndWriteAssigned)
      pure $ "case" <+> value <+> "of" <> line <> indent
        2
        (vsep
          [ "Left _ -> pure nullPtr"
          , "Right" <+> vecVar <+> "->" <+> allocAndWriteDoc
          ]
        )

  pure $ allocAndWrite

----------------------------------------------------------------
-- Shortcut pokes
----------------------------------------------------------------

contTPoke
  :: (Type -> AddrDoc -> ValueDoc -> P a (Doc ())) -> Poke (Unassigned a)
contTPoke t = ContTPoke
  (Unassigned $ \ty addr value -> do
    tellImportWithAll ''ContT
    ("ContT $" <+>) <$> t ty addr value
  )

contTPokeDirect
  :: (Type -> ValueDoc -> P a (Doc ())) -> Poke (Direct a)
contTPokeDirect t = ContTPoke
  (Direct $ \ty value -> do
    tellImportWithAll ''ContT
    ("ContT $" <+>) <$> t ty value
  )

ioPoke :: (Type -> AddrDoc -> ValueDoc -> P a (Doc ())) -> Poke (Unassigned a)
ioPoke t = IOPoke (Unassigned t)

intermediateAddrPoke :: Poke (ValueDoc -> Unassigned a)
intermediateAddrPoke =
  (\(Unassigned f :: Unassigned a) (ValueDoc intermediate) ->
      Unassigned @a $ \ty _ value -> f ty (AddrDoc intermediate) value
    )
    <$> storablePoke

intermediateStorablePoke :: Poke (ValueDoc -> Unassigned a)
intermediateStorablePoke =
  (\(Unassigned f :: Unassigned a) intermediate ->
      Unassigned @a $ \ty addr _ -> f ty addr intermediate
    )
    <$> storablePoke

mapPokeValue :: (ValueDoc -> P a ValueDoc) -> UnassignedPoke a -> UnassignedPoke a
mapPokeValue f =
  fmap (\(Unassigned p) -> Unassigned $ \ty addr value -> p ty addr =<< f value)

mapPokeAddr :: (AddrDoc -> P a AddrDoc) -> UnassignedPoke a -> UnassignedPoke a
mapPokeAddr f =
  fmap
  (\(Unassigned p) -> Unassigned $ \ty addr value -> do
    addr' <- f addr
    p ty addr' value
  )

constPoke :: Inline -> P a (Doc ()) -> DirectPoke a
constPoke inline c = Pure inline $ Direct $ \_ _ -> c

storablePoke :: UnassignedPoke a
storablePoke = ioPoke $ \ty (AddrDoc addr) (ValueDoc value) -> do
  tDoc <- renderTypeHighPrec ty
  tellImportWith ''Storable 'poke
  pure $ "poke @" <> tDoc <+> addr <+> value

directToUnassigned :: Direct a -> Unassigned a
directToUnassigned (Direct f) = Unassigned $ \ty _ v -> f ty v

-- | Might wrap types in their "idiomatic constructors" such as CSize for Word64
idPoke :: DirectPoke a
idPoke = Pure DoInline $ Direct $ \ty (ValueDoc value) -> do
  RenderParams {..} <- ask
  xDoc              <- case mkIdiomaticType ty of
    Nothing                       -> pure value
    Just (IdiomaticType _ from _) -> do
      fromDoc <- from
      pure $ parens (fromDoc <+> value)
  pure xDoc

idPokeIntermediate :: Poke (ValueDoc -> Direct a)
idPokeIntermediate =
  idPoke <&> \(Direct f :: Direct a) v -> Direct @a $ \ty _ -> f ty v
