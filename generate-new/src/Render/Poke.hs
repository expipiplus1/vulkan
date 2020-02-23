{-# language DeriveFunctor #-}
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
import           Text.Read (readMaybe)
import           Data.List                      ( last
                                                , init
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.NonDet         hiding ( Empty )
import           Polysemy.Reader
import           Polysemy.Fail
import           Data.Vector.Extra              ( Vector
                                                , pattern Empty
                                                , pattern Singleton
                                                )

import qualified Data.Vector                   as V
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Utils
import           Foreign.Marshal.Array
import           Foreign.Marshal.Alloc
import           Control.Monad.Trans.Cont       ( ContT
                                                , runContT
                                                )
import           Control.Monad.IO.Class         ( liftIO )
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
    -- second poke
  deriving (Functor)

data Inline = DoInline | DoNotInline

newtype AddrDoc = AddrDoc (Doc ())
newtype ValueDoc = ValueDoc { unValueDoc :: Doc () }

newtype Unassigned a = Unassigned (Type -> AddrDoc -> ValueDoc -> P a (Doc ()))
newtype Assigned a = Assigned (P a (Doc ()))
type UnassignedPoke a = Poke (Unassigned a)
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

isIOPoke :: Poke a -> Bool
isIOPoke = \case
  IOPoke _ -> True
  _ -> False

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
  RenderParams {..} <- ask
  case s of
    Unit                  -> throw "Getting poke for a unit member"
    Normal          from  -> normalPoke valueName toType from
    ElidedUnivalued value -> do
      let vName = mkPatternName value
      pure $ constantStorablePoke vName
    ElidedLength os rs -> elidedLengthPoke os rs
    ByteString         -> byteStringPoke valueName toType
    Maybe ByteString   -> maybeByteStringPoke valueName toType
    Maybe _            -> maybePoke valueName toType
    VoidPtr            -> pure storablePoke
    Vector fromElem    -> case toType of
      Array NonConst (SymbolicArraySize n) toElem ->
        vectorToFixedArray valueName n fromElem toElem
      Ptr Const toElem -> vectorToPointer valueName fromElem toElem
      t                -> throw $ "Unhandled Vector conversion to: " <> show t
    EitherWord32 fromElem -> case toType of
      Ptr Const toElem -> eitherLengthVec valueName fromElem toElem
      t -> throw $ "Unhandled EitherWord32 conversion to: " <> show t
    Tupled n fromElem -> case toType of
      Array _ (NumericArraySize n') toElem | n == n' ->
        tupleToFixedArray valueName n fromElem toElem
      t ->
        throw $ "Unhandled Tupled " <> show n <> " conversion to: " <> show t
    Preserve fromType | fromType == toType -> pure storablePoke
    scheme -> throw ("Unhandled scheme: " <> show scheme)

-- TODO, find Pures with the same RHS and combine them
resolveChainedPoke
  :: forall a r
   . (HasErr r, HasRenderParams r, HasRenderElem r)
  => AssignedPoke a
  -> Sem r [AssignedPoke a]
resolveChainedPoke = \case
  -- Inline the value if it only has one dependent
  ChainedPoke _ (Pure DoInline (Assigned d)) dependentPokes ->
    fmap concat
      . traverse resolveChainedPoke
      $ (   fmap
            (\case
              f -> Assigned $ do
                intermediate <- d
                case f (ValueDoc (parens intermediate)) of
                  Assigned x -> x
            )
        <$> dependentPokes
        )
  ChainedPoke intermediate initialPoke dependentPokes -> do
    initialPokeDocs   <- resolveChainedPoke initialPoke
    dependentPokeDocs <- traverseV
      (resolveChainedPoke . fmap ($ intermediate))
      dependentPokes
    pure
      $  init initialPokeDocs
      <> [ let op = case last initialPokeDocs of
                 Pure _ _ -> "=" :: Doc ()
                 _        -> "<-"
           in
             (\(Assigned d) ->
                 Assigned $ ((unValueDoc intermediate <+> op) <+>) <$> d
               )
               <$> last initialPokeDocs
         ]
      <> concat dependentPokeDocs
  p -> pure [p]

renderPokesInIO
  :: Vector (AssignedPoke a)
  -> P a (Doc ())
renderPokesInIO pokes = do
  renderedContT <- renderPokesContT pokes
  tellImport 'runContT
  pure $ "(. const) . runContT $" <+> renderedContT

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

normalPoke
  :: (HasErr r, HasSpecInfo r)
  => Text
  -> CType
  -> CType
  -> Sem r (UnassignedPoke a)
normalPoke valueName to from =
  runNonDet (asum [same, inlineStruct, pointerStruct]) >>= \case
    Nothing ->
      throw ("Unhandled " <> show from <> " conversion to: " <> show to)
    Just ps -> pure ps
 where
  same = failToNonDet $ do
    guard (from == to)
    case to of
      TypeName n -> do
        guard . isNothing =<< getStruct n
        guard . isNothing =<< getUnion n
      _ -> pure ()
    pure storablePoke

  inlineStruct = failToNonDet $ do
    TypeName n <- pure to
    Just     _ <- liftA2 (<|>) (void <$> getStruct n) (void <$> getUnion n)
    pure $ contTPoke $ \_ (AddrDoc addr) (ValueDoc value) -> do
      tellImportWithAll (TyConName "ToCStruct")
      pure $ "pokeCStruct" <+> addr <+> value <+> ". ($ ())"

  pointerStruct = failToNonDet $ do
    Ptr Const (TypeName toName) <- pure to
    TypeName fromName           <- pure from
    guard (toName == fromName)
    Just _ <- getStruct toName
    pure $ ChainedPoke
      (ValueDoc ("p" <> pretty valueName))
      (contTPoke $ \_ _ (ValueDoc value) -> do
        tellImportWithAll (TyConName "ToCStruct")
        pure $ "withCStruct" <+> value
      )
      [intermediateStorablePoke]

elidedLengthPoke
  :: forall a r
   . (HasErr r, HasSpecInfo r, Marshalable a, HasSiblingInfo a r, Show a)
  => Vector a
  -> Vector a
  -> Sem r (UnassignedPoke a)
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
      :: Bool -> (Text, Poke (Unassigned a)) -> Poke (ValueDoc -> Unassigned a)
    assertSamePoke isOptVector (n, p) = ChainedPoke
      (ValueDoc (pretty (n <> "Length")))
      (const <$> p)
      [ IOPoke $ \(ValueDoc otherVecLength) (ValueDoc firstVecLength') ->
          Unassigned $ \_ _ _ -> do
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
    <> Singleton intermediateStorablePoke
    )

byteStringPoke
  :: (HasErr r, HasSpecInfo r) => Text -> CType -> Sem r (UnassignedPoke a)
byteStringPoke valueName = \case
  Ptr Const Char -> pure $ ChainedPoke
    (ValueDoc ("p" <> pretty valueName))
    (contTPoke $ \_ _ (ValueDoc value) -> do
      tellImport 'BS.useAsCString
      pure $ "useAsCString" <+> value
    )
    [intermediateStorablePoke]

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
  :: (HasErr r, HasSpecInfo r) => Text -> CType -> Sem r (UnassignedPoke a)
maybeByteStringPoke valueName = \case
  Ptr Const Char -> pure $ ChainedPoke
    (ValueDoc ("p" <> pretty valueName))
    (contTPoke $ \_ _ (ValueDoc value) -> do
      tellImport 'BS.useAsCString
      tellImport 'maybeWith
      pure $ "maybeWith useAsCString" <+> value
    )
    [intermediateStorablePoke]
  t -> throw $ "Unhandled Maybe Bytestring conversion to: " <> show t

maybePoke :: (HasErr r, HasSpecInfo r) => Text -> CType -> Sem r (UnassignedPoke a)
maybePoke valueName = \case
  Ptr Const t@(TypeName n) -> getStruct n >>= \case
    Nothing -> throw $ "Unhandled Maybe conversion to: " <> show t
    Just _  -> pure $ ChainedPoke
      (ValueDoc ("p" <> pretty valueName))
      (contTPoke $ \_ _ (ValueDoc value) -> do
        tellImportWithAll (TyConName "ToCStruct")
        tellImport 'maybeWith
        pure $ "maybeWith withCStruct" <+> value
      )
      [intermediateStorablePoke]
  _ -> pure $ mapPokeValue
    (\(ValueDoc v) -> do
      tellImport 'fromMaybe
      tellImportWithAll (TyConName "Zero")
      pure . ValueDoc . parens $ "fromMaybe zero" <+> v
    )
    storablePoke

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
  -> Sem r (Maybe Int, Unassigned a -> UnassignedPoke a, Unassigned a)
elemPokeAndSize vecName fromElem toElem = do
  sizeMaybe <- case toElem of
    TypeName n -> do
      s <- fmap sSize <$> getStruct n
      u <- fmap sSize <$> getUnion n
      pure (s <|> u)
    _ -> pure Nothing
  (pokeConstructor, elemPoke) <-
    getPoke' toElem (vecName <> "Elem") fromElem >>= \case
      Pure _ _           -> throw "Pure poke as vector element?"
      IOPoke    elemPoke -> pure (IOPoke, elemPoke)
      ContTPoke elemPoke -> pure (ContTPoke, elemPoke)
      p@ChainedPoke{} ->
        let elemPoke = Unassigned $ \ty addr value -> do
              let assigned =
                    (\(Unassigned f) -> Assigned @a $ f ty addr value) <$> p
              unchained <- resolveChainedPoke assigned
              renderPokesContT (V.fromList unchained)
        in  pure (ContTPoke, elemPoke)
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
  -> Sem r (UnassignedPoke a)
vectorToPointer vecName fromElem toElem = do
  (alloc, writeElems, write) <- vectorToPointerPokes vecName fromElem toElem
  pure
    $ ChainedPoke (ValueDoc $ "p" <> pretty vecName) alloc [writeElems, write]

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
  -> Sem
       r
       ( UnassignedPoke a
       , Poke (ValueDoc -> Unassigned a)
       , Poke (ValueDoc -> Unassigned a)
       )
vectorToPointerPokes vecName fromElem toElem = do
  toArray   <- vectorToArray vecName fromElem toElem
  sizeMaybe <- case toElem of
    TypeName n -> do
      s <- fmap (sSize &&& sAlignment) <$> getStruct n
      u <- fmap (sSize &&& sAlignment) <$> getUnion n
      pure $ s <|> u
    _ -> pure Nothing
  pure
    ( contTPoke $ \_ _ (ValueDoc value) -> do
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
        Unassigned $ \ty _ value -> p ty (AddrDoc addr) value
      )
      <$> toArray
    , intermediateStorablePoke
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
  -> Sem r (UnassignedPoke a)
eitherLengthVec vecName fromElem toElem = do
  (alloc, writeElems, write) <- vectorToPointerPokes vecName fromElem toElem
  let
    allocAndWrite = ContTPoke $ Unassigned $ \ty addr (ValueDoc value) -> do
      tellImport 'nullPtr
      let vecVar                = "vec"
          ptrVar                = "pVec"
          allocAndWriteChained  = ChainedPoke (ValueDoc ptrVar) alloc [writeElems]
          allocAndWriteAssigned = allocAndWriteChained
            <&> \(Unassigned f) -> Assigned @a (f ty addr (ValueDoc vecVar))
      -- let allocAndWriteAssigned = V.fromList
      --       [ alloc
      --         <&> \(Unassigned f) -> Assigned @a (f ty addr (ValueDoc value))
      --       , writeElems <&> \w -> case w (ValueDoc var) of
      --         Unassigned f -> Assigned @a (f ty addr (ValueDoc value))
      --       ]
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

  pure $ ChainedPoke (ValueDoc $ "p" <> pretty vecName) allocAndWrite [write]

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

ioPoke :: (Type -> AddrDoc -> ValueDoc -> P a (Doc ())) -> Poke (Unassigned a)
ioPoke t = IOPoke (Unassigned t)

constPoke :: Inline -> P a (Doc ()) -> Poke (Unassigned a)
constPoke i t = Pure i (Unassigned $ \_ _ _ -> t)

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

constantStorablePoke :: Text -> UnassignedPoke a
constantStorablePoke con = mapPokeValue
  (const $ tellImport (ConName con) >> pure (ValueDoc (pretty con)))
  storablePoke

storablePoke :: UnassignedPoke a
storablePoke = ioPoke $ \ty (AddrDoc addr) (ValueDoc value) -> do
  RenderParams {..} <- ask
  tDoc              <- renderTypeHighPrec ty
  tellConImport ''Storable 'poke
  xDoc <- case mkIdiomaticType ty of
    Nothing                       -> pure value
    Just (IdiomaticType _ from _) -> do
      fromDoc <- from
      pure $ parens (fromDoc <+> value)
  pure $ "poke @" <> tDoc <+> addr <+> xDoc
