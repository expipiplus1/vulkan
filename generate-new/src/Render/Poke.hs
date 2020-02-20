{-# language DeriveFunctor #-}
module Render.Poke
  where

import           Relude                  hiding ( Type
                                                , ask
                                                , last
                                                , init
                                                , Const
                                                )
import           Data.List                      ( last
                                                , init
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.NonDet
import           Polysemy.Fail
import           Data.Vector                    ( Vector )
import           Polysemy.Reader

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

import           Error
import           Marshal.Scheme
import           Render.Element
import           Render.SpecInfo
import           Render.Type
import           CType                         as C
import           Marshal.Marshalable
import           Haskell
import           Spec.Types


type P a = forall r . (HasErr r, HasRenderElem r, HasRenderParams r) => Sem r a

data Poke a
  = IOPoke a
  | ContTPoke a
  | ChainedPoke ValueDoc (Poke a) [ValueDoc -> Poke a]
    -- The first poke produces a result, and the name of that is fed into the
    -- second poke
  deriving (Functor)

newtype AddrDoc = AddrDoc (Doc ())
newtype ValueDoc = ValueDoc { unValueDoc :: Doc () }

newtype Unassigned = Unassigned (Type -> AddrDoc -> ValueDoc -> P (Doc ()))
newtype Assigned = Assigned (P (Doc ()))
type UnassignedPoke = Poke Unassigned
type AssignedPoke = Poke Assigned

isIOPoke :: Poke a -> Bool
isIOPoke = \case
  IOPoke _ -> True
  _ -> False

getPoke
  :: (Show a, Marshalable a, HasErr r, HasSpecInfo r, HasRenderParams r)
  => a
  -> MarshalScheme a
  -> Sem r UnassignedPoke
getPoke a = getPoke' (type' a) (name a)

getPoke'
  :: (Show a, Marshalable a, HasErr r, HasSpecInfo r, HasRenderParams r)
  => CType
  -- ^ The type we are translating to, i.e. the C-side type
  -> Text
  -- ^ The name of the thing being poked
  -> MarshalScheme a
  -> Sem r UnassignedPoke
getPoke' toType valueName s = do
  RenderParams {..} <- ask
  case s of
    Unit                  -> throw "Getting poke for a unit member"
    Normal          from  -> normalPoke valueName toType from
    ElidedUnivalued value -> do
      let vName = mkPatternName value
      pure $ constantStorablePoke vName
      -- pure $
      -- ChainedPoke (ValueDoc (pretty (name' a)))
      -- ()
      -- ioPoke (\_ )
      -- pure
      --     [ WrappedPokePure $ do
      --       v <- case T.dropPrefix "VK_" value of
      --         Nothing -> pure value
      --         Just v  -> tellDepend (WE.PatternName v) >> pure v
      --       pure $ pretty v
      --     , SimplePoke
      --     ]

    ByteString       -> byteStringPoke valueName toType
    Maybe ByteString -> maybeByteStringPoke valueName toType
    Maybe t          -> maybePoke valueName toType
    VoidPtr          -> pure storablePoke
    Vector fromElem  -> case toType of
      Array NonConst (SymbolicArraySize n) toElem ->
        vectorToFixedArray valueName n fromElem toElem
      Ptr Const toElem -> vectorToPointer valueName fromElem toElem
      t -> throw $ "Unhandled Vector conversion to: " <> show t
    -- _      -> pure (ioPoke $ \_ _ _ -> pure "undefined")
    scheme -> throw ("Unhandled scheme: " <> show scheme)

resolveChainedPoke
  :: forall r
   . (HasErr r, HasRenderParams r, HasRenderElem r)
  => AssignedPoke
  -> Sem r [AssignedPoke]
resolveChainedPoke = \case
  ChainedPoke intermediate initialPoke dependentPokes -> do
    initialPokeDocs   <- resolveChainedPoke initialPoke
    dependentPokeDocs <- traverseV (resolveChainedPoke . ($ intermediate))
                                   dependentPokes
    pure
      $  init initialPokeDocs
      <> [ (\(Assigned d) ->
             Assigned $ (unValueDoc intermediate <+> "<-" <+>) <$> d
           )
             <$> last initialPokeDocs
         ]
      <> concat dependentPokeDocs
  p -> pure [p]

renderPokesInIO
  :: forall r
   . (HasErr r, HasRenderParams r, HasRenderElem r)
  => Vector AssignedPoke
  -> Sem r (Doc ())
renderPokesInIO pokes = do
  renderedContT <- renderPokesContT pokes
  tellImport 'runContT
  pure $ "(. const) . runContT $" <+> renderedContT

renderPokesContT
  :: forall r
   . (HasErr r, HasRenderParams r, HasRenderElem r)
  => Vector AssignedPoke
  -> Sem r (Doc ())
renderPokesContT pokes = do
  unchained <- concat . toList <$> traverse resolveChainedPoke pokes
  let contTDocs = [ d | ContTPoke (Assigned d) <- toList unchained ]
      ioDocs    = [ d | IOPoke (Assigned d) <- toList unchained ]
      doBlock   = \case
        []    -> "pure ()"
        [s]   -> s
        stmts -> "do" <> line <> indent 2 (vsep stmts)
  contTStmts <- sequence contTDocs
  ioStmt     <- if null ioDocs
    then pure []
    else do
      ioStmts <- sequence ioDocs
      tellImport 'liftIO
      pure ["liftIO $" <+> doBlock ioStmts]
  pure $ doBlock (contTStmts <> ioStmt)

----------------------------------------------------------------
-- Pokes
----------------------------------------------------------------

normalPoke
  :: (HasErr r, HasSpecInfo r) => Text -> CType -> CType -> Sem r UnassignedPoke
normalPoke valueName to from =
  runNonDet (asum [same, inlineStruct, pointerStruct, unionPoke]) >>= \case
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
  unionPoke    = failToNonDet $ pure $ contTPoke $ \_ _ _ -> pure "undefined"
  inlineStruct = failToNonDet $ do
    TypeName n <- pure to
    Just     _ <- getStruct n
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
      [ \intermediate ->
          (\(Unassigned f) -> Unassigned $ \ty addr _ -> f ty addr intermediate)
            <$> storablePoke
      ]

byteStringPoke
  :: (HasErr r, HasSpecInfo r) => Text -> CType -> Sem r UnassignedPoke
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
  :: (HasErr r, HasSpecInfo r) => Text -> CType -> Sem r UnassignedPoke
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

maybePoke :: (HasErr r, HasSpecInfo r) => Text -> CType -> Sem r UnassignedPoke
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
  t -> pure $ mapPokeValue
    (\(ValueDoc v) -> do
      tellImport 'fromMaybe
      tellImportWithAll (TyConName "Zero")
      pure . ValueDoc . parens $ "fromMaybe zero" <+> v
    )
    storablePoke
  -- t -> conversionFunction t >>= \case
  --   Nothing -> pure
  --     [ WrappedPokePure $ do
  --       tellImport "Data.Maybe" "maybe"
  --       tellDepend (WE.TypeName "Zero")
  --       pure $ "maybe zero" <+> marshalledNameDoc p
  --     , SimplePoke
  --     ]
  --   Just conv -> pure
  --     [ WrappedPokePure $ do
  --       tellImport "Data.Maybe" "maybe"
  --       tellDepend (WE.TypeName "Zero")
  --       conv <- conv
  --       pure $ "maybe zero" <+> conv <+> marshalledNameDoc p
  --     , SimplePoke
  --     ]

vectorToArray
  :: forall r a
   . (Show a, Marshalable a, HasErr r, HasSpecInfo r, HasRenderParams r)
  => Text
  -> MarshalScheme a
  -> CType
  -> Sem r UnassignedPoke
vectorToArray vecName fromElem toElem = do
  sizeMaybe <- case toElem of
    TypeName n -> do
      s <- fmap sSize <$> getStruct n
      u <- fmap sSize <$> getUnion n
      pure (s <|> u)
    _ -> pure Nothing
  (pokeConstructor, Unassigned elemPoke) <-
    getPoke' toElem (vecName <> "Elem") fromElem >>= \case
      IOPoke    elemPoke -> pure (IOPoke, elemPoke)
      ContTPoke elemPoke -> pure (ContTPoke, elemPoke)
      p@ChainedPoke{} ->
        let elemPoke = Unassigned $ \ty addr value -> do
              let assigned =
                    (\(Unassigned f) -> Assigned $ f ty addr value) <$> p
              unchained <- resolveChainedPoke assigned
              renderPokesContT (V.fromList unchained)
        in  pure (ContTPoke, elemPoke)

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
   . (Show a, Marshalable a, HasErr r, HasSpecInfo r, HasRenderParams r)
  => Text
  -> Text
  -> MarshalScheme a
  -> CType
  -> Sem r UnassignedPoke
vectorToFixedArray vecName arrayLength fromElem toElem = do
  -- TODO: Error on truncation! Easy to implement with ChainedPoke
  toArray <- vectorToArray vecName fromElem toElem
  pure $
    mapPokeValue
    (\(ValueDoc v) -> do
      tellImport (ConName arrayLength)
      tellQualImport 'V.take
      pure . ValueDoc $ parens ("Data.Vector.take" <+> pretty arrayLength <+> v)
    )
    toArray

vectorToPointer
  :: forall r a
   . (Show a, Marshalable a, HasErr r, HasSpecInfo r, HasRenderParams r)
  => Text
  -> MarshalScheme a
  -> CType
  -> Sem r UnassignedPoke
vectorToPointer vecName fromElem toElem = do
  -- TODO: Error on truncation! Easy to implement with ChainedPoke
  toArray   <- vectorToArray vecName fromElem toElem
  sizeMaybe <- case toElem of
    TypeName n -> do
      s <- fmap (sSize &&& sAlignment) <$> getStruct n
      u <- fmap (sSize &&& sAlignment) <$> getUnion n
      pure $ s <|> u
    _ -> pure Nothing
  pure $ ChainedPoke
    (ValueDoc $ "p" <> pretty vecName)
    (contTPoke $ \_ _ (ValueDoc value) -> do
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
    )
    [ \(ValueDoc addr) -> mapPokeAddr (const (pure (AddrDoc addr))) toArray
    , \v -> mapPokeValue (const (pure v)) storablePoke
    ]

----------------------------------------------------------------
-- Shortcut pokes
----------------------------------------------------------------

contTPoke :: (Type -> AddrDoc -> ValueDoc -> P (Doc ())) -> Poke Unassigned
contTPoke t = ContTPoke
  (Unassigned $ \ty addr value -> do
    tellImportWithAll ''ContT
    ("ContT $" <+>) <$> t ty addr value
  )

ioPoke :: (Type -> AddrDoc -> ValueDoc -> P (Doc ())) -> Poke Unassigned
ioPoke t = IOPoke (Unassigned t)

intermediateStorablePoke :: ValueDoc -> UnassignedPoke
intermediateStorablePoke intermediate =
  (\(Unassigned f) -> Unassigned $ \ty addr _ -> f ty addr intermediate)
    <$> storablePoke

mapPokeValue :: (ValueDoc -> P ValueDoc) -> UnassignedPoke -> UnassignedPoke
mapPokeValue f =
  fmap (\(Unassigned p) -> Unassigned $ \ty addr value -> p ty addr =<< f value)

mapPokeAddr :: (AddrDoc -> P AddrDoc) -> UnassignedPoke -> UnassignedPoke
mapPokeAddr f = fmap
  (\(Unassigned p) -> Unassigned $ \ty addr value -> do
    addr' <- f addr
    p ty addr' value
  )

constantStorablePoke :: Text -> UnassignedPoke
constantStorablePoke con = mapPokeValue
  (const $ tellImport (ConName con) >> pure (ValueDoc (pretty con)))
  storablePoke

storablePoke :: UnassignedPoke
storablePoke = ioPoke $ \ty (AddrDoc addr) (ValueDoc value) -> do
  RenderParams {..} <- ask
  tDoc              <- renderTypeHighPrec ty
  tellConImport ''Storable 'poke
  xDoc <- case mkIdiomaticType ty of
    Nothing                     -> pure value
    Just (IdiomaticType _ from) -> do
      fromDoc <- from
      pure $ parens (fromDoc <+> value)
  pure $ "poke @" <> tDoc <+> addr <+> xDoc
