module Render.Peek
  ( renderPeekStmt
  , renderPeek
  )
  where

import           Relude                  hiding ( Type
                                                , ask
                                                , asks
                                                , last
                                                , init
                                                , Const
                                                , Reader
                                                , runReader
                                                )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.NonDet         hiding ( Empty )
import           Polysemy.Fail
import           Data.Vector.Extra              ( pattern (:<|) )
import           Data.Vector                    ( Vector )
import           Polysemy.Reader
import qualified Data.Vector.Extra             as V

import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Utils
import           Foreign.Marshal.Array
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
import           Render.Poke

renderPeekStmt
  :: ( HasErr r
     , Marshalable a
     , Show a
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => (a -> Text)
  -> a
  -> AddrDoc
  -> MarshalScheme a
  -> Sem r (Maybe (Doc ()))
renderPeekStmt getName a addr scheme = do
  RenderParams {..} <- ask
  let fromType = type' a
  hsType <- cToHsType DoPreserve fromType
  wrap   <- case mkIdiomaticType hsType of
    Nothing                     -> pure ((pretty (getName a) <+> "<-") <+>)
    Just (IdiomaticType _ _ to) -> to >>= \case
      Left  con -> pure ((con <+> pretty (getName a) <+> "<-") <+>)
      Right fun -> pure ((pretty (getName a) <+> "<-" <+> fun <+> "<$>") <+>)
  -- We run renderPeek with the pointer to something of a's type
  fmap wrap <$> renderPeekWrapped (lengths a) (Ptr Const (type' a)) addr scheme

type Lengths = Vector ParameterLength

-- | Render a peek and don't do any unwrapping to idiomatic haskell types
renderPeekWrapped
  :: forall a r
   . ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     , Show a
     )
  => Lengths
  -> CType
  -> AddrDoc
  -> MarshalScheme a
  -> Sem r (Maybe (Doc ()))
renderPeekWrapped lengths fromType addr = \case
  Normal   toType     -> Just <$> normalPeek (Proxy @a) addr toType fromType
  Preserve toType     -> Just <$> storablePeek addr fromType
  ElidedVoid          -> pure Nothing
  ElidedLength _ _    -> Just <$> storablePeek addr fromType
  ElidedUnivalued _   -> pure Nothing
  ByteString          -> Just <$> byteStringPeek addr fromType
  VoidPtr             -> Just <$> storablePeek addr fromType
  Maybe  toType       -> Just <$> maybePeek' lengths addr fromType toType
  Vector toElem       -> Just <$> vectorPeek lengths addr fromType toElem
  Tupled n toElem     -> Just <$> tuplePeek addr fromType toElem
  EitherWord32 toElem -> Just <$> eitherWord32Peek lengths addr fromType toElem
  s                   -> throw ("Unhandled peek " <> show s)

-- | Render a peek unwrap it to an idiomatic haskell type
renderPeek
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     , Show a
     )
  => Lengths
  -> CType
  -> AddrDoc
  -> MarshalScheme a
  -> Sem r (Maybe (Doc ()))
renderPeek lengths fromType addr scheme = do
  RenderParams {..} <- ask
  let unwrappedVar = "a"

  hsType <- cToHsType DoPreserve fromType

  unwrap <- case mkIdiomaticType hsType of
    Nothing                     -> pure id
    Just (IdiomaticType _ _ to) -> to >>= \case
      Left con -> pure
        ((   parens
             ("\\" <> parens (con <+> unwrappedVar) <+> "->" <+> unwrappedVar)
         <+> "<$>"
         ) <+>
        )
      Right fun -> pure ((fun <+> "<$>") <+>)

  wrapped <- renderPeekWrapped lengths (Ptr Const fromType) addr scheme
  pure (unwrap <$> wrapped)

----------------------------------------------------------------
-- Peeks
----------------------------------------------------------------

storablePeek
  :: (HasErr r, HasRenderElem r, HasRenderParams r)
  => AddrDoc
  -> CType
  -> Sem r (Doc ())
storablePeek (AddrDoc addr) fromPtr = case fromPtr of
  Ptr _ from -> do
    tellImportWithAll ''Storable
    tDoc <- renderTypeHighPrec =<< cToHsType DoPreserve from
    pure $ "peek @" <> tDoc <+> addr
  _ -> throw "Trying to generate a storable peek for a non-pointer"

normalPeek
  :: forall a r proxy
   . ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => proxy a
  -> AddrDoc
  -> CType
  -> CType
  -> Sem r (Doc ())
normalPeek _ (AddrDoc addr) to fromPtr =
  runNonDet (asum [same, inlineStruct, pointerStruct, union]) >>= \case
    Nothing ->
      throw ("Unhandled " <> show fromPtr <> " conversion to: " <> show to)
    Just ps -> pure ps
 where
  same = failToNonDet $ do
    Ptr _ from <- pure fromPtr
    guard (from == to)
    case to of
      TypeName n -> do
        guard . isNothing =<< getStruct n
        guard . isNothing =<< getUnion n
      _ -> pure ()
    -- Assume a storable instance...
    storablePeek (AddrDoc addr) fromPtr

  inlineStruct = failToNonDet $ do
    Ptr _ from@(TypeName n) <- pure fromPtr
    guard (from == to)
    Just _ <- getStruct n
    tDoc   <- renderTypeHighPrec =<< cToHsType DoPreserve from
    pure $ "peekCStruct @" <> tDoc <+> addr

  pointerStruct = failToNonDet $ do
    Ptr _     from         <- pure fromPtr
    Ptr Const (TypeName n) <- pure from
    guard (TypeName n == to)
    Just _ <- getStruct n
    tDoc   <- renderTypeHighPrec =<< cToHsType DoPreserve (TypeName n)
    pure $ "peekCStruct @" <> tDoc <+> "=<< peek" <+> addr

  union = failToNonDet $ do
    Ptr _ from <- pure fromPtr
    TypeName n <- pure from
    Just     u <- getUnion n
    guard (from == to)
    unionPeek (Proxy @a) (AddrDoc addr) u to fromPtr

unionPeek
  :: forall a r proxy
   . ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     )
  => proxy a
  -> AddrDoc
  -> Union
  -> CType
  -> CType
  -> Sem r (Doc ())
unionPeek _ (AddrDoc addr) Struct {..} to fromPtr =
  failToError (V.singleton . T.pack) $ do
    RenderParams {..} <- ask
    getSibling        <- ask @(Text -> Maybe (SiblingInfo a))
    Ptr _ from        <- pure fromPtr
    TypeName n        <- pure from

    let discs :: [(UnionDiscriminator, SiblingInfo a)]
        discs =
          [ (d, sib)
          | d@UnionDiscriminator {..} <- toList unionDiscriminators
          , udUnionType == n
          , Just sib <- pure $ getSibling udSiblingName
          ]

    (UnionDiscriminator {..}, discSibling) <- case discs of
      []  -> throw ("Unable to find union discriminator for " <> n)
      [d] -> pure d
      ds  -> throw ("Found multiple union discriminators for " <> n)

    let peekName = "peek" <> mkTyName n
    tellImport (TermName peekName)
    pure $ pretty peekName <+> siReferrer discSibling <+> addr

-- TODO: Check lengths here for null termination
byteStringPeek
  :: (HasErr r, HasRenderElem r, HasSpecInfo r, HasRenderParams r)
  => AddrDoc
  -> CType
  -> Sem r (Doc ())
byteStringPeek (AddrDoc addr) = \case
  Ptr _ (Ptr Const Char) -> do
    tellImport 'BS.packCString
    pure $ "packCString =<< peek" <+> addr

  -- TODO: abstract away this repetition
  Ptr Const Char -> do
    tellImport 'BS.packCString
    pure $ "packCString" <+> addr

  Ptr _ (Array NonConst (SymbolicArraySize _) Char) -> do
    tellImport 'BS.packCString
    tellImport 'castPtr
    pure $ "packCString" <+> parens ("castPtr" <+> addr)

  Ptr _ (Array NonConst (SymbolicArraySize _) (TypeName "uint8_t")) -> do
    let fn = "peekByteStringFromSizedVectorPtr"
    tellImport (TermName fn)
    pure $ pretty fn <+> addr

  t -> throw ("Unhandled conversion to ByteString from " <> show t)

maybePeek'
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     , Show a
     )
  => Lengths
  -> AddrDoc
  -> CType
  -> MarshalScheme a
  -> Sem r (Doc ())
maybePeek' lengths (AddrDoc addr) fromPtr to = case fromPtr of
  Ptr _ from@(Ptr _ fromElem) -> do
    tellImport 'maybePeek
    let addrDoc     = "j"
        maybePtrDoc = "m"
    subPeek <- renderPeekWrapped lengths from (AddrDoc addrDoc) to >>= \case
      Nothing -> throw "Nothing to peek to fill Maybe"
      Just p  -> pure p
    ptrTDoc <- renderTypeHighPrec =<< cToHsType DoPreserve from
    pure $ doBlock
      [ maybePtrDoc <+> "<- peek @" <> ptrTDoc <+> addr
      , "maybePeek"
      <+> parens ("\\" <> addrDoc <+> "->" <+> subPeek)
      <+> maybePtrDoc
      ]
  t -> throw ("Unhandled conversion to Maybe from " <> show t)

vectorPeek
  :: forall a r
   . ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     , Show a
     )
  => Vector ParameterLength
  -> AddrDoc
  -> CType
  -> MarshalScheme a
  -> Sem r (Doc ())
vectorPeek lengths (AddrDoc addr) fromPtr toElem = case fromPtr of

  Ptr _ from@(Ptr _ fromElem) | NamedLength len :<| lenTail <- lengths -> do
    lenName <- siReferrer <$> getSiblingInfo @a len

    let ptrDoc = "v"
    ptrTDoc <- renderTypeHighPrec =<< cToHsType DoPreserve from

    gen     <- generate (AddrDoc ptrDoc)
                        fromElem
                        lenTail
                        (parens ("fromIntegral" <+> lenName))

    pure $ doBlock [ptrDoc <+> "<- peek @" <> ptrTDoc <+> addr, gen]

  Ptr _ from@(Array _ (SymbolicArraySize len) fromElem) -> do
    RenderParams {..} <- ask
    tDoc              <-
      renderTypeHighPrec . (ConT ''Ptr :@) =<< cToHsType DoPreserve fromElem
    let lenName = mkPatternName len
        addr'   = parens $ "castPtr @_ @" <> tDoc <+> addr
        ptrDoc  = "v"
    tellImport (ConName lenName)
    tellImport 'castPtr

    ptrTDoc <- renderTypeHighPrec =<< cToHsType DoPreserve (Ptr Const fromElem)

    gen     <- generate (AddrDoc ptrDoc) fromElem V.empty (pretty lenName)

    pure $ doBlock [ptrDoc <+> "<- peek @" <> ptrTDoc <+> addr', gen]

  -- YUCK, do this initial pointer peeking elsewhere please!
  Ptr _ fromElem | (V.uncons -> Just ((NamedLength len), lenTail)) <- lengths ->
    do
      lenName <- siReferrer <$> getSiblingInfo @a len

      generate (AddrDoc addr)
               fromElem
               lenTail
               (parens ("fromIntegral" <+> lenName))


  t -> throw ("Unhandled conversion to Vector from " <> show t)

 where

  generate (AddrDoc addr) fromElem lenTail lenName = do
    let indexDoc = "i"
    tellImport 'V.generateM

    structSize <- case fromElem of
      TypeName n -> do
        s <- fmap sSize <$> getStruct n
        u <- fmap sSize <$> getUnion n
        pure (s <|> u)
      _ -> pure Nothing

    advance <- case structSize of
      Just elemSize -> do
        tellImport 'plusPtr
        pure $ AddrDoc
          (parens
            (addr <+> "`plusPtr`" <+> parens
              (viaShow elemSize <+> "*" <+> indexDoc)
            )
          )
      Nothing -> do
        tellImport 'advancePtr
        pure (AddrDoc (parens (addr <+> "`advancePtr`" <+> indexDoc)))

    subPeek <- renderPeek lenTail fromElem advance toElem >>= \case
      Nothing -> throw "Unable to get vector element peek"
      Just p  -> pure p

    pure $ "generateM" <+> lenName <+> parens
      ("\\" <> indexDoc <+> "->" <+> subPeek)


eitherWord32Peek
  :: forall a r
   . ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     , Show a
     )
  => Lengths
  -> AddrDoc
  -> CType
  -> MarshalScheme a
  -> Sem r (Doc ())
eitherWord32Peek lengths addr fromPtr toElem = case fromPtr of
  Ptr _ from@(Ptr _ fromElem) | (NamedLength len) :<| _ <- lengths -> do
    lenName <- siReferrer <$> getSiblingInfo @a len
    m       <-
      note "Unable to get wrapped peek for EitherWord32"
        =<< renderPeekWrapped lengths fromPtr addr (Maybe (Vector toElem))
    pure $ "maybe" <+> parens ("Left" <+> lenName) <+> "Right <$>" <+> m
  t -> throw ("Unhandled conversion to EitherWord32 from " <> show t)

tuplePeek
  :: forall a r
   . ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     , Show a
     )
  => AddrDoc
  -> CType
  -> MarshalScheme a
  -> Sem r (Doc ())
tuplePeek (AddrDoc addr) fromPtr toElem = case fromPtr of
  Ptr _ (Array NonConst (NumericArraySize len) fromElem) -> do
    let elemDoc n = "e" <> viaShow n
        tupPtrDoc = "t"

    structSize <- case fromElem of
      TypeName n -> do
        s <- fmap sSize <$> getStruct n
        u <- fmap sSize <$> getUnion n
        pure (s <|> u)
      _ -> pure Nothing

    advance <- case structSize of
      Just elemSize -> do
        tellImport 'plusPtr
        pure $ \i ->
          AddrDoc
            (parens
              (tupPtrDoc <+> "`plusPtr`" <+> viaShow (elemSize * i))
            )
      Nothing -> do
        tellImport 'advancePtr
        pure $ \i ->
          AddrDoc (parens (tupPtrDoc <+> "`advancePtr`" <+> viaShow i))

    subPeeks <- forV [0 .. len - 1] $ \i ->
      renderPeek mempty fromElem (advance (fromIntegral i)) toElem >>= \case
        Nothing -> throw "Unable to get tuple element peek"
        Just p  -> pure (elemDoc i <+> "<-" <+> p)

    tDoc <-
      renderTypeHighPrec =<< cToHsType DoPreserve fromElem

    tellImport 'castPtr
    pure $ doBlock
      (  ["let" <+> tupPtrDoc <+> "= castPtr @_ @" <> tDoc <+> addr]
      <> subPeeks
      <> ["pure" <+> tupled (elemDoc <$> [0 .. len - 1])]
      )
  t -> throw ("Unhandled conversion to Tuple from " <> show t)
