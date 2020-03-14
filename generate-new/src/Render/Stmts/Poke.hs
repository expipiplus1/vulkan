{-# language AllowAmbiguousTypes #-}

module Render.Stmts.Poke
  ( getPokeIndirect
  , ValueDoc(..)
  , AddrDoc(..)
  , HasSiblingInfo
  , SiblingInfo(..)
  , getSiblingInfo
  ) where

import           Relude                  hiding ( Type
                                                , ask
                                                , asks
                                                , last
                                                , init
                                                , head
                                                , Const
                                                , Reader
                                                )
import           Data.List                      ( last
                                                , init
                                                , head
                                                )
import           Data.Text.Prettyprint.Doc
import qualified Data.List.NonEmpty            as NE
import           Polysemy
import           Polysemy.NonDet         hiding ( Empty )
import           Polysemy.Reader
import           Polysemy.Fail
import Polysemy.Input
import Language.Haskell.TH (nameBase)
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

import           CType                         as C
import           Error
import           Haskell
import           Marshal.Marshalable
import           Marshal.Scheme
import           Render.Element
import           Render.SpecInfo
import           Render.Type
import           Render.Utils
import           Spec.Types
import           Render.Stmts
import           Render.Scheme
import           Render.Poke                    ( ValueDoc(..)
                                                , AddrDoc(..)
                                                , HasSiblingInfo
                                                , SiblingInfo(..)
                                                , getSiblingInfo
                                                )


-- | Generate a poke action which puts the given value at the given memory
-- address
getPokeIndirect
  :: ( Marshalable a
     , HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , HasStmts r
     , Show a
     )
  => a
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
getPokeIndirect a = getPokeIndirect' (name a) (type' a)

-- | Generate a poke action which puts the given value at the given memory
-- address
getPokeIndirect'
  :: ( Marshalable a
     , HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , HasStmts r
     , Show a
     )
  => Text
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
getPokeIndirect' name toType scheme value addr = runNonDetMaybe go >>= \case
  Nothing -> storablePoke addr =<< getPokeDirect' name toType scheme value
  Just r  -> pure r
 where
  go = case scheme of
    Vector fromElem | Array _ size toElem <- toType ->
      raise $ fixedArrayIndirect name size toElem fromElem value addr
    Tupled _ fromElem | Array _ (NumericArraySize size) toElem <- toType ->
      raise $ tupleIndirect name size toElem fromElem value addr
    ByteString | Array _ size toElem <- toType ->
      raise $ byteStringFixedArrayIndirect name size toElem value addr
    _ -> empty


-- | Generate an action returning the C-side type for a value, this isn't
-- always possible (for example fixed arrrays, or structs with complex pokes)
-- and some values can only be poked indirectly
--
-- Fixed arrays could be returned as Sized vectors, but then there's an
-- overhead of vector construction and deconstruction which we should avoid.
getPokeDirect
  :: ( Marshalable a
     , HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , HasStmts r
     , Show a
     )
  => a
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
getPokeDirect a = getPokeDirect' (name a) (type' a)

getPokeDirect'
  :: ( Marshalable a
     , HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , HasStmts r
     , Show a
     )
  => Text
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
getPokeDirect' name toType fromType value = case fromType of
  Unit                    -> throw "Getting poke for a unit member"
  Normal   from           -> normal name toType from value
  Preserve _              -> pure value
  ElidedLength os rs      -> elidedLength name os rs
  ElidedUnivalued value   -> elidedUnivalued name toType value
  VoidPtr -> normal name (Ptr NonConst Void) (Ptr NonConst Void) value
  ByteString              -> byteString name toType value
  Vector       fromElem   -> vector name toType fromElem value
  EitherWord32 fromElem   -> eitherWord32 name toType fromElem value
  Maybe        (Vector _) -> throw "TODO: optional vectors without length"
  Maybe        from       -> maybe' name toType from value
  s -> throw $ "Unhandled direct poke from " <> show s <> " to " <> show toType

----------------------------------------------------------------
-- Pokes
----------------------------------------------------------------

normal
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => Text
  -> CType
  -> CType
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
normal name to from valueRef =
  note ("Unhandled " <> show from <> " conversion to: " <> show to)
    =<< runNonDetMaybe (asum [idiomatic, same, indirectStruct])
 where
  idiomatic = failToNonDet $ do
    RenderParams {..}                     <- ask
    toTy                                  <- cToHsType DoPreserve to
    Just (IdiomaticType fromTy fromFun _) <- pure $ mkIdiomaticType toTy
    fromTy'                               <- cToHsType DoNotPreserve from
    guard (fromTy == fromTy')
    raise2 $ stmt (Just toTy) (Just name) $ do
      fromDoc        <- fromFun
      ValueDoc value <- use valueRef
      pure . Pure InlineOnce . ValueDoc $ fromDoc <+> value

  same = failToNonDet $ do
    RenderParams {..} <- ask
    guard (from == to)
    pure valueRef

  indirectStruct = failToNonDet $ do
    Ptr Const toElem@(TypeName n) <- pure to
    guard (from == toElem)
    Just _ <- getStruct n
    ty     <- cToHsType DoNotPreserve to
    raise2 $ stmt (Just ty) (Just name) . fmap (ContTAction . ValueDoc) $ do
      tellImportWithAll (TyConName "ToCStruct")
      tellImportWithAll ''ContT
      ValueDoc value <- use valueRef
      pure $ "ContT $ withCStruct" <+> value

elidedLength
  :: forall r a s
   . ( HasRenderParams r
     , HasRenderElem r
     , Marshalable a
     , HasErr r
     , HasSiblingInfo a r
     )
  => Text
  -> Vector a
  -> Vector a
  -> Stmt s r (Ref s ValueDoc)
elidedLength _ Empty Empty = throw "No vectors to get length from"
elidedLength _ os    rs    = do
  rsLengthRefs <- forV rs $ \r -> (name r, False, ) <$> lenRefFromSibling @a (name r)
  osLengthRefs <- forV os $ \o -> (name o, True, ) <$> lenRefFromSibling @a (name o)
  let assertSame
        :: (Text, Bool, Ref s (Doc ()))
        -> (Text, Bool, Ref s (Doc ()))
        -> Stmt s r (Ref s (Doc ()))
      assertSame (n1, opt1, ref1) (n2, opt2, ref2) = unitStmt $ do
        when opt1
          $ throw "FIXME: handle comparing sizes of all optional vectors"
        tellImport 'throwIO
        tellImportWithAll ''IOException
        tellImportWithAll ''IOErrorType
        tellImport 'unless
        l1     <- use ref1
        l2     <- use ref2
        l2'    <- use ref2
        orNull <- if opt2
          then do
            tellQualImport 'V.null
            pure $ " ||" <+> l2 <+> "== 0"
          else pure mempty
        let err :: Text
            err = n2 <> " and " <> n1 <> " must have the same length"
        pure
          .   IOAction
          $   "unless"
          <+> parens (l2' <+> "==" <+> l1 <> orNull)
          <+> "$"
          <>  line
          <>  indent
                2
                (   "throwIO $ IOError Nothing InvalidArgument"
                <+> viaShow ("" :: Text) -- TODO: function name
                <+> viaShow err
                <+> "Nothing Nothing"
                )
  let firstLengthRef@(name1, _, len1) : otherLengthRefs =
        toList (rsLengthRefs <> osLengthRefs)
  assertions <- traverse (assertSame firstLengthRef) otherLengthRefs
  stmt (Just (ConT ''Word32)) (Just name1) $ do
    traverse_ after assertions
    l1 <- use len1
    pure $ Pure AlwaysInline (ValueDoc l1)

lenRefFromSibling
  :: forall a r s
   . (HasRenderParams r, HasRenderElem r, HasErr r, HasSiblingInfo a r)
  => Text
  -> Stmt s r (Ref s (Doc ()))
lenRefFromSibling name = stmt Nothing (Just (name <> "Length")) $ do
  SiblingInfo {..} <- getSiblingInfo @a name
  tellQualImport 'V.length
  ValueDoc vec <- useViaName name
  tyDoc        <- renderType (ConT ''Word32)
  case siScheme of
    Vector _ -> pure $ Pure
      InlineOnce
      ("fromIntegral . Data.Vector.length $" <+> vec <+> "::" <+> tyDoc)
    EitherWord32 _ -> pure $ Pure
      InlineOnce
      (   "either id (fromIntegral . Data.Vector.length)"
      <+> vec
      <+> "::"
      <+> tyDoc
      )
    _ -> throw "Trying to get the length of a non vector type sibling"


elidedUnivalued
  :: (HasRenderParams r, HasRenderElem r, HasErr r)
  => Text
  -> CType
  -> Text
  -> Stmt s r (Ref s ValueDoc)
elidedUnivalued name to value = do
  RenderParams {..} <- ask
  ty                <- cToHsType DoPreserve to
  stmt (Just ty) (Just name) $ do
    let vName = mkPatternName value
    tellImport (ConName vName)
    pure . Pure InlineOnce . ValueDoc . pretty $ value

eitherWord32
  :: ( HasRenderParams r
     , HasRenderElem r
     , HasErr r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , Marshalable a
     , HasStmts r
     , Show a
     )
  => Text
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
eitherWord32 name toType fromElem valueRef = case toType of
  Ptr Const toElem -> do
    elemTy <- cToHsType DoPreserve toElem
    stmt (Just (ConT ''Ptr :@ elemTy)) (Just name) $ do
      ValueDoc value <- use valueRef
      let vecName = "v"
      subPoke <- renderSubStmts $ do
        valueRef' <- pureStmt . ValueDoc $ vecName
        getPokeDirect' name toType (Vector fromElem) valueRef'

      let (con, d) = case subPoke of
            IOStmts    d -> (IOAction, d)
            ContTStmts d -> (ContTAction, d)

      tellImport 'nullPtr
      pure . con . ValueDoc $ "case" <+> value <+> "of" <> line <> indent
        2
        (vsep ["Left _ -> pure nullPtr", "Right" <+> vecName <+> "->" <+> d])

  _ -> throw $ "Unhandled EitherWord32 conversion to " <> show toType

-- TODO: Reduce duplication here with eitherWord32
maybe'
  :: ( HasRenderParams r
     , HasRenderElem r
     , HasErr r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , Marshalable a
     , HasStmts r
     , Show a
     )
  => Text
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
maybe' name toTypePtr fromType valueRef = case toTypePtr of
  Ptr Const toType -> do
    elemTy <- cToHsType DoPreserve toType
    stmt (Just (ConT ''Ptr :@ elemTy)) (Just name) $ do
      ValueDoc value <- use valueRef
      let justName = "j"
      subPoke <- renderSubStmts $ do
        valueRef' <- pureStmt . ValueDoc $ justName
        getPokeDirect' name toTypePtr fromType valueRef'

      let (con, d) = case subPoke of
            IOStmts    d -> (IOAction, d)
            ContTStmts d -> (ContTAction, d)

      tellImport 'nullPtr
      pure . con . ValueDoc $ "case" <+> value <+> "of" <> line <> indent
        2
        (vsep ["Nothing -> pure nullPtr", "Just" <+> justName <+> "->" <+> d])

  _ -> throw $ "Unhandled Maybe conversion to " <> show toTypePtr

vector
  :: ( HasRenderParams r
     , HasRenderElem r
     , HasErr r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , Marshalable a
     , HasStmts r
     , Show a
     )
  => Text
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
vector name toType fromElem valueRef = case toType of
  Ptr Const toElem -> do
    addrRef <- stmt Nothing (Just $ "p" <> T.upperCaseFirst name) $ do
      sizeMaybe <- case toElem of
        TypeName n -> do
          s <- fmap (sSize &&& sAlignment) <$> getStruct n
          u <- fmap (sSize &&& sAlignment) <$> getUnion n
          pure $ s <|> u
        _ -> pure Nothing

      ValueDoc value <- use valueRef
      tellQualImport 'V.length
      elemTyDoc <- renderTypeHighPrec =<< cToHsType DoPreserve toElem
      let vecLengthDoc = "Data.Vector.length" <+> value
      case sizeMaybe of
        Nothing -> do
          -- Use a storable instance
          tellImportWithAll ''ContT
          tellImport 'allocaArray
          pure
            .   ContTAction
            .   AddrDoc
            $   "ContT $ allocaArray @"
            <>  elemTyDoc
            <+> parens vecLengthDoc
        Just (elemSize, elemAlignment) -> do
          -- Size things explicitly
          tellImportWithAll ''ContT
          tellImport 'allocaBytesAligned
          pure
            .   ContTAction
            .   AddrDoc
            $   "ContT $ allocaBytesAligned @"
            <>  elemTyDoc
            <+> parens (viaShow elemSize <+> "*" <+> vecLengthDoc)
            <+> viaShow elemAlignment

    store  <- vectorIndirect name toElem fromElem valueRef addrRef
    elemTy <- cToHsType DoPreserve toElem
    stmt (Just (ConT ''Ptr :@ elemTy)) Nothing $ do
      AddrDoc addr <- use addrRef
      after store
      pure . Pure AlwaysInline . ValueDoc $ addr
  _ -> throw $ "Unhandled vector conversion to " <> show toType


byteString
  :: (HasRenderParams r, HasRenderElem r, HasErr r)
  => Text
  -> CType
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
byteString name to valueRef = case to of
  Ptr Const Char -> do
    ty <- cToHsType DoPreserve to
    stmt (Just ty) (Just name) $ do
      tellImport 'BS.useAsCString
      tellImportWithAll ''ContT
      ValueDoc value <- use valueRef
      pure . ContTAction . ValueDoc $ ("ContT $ useAsCString" <+> value)
  _ -> throw $ "Unhandled ByteString conversion to " <> show to

----------------------------------------------------------------
-- Indirect pokes
----------------------------------------------------------------

byteStringFixedArrayIndirect
  :: (HasRenderElem r, HasRenderParams r, HasErr r)
  => Text
  -> ArraySize
  -> CType
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
byteStringFixedArrayIndirect _name size toElem valueRef addrRef = case size of
  SymbolicArraySize n -> unitStmt $ do
    RenderParams {..} <- ask
    fn                <- case toElem of
      Char -> do
        let fn = "pokeFixedLengthNullTerminatedByteString"
        tellImport (TermName fn)
        pure fn
      _ -> do
        let fn = "pokeFixedLengthByteString"
        tellImport (TermName fn)
        pure fn
    tellImport (ConName (mkPatternName n))
    AddrDoc  addr  <- use addrRef
    ValueDoc value <- use valueRef
    pure . IOAction . ValueDoc $ pretty fn <+> pretty n <+> addr <+> value

  t -> throw $ "Unhandled indirect ByteString conversion to: " <> show t

tupleIndirect
  :: ( HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , Marshalable a
     , HasSiblingInfo a r
     , HasStmts r
     , Show a
     )
  => Text
  -> Word
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
tupleIndirect name size toElem fromElem valueRef addrRef =
  stmt Nothing (Just name) $ do
    let indices = [0 .. size - 1]
        elemName n = "e" <> show n :: Text
        elemNames = [ elemName n | n <- indices ]

    subPokes <- renderSubStmts $ do

      elemTy <- schemeType fromElem
      es     <- forV indices $ \i -> do
        valueRef <-
          stmt elemTy (Just (name <> "Elem"))
          . pure
          . Pure AlwaysInline
          . ValueDoc
          . pretty
          . elemName
          $ i
        addrRef <- elemAddrRef toElem addrRef (Left (fromIntegral i))
        getPokeIndirect' (name <> show i) toElem fromElem valueRef addrRef

      unitStmt $ do
        traverse_ after es
        pure $ Pure AlwaysInline ("()" :: Doc ())


    let (con, d) = case subPokes of
          IOStmts    d -> (IOAction, d)
          ContTStmts d -> (ContTAction, d)

    ValueDoc value <- use valueRef
    pure . con . ValueDoc $ "case" <+> value <+> "of" <> line <> indent
      2
      (tupled (pretty <$> elemNames) <+> "->" <+> d)


fixedArrayIndirect
  :: forall a r s
   . ( HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , Marshalable a
     , HasSiblingInfo a r
     , HasStmts r
     , Show a
     )
  => Text
  -> ArraySize
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
fixedArrayIndirect name size toElem fromElem valueRef addrRef = do
  RenderParams {..} <- ask
  checkLength       <- stmt Nothing (Just name) $ do
    len     <- use =<< lenRefFromSibling @a name
    maxSize <- case size of
      NumericArraySize  n -> pure $ show n
      SymbolicArraySize n -> do
        let n' = mkPatternName n
        tellImport (ConName n')
        pure n'
    let err :: Text
        err =
          name
            <> " is too long, a maximum of "
            <> maxSize
            <> " elements are allowed"
    tellImport 'throwIO
    tellImportWithAll ''IOException
    tellImportWithAll ''IOErrorType
    tellImport 'unless
    pure
      .   IOAction
      $   "unless"
      <+> parens (len <+> "<=" <+> pretty maxSize)
      <+> "$"
      <>  line
      <>  indent
            2
            ("throwIO $ IOError Nothing InvalidArgument"
            <+> viaShow ("" :: Text) -- TODO: function name
            <+> viaShow err
            <+> "Nothing Nothing" :: Doc ()
            )
  pokeVector <- vectorIndirect name toElem fromElem valueRef addrRef
  -- check length
  -- poke vector indirect
  stmt Nothing (Just name) $ do
    -- TODO: the interface in Stmts doesn't actually define the ordering here,
    -- however we need checkLength to come first.
    after checkLength
    after pokeVector
    pure $ Pure AlwaysInline (ValueDoc "()")

vectorIndirect
  :: ( HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , Marshalable a
     , HasSiblingInfo a r
     , HasStmts r
     , Show a
     )
  => Text
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
vectorIndirect name toElem fromElem valueRef addrRef =
  stmt (Just (ConT ''())) (Just name) $ do
    -- TODO: add these to the forbidden names for the subpoke
    let indexDoc = "i"
    let elemDoc  = "e"
    indexRef <- pureStmt indexDoc

    subPoke  <- renderSubStmts $ do
      -- Get the value of the pointer to this element
      -- TODO: Reduce duplication here and in peek
      elemAddr <- elemAddrRef toElem addrRef (Right indexRef)

      elemTy   <- schemeType fromElem
      elemRef  <- stmt elemTy (Just (name <> "Elem")) $ pure $ Pure
        AlwaysInline
        (ValueDoc elemDoc)

      getPokeIndirect' name toElem fromElem elemRef elemAddr

    let (con, sub) = case subPoke of
          IOStmts    d -> (IOAction, d)
          ContTStmts d -> (ContTAction, d)

    tellQualImport 'V.imapM_
    ValueDoc value <- use valueRef
    pure
      .   con
      .   ValueDoc
      $   "Data.Vector.imapM_"
      <+> parens ("\\" <> indexDoc <+> elemDoc <+> "->" <+> sub)
      <+> value

-- Get the value of the pointer to this element
elemAddrRef
  :: (HasErr r, HasSpecInfo r, HasRenderParams r, HasRenderElem r)
  => CType
  -> Ref s' AddrDoc
  -- ^ The address of the first element
  -> Either Int (Ref s' (Doc ()))
  -- ^ The index
  -> Stmt s (StmtE s' r : r) (Ref s AddrDoc)
elemAddrRef toElem addrRef index = stmt Nothing Nothing $ do
  structSize <- case toElem of
    TypeName n -> do
      s <- fmap sSize <$> getStruct n
      u <- fmap sSize <$> getUnion n
      pure (s <|> u)
    _ -> pure Nothing

  -- TODO: be able to coerce refs
  AddrDoc addr <- raise $ use addrRef

  Pure InlineOnce . AddrDoc <$> case structSize of
    Just elemSize -> do
      tellImport 'plusPtr
      case index of
        Left 0 -> pure addr
        Left n ->
          pure $ addr <+> "`plusPtr`" <+> parens (viaShow (elemSize * n))
        Right r -> do
          indexDoc <- raise $ use r
          pure $ addr <+> "`plusPtr`" <+> parens
            (viaShow elemSize <+> "*" <+> indexDoc)

    Nothing -> do
      tellImport 'advancePtr
      case index of
        Left  0 -> pure addr
        Left  n -> pure (addr <+> "`advancePtr`" <+> viaShow n)
        Right r -> do
          indexDoc <- raise $ use r
          pure (addr <+> "`advancePtr`" <+> indexDoc)

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Store using regular ol' poke
storablePoke
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => Ref s AddrDoc
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
storablePoke addr value = do
  ty       <- refType value
  isStruct <- case ty of
    ConT n ->
      (isJust <$> getStruct (T.pack (nameBase n)))
        <||> (isJust <$> getUnion (T.pack (nameBase n)))
    _ -> pure False
  if isStruct
    then unitStmt $ do
      AddrDoc  a <- use addr
      ValueDoc v <- use value
      tellImportWithAll (TyConName "ToCStruct")
      tellImportWithAll ''ContT
      pure
        .   ContTAction
        .   ValueDoc
        $   "ContT $ pokeCStruct"
        <+> a
        <+> v
        <+> ". ($ ())"
    else unitStmt $ do
      tellImportWithAll ''Storable
      AddrDoc  a <- use addr
      ValueDoc v <- use value
      pure . IOAction . ValueDoc $ "poke" <+> a <+> v

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

raise2 :: Sem r a -> Sem (e1 : e2 : r) a
raise2 = raise . raise

-- -- TODO: Make this into getPokeDirect which returns a poke whose value is the
-- -- pointed to element. This can then be poked into a value (via storable) if
-- -- necessary.
-- getPoke
--   :: ( Show a
--      , Marshalable a
--      , HasErr r
--      , HasSpecInfo r
--      , HasRenderParams r
--      , HasSiblingInfo a r
--      )
--   => a
--   -> MarshalScheme a
--   -> Sem r (UnassignedPoke a)
-- getPoke a = getPoke' (type' a) (name a)

-- getPokeDirect
--   :: ( Show a
--      , Marshalable a
--      , HasErr r
--      , HasSpecInfo r
--      , HasRenderParams r
--      , HasSiblingInfo a r
--      )
--   => a
--   -> MarshalScheme a
--   -> Sem r (DirectPoke a)
-- getPokeDirect a s =
--   note ("Unable to get direct poke for " <> show a <> " from " <> show s)
--     =<< getPokeDirect' (type' a) (name a) s

-- getPoke'
--   :: ( Show a
--      , Marshalable a
--      , HasErr r
--      , HasSpecInfo r
--      , HasRenderParams r
--      , HasSiblingInfo a r
--      )
--   => CType
--   -- ^ The type we are translating to, i.e. the C-side type
--   -> Text
--   -- ^ The name of the thing being poked
--   -> MarshalScheme a
--   -> Sem r (UnassignedPoke a)
-- getPoke' toType valueName s = do
--   getPokeDirect' toType valueName s >>= \case
--     Just directPoke -> pure $ ChainedPoke
--       (ValueDoc (pretty ("p" <> valueName)))
--       (directToUnassigned <$> directPoke)
--       [intermediateStorablePoke]
--     Nothing -> case s of
--       Unit            -> throw "Getting poke for a unit member"
--       Normal from     -> normalPokeIndirect toType from
--       ByteString      -> byteStringPokeIndirect toType
--       Vector fromElem -> case toType of
--         Array NonConst (SymbolicArraySize n) toElem ->
--           vectorToFixedArray valueName n fromElem toElem
--         t -> throw $ "Unhandled Vector conversion to: " <> show t
--       Tupled n fromElem -> case toType of
--         Array _ (NumericArraySize n') toElem | n == n' ->
--           tupleToFixedArray valueName n fromElem toElem
--         t ->
--           throw $ "Unhandled Tupled " <> show n <> " conversion to: " <> show t
--       scheme -> throw ("Unhandled scheme: " <> show scheme)

-- getPokeDirect'
--   :: ( Show a
--      , Marshalable a
--      , HasErr r
--      , HasSpecInfo r
--      , HasRenderParams r
--      , HasSiblingInfo a r
--      )
--   => CType
--   -- ^ The type we are translating to, i.e. the C-side type
--   -> Text
--   -- ^ The name of the thing being poked
--   -> MarshalScheme a
--   -> Sem r (Maybe (DirectPoke a))
-- getPokeDirect' toType valueName s = do
--   RenderParams {..} <- ask
--   case s of
--     Unit                  -> throw "Getting poke for a unit member"
--     Normal          from  -> normalPoke toType from
--     ElidedUnivalued value -> do
--       let vName = mkPatternName value
--       pure . Just $ constPoke DoInline $ do
--         tellImport (ConName vName)
--         pure (pretty vName)
--     ElidedLength os rs -> Just <$> elidedLengthPoke os rs
--     ByteString         -> byteStringPoke toType
--     Maybe ByteString   -> Just <$> maybeByteStringPoke toType
--     Maybe _            -> Just <$> maybePoke toType
--     VoidPtr            -> pure . Just $ idPoke
--     Vector fromElem    -> case toType of
--       Array NonConst (SymbolicArraySize _) _ -> pure $ Nothing
--       Ptr Const toElem -> Just <$> vectorToPointer valueName fromElem toElem
--       t                -> throw $ "Unhandled Vector conversion to: " <> show t
--     Tupled n fromElem -> case toType of
--       Ptr _ toElem -> do
--         let a = ContTPoke $ Direct $ \_ _ -> do
--               tellImport 'allocaArray
--               tyDoc <- renderTypeHighPrec =<< cToHsType DoPreserve toElem
--               pure $ "ContT $ allocaArray @" <> tyDoc <+> viaShow n
--         u <- tupleToFixedArray valueName n fromElem toElem
--         pure . Just $ ChainedPoke
--           (ValueDoc ("p" <> pretty (T.upperCaseFirst valueName)))
--           a
--           [ u <&> \(Unassigned f) (ValueDoc p) ->
--             Direct $ \ty v -> f ty (AddrDoc p) v
--           , Pure DoInline $ \(ValueDoc v) -> Direct $ \_ _ -> pure v
--           ]
--       _ -> pure Nothing
--     EitherWord32 fromElem -> case toType of
--       Ptr Const toElem -> Just <$> eitherLengthVec valueName fromElem toElem
--       t -> throw $ "Unhandled EitherWord32 conversion to: " <> show t
--     Preserve fromType | fromType == toType -> pure . Just $ idPoke
--     _ -> pure Nothing

----------------------------------------------------------------
-- Pokes
----------------------------------------------------------------

{-

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
    -- inline = if V.length os + V.length rs == 1 then DoInline else DoNotInline
    inline = DoNotInline
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

-}
