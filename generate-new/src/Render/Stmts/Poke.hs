{-# language AllowAmbiguousTypes #-}

module Render.Stmts.Poke
  ( getPokeDirect
  , getPokeDirectElided
  , getPokeIndirect
  , ValueDoc(..)
  , AddrDoc(..)
  , UnitDoc(..)
  , FunDoc(..)
  , CmdsDoc(..)
  , HasSiblingInfo
  , SiblingInfo(..)
  , getSiblingInfo
  , allocArray
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
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.NonDet         hiding ( Empty )
import           Polysemy.Reader
import           Polysemy.Fail
import           Data.Char                      ( isUpper )
import           Language.Haskell.TH            ( nameBase )
import           Data.Vector.Extra              ( Vector
                                                , pattern Empty
                                                )
import qualified Data.Text.Extra               as T

import qualified Data.Vector                   as V
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Control.Monad.Trans.Cont       ( ContT )
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
import           Render.Stmts
import           Render.Scheme

-- TODO: Reduct this duplication
newtype AddrDoc = AddrDoc { unAddrDoc  :: Doc () }
  deriving Show
newtype ValueDoc = ValueDoc { unValueDoc :: Doc () }
  deriving Show
newtype UnitDoc = UnitDoc { unUnitDoc :: Doc () }
  deriving Show
newtype FunDoc = FunDoc { unFunDoc :: Doc () }
  deriving Show
newtype CmdsDoc = CmdsDoc { unCmdsDoc :: Doc () }
  deriving Show

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

type HasPoke a r
  = ( Marshalable a
    , HasRenderElem r
    , HasRenderParams r
    , HasErr r
    , HasSpecInfo r
    , HasSiblingInfo a r
    , HasStmts r
    , Show a
    )

-- | Generate a poke action which puts the given value at the given memory
-- address
getPokeIndirect
  :: HasPoke a r
  => a
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
getPokeIndirect a = getPokeIndirect' (name a) (type' a)

-- | Generate a poke action which puts the given value at the given memory
-- address
getPokeIndirect'
  :: HasPoke a r
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
  :: HasPoke a r
  => a
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
getPokeDirect a = getPokeDirect' (name a) (type' a)

-- | Generate an action returning the C-side type for a value, this isn't
-- always possible (for example fixed arrrays, or structs with complex pokes)
-- and some values can only be poked indirectly
--
-- Fixed arrays could be returned as Sized vectors, but then there's an
-- overhead of vector construction and deconstruction which we should avoid.
getPokeDirectElided
  :: HasPoke a r => a -> MarshalScheme a -> Stmt s r (Ref s ValueDoc)
getPokeDirectElided a = getPokeDirectElided' (name a) (type' a)

getPokeDirect'
  :: HasPoke a r
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
  Tupled size fromElem    -> tuple name size toType fromElem value
  s -> throw $ "Unhandled direct poke from " <> show s <> " to " <> show toType

getPokeDirectElided'
  :: HasPoke a r
  => Text
  -> CType
  -> MarshalScheme a
  -> Stmt s r (Ref s ValueDoc)
getPokeDirectElided' name toType fromType = case fromType of
  ElidedLength os rs -> elidedLength name os rs
  ElidedUnivalued value -> elidedUnivalued name toType value
  s -> throw $ "Unhandled elided poke from " <> show s <> " to " <> show toType
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
    guard =<< ((isJust <$> getStruct n) <||> (isJust <$> getUnion n))
    ty <- cToHsType DoNotPreserve to
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
  rsLengthRefs <- forV rs
    $ \r -> (name r, False, ) <$> lenRefFromSibling @a (name r)
  osLengthRefs <- forV os
    $ \o -> (name o, True, ) <$> lenRefFromSibling @a (name o)
  let assertSame
        :: (Text, Bool, Ref s (Doc ()))
        -> (Text, Bool, Ref s (Doc ()))
        -> Stmt s r (Ref s (Doc ()))
      assertSame (n1, opt1, ref1) (n2, opt2, ref2) = unitStmt $ do
        when opt1
          $ throw "FIXME: handle comparing sizes of all optional vectors"
        let fi = if opt2 then ("fromIntegral" <+>) else id
        l1     <- use ref1
        l2     <- use ref2
        l2'    <- fi <$> use ref2
        orNull <- if opt2
          then do
            tellQualImport 'V.null
            pure $ " ||" <+> l2 <+> "== 0"
          else pure mempty
        let err :: Text
            err  = n2 <> " and " <> n1 <> " must have the same length"
            cond = parens (l2' <+> "==" <+> l1 <> orNull)
        throwErrDoc err cond
  let firstLengthRef@(name1, _, len1) : otherLengthRefs =
        toList (rsLengthRefs <> osLengthRefs)
  assertions <- traverse (assertSame firstLengthRef) otherLengthRefs
  stmt (Just (ConT ''Word32)) (Just (name1 <> "Count")) $ do
    traverse_ after assertions
    l1 <- use len1
    pure . Pure AlwaysInline . ValueDoc $ "fromIntegral" <+> l1

lenRefFromSibling
  :: forall a r s
   . (HasRenderParams r, HasRenderElem r, HasErr r, HasSiblingInfo a r)
  => Text
  -> Stmt s r (Ref s (Doc ()))
lenRefFromSibling name = stmt Nothing (Just (name <> "Length")) $ do
  SiblingInfo {..} <- getSiblingInfo @a name
  tellQualImport 'V.length
  ValueDoc vec <- useViaName name
  case siScheme of
    Vector       _ -> pure $ Pure InlineOnce ("Data.Vector.length $" <+> vec)
    EitherWord32 _ -> pure $ Pure
      InlineOnce
      ("either id (fromIntegral . Data.Vector.length)" <+> vec)
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
    vName <- case value of
      "nullPtr" -> do
        tellImport 'nullPtr
        pure value
      _ | isUpper (T.head value) -> do
        let vName = mkPatternName value
        tellImport (ConName vName)
        pure vName
      _ -> do
        tellImport (TermName value)
        pure value
    pure . Pure InlineOnce . ValueDoc . pretty $ vName

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
    lenRef <- stmt Nothing Nothing $ do
      ValueDoc value <- use valueRef
      tellQualImport 'V.length
      pure . Pure AlwaysInline . ValueDoc $ "Data.Vector.length" <+> value

    addrRef <- allocArray name toElem (Right lenRef)
    store   <- vectorIndirect name toElem fromElem valueRef addrRef
    addrWithStore toElem addrRef store
  _ -> throw $ "Unhandled Vector conversion to " <> show toType

tuple
  :: ( HasRenderParams r
     , HasRenderElem r
     , HasErr r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , HasStmts r
     , Marshalable a
     , Show a
     )
  => Text
  -> Word
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
tuple name size toType fromElem valueRef = case toType of
  Ptr Const toElem -> do
    addrRef <- allocArray name toElem (Left size)
    store   <- tupleIndirect name size toElem fromElem valueRef addrRef
    addrWithStore toElem addrRef store
  _ -> throw $ "Unhandled Tupled conversion to " <> show toType

addrWithStore
  :: (HasErr r, HasRenderParams r, HasRenderElem r)
  => CType
  -> Ref s AddrDoc
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
addrWithStore toElem addrRef store = do
  elemTy <- cToHsType DoPreserve toElem
  stmt (Just (ConT ''Ptr :@ elemTy)) Nothing $ do
    AddrDoc addr <- use addrRef
    after store
    pure . Pure AlwaysInline . ValueDoc $ addr

-- | Generate a pointer which has some memory for elements allocated
allocArray
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => Text
  -> CType
  -> Either Word (Ref s ValueDoc)
  -> Stmt s r (Ref s AddrDoc)
allocArray name elemType size =
  stmt Nothing (Just $ "p" <> T.upperCaseFirst name) $ do
    (elemSize, elemAlign) <- getTypeSize elemType
    elemTyDoc             <- renderTypeHighPrec =<< case size of
      Left  n -> cToHsType DoPreserve (Array Const (NumericArraySize n) elemType)
      Right _ -> cToHsType DoPreserve elemType

    vecSizeDoc <- case size of
      Left  i -> pure $ viaShow (elemSize * fromIntegral i)
      Right v -> do
        ValueDoc length <- use v
        pure $ parens (length <+> "*" <+> viaShow elemSize)
    tellImportWithAll ''ContT
    tellImport 'allocaBytesAligned
    pure
      .   ContTAction
      .   AddrDoc
      $   "ContT $ allocaBytesAligned @"
      <>  elemTyDoc
      <+> vecSizeDoc
      <+> viaShow elemAlign

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
  SymbolicArraySize _ -> unitStmt $ do
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
    AddrDoc  addr  <- use addrRef
    ValueDoc value <- use valueRef
    pure . IOAction . ValueDoc $ pretty fn <+> addr <+> value

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
tupleIndirect name size toElem fromElem valueRef firstAddrRef =
  stmt Nothing (Just name) $ do
    let indices = [0 .. size - 1]
        elemName n = "e" <> show n :: Text
        elemNames = [ elemName n | n <- indices ]

    castFirstAddrRef <- stmt Nothing (Just ("p" <> T.upperCaseFirst name)) $ do
      tellImport (TermName "lowerArrayPtr")
      AddrDoc addr <- use firstAddrRef
      pure . Pure InlineOnce . AddrDoc $ "lowerArrayPtr" <+> addr

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
        addrRef <- elemAddrRef toElem castFirstAddrRef (Left (fromIntegral i))
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
        cond = parens (len <+> "<=" <+> pretty maxSize)
    throwErrDoc err cond
  castAddrRef <- stmt Nothing (Just ("p" <> T.upperCaseFirst name)) $ do
    tellImport (TermName "lowerArrayPtr")
    AddrDoc addr <- use addrRef
    pure . Pure InlineOnce . AddrDoc $ "lowerArrayPtr" <+> addr
  pokeVector <- vectorIndirect name toElem fromElem valueRef castAddrRef
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
  (elemSize, _elemAlign) <- getTypeSize toElem

  -- TODO: be able to coerce refs
  AddrDoc addr           <- raise $ use addrRef

  Pure InlineOnce . AddrDoc <$> do
    tellImport 'plusPtr
    untyped <- case index of
      Left  0 -> pure addr
      Left  n -> pure $ addr <+> "`plusPtr`" <+> viaShow (elemSize * n)
      Right r -> do
        indexDoc <- raise $ use r
        pure $ addr <+> "`plusPtr`" <+> parens
          (viaShow elemSize <+> "*" <+> indexDoc)
    pTyDoc <- renderType . (ConT ''Ptr :@) =<< cToHsType DoPreserve toElem
    pure $ untyped <+> "::" <+> pTyDoc

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

-- | A doc which is an @IO a@ throwing an error as InvalidArgument unless some
-- condition is met
throwErrDoc
  :: (HasRenderElem r, HasRenderParams r)
  => Text
  -> Doc ()
  -> Sem r (Value (Doc ()))
throwErrDoc err cond = do
  tellImport 'throwIO
  tellImportWithAll ''IOException
  tellImportWithAll ''IOErrorType
  tellImport 'unless
  pure . IOAction $ "unless" <+> cond <+> "$" <> line <> indent
    2
    (   "throwIO $ IOError Nothing InvalidArgument"
    <+> viaShow ("" :: Text) -- TODO: function name
    <+> viaShow err
    <+> "Nothing Nothing"
    )

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

raise2 :: Sem r a -> Sem (e1 : e2 : r) a
raise2 = raise . raise

