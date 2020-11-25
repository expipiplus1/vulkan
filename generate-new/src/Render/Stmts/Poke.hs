{-# language AllowAmbiguousTypes #-}

module Render.Stmts.Poke
  ( getPokeDirect
  , getPokeDirect'
  , getPokeDirectElided
  , getPokeIndirect
  , elidedLengthPoke
  , ValueDoc(..)
  , AddrDoc(..)
  , UnitDoc(..)
  , FunDoc(..)
  , CmdsDoc(..)
  , HasSiblingInfo
  , SiblingInfo(..)
  , getSiblingInfo
  , allocArray
  , AllocType(..)
  , getVectorPoke
  ) where

import           Data.Char                      ( isUpper )
import           Data.List                      ( (!!) )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Data.Vector.Extra              ( pattern Empty
                                                , Vector
                                                )
import           Polysemy
import           Polysemy.Fail
import           Polysemy.Input
import           Polysemy.NonDet         hiding ( Empty )
import           Relude                  hiding ( Const
                                                , Type
                                                , head
                                                , init
                                                , last
                                                )

import           Control.Exception              ( bracket )
import           Control.Monad.Trans.Cont       ( ContT )
import qualified Data.ByteString               as BS
import qualified Data.Vector                   as V
import           Foreign.Marshal.Alloc
import           Foreign.Ptr

import           CType                         as C
import           Error
import           Haskell
import           Marshal.Marshalable
import           Marshal.Scheme
import           Render.Element
import           Render.Names
import           Render.Scheme
import           Render.SpecInfo
import           Render.State
import           Render.Stmts
import           Render.Stmts.Poke.SiblingInfo
import           Render.Stmts.Utils
import           Render.Type
import           Render.Utils

type HasPoke a r
  = ( Marshalable a
    , HasRenderElem r
    , HasRenderParams r
    , HasErr r
    , HasSpecInfo r
    , HasSiblingInfo a r
    , HasStmts r
    , HasRenderedNames r
    , HasRenderState r
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
  => CName
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
getPokeIndirect' name toType scheme value addr = runNonDetMaybe go >>= \case
  Nothing | Custom CustomScheme {..} <- scheme, NoPoke <- csDirectPoke ->
    unitStmt (pure . Pure AlwaysInline . ValueDoc $ "()")
  Nothing -> storablePoke addr =<< getPokeDirect' name toType scheme value
  Just r  -> pure r
 where
  go = case scheme of
    Vector NotNullable fromElem | Array _ size toElem <- toType ->
      raise $ fixedArrayIndirect name size toElem fromElem value addr
    Tupled _ fromElem | Array _ (NumericArraySize size) toElem <- toType ->
      raise $ tupleIndirect name size toElem fromElem value addr
    ByteString | Array _ size toElem <- toType ->
      raise $ byteStringFixedArrayIndirect name size toElem value addr
    WrappedStruct from ->
      raise $ wrappedStructIndirect name toType from value addr
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
  => CName
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
getPokeDirect' name toType fromType value = case fromType of
  Unit                            -> throw "Getting poke for a unit member"
  Normal from                     -> normal name toType from value
  Length ty os rs                 -> nonElidedLength name ty os rs
  Preserve _                      -> pure value
  ElidedLength ty os rs           -> elidedLength name ty os rs
  ElidedUnivalued value           -> elidedUnivalued name toType value
  VoidPtr -> normal name (Ptr NonConst Void) (Ptr NonConst Void) value
  ByteString                      -> byteString name toType value
  Vector nullable fromElem        -> vector name toType fromElem nullable value
  EitherWord32 fromElem           -> eitherWord32 name toType fromElem value
  Maybe (Vector _ _) -> throw "TODO: optional vectors without length"
  Maybe        from               -> maybe' name toType from value
  Tupled size fromElem            -> tuple name size toType fromElem value
  WrappedStruct fromName          -> wrappedStruct name toType fromName value
  Custom        CustomScheme {..} -> case csDirectPoke of
    NoPoke ->
      throw $ "Getting direct poke for custom scheme with no poke: " <> csName
    APoke p -> p value
  ElidedCustom CustomSchemeElided {..} -> cseDirectPoke
  s -> throw $ "Unhandled direct poke from " <> show s <> " to " <> show toType

getPokeDirectElided'
  :: HasPoke a r
  => CName
  -> CType
  -> MarshalScheme a
  -> Stmt s r (Ref s ValueDoc)
getPokeDirectElided' name toType fromType = case fromType of
  ElidedLength ty os rs -> elidedLength name ty os rs
  ElidedUnivalued value -> elidedUnivalued name toType value
  s -> throw $ "Unhandled elided poke from " <> show s <> " to " <> show toType
----------------------------------------------------------------
-- Pokes
----------------------------------------------------------------

normal
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => CName
  -> CType
  -> CType
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
normal name to from valueRef =
  note ("Unhandled " <> show from <> " conversion to: " <> show to)
    =<< runNonDetMaybe (asum [idiomatic, same, indirectStruct])
 where
  idiomatic = failToNonDet $ do
    RenderParams {..}                     <- input
    toTy                                  <- cToHsTypeWithHoles DoPreserve to
    Just (IdiomaticType fromTy fromFun _) <- pure $ mkIdiomaticType toTy
    fromTy'                               <- cToHsTypeWithHoles DoNotPreserve from
    guard (fromTy == fromTy')
    raise2 $ stmtC (Just toTy) name $ do
      fromDoc        <- fromFun
      ValueDoc value <- use valueRef
      pure . Pure InlineOnce . ValueDoc $ fromDoc <+> value

  same = failToNonDet $ do
    guard (from == to)
    pure valueRef

  indirectStruct = failToNonDet $ do
    Ptr Const toElem@(TypeName n) <- pure to
    guard (from == toElem)
    guard =<< ((isJust <$> getStruct n) <||> (isJust <$> getUnion n))
    ty <- cToHsTypeWithHoles DoNotPreserve to
    raise2 $ stmtC (Just ty) name . fmap (ContTAction . ValueDoc) $ do
      tellImportWithAll (TyConName "ToCStruct")
      tellImportWithAll ''ContT
      ValueDoc value <- use valueRef
      pure $ "ContT $ withCStruct" <+> value

wrappedStructIndirect
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderedNames r
     , HasRenderState r
     )
  => CName
  -> CType
  -> CName
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
wrappedStructIndirect name toType fromName valueRef addrRef = case toType of
  Ptr Const (TypeName n) | n == fromName ->
    storablePoke addrRef =<< wrappedStruct name toType fromName valueRef
  TypeName n | n == fromName -> do
    ty <- cToHsTypeWithHoles DoNotPreserve toType
    stmtC (Just ty) name $ do
      tellImport (TermName "pokeSomeCStruct")
      tellImport (TermName "forgetExtensions")
      AddrDoc  addr <- use addrRef
      ValueDoc val  <- use valueRef
      pure
        .   ContTAction
        .   ValueDoc
        $   "ContT $ pokeSomeCStruct"
        <+> parens ("forgetExtensions" <+> addr)
        <+> val
        <+> ". ($ ())"
  _ -> throw $ "Unhandled WrappedStruct conversion to " <> show toType

wrappedStruct
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => CName
  -> CType
  -> CName
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
wrappedStruct name toType fromName valueRef = case toType of
  Ptr Const (TypeName n) | n == fromName -> do
    RenderParams {..} <- input
    ty                <- cToHsTypeWithHoles DoNotPreserve toType
    stmtC (Just ty) name $ do
      ValueDoc val <- use valueRef
      tellImport (TermName "withSomeCStruct")
      tellImport 'castPtr
      tellImportWithAll ''ContT
      headTDoc       <- renderTypeHighPrec $ ConT (typeName (mkTyName n))
      unextendedTDoc <-
        renderTypeHighPrec
        $  ConT ''Ptr
        :@ (ConT (typeName (mkTyName n)) :@ PromotedNilT)
      pure
        .   ContTAction
        .   ValueDoc
        $   "ContT @_ @_ @"
        <>  unextendedTDoc
        <+> "$ \\cont -> withSomeCStruct @"
        <>  headTDoc
        <+> val
        <+> "(cont . castPtr)"
  _ -> throw $ "Unhandled WrappedStruct conversion to " <> show toType

elidedLength, nonElidedLength
  :: forall r a s
   . HasPoke a r
  => CName
  -> CType
  -> Vector a
  -> Vector a
  -> Stmt s r (Ref s ValueDoc)
elidedLength n t os rs =
  lengthFromNames @_ @a True n t (name <$> os) (name <$> rs)
nonElidedLength n t os rs =
  lengthFromNames @_ @a False n t (name <$> os) (name <$> rs)

elidedLengthPoke
  :: forall r a s
   . HasPoke a r
  => CName
  -> CType
  -> Vector CName
  -> Vector CName
  -> Stmt s r (Ref s ValueDoc)
elidedLengthPoke = lengthFromNames @r @a @s True

lengthFromNames
  :: forall r a s
   . HasPoke a r
  => Bool
  -- ^ Is elided
  -> CName
  -> CType
  -> Vector CName
  -> Vector CName
  -> Stmt s r (Ref s ValueDoc)
lengthFromNames True _ _ Empty Empty = throw "No vectors to get length from"
lengthFromNames isElided lenName lenType os rs = do
  rsLengthRefs <- forVMaybes
    rs
    (\r -> fmap (r, False, ) <$> lenRefFromSibling @a r)
  osLengthRefs <- forVMaybes
    os
    (\o -> fmap (o, True, ) <$> lenRefFromSibling @a o)
  let
    givenErr vecName opt givenName =
            -- TODO: these should be the names we give to the variable, not the C names
      unCName vecName
        <> (if opt then " must be empty or have '" else " must have '")
        <> unCName givenName
        <> "' elements"

    -- If the length is not elided, construct an assertion that a vector has
    -- the same length as the given value
    assertSameAsGiven
      :: (CName, Bool, Ref s (Doc ())) -> Stmt s r (Ref s (Doc ()))
    assertSameAsGiven (n, opt, ref) = unitStmt $ do
      ValueDoc givenLen <- useViaName (unCName lenName)
      let fi = if opt then ("fromIntegral" <+>) else id
      l      <- use ref
      orNull <- if opt
        then do
          -- Use again here to stop InlineOnce firing
          l' <- use ref
          pure $ " ||" <+> l' <+> "== 0"
        else pure mempty
      let -- TODO: these should be the names we give to the variable, not the C names
          err  = givenErr n opt lenName
          cond = parens (fi l <+> "==" <+> givenLen <> orNull)
      throwErrDoc err cond

    -- Construct an assertion that two vectors have the same length
    assertSame
      :: (CName, Bool, Ref s (Doc ()))
      -> (CName, Bool, Ref s (Doc ()))
      -> Stmt s r (Ref s (Doc ()))
    assertSame (n1, opt1, ref1) (n2, opt2, ref2) = unitStmt $ do
      when opt1 $ throw "FIXME: handle comparing sizes of all optional vectors"
      let fi = if opt2 then ("fromIntegral" <+>) else id
      l1     <- use ref1
      l2     <- fi <$> use ref2
      orNull <- if opt2
        then do
          -- Use again here to stop InlineOnce firing
          l2' <- use ref2
          pure $ " ||" <+> l2' <+> "== 0"
        else pure mempty
      let -- TODO: these should be the names we give to the variable, not the C names
          err =
            unCName n2 <> " and " <> unCName n1 <> " must have the same length"
          cond = parens (l2 <+> "==" <+> l1 <> orNull)
      throwErrDoc err cond

    -- If there is only one vector and the length is zero, infer the length
    -- from the vector size
    inferLength :: (CName, Bool, Ref s (Doc ())) -> Stmt s r (Ref s (Doc ()))
    inferLength (n, opt, ref) = stmt Nothing (Just (unCName lenName)) $ do
      ValueDoc givenLen <- useViaName (unCName lenName)
      vecLen            <- use ref
      orNull            <- if opt
        then do
          -- Use again here to stop InlineOnce firing
          vecLen2 <- use ref
          pure $ " ||" <+> vecLen2 <+> "== 0"
        else pure mempty
      let err = givenErr n opt lenName
          cond =
            parens ("fromIntegral" <+> vecLen <+> "==" <+> givenLen <> orNull)
      throwDoc <- throwErrDocStmtString err cond
      pure . IOAction $ "if" <+> givenLen <+> "== 0" <> line <> indent
        2
        (vsep
          [ "then pure $ fromIntegral" <+> vecLen
          , "else" <+> doBlock [throwDoc, "pure" <+> givenLen]
          ]
        )

  let allVecs = toList (rsLengthRefs <> osLengthRefs)
  (assertions, getLenValue) <- if isElided
    then do
      let firstLengthRef@(_, _, len1) : otherLengthRefs = allVecs
      assertions <- traverse (assertSame firstLengthRef) otherLengthRefs
      pure (assertions, use @r len1)
    else case allVecs of
      -- If we just have one vector, infer the length from that
      [v] -> pure ([], use =<< inferLength v)
      _   -> do
        assertions <- traverse assertSameAsGiven allVecs
        pure (assertions, unValueDoc <$> useViaName (unCName lenName))
  stmt (Just (ConT ''Word32)) (Just (unCName lenName)) $ do
    traverse_ after assertions
    l1       <- getLenValue
    lenHType <- cToHsType DoPreserve lenType
    Pure AlwaysInline . ValueDoc <$> case lenHType of
      ConT i | i == ''Int -> pure l1
      _                  -> if isElided
        then do
          lenTyDoc <- renderType lenHType
          pure $ parens ("fromIntegral" <+> l1 <+> "::" <+> lenTyDoc)
        else pure l1

lenRefFromSibling
  :: forall a r s . HasPoke a r => CName -> Stmt s r (Maybe (Ref s (Doc ())))
lenRefFromSibling name = do
  SiblingInfo {..} <- getSiblingInfo @a name
  if isElided siScheme
    then pure Nothing
    else fmap Just . stmt Nothing (Just (unCName name <> "Length")) $ do
      tellQualImport 'V.length
      ValueDoc vec <- useViaName (unCName name)
      case siScheme of
        Vector _ _ -> pure $ Pure InlineOnce ("Data.Vector.length $" <+> vec)
        EitherWord32 _ -> pure $ Pure
          InlineOnce
          ("either id (fromIntegral . Data.Vector.length)" <+> vec)
        -- Assume vector for now, TODO, put this in CustomScheme
        Custom _ -> pure $ Pure InlineOnce ("Data.Vector.length $" <+> vec)
        _ -> throw "Trying to get the length of a non vector type sibling"


-- TODO: the type of the value here could be improved
elidedUnivalued
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => CName
  -> CType
  -> Text
  -> Stmt s r (Ref s ValueDoc)
elidedUnivalued name to value = do
  RenderParams {..} <- input
  ty                <- cToHsTypeWithHoles DoPreserve to
  stmtC (Just ty) name $ do
    vName <- case value of
      "nullPtr" -> do
        tellImport 'nullPtr
        pure (TermName value)
      _ | isUpper (T.head value) -> do
        let vName = mkPatternName (CName value)
        tellImport vName
        pure vName
      t ->
        throw $ "TODO, univalued schemes with terms, couldn't handle " <> show t
    pure . Pure InlineOnce . ValueDoc . pretty $ vName

eitherWord32
  :: HasPoke a r
  => CName
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
eitherWord32 name toType fromElem valueRef = case toType of
  Ptr Const toElem -> do
    elemTy <- cToHsTypeWithHoles DoPreserve toElem
    stmtC (Just (ConT ''Ptr :@ elemTy)) name $ do
      ValueDoc value <- use valueRef
      vecName <- freshName (Just "v")
      subPoke <- renderSubStmts $ do
        valueRef' <- pureStmt . ValueDoc . pretty $ vecName
        getPokeDirect' name toType (Vector NotNullable fromElem) valueRef'
      freeNames [vecName]

      let (con, d) = case subPoke of
            IOStmts    d -> (IOAction, d)
            ContTStmts d -> (ContTAction, d)

      tellImport 'nullPtr
      pure . con . ValueDoc $ "case" <+> value <+> "of" <> line <> indent
        2
        (vsep
          ["Left _ -> pure nullPtr", "Right" <+> pretty vecName <+> "->" <+> d]
        )

  _ -> throw $ "Unhandled EitherWord32 conversion to " <> show toType

-- TODO: Reduce duplication here with eitherWord32
maybe'
  :: HasPoke a r
  => CName
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
maybe' name toTypePtr fromType valueRef = case toTypePtr of
  Ptr Const toType -> do
    elemTy <- cToHsTypeWithHoles DoPreserve toType
    stmtC (Just (ConT ''Ptr :@ elemTy)) name $ do
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

vector, getVectorPoke
  :: HasPoke a r
  => CName
  -> CType
  -> MarshalScheme a
  -> Nullable
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
vector name toType fromElem nullable valueRef = case toType of
  Ptr Const toElem -> do
    lenRef <- stmt Nothing Nothing $ do
      ValueDoc value <- use valueRef
      tellQualImport 'V.length
      pure . Pure AlwaysInline . ValueDoc $ "Data.Vector.length" <+> value

    case nullable of
      NotNullable -> do
        addrRef <- allocArray Uninitialized name toElem (Right lenRef)
        store   <- vectorIndirect name toElem fromElem valueRef addrRef
        addrWithStore toElem addrRef store
      Nullable -> do
        subPoke <- renderSubStmts $ do
          lenRef' <-
            stmt Nothing Nothing . fmap (Pure AlwaysInline) $ raise $ use lenRef
          valueRef' <-
            stmt Nothing Nothing . fmap (Pure AlwaysInline) $ raise $ use
              valueRef
          addrRef <- allocArray Uninitialized name toElem (Right lenRef')
          store   <- vectorIndirect name toElem fromElem valueRef' addrRef
          addrWithStore toElem addrRef store
        let (con, d) = case subPoke of
              IOStmts    d -> (IOAction, d)
              ContTStmts d -> (ContTAction, d)
        elemTy <- cToHsTypeWithHoles DoPreserve toElem
        stmt (Just (ConT ''Ptr :@ elemTy)) (Just (unCName name)) $ do
          tellQualImport 'V.null
          tellImport 'nullPtr
          ValueDoc value <- use valueRef
          pure
            .   con
            .   ValueDoc
            $   "if Data.Vector.null"
            <+> value
            <>  line
            <>  indent 2 (vsep ["then pure nullPtr", "else" <+> d])
  _ -> throw $ "Unhandled Vector conversion to " <> show toType

-- A nicer name for exporting
getVectorPoke = vector

tuple
  :: HasPoke a r
  => CName
  -> Word
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
tuple name size toType fromElem valueRef = case toType of
  Ptr Const toElem -> do
    addrRef <- allocArray Uninitialized name toElem (Left size)
    store   <- tupleIndirect name size toElem fromElem valueRef addrRef
    addrWithStore toElem addrRef store
  _ -> throw $ "Unhandled Tupled conversion to " <> show toType

addrWithStore
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => CType
  -> Ref s AddrDoc
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
addrWithStore toElem addrRef store = do
  elemTy <- cToHsTypeWithHoles DoPreserve toElem
  stmt (Just (ConT ''Ptr :@ elemTy)) Nothing $ do
    AddrDoc addr <- use addrRef
    after store
    pure . Pure AlwaysInline . ValueDoc $ addr

data AllocType = Uninitialized | Zeroed

-- | Generate a pointer which has some memory for elements allocated
allocArray
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => AllocType
  -> CName
  -> CType
  -> Either Word (Ref s ValueDoc)
  -> Stmt s r (Ref s AddrDoc)
allocArray allocType name elemType size = do
  (elemSize, elemAlign) <- case elemType of
    Void -> pure (1, 1)
    _    -> getTypeSize elemType
  mem <- stmt Nothing (Just $ "p" <> T.upperCaseFirst (unCName name)) $ do
    elemTyDoc <- renderTypeHighPrec =<< case size of
      Left n -> cToHsTypeWithHoles
        DoPreserve
        (Array Const (NumericArraySize n) elemType)
      Right _ -> cToHsTypeWithHoles DoPreserve elemType

    vecSizeDoc <- case size of
      Left 1                  -> pure $ viaShow elemSize
      Left i                  -> pure $ viaShow (elemSize * fromIntegral i)
      Right v | elemSize == 1 -> do
        ValueDoc length <- use v
        pure length
      Right v -> do
        ValueDoc length <- use v
        pure $ parens (length <+> "*" <+> viaShow elemSize)
    tellImportWithAll ''ContT
    alloc <- case allocType of
      Uninitialized -> do
        tellImport 'allocaBytesAligned
        pure
          $   "allocaBytesAligned @"
          <>  elemTyDoc
          <+> vecSizeDoc
          <+> viaShow elemAlign
      Zeroed -> do
        tellImport 'callocBytes
        tellImport 'free
        tellImport 'bracket
        pure
          $   "bracket"
          <+> parens ("callocBytes @" <> elemTyDoc <+> vecSizeDoc)
          <+> "free"
    pure . ContTAction . AddrDoc $ "ContT $" <+> alloc
  case allocType of
    Uninitialized -> pure mem
    Zeroed        -> case elemType of
      TypeName n -> getStruct n >>= \case
        Nothing -> pure mem
        Just _  -> do
          init <- stmt Nothing (Just "_") $ do
            let indexDoc = "i"
            lastElem <- case size of
              Left  n -> pure $ viaShow (n - 1)
              Right v -> do
                ValueDoc n <- use v
                pure (n <+> "- 1")
            AddrDoc mem <- use mem
            tellImport 'plusPtr
            elemPtrTyDoc <-
              renderType
              .   (ConT ''Ptr :@)
              =<< cToHsTypeWithHoles DoPreserve elemType
            tellImport (TermName "advancePtrBytes")
            let offset = indexDoc <+> "*" <+> viaShow elemSize
                elemPtr =
                  mem
                    <+> "`advancePtrBytes`"
                    <+> parens offset
                    <+> "::"
                    <+> elemPtrTyDoc
            pure
              .   ContTAction @(Doc ())
              $   "traverse (\\"
              <>  indexDoc
              <+> "-> ContT $ pokeZeroCStruct"
              <+> parens elemPtr
              <+> ". ($ ())) [0.."
              <>  lastElem
              <>  "]"

          stmt Nothing Nothing $ do
            after init
            mem <- use mem
            pure $ Pure AlwaysInline mem
      _ -> pure mem

byteString
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => CName
  -> CType
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
byteString name to valueRef = case to of
  Ptr Const Char -> do
    ty <- cToHsTypeWithHoles DoPreserve to
    stmtC (Just ty) name $ do
      tellImport 'BS.useAsCString
      tellImportWithAll ''ContT
      ValueDoc value <- use valueRef
      pure . ContTAction . ValueDoc $ ("ContT $ useAsCString" <+> value)
  _ -> throw $ "Unhandled ByteString conversion to " <> show to

----------------------------------------------------------------
-- Indirect pokes
----------------------------------------------------------------

byteStringFixedArrayIndirect
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => CName
  -> ArraySize
  -> CType
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
byteStringFixedArrayIndirect _name size toElem valueRef addrRef = case size of
  SymbolicArraySize _ -> unitStmt $ do
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
  :: HasPoke a r
  => CName
  -> Word
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
tupleIndirect name size toElem fromElem valueRef firstAddrRef =
  stmtC Nothing name $ do
    let indices = [0 .. size - 1]
    elemNames <- traverse (freshName . Just . ("e" <>) . show) indices
    let elemName = (elemNames !!)

    castFirstAddrRef <-
      stmt Nothing (Just ("p" <> T.upperCaseFirst (unCName name))) $ do
        tellImport (TermName "lowerArrayPtr")
        AddrDoc addr <- use firstAddrRef
        pure . Pure InlineOnce . AddrDoc $ "lowerArrayPtr" <+> addr

    subPokes <- renderSubStmts $ do

      elemTy <- schemeTypeNegative fromElem
      es     <- forV indices $ \i -> do
        valueRef <-
          stmt elemTy (Just (unCName name <> "Elem"))
          . pure
          . Pure AlwaysInline
          . ValueDoc
          . pretty
          . elemName
          . fromIntegral
          $ i
        addrRef <- elemAddrRef toElem castFirstAddrRef (Left (fromIntegral i))
        getPokeIndirect' (CName (unCName name <> show i))
                         toElem
                         fromElem
                         valueRef
                         addrRef

      unitStmt $ do
        traverse_ after es
        pure $ Pure AlwaysInline ("()" :: Doc ())

    freeNames elemNames

    let (con, d) = case subPokes of
          IOStmts    d -> (IOAction, d)
          ContTStmts d -> (ContTAction, d)

    ValueDoc value <- use valueRef
    pure . con . ValueDoc $ "case" <+> value <+> "of" <> line <> indent
      2
      (tupled (pretty <$> elemNames) <+> "->" <+> d)


fixedArrayIndirect
  :: forall a r s
   . HasPoke a r
  => CName
  -> ArraySize
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
fixedArrayIndirect name size toElem fromElem valueRef addrRef = do
  RenderParams {..} <- input
  checkLength       <- lenRefFromSibling @a name >>= \case
    Nothing            -> pure Nothing
    Just siblingLenRef -> fmap Just . stmtC Nothing name $ do
      len     <- use siblingLenRef
      maxSize <-
        let arraySizeDoc = \case
              NumericArraySize  n -> pure $ viaShow n
              SymbolicArraySize n -> do
                let n' = mkPatternName n
                tellImport n'
                pure (pretty n')
              MultipleArraySize a b -> do
                a' <- arraySizeDoc (NumericArraySize a)
                b' <- arraySizeDoc b
                pure (a' <+> "*" <+> b')
        in  arraySizeDoc size
      let err :: Text
          err =
            unCName name
              <> " is too long, a maximum of "
              <> show maxSize
              <> " elements are allowed"
          cond = parens (len <+> "<=" <+> maxSize)
      throwErrDoc err cond
  castAddrRef <-
    stmt Nothing (Just ("p" <> T.upperCaseFirst (unCName name))) $ do
      tellImport (TermName "lowerArrayPtr")
      AddrDoc addr <- use addrRef
      pure . Pure InlineOnce . AddrDoc $ "lowerArrayPtr" <+> addr
  pokeVector <- vectorIndirect name toElem fromElem valueRef castAddrRef
  -- check length
  -- poke vector indirect
  stmtC Nothing name $ do
    -- TODO: the interface in Stmts doesn't actually define the ordering here,
    -- however we need checkLength to come first.
    traverse_ after checkLength
    after pokeVector
    pure $ Pure AlwaysInline (ValueDoc "()")

vectorIndirect
  :: HasPoke a r
  => CName
  -> CType
  -> MarshalScheme a
  -> Ref s ValueDoc
  -> Ref s AddrDoc
  -> Stmt s r (Ref s ValueDoc)
vectorIndirect name toElem fromElem valueRef addrRef =
  stmtC (Just (ConT ''())) name $ do
    -- TODO: add these to the forbidden names for the subpoke
    indexDoc <- freshName (Just "i")
    elemDoc  <- freshName (Just "e")
    indexRef <- pureStmt (pretty indexDoc)

    subPoke  <- renderSubStmts $ do
      -- Get the value of the pointer to this element
      -- TODO: Reduce duplication here and in peek
      elemAddr <- elemAddrRef toElem addrRef (Right indexRef)

      elemTy   <- schemeTypeNegative fromElem
      elemRef  <- stmt elemTy (Just (unCName name <> "Elem")) $ pure $ Pure
        AlwaysInline
        (ValueDoc (pretty elemDoc))

      getPokeIndirect' name toElem fromElem elemRef elemAddr

    freeNames [indexDoc, elemDoc]

    let (con, sub) = case subPoke of
          IOStmts    d -> (IOAction, d)
          ContTStmts d -> (ContTAction, d)

    tellQualImport 'V.imapM_
    ValueDoc value <- use valueRef
    pure
      .   con
      .   ValueDoc
      $   "Data.Vector.imapM_"
      <+> parens ("\\" <> pretty indexDoc <+> pretty elemDoc <+> "->" <+> sub)
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
    pTyDoc <-
      renderType . (ConT ''Ptr :@) =<< cToHsTypeWithHoles DoPreserve toElem
    pure $ untyped <+> "::" <+> pTyDoc

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

raise2 :: Sem r a -> Sem (e1 : e2 : r) a
raise2 = raise . raise

forVMaybes :: HasErr r => Vector a -> (a -> Sem r (Maybe b)) -> Sem r (Vector b)
forVMaybes xs f = V.mapMaybe id <$> traverseV f xs
