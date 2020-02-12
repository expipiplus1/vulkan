{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Write.Marshal.ToC where

import           Data.Bifunctor                           ( first )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                          ( Text )
import           Control.Applicative               hiding ( Const )
import qualified Data.Text.Extra               as T
import           Data.Either
import           Data.Maybe
import           Control.Monad.Except
import           Data.List.Extra                          ( intersperse
                                                          , partition
                                                          )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Control.Applicative                      ( Alternative(..) )
import           Control.Bool
import           Data.Foldable                            ( asum
                                                          , toList
                                                          )
import qualified Data.Map                      as Map

import           Spec.Savvy.Command
import           Spec.Savvy.Handle
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import qualified Write.Element                 as WE
import           Write.Marshal.Util
import           Write.Monad
import           Write.Monad.Lookup
import           Write.Marshal.Marshallable
import           Write.Marshal.Scheme
import           Write.Marshal.Type

marshalStructMembers :: Struct -> Write [MarshalScheme StructMember]
marshalStructMembers s = do
  let mar m = bespokeScheme s m >>= \case
        Nothing     -> Right <$> marshalStructType s m
        Just scheme -> pure $ Left scheme
  types <- zip (sMembers s) <$> traverseV mar (sMembers s)
  let m = Map.fromList [ (smName m, x) | (m, Right x) <- types ]
  traverseV (\(m, x) -> either pure (typeToScheme m) x) types

-- TODO: Remove duplication and neaten
marshalCommandParams :: Command -> Write [MarshalScheme Parameter]
marshalCommandParams c = do
  let mar p = bespokeCommandScheme c p >>= \case
        Nothing     -> Right <$> marshalCommandType c p
        Just scheme -> pure $ Left scheme
  types <- zip (cParameters c) <$> traverseV mar (cParameters c)
  let m = Map.fromList [ (pName p, x) | (p, Right x) <- types ]
  traverseV (\(m, x) -> either pure (typeToScheme m) x) types

typeToScheme
  :: Show a => Marshallable a => a -> MarshalType a -> Write (MarshalScheme a)
typeToScheme p t = do
  d <- case renderMarshalTypePrec 0 t of
    Nothing -> pure Elided
    Just w | Just z <- zeroMarshalType t -> pure $ Present w z
           | otherwise -> throwError "Present value with no zero type"
  MarshalScheme "" p d <$> case t of
    Returned _ -> pure Positive
    _          -> Negative <$> typePokes p t

typePokes :: Show a => Marshallable a => a -> MarshalType a -> Write [Poke]
typePokes p marshalledType =
  let to = type' p
  in
    case marshalledType of
      Returned t            -> pure []
      Normal   from         -> normalPoke p from to
      ElidedLength os vs    -> elidedLength p os vs
      ElidedVoid            -> pure []
      ElidedUnivalued value -> pure
        [ WrappedPokePure $ do
          v <- case T.dropPrefix "VK_" value of
            Nothing -> pure value
            Just v  -> tellDepend (WE.PatternName v) >> pure v
          pure $ pretty v
        , SimplePoke
        ]
      VoidPtr ->
        pure [WrappedPokePure $ pure (marshalledNameDoc p), SimplePoke]
      ByteString -> case to of
        Ptr Const Char -> pure
          [ WrappedPokeContT $ do
            tellImport "Control.Monad.Trans.Cont" "ContT(..)"
            tellImport "Data.ByteString"          "useAsCString"
            pure $ "useAsCString" <+> marshalledNameDoc p
          , SimplePoke
          ]
        Array _ (SymbolicArraySize n) toElem -> pure
          [ ComplexPoke $ \offset -> do
              f <- case toElem of
                Char -> do
                  tellDepend
                    (WE.TermName "pokeFixedLengthNullTerminatedByteString")
                  pure "pokeFixedLengthNullTerminatedByteString"
                _ -> do
                  tellDepend (WE.TermName "pokeFixedLengthByteString")
                  pure "pokeFixedLengthByteString"
              tellDepend (WE.PatternName n)
              pure $ f <+> pretty n <+> offset <+> marshalledNameDoc p
          ]
        t -> throwError $ "Unhandled ByteString conversion to: " <> T.tShow t
      Maybe ByteString -> case to of
        Ptr Const Char -> pure
          [ WrappedPokeContT $ do
            tellImport "Control.Monad.Trans.Cont" "ContT(..)"
            tellImport "Data.ByteString"          "useAsCString"
            tellImport "Foreign.Marshal.Utils"    "maybeWith"
            pure $ "maybeWith useAsCString" <+> marshalledNameDoc p
          , SimplePoke
          ]
        t ->
          throwError $ "Unhandled Maybe Bytestring conversion to: " <> T.tShow t
      Maybe fromElem -> case to of
        Ptr Const t@(TypeName n) -> isStruct n >>= \case
          Nothing ->
            throwError $ "Unhandled Maybe conversion to: " <> T.tShow t
          Just _ -> pure
            [ WrappedPokeContT $ do
              tellSourceDepend (WE.TypeName "ToCStruct")
              tellImport "Foreign.Marshal.Utils" "maybeWith"
              pure $ "maybeWith" <+> "withCStruct" <+> marshalledNameDoc p
            , SimplePoke
            ]
        t -> conversionFunction t >>= \case
          Nothing -> pure
            [ WrappedPokePure $ do
              tellImport "Data.Maybe" "maybe"
              tellDepend (WE.TypeName "Zero")
              pure $ "maybe zero" <+> marshalledNameDoc p
            , SimplePoke
            ]
          Just conv -> pure
            [ WrappedPokePure $ do
              tellImport "Data.Maybe" "maybe"
              tellDepend (WE.TypeName "Zero")
              conv <- conv
              pure $ "maybe zero" <+> conv <+> marshalledNameDoc p
            , SimplePoke
            ]
      Vector fromElem -> case to of
        Array NonConst (SymbolicArraySize n) toElem ->
          vectorToFixedArray p n fromElem toElem
        Ptr Const toElem -> vectorToPointer p fromElem toElem
        t -> throwError $ "Unhandled Vector conversion to: " <> T.tShow t
      EitherWord32 fromElem -> case to of
        Ptr Const toElem -> eitherWord32ToPointer p fromElem toElem
        t ->
          throwError $ "Unhandled Either Word32 conversion to: " <> T.tShow t
      Tupled n fromElem -> case to of
        Array _ (NumericArraySize _) toElem -> tuplePoke n p fromElem toElem
        _ ->
          throwError $ "Unhandled tuple conversion to non numeric sized array"

elidedLength :: Marshallable a => a -> [a] -> [a] -> Write [Poke]
elidedLength _ []  [] = throwError "No vectors to get length from"
elidedLength _ [o] [] = pure
  [ WrappedPokePure $ do
    tellQualifiedImport "Data.Vector" "length"
    tellImport "Data.Either" "either"
    pure
      $   "either id (fromIntegral . Data.Vector.length)"
      <+> marshalledNameDoc o
  , SimplePoke
  ]
elidedLength _ (o : os) []  = error "TODO: handle multiple optional vectors"
elidedLength _ []       [v] = pure
  [ WrappedPokePure $ do
    tellQualifiedImport "Data.Vector" "length"
    pure $ "fromIntegral $ Data.Vector.length" <+> marshalledNameDoc v
  , SimplePoke
  ]
elidedLength p opts (v : vs) = pure
  [ WrappedPokeIO $ do
    tellQualifiedImport "Data.Vector" "length"
    tellQualifiedImport "Data.Vector" "null"
    tyDoc <- toHsType (type' p)
    tellImport "Control.Exception" "throwIO"
    tellImport "GHC.IO.Exception"  "IOException(..)"
    tellImport "GHC.IO.Exception"  "IOErrorType(InvalidArgument)"
    when (not (null opts)) $ tellImport "Data.Either" "either"
    let collection = \case
          []  -> "nothing"
          [x] -> x
          xs  -> hsep (punctuate "," (init xs)) <+> "and" <+> last xs
        message =
          collection (marshalledNameDoc <$> (v : vs ++ opts))
            <+> "must have the same length"
    pure
      $   "let l = Data.Vector.length"
      <+> marshalledNameDoc v
      <+> "in if"
      <+> (hsep $ intersperse
            "&&"
            (  (("l == Data.Vector.length" <+>) . marshalledNameDoc <$> vs)
            ++ (   (\n ->
                     parens $ "l == either fromIntegral Data.Vector.length" <+> n
                   )
               .   marshalledNameDoc
               <$> opts
               )
            )
          )
      <+> "then pure"
      <+> parens ("fromIntegral l ::" <+> tyDoc)
      <+> "else throwIO $ IOError Nothing InvalidArgument \"\" \""
      <>  message
      <>  "\" Nothing Nothing"
  , SimplePoke
  ]

vectorToFixedArray
  :: (Show a, Marshallable a)
  => a
  -> Text
  -> MarshalType a
  -> Type
  -> Write [Poke]
vectorToFixedArray p n fromElem toElem = getElemPoke fromElem toElem >>= \case
  IOElem poke -> pure
    [ ComplexPoke $ \baseOffset -> do
        let v = "Data.Vector.take" <+> pretty n <+> marshalledNameDoc p
        tellDepend $ WE.PatternName n
        tellQualifiedImport "Data.Vector" "take"
        tellQualifiedImport "Data.Vector" "imapM_"
        tDoc <- toHsTypePrec 10 toElem
        poke <- poke baseOffset "i" "e" tDoc
        pure
          $   "Data.Vector.imapM_"
          <+> parens ("\\i e ->" <+> poke)
          <+> parens v
    ]
  ContTElem poke -> pure
    [ ComplexContTPoke $ \baseOffset -> do
        let v = "Data.Vector.take" <+> pretty n <+> marshalledNameDoc p
        tellDepend $ WE.PatternName n
        tellQualifiedImport "Data.Vector" "take"
        tellQualifiedImport "Data.Vector" "imapM_"
        tDoc <- toHsTypePrec 10 toElem
        poke <- poke baseOffset "i" "e"
        pure
          $   "Data.Vector.imapM_"
          <+> parens ("\\i e ->" <+> poke)
          <+> parens v
    ]
  _ ->
    throwError
      $  "Unhandle elem type in fixed array conversion: "
      <> T.tShow fromElem

eitherWord32ToPointer
  :: (Show a, Marshallable a) => a -> MarshalType a -> Type -> Write [Poke]
eitherWord32ToPointer p fromElem toElem = getElemPoke fromElem toElem >>= \case
  IOElem poke -> pure
    [ WrappedPokeContT $ do
      toElem <- toHsTypePrec 10 toElem
      tellImport "Foreign.Marshal.Array" "allocaArray"
      tellImport "Foreign.Ptr"           "nullPtr"
      tellQualifiedImport "Data.Vector" "length"
      tellExtension "TypeApplications"
      let len = "Data.Vector.length" <+> marshalledNameDoc p
      pure $ "case" <+> marshalledNameDoc p <+> "of" <> line <> indent
        2
        (   "Left _ -> ($ nullPtr)"
        <>  line
        <>  "Right v -> allocaArray @"
        <>  toElem
        <+> "(Data.Vector.length v)"
        )
    , IOPoke $ do
      let v = marshalledNameDoc p
      tellQualifiedImport "Data.Vector" "imapM_"
      tellImport "Data.Foldable" "traverse_"
      tDoc <- toHsTypePrec 10 toElem
      poke <- poke (unmarshalledNameDoc p) "i" "e" tDoc
      pure
        $   "traverse_"
        <+> parens ("Data.Vector.imapM_" <+> parens ("\\i e ->" <+> poke))
        <+> v
    , SimplePoke
    ]
  ContTElem poke -> pure
    [ WrappedPokeContT $ do
      toElem <- toHsTypePrec 10 toElem
      tellImport "Foreign.Marshal.Array" "allocaArray"
      tellImport "Foreign.Ptr"           "nullPtr"
      tellQualifiedImport "Data.Vector" "length"
      tellExtension "TypeApplications"
      let len = "Data.Vector.length" <+> marshalledNameDoc p
      pure $ "case" <+> marshalledNameDoc p <+> "of" <> line <> indent
        2
        (   "Left _ -> ($ nullPtr)"
        <>  line
        <>  "Right v -> allocaArray @"
        <>  toElem
        <+> "(Data.Vector.length v)"
        )
    , ContTPoke $ do
      let v = marshalledNameDoc p
      tellQualifiedImport "Data.Vector" "imapM_"
      tellImport "Data.Foldable" "traverse_"
      tDoc <- toHsTypePrec 10 toElem
      poke <- poke (unmarshalledNameDoc p) "i" "e"
      pure
        $   "traverse_"
        <+> parens ("Data.Vector.imapM_" <+> parens ("\\i e ->" <+> poke))
        <+> v
    , SimplePoke
    ]
  _ ->
    throwError $ "Unhandle elem type in array conversion: " <> T.tShow fromElem

vectorToPointer
  :: (Show a, Marshallable a) => a -> MarshalType a -> Type -> Write [Poke]
vectorToPointer p fromElem toElem = getElemPoke fromElem toElem >>= \case
  IOElem poke -> pure
    [ WrappedPokeContT $ do
      toElem <- toHsTypePrec 10 toElem
      tellImport "Foreign.Marshal.Array" "allocaArray"
      tellQualifiedImport "Data.Vector" "length"
      tellExtension "TypeApplications"
      let len = "Data.Vector.length" <+> marshalledNameDoc p
      pure $ "allocaArray @" <> toElem <+> parens len
    , IOPoke $ do
      let v = marshalledNameDoc p
      tellQualifiedImport "Data.Vector" "imapM_"
      tDoc <- toHsTypePrec 10 toElem
      poke <- poke (unmarshalledNameDoc p) "i" "e" tDoc
      pure $ "Data.Vector.imapM_" <+> parens ("\\i e ->" <+> poke) <+> v
    , SimplePoke
    ]
  ContTElem poke -> pure
    [ WrappedPokeContT $ do
      toElem <- toHsTypePrec 10 toElem
      tellImport "Foreign.Marshal.Array" "allocaArray"
      tellQualifiedImport "Data.Vector" "length"
      tellExtension "TypeApplications"
      let len = "Data.Vector.length" <+> marshalledNameDoc p
      pure $ "allocaArray @" <> toElem <+> parens len
    , ComplexContTPoke $ \baseOffset -> do
      let v = marshalledNameDoc p
      tellQualifiedImport "Data.Vector" "imapM_"
      tDoc <- toHsTypePrec 10 toElem
      poke <- poke (unmarshalledNameDoc p) "i" "e"
      pure $ "Data.Vector.imapM_" <+> parens ("\\i e ->" <+> poke) <+> v
    , SimplePoke
    ]
  _ ->
    throwError $ "Unhandle elem type in array conversion: " <> T.tShow fromElem

normalPoke :: Marshallable a => a -> Type -> Type -> Write [Poke]
normalPoke p from to =
  runMaybeT (asum [same, bool, inlineStruct, pointerStruct, pureConversion])
    >>= \case
          Nothing ->
            throwError
              $  "Unhandled "
              <> T.tShow from
              <> " conversion to: "
              <> T.tShow to
          Just ps -> pure ps
  where
    same = do
      guard =<< lift (isUnmarshalled to) .||. pure (from == to)
      let nameChange = case to of
            Ptr _ _ -> Just . WrappedPokePure . pure . marshalledNameDoc $ p
            _       -> Nothing
      pure (toList nameChange ++ [SimplePoke])
    bool = do
      guard $ from == TypeName "Bool"
      guard $ to == TypeName "VkBool32"
      pure
        [ WrappedPokePure $ do
          tellDepend (WE.TermName "boolToBool32")
          pure $ "boolToBool32" <+> marshalledNameDoc p
        , SimplePoke
        ]
    inlineStruct = do
      TypeName n        <- pure to
      TypeName fromName <- pure from
      guard $ n == "Vk" <> fromName
      _ <- MaybeT (isStruct n)
      pure
        [ ComplexContTPoke $ \offset -> do
            tellSourceDepend (WE.TypeName "ToCStruct")
            tellImport "Control.Monad.Trans.Cont" "ContT(..)"
            pure
              $   "ContT $ pokeCStruct"
              <+> offset
              <+> marshalledNameDoc p
              <+> ". ($ ())"
        ]
    pointerStruct = do
      Ptr Const (TypeName n) <- pure to
      TypeName fromName      <- pure from
      guard $ n == "Vk" <> fromName
      _ <- MaybeT (isStruct n)
      pure
        [ WrappedPokeContT $ do
          tellSourceDepend (WE.TypeName "ToCStruct")
          pure $ "withCStruct" <+> marshalledNameDoc p
        , SimplePoke
        ]
    pureConversion = do
      conv <- MaybeT $ conversionFunction to
      pure [WrappedPokePure ((<+> marshalledNameDoc p) <$> conv), SimplePoke]

convert :: Type -> Doc () -> WE (Doc ())
convert t x = liftWrite (conversionFunction t) >>= \case
  Nothing -> pure x
  Just c  -> parens . (<+> x) <$> c

conversionFunction :: Type -> Write (Maybe (WE (Doc ())))
conversionFunction = runMaybeT . \case
  TypeName "VkBool32" -> pure $ do
    tellDepend (WE.TermName "boolToBool32")
    pure "boolToBool32"
  Float -> pure $ do
    tellImport "Foreign.C.Types" "CFloat(..)"
    pure "CFloat"
  TypeName n -> isHandle n >>= \case
    Just h | Dispatchable <- hHandleType h -> pure $ do
      let n = dropVkType $ hName h
      tellDepend (WE.TypeName n)
      pure $ pretty (T.lowerCaseFirst n) <> "Handle"
    _ -> empty
  _ -> empty

tuplePoke
  :: Show a
  => Marshallable a => Word -> a -> MarshalType a -> Type -> Write [Poke]
tuplePoke n p fromElem to = do
  getElemPoke fromElem to >>= \case
    ContTElem poke -> pure
      [ ComplexContTPoke $ \offset -> do
          let vars = (marshalledNameDoc p <>) . pretty . show <$> [1 .. n]
          pokes <- sequence
            [ poke offset (pretty (show i)) v | (v, i) <- zip vars [0 ..] ]
          pure $ "case" <+> marshalledNameDoc p <+> "of" <> line <> indent
            2
            (tupled vars <+> "-> do" <> line <> indent 2 (vsep pokes))
      ]
    IOElem poke -> pure
      [ ComplexPoke $ \offset -> do
          let vars = (marshalledNameDoc p <>) . pretty . show <$> [1 .. n]
          tDoc  <- toHsTypePrec 10 to
          pokes <- sequence
            [ poke offset (pretty (show i)) v tDoc | (v, i) <- zip vars [0 ..] ]
          pure $ "case" <+> marshalledNameDoc p <+> "of" <> line <> indent
            2
            (tupled vars <+> "-> do" <> line <> indent 2 (vsep pokes))
      ]
    _ ->
      throwError
        $  "Unhandled conversion from Tuple of "
        <> T.tShow fromElem
        <> " to "
        <> T.tShow to

data ElemPoke
  = IOElem (Doc () -> Doc () -> Doc () -> Doc () -> WE (Doc ()))
  | ContTElem (Doc () -> Doc () -> Doc () -> WE (Doc ()))

getElemPoke :: Show a => MarshalType a -> Type -> Write ElemPoke
getElemPoke marshalType toType = do
  runMaybeT (asum [struct, normal, wrapped, bytestring]) >>= \case
    Just p -> pure p
    Nothing ->
      throwError
        $  "Unable to get ElemPoke from "
        <> T.tShow marshalType
        <> " to "
        <> T.tShow toType
  where
    struct = do
      Normal   _ <- pure marshalType
      TypeName n <- pure toType
      s          <- MaybeT (isStruct n)
      pure . ContTElem $ \baseOffset i e -> do
        tellSourceDepend (WE.TypeName "ToCStruct")
        tellImport "Control.Monad.Trans.Cont" "ContT(..)"
        tellImport "Foreign.Marshal.Array"    "advancePtr"
        let p = parens (baseOffset <+> "`advancePtr`" <+> i)
        pure $ "ContT $ pokeCStruct" <+> p <+> e <+> ". ($ ())"
    normal = do
      Normal fromType <- pure marshalType
      isUnmarshalled' <- lift (isUnmarshalled toType)
      guard (toType == fromType || isUnmarshalled')
      pure . IOElem $ \baseOffset i e tDoc -> do
        tellExtension "TypeApplications"
        tellImport "Foreign.Storable" "pokeElemOff"
        pure $ "pokeElemOff" <+> "@" <> tDoc <+> baseOffset <+> i <+> e
    wrapped = do
      Normal _ <- pure marshalType
      conv     <- MaybeT (conversionFunction toType)
      pure . IOElem $ \baseOffset i e tDoc -> do
        tellExtension "TypeApplications"
        tellImport "Foreign.Storable" "pokeElemOff"
        conv <- conv
        pure $ "pokeElemOff" <+> "@" <> tDoc <+> baseOffset <+> i <+> parens
          (conv <+> e)
    bytestring = do
      ByteString     <- pure marshalType
      Ptr Const Char <- pure toType
      pure . ContTElem $ \baseOffset i e -> do
        tellImport "Control.Monad.Trans.Cont" "ContT(..)"
        tellImport "Data.ByteString"          "useAsCString"
        tellImport "Foreign.Storable"         "pokeElemOff"
        tellImport "Control.Monad.IO.Class"   "liftIO"
        tellQualifiedImport "Data.Vector" "imapM_"
        pure $ "ContT (useAsCString" <+> e <> ") >>=" <+> parens
          ("liftIO . pokeElemOff" <+> baseOffset <+> i)



isUnmarshalled :: Type -> Write Bool
isUnmarshalled = \case
  t | isIntegral t -> pure True
  TypeName n ->
    fmap isJust (isBitmask n)
      .||. fmap isJust (isEnum n)
      .||. isNonDispatchableHandle n
  _ -> pure False
  where
    isNonDispatchableHandle n = isHandle n >>= \case
      Nothing -> pure False
      Just h  -> pure $ hHandleType h == NonDispatchable

isIntegral :: Type -> Bool
isIntegral = \case
  Int                        -> True
  Char                       -> True
  TypeName "uint8_t"         -> True
  TypeName "uint16_t"        -> True
  TypeName "uint32_t"        -> True
  TypeName "uint64_t"        -> True
  TypeName "int8_t"          -> True
  TypeName "int16_t"         -> True
  TypeName "int32_t"         -> True
  TypeName "int64_t"         -> True
  TypeName "size_t"          -> True
  TypeName "VkDeviceSize"    -> True
  TypeName "VkDeviceAddress" -> True
  TypeName "VkSampleMask"    -> True
  _                          -> False

zeroMarshalType :: MarshalType a -> Maybe ZeroType
zeroMarshalType = \case
  Normal (TypeName "Bool") -> Just ZeroFalse
  Normal t                 -> Just Zero
  ElidedLength _ _         -> Nothing
  ElidedUnivalued _        -> Nothing
  ElidedVoid               -> Nothing
  VoidPtr                  -> Just ZeroNullPtr
  ByteString               -> Just ZeroMempty
  Maybe        _           -> Just ZeroNothing
  Vector       _           -> Just ZeroMempty
  EitherWord32 _           -> Just ZeroLeftZero
  Tupled n t               -> ZeroTuple n <$> zeroMarshalType t
  Returned t               -> zeroMarshalType t

----------------------------------------------------------------
-- Bespoke stuff
----------------------------------------------------------------

-- | This always takes priority
bespokeScheme
  :: Struct -> StructMember -> Write (Maybe (MarshalScheme StructMember))
bespokeScheme struct p =
  let schemes =
          [ do
            Name "pNext" <- pure $ name p
            Ptr _ Void   <- pure $ type' p
            let tDoc = do
                  tellSourceDepend (WE.TypeName "SomeVkStruct")
                  pure "Maybe SomeVkStruct"
                poke = WrappedPokeContT $ do
                  tellSourceDepend (WE.TermName "withSomeVkStruct")
                  tellImport "Foreign.Marshal.Utils" "maybeWith"
                  pure $ "maybeWith withSomeVkStruct" <+> marshalledNameDoc p
            pure $ MarshalScheme "pNext"
                                 p
                                 (Present tDoc ZeroNothing)
                                 (Negative [poke, SimplePoke])
          , case
              [ v
              | (s, c, v) <- fixedArrayLengths
              , s == Name (sName struct)
              , c == name p
              ]
            of
              []  -> empty
              [v] -> pure $ MarshalScheme
                "fixed array length"
                p
                Elided
                (Negative
                  [ WrappedPokePure $ do
                    tellQualifiedImport "Data.Vector" "length"
                    pure $ "fromIntegral $ Data.Vector.length" <+> pretty
                      (unName (getMarshalledName v))
                  , SimplePoke
                  ]
                )
              vs -> lift $ throwError "Multiple vectors for fixed array length"
          , case
              [ v
              | (s, c, v) <- cacheDataArrays
              , s == Name (sName struct)
              , c == name p
              ]
            of
              []  -> empty
              [v] -> pure $ MarshalScheme
                "Cache data length"
                p
                Elided
                (Negative
                  [ WrappedPokePure $ do
                    tellQualifiedImport "Data.ByteString" "length"
                    pure $ "fromIntegral $ Data.ByteString.length" <+> pretty
                      (unName (getMarshalledName v))
                  , SimplePoke
                  ]
                )
              vs -> lift $ throwError "Multiple vectors for cache array length"
          , case
              [ v
              | (s, _, v) <- cacheDataArrays
              , s == Name (sName struct)
              , v == name p
              ]
            of
              [v] -> do
                let tDoc = do
                      tellImport "Data.ByteString" "ByteString"
                      pure "ByteString"
                    poke = WrappedPokeContT $ do
                      tellImport "Data.ByteString.Unsafe" "unsafeUseAsCString"
                      tellImport "Data.Coerce"            "coerce"
                      tellImport "Foreign.Ptr"            "Ptr"
                      tellImport "Foreign.C.String"       "CString"
                      tellExtension "TypeApplications"
                      pure
                        $ parens ("unsafeUseAsCString" <+> marshalledNameDoc p)
                        <+> ". (. coerce @CString @(Ptr ()))"
                pure $ MarshalScheme "Cache data"
                                     p
                                     (Present tDoc ZeroMempty)
                                     (Negative [poke, SimplePoke])
              _ -> empty
          , do
            "VkShaderModuleCreateInfo" <- pure (sName struct)
            Name "codeSize"            <- pure $ name p
            let len = do
                  tellQualifiedImport "Data.ByteString" "length"
                  pure $ "fromIntegral $ Data.ByteString.length code"
            pure $ MarshalScheme "Shader module code size"
                                 p
                                 Elided
                                 (Negative [WrappedPokePure len, SimplePoke])
          , do
            "VkShaderModuleCreateInfo" <- pure (sName struct)
            Name "pCode"               <- pure $ name p
            let tDoc = do
                  tellImport "Data.ByteString" "ByteString"
                  pure "ByteString"
                poke = WrappedPokeContT $ do
                  tellImport "Data.ByteString.Unsafe" "unsafeUseAsCString"
                  tellImport "Data.Coerce"            "coerce"
                  tellImport "Foreign.Ptr"            "Ptr"
                  tellImport "Foreign.C.String"       "CString"
                  tellImport "Data.Word"              "Word32"
                  tellExtension "TypeApplications"
                  pure
                    $   parens ("unsafeUseAsCString" <+> marshalledNameDoc p)
                    <+> ". (. coerce @CString @(Ptr Word32))"
            pure $ MarshalScheme "Shader module code"
                                 p
                                 (Present tDoc ZeroMempty)
                                 (Negative [poke, SimplePoke])
          ]
  in  runMaybeT (asum schemes)
  where
     -- A list of (Struct name, length member name, array member name) for
     -- fixed sized arrays where only the first 'length member name' elements
     -- are valid'
    fixedArrayLengths
      :: [(Name Unmarshalled, Name Unmarshalled, Name Unmarshalled)]
    fixedArrayLengths =
      [ ( Name "VkPhysicalDeviceGroupProperties"
        , Name "physicalDeviceCount"
        , Name "physicalDevices"
        )
      , ( Name "VkPhysicalDeviceMemoryProperties"
        , Name "memoryTypeCount"
        , Name "memoryTypes"
        )
      , ( Name "VkPhysicalDeviceMemoryProperties"
        , Name "memoryHeapCount"
        , Name "memoryHeaps"
        )
      ]
    cacheDataArrays
      :: [(Name Unmarshalled, Name Unmarshalled, Name Unmarshalled)]
    cacheDataArrays =
      [ ( Name "VkPipelineCacheCreateInfo"
        , Name "initialDataSize"
        , Name "pInitialData"
        )
      , ( Name "VkValidationCacheCreateInfoEXT"
        , Name "initialDataSize"
        , Name "pInitialData"
        )
      ]

-- | This always takes priority
bespokeCommandScheme
  :: Command -> Parameter -> Write (Maybe (MarshalScheme Parameter))
bespokeCommandScheme c p =
  let schemes =
          [ do
              "vkRegisterObjectsNVX"      <- pure $ cName c
              Name "ppObjectTableEntries" <- pure $ name p
              Ptr Const t                 <- pure $ type' p
              let marshalType = Vector (Normal t)
              d <- case renderMarshalTypePrec 0 marshalType of
                Nothing -> pure Elided
                Just w
                  | Just z <- zeroMarshalType marshalType -> pure $ Present w z
                  | otherwise -> throwError "Present value with no zero type"
              pokes <- lift $ vectorToPointer p (Normal t) t
              pure $ MarshalScheme "ppObjectTableEntries" p d (Negative pokes)
          ]
  in  runMaybeT (asum schemes)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)
