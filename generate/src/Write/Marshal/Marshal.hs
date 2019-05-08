{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Write.Marshal.Marshal where

import           Control.Arrow                            ( second )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                          ( Text )
import qualified Data.Text.Extra               as T
import           Data.Maybe
import           Control.Monad.Except
import           Data.List.Extra                          ( intersperse
                                                          , partition
                                                          )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Control.Applicative                      ( Alternative(..) )
import           Control.Bool
import           Data.Foldable                            ( asum )

import           Spec.Savvy.Command
import           Spec.Savvy.Handle
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import qualified Write.Element                 as WE
import           Write.Marshal.Util
import           Write.Monad
import           Write.Monad.Lookup

marshalStructMembers :: Struct -> Write [MarshalScheme StructMember]
marshalStructMembers s@Struct {..} =
  traverseV id $ fmap (extractMarshalledMember s) sMembers

data Namespace
  = Unmarshalled
  | Marshalled

newtype Name (a :: Namespace) = Name {unName :: Text}
  deriving (Eq, Show)

msUnmarshalledName :: Param a => MarshalScheme a -> Name Unmarshalled
msUnmarshalledName = name . msParam

msMarshalledName :: Param a => MarshalScheme a -> Name Marshalled
msMarshalledName = getMarshalledName . msUnmarshalledName

unmarshalledParamNameDoc :: Param a => a -> Doc ()
unmarshalledParamNameDoc = pretty . unName . name

marshalledParamNameDoc :: Param a => a -> Doc ()
marshalledParamNameDoc = pretty . unName . getMarshalledName . name

-- TODO: Rename
data MarshalScheme a = MarshalScheme
  { msSchemeName :: Text
  , msParam :: a
  , msMarshalledType :: Maybe (WE (Doc ()))
  , msPokes :: [Poke]
  }

data Poke
  = SimplePoke
  | ComplexPoke (Doc () -> WE (Doc ()))
  | ContTPoke (WE (Doc ()))
    -- ^ A poke which doesn't bind any variables taking place in ContT
  | IOPoke (WE (Doc ()))
    -- ^ A poke which doesn't bind any variables
  | WrappedPokeIO (WE (Doc ()))
  | WrappedPokeContT (WE (Doc ()))
  | WrappedPokePure (WE (Doc ()))

----------------------------------------------------------------
-- Schemes
--
-- # Optional arrys:
--
-- - Optional Count, Optional Array:
--   The count can be zero, and the array can be null
--   Represent this as @Either Word32 (Vector a)@.
--   For example: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
--
-- - Optional Count, Non Optional array:
--   The array can be null if the count is 0.
--   Represent this as @Vector a@.
--   For example VkDescriptorSetLayoutCreateInfo
--
-- - Non Optional Count, Optional array:
--   The count must be greater than zero, the array can be null
--   Represent this as @Either Word32 (Vector a)@.
--   For example: VkPresentTimesInfoGOOGLE, VkPipelineViewportStateCreateInfo
--
-- - Non Optional Count, Non Optional array:
--   The array must have a size greater than 0, represent this as @Vector a@
--   and rely on the programmer to do the right thing, the validation layers
--   will catch it otherwise.
--   Represent this as @Vector a@.
--   For example VkDescriptorPoolCreateInfo
--
-- There are oddities such as VkDrmFormatModifierPropertiesListEXT which is
-- used for returning values under a mutable pointer.
--
-- Because we don't represent things as non-empty the optionalness of the
-- count is uninteresting.
--
-- The @Either Word32@ part can be dropped if there is a non-optional vector
-- with the same length
----------------------------------------------------------------

-- | This always takes priority
bespokeScheme
  :: Struct -> StructMember -> MaybeT Write (MarshalScheme StructMember)
bespokeScheme struct p =
  let
    schemes =
      [ do
        Name "pNext" <- pure $ name p
        Ptr _ Void   <- pure $ type' p
        let
          tDoc = do
            tellSourceDepend (WE.TypeName "SomeVkStruct")
            pure "Maybe SomeVkStruct"
          poke = WrappedPokeContT $ do
            tellSourceDepend (WE.TermName "withSomeVkStruct")
            pure
              $   "maybeWith withSomeVkStruct"
              <+> marshalledParamNameDoc p
        pure $ MarshalScheme "pNext" p (Just tDoc) [poke, SimplePoke]
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
            Nothing
            [ WrappedPokePure $ do
              tellQualifiedImport "Data.Vector" "length"
              pure $ "fromIntegral $ Data.Vector.length" <+> pretty
                (unName (getMarshalledName v))
            , SimplePoke
            ]
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
            Nothing
            [ WrappedPokePure $ do
              tellQualifiedImport "Data.ByteString" "length"
              pure $ "fromIntegral $ Data.ByteString.length" <+> pretty
                (unName (getMarshalledName v))
            , SimplePoke
            ]
          vs -> lift $ throwError "Multiple vectors for cache array length"
      , case
          [ v
          | (s, _, v) <- cacheDataArrays
          , s == Name (sName struct)
          , v == name p
          ]
        of
          [v] -> do
            let
              tDoc = do
                tellImport "Data.ByteString" "ByteString"
                pure "ByteString"
              poke = WrappedPokeContT $ do
                tellImport "Data.ByteString.Unsafe"   "unsafeUseAsCString"
                tellImport "Data.Coerce"              "coerce"
                tellExtension "TypeApplications"
                pure
                  $ "coerce @_ @((Ptr () -> IO a) -> IO a) unsafeUseAsCString"
                  <+> pretty (unName v)
            pure $ MarshalScheme "Cache data" p (Just tDoc) [poke, SimplePoke]
          _ -> empty
      , do
        "VkShaderModuleCreateInfo" <- pure (sName struct)
        Name "codeSize"            <- pure $ name p
        pure $ MarshalScheme
          "Shader module code size"
          p
          Nothing
          [WrappedPokePure (pure "SHADER LENGTH"), SimplePoke]
      , do
        "VkShaderModuleCreateInfo" <- pure (sName struct)
        Name "pCode"               <- pure $ name p
        let tDoc = do
              tellImport "Data.ByteString" "ByteString"
              pure "ByteString"
            poke = WrappedPokeContT $ do
              tellImport "Data.ByteString.Unsafe"   "unsafeUseAsCString"
              pure $ "unsafeUseAsCString" <+> marshalledParamNameDoc p
        pure $ MarshalScheme "Shader module code"
                             p
                             (Just tDoc)
                             [poke, SimplePoke]
      ]
  in  asum schemes
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

-- | The "default" method of marshalling something, store as ordinary member
scalarScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
scalarScheme p = do
  -- Sometimes pointers to non-optional structs are used, remove these for the
  -- marshalled version.
  t <- case type' p of
    t@(Ptr Const (TypeName n)) -> isStruct n >>= \case
      Nothing -> pure t
      Just _  -> pure $ TypeName n
    t -> pure t
  let tDoc = toHsType . deepMarshalledType $ t
  -- Some sanity checking
  guard . isNothing . values $ p
  guard . isNothing . lengths $ p
  guard ((isNothing . isOptional $ p) || ((== Just [False]) . isOptional $ p))
  guard . (/= Void) $ t
  guard $ not (isPtrType t) || isPassAsPointerType (unPtrType t)
  pure $ MarshalScheme "scalar" p (Just tDoc) [SimplePoke]

univaluedScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
univaluedScheme p = do
  [value] <- values p
  pure $ MarshalScheme "univalued"
                       p
                       Nothing
                       [WrappedPokePure (pure (pretty value)), SimplePoke]

arrayScheme :: Param a => [a] -> a -> MaybeT Write (MarshalScheme a)
arrayScheme ps p = do
  case lengths p of
    Just (NamedLength _ : _) -> do
      Ptr Const t <- pure $ type' p
      -- TODO: find a non-optional sibling, or make the type Either Word32...
      let
        (eDoc, fill) = case t of
          Ptr Const c | isByteArrayElem c -> case isOptional p of
            Just [True] ->
              ( do
                tellImport "Data.ByteString" "ByteString"
                pure "Maybe ByteString"
              , ContTPoke (pure "WRITE MAYBE BS")
              )
            _ ->
              ( do
                tellImport "Data.ByteString" "ByteString"
                pure "ByteString"
              , ContTPoke $ do
                tellImport "Data.Foldable"            "traverse_"
                tellImport "Control.Monad.Trans.Cont" "ContT(..)"
                tellImport "Data.ByteString"          "useAsCString"
                tellImport "Foreign.Storable"         "pokeElemOff"
                tellQualifiedImport "Data.Vector" "imapM_"
                pure
                  $   "Data.Vector.imapM_"
                  <+> parens
                        (   "\\i bs -> ContT (useAsCString bs) >>="
                        <+> parens
                              (   "lift . pokeElemOff"
                              <+> unmarshalledParamNameDoc p
                              <+> "i"
                              )
                        )
                  <+> marshalledParamNameDoc p
              )
          _ ->
            ( toHsTypePrec 10 (deepMarshalledType t)
            , IOPoke $ do
              tellQualifiedImport "Data.Vector" "imapM_"
              tellImport "Foreign.Storable" "pokeElemOff"
              pure
                $   "Data.Vector.imapM_"
                <+> parens ("pokeElemOff" <+> unmarshalledParamNameDoc p)
                <+> marshalledParamNameDoc p
            )
        vDoc = do
          e <- eDoc
          tellImport "Data.Vector" "Vector"
          pure $ "Vector" <+> e
        tDoc = vDoc
        -- Allocate the array
        poke = WrappedPokeContT $ do
          elemTyDoc <- toHsTypePrec 10 t
          tellExtension "TypeApplications"
          tellQualifiedImport "Data.Vector" "length"
          tellImport "Foreign.Storable"      "sizeOf"
          tellImport "Foreign.Storable"      "alignment"
          tellImport "Foreign.Marshal.Alloc" "allocaBytesAligned"
          let elemSize = "sizeOf @" <> elemTyDoc <+> "undefined"
              len      = "Data.Vector.length" <+> marshalledParamNameDoc p
              size     = parens $ elemSize <+> "*" <+> len
              align    = parens $ "alignment @" <> elemTyDoc <+> "undefined"
          pure $ "allocaBytesAligned @" <> elemTyDoc <+> size <+> align
      pure $ MarshalScheme "array" p (Just tDoc) [poke, fill, SimplePoke]
    Just [NullTerminated] -> do
      Ptr Const c <- pure $ type' p
      guard $ isByteArrayElem c
      let tDoc = do
            tellImport "Data.ByteString" "ByteString"
            pure $ case isOptional p of
              Just [True] -> "Maybe ByteString"
              _           -> "ByteString"
      pure $ MarshalScheme
        "C string"
        p
        (Just tDoc)
        [ WrappedPokeContT $ do
          tellImport "Data.ByteString" "useAsCString"
          pure
            $   "useAsCString"
            <+> (pretty . unName . getMarshalledName . name $ p)
        , SimplePoke
        ]
    _ -> empty

fixedArrayScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
fixedArrayScheme p = do
  guard . isNothing . lengths $ p
  case type' p of
    Array _ (SymbolicArraySize s) t | isByteArrayElem t -> do
      let tDoc = do
            tellImport "Data.ByteString" "ByteString"
            pure "ByteString"
          poke = ComplexPoke $ \offset -> do
            let f = case t of
                  Char -> "pokeFixedLengthNullTerminatedByteString"
                  _    -> "pokeFixedLengthByteString"
            pure $ f <+> pretty s <+> offset <+> marshalledParamNameDoc p

      pure $ MarshalScheme "fixed array" p (Just tDoc) [poke]
    Array _ (SymbolicArraySize s) t -> do
      let
        tDoc = do
          e <- toHsTypePrec 10 (deepMarshalledType t)
          tellImport "Data.Vector" "Vector"
          pure $ "Vector" <+> e
        poke = ComplexPoke $ \offset -> do
          let f = "pokeElemOff" <+> offset
              v = "Data.Vector.take" <+> pretty s <+> marshalledParamNameDoc p
          tellDepend $ WE.PatternName s
          tellImport "Foreign.Storable" "pokeElemOff"
          tellImport "Foreign.Storable" "Ptr"
          tellQualifiedImport "Data.Vector" "take"
          tellQualifiedImport "Data.Vector" "imapM_"
          pure $ "Data.Vector.imapM_" <+> parens f <+> parens v
      pure $ MarshalScheme "fixed array" p (Just tDoc) [poke]
    Array _ (NumericArraySize n) t -> do
      let tDoc = do
            e <- if isByteArrayElem t
              then do
                tellImport "Data.ByteString" "ByteString"
                pure "ByteString"
              else toHsType (deepMarshalledType t)
            pure (tupled (replicate (fromIntegral n) e))
          poke = ComplexPoke $ \offset -> do
            let vars =
                  (marshalledParamNameDoc p <>) . pretty . show <$> [1 .. n]
            pure
              $   "let"
              <+> tupled vars
              <+> "="
              <+> marshalledParamNameDoc p
              <+> "in"
              <+> hsep
                    (punctuate
                      " >>"
                      [ "pokeElemOff" <+> offset <+> pretty (show i) <+> v
                      | (v, i) <- zip vars [0 ..]
                      ]
                    )

      pure $ MarshalScheme "fixed array" p (Just tDoc) [poke]
    _ -> empty

-- | Matches if this parameter is the length of one or more vectors
lengthScheme :: Param a => [a] -> a -> MaybeT Write (MarshalScheme a)
lengthScheme ps p = do
  -- Get the vectors which are sized with this parameter
  vs <- pure $ getSizedWith (name p) ps
  -- Make sure they are not all void pointer, those do not have the length
  -- elided
  guard (any (\v -> type' v /= Ptr Const Void) $ vs)
  poke <- case partition (maybe False head . isOptional) vs of
    -- Make sure they exist
    ([] , []) -> empty
    ([o], []) -> pure $ WrappedPokePure $ do
      tellQualifiedImport "Data.Vector" "length"
      tellImport "Data.Either" "either"
      pure
        $   "either id (fromIntegral . Data.Vector.length)"
        <+> marshalledParamNameDoc o
    (o : os, [] ) -> error "TODO: handle multiple optional vectors"
    ([]    , [v]) -> pure $ WrappedPokePure $ do
      tellQualifiedImport "Data.Vector" "length"
      pure $ "fromIntegral $ Data.Vector.length" <+> marshalledParamNameDoc v
    (opts, v : vs) -> pure $ WrappedPokeIO $ do
      tellQualifiedImport "Data.Vector" "length"
      tellQualifiedImport "Data.Vector" "length"
      tyDoc <- toHsType (type' p)
      pure
        $   "let l = Data.Vector.length"
        <+> marshalledParamNameDoc v
        <+> "in if"
        <+> (hsep $ intersperse
              "&&"
              ((("l == Data.Vector.length" <+>) . marshalledParamNameDoc <$> vs)
              ++ (   (\n ->
                       parens
                         $   "Data.Vector.null"
                         <+> n
                         <+> "|| l == Data.Vector.length"
                         <+> n
                     )
                 .   marshalledParamNameDoc
                 <$> opts
                 )
              )
            )
        <+> "then fromIntegral l ::"
        <+> tyDoc
        <+> "else _"

  pure $ MarshalScheme "length" p Nothing [poke, SimplePoke]

-- | An optional value with a default, so we don't need to wrap it in a Maybe
optionalDefaultScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
optionalDefaultScheme p = do
  Just [True] <- pure $ isOptional p
  guard . isNothing . lengths $ p
  guard =<< lift (isDefaultable (type' p))
  let tDoc = do
        e <- toHsType . deepMarshalledType . type' $ p
        pure e
  pure $ MarshalScheme "optional default" p (Just tDoc) [SimplePoke]

-- | An optional value to be wrapped in a Maybe
optionalScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
optionalScheme p = do
  Just [True] <- pure $ isOptional p
  guard . isNothing . lengths $ p
  let tDoc = do
        e <- toHsTypePrec 10 . deepMarshalledType . unPtrType . type' $ p
        pure ("Maybe " <> e)
  pure $ MarshalScheme "optional" p (Just tDoc) [SimplePoke] -- TODO: this is wrong if this is a ptr

-- | Matches const and non-const void pointers, exposes them as 'Ptr ()'
voidPointerScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
voidPointerScheme p = do
  Ptr _ Void <- pure $ type' p
  let tDoc = do
        tellImport "Foreign.Ptr" "Ptr"
        pure "Ptr ()"
  pure $ MarshalScheme "void pointer" p (Just tDoc) [SimplePoke]

returnPointerScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
returnPointerScheme p = do
  Ptr NonConst t <- pure $ type' p
  let tDoc = toHsType $ type' p
  pure $ MarshalScheme "void pointer" p (Just tDoc) [SimplePoke]

----------------------------------------------------------------
--
----------------------------------------------------------------

class Param a where
  name    :: a -> Name Unmarshalled
  values  :: Alternative f => a -> f [Text]
  type'   :: a -> Type
  lengths :: Alternative f => a -> f [ParameterLength]
  isOptional :: a -> Maybe [Bool]

instance Param StructMember where
  name       = Name . smName
  values     = maybe empty pure . smValues
  type'      = smType
  lengths    = maybe empty pure . fmap (fmap lengthStringToParamLength) . smLengths
  isOptional = smIsOptional

instance Param Parameter where
  name       = Name . pName
  values     = const empty
  type'      = pType
  lengths    = maybe empty pure . fmap pure . pLength
  isOptional = pIsOptional

data Optional = NonOptional | Optional
  deriving (Show)

getOptional :: Param a => a -> Optional
getOptional = bool NonOptional Optional . isJust . isOptional


extractMarshalledMember
  :: Struct
  -> StructMember
  -> Write (MarshalScheme StructMember)
extractMarshalledMember s@Struct {..} member = do
  let schemes =
        [ -- Anything we specifically single out should go first
          bespokeScheme s
          -- These two are for value constrained params:
        , univaluedScheme
        , lengthScheme sMembers
          -- Pointers to Void have some special handling
        , voidPointerScheme
          -- Pointers to return values in
        , returnPointerScheme
          -- Optional and non optional arrays
        , arrayScheme sMembers
        , fixedArrayScheme
          -- Optional things:
        , optionalDefaultScheme
        , optionalScheme
          -- Everything left over is treated as a boring scalar parameter
        , scalarScheme
        ]
  m <- runMaybeT . asum . fmap ($ member) $ schemes
  case m of
    Just x -> pure x
    Nothing ->
      throwError
        $     sName
        <>    "."
        <>    smName member
        T.<+> "is not handled by any marshalling scheme"

----------------------------------------------------------------
-- Type utils
----------------------------------------------------------------

marshalledType :: Type -> Type
marshalledType t
  | Just "VkBool32" <- simpleTypeName t
  = TypeName "Bool"
  | Just tyName <- simpleTypeName t, Just n <- T.dropPrefix "Vk" tyName
  = TypeName n
  | otherwise
  = t

deepMarshalledType :: Type -> Type
deepMarshalledType = \case
  Float           -> TypeName "Float"
  Ptr cv t        -> Ptr cv (deepMarshalledType t)
  Array cv size t -> Array cv size (deepMarshalledType t)
  Proto t ps' ->
    Proto (deepMarshalledType t) (second deepMarshalledType <$> ps')
  t -> marshalledType t

----------------------------------------------------------------
-- Defaulting types
----------------------------------------------------------------

isDefaultable :: Type -> Write Bool
isDefaultable t = do
  isBitmask'              <- (isJust .) <$> asks lIsBitmask
  isNonDispatchableHandle <-
    (maybe False (\h -> hHandleType h == NonDispatchable) .) <$> asks lIsHandle
  pure $ isDefaultableForeignType t || isIntegral t || maybe
    False
    (isNonDispatchableHandle <||> isBitmask')
    (simpleTypeName t)

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
  _                          -> False

isDefaultableForeignType :: Type -> Bool
isDefaultableForeignType =
  (`elem` [ TypeName "HANDLE"
          , TypeName "DWORD"
          , TypeName "LPCWSTR"
          , TypeName "PFN_vkInternalAllocationNotification"
          , TypeName "PFN_vkInternalFreeNotification"
          , TypeName "PFN_vkAllocationFunction"
          , TypeName "PFN_vkReallocationFunction"
          , TypeName "PFN_vkFreeFunction"
          , Ptr Const (TypeName "SECURITY_ATTRIBUTES")
          ]
  )

----------------------------------------------------------------
-- Rendering
----------------------------------------------------------------

data PokeSet = PokeSet
  { pokeSetContT :: [Doc ()]
  , pokeSetIO    :: [Doc ()]
  , pokeSetPure  :: [Doc ()]
  , pokeSetPoke  :: [Doc ()]
  }

instance Semigroup PokeSet where
  PokeSet a1 b1 c1 d1 <> PokeSet a2 b2 c2 d2 =
    PokeSet (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Monoid PokeSet where
  mempty = PokeSet [] [] [] []

renderStructMemberPoke :: MarshalScheme StructMember -> WE PokeSet
renderStructMemberPoke ms@MarshalScheme {..} = do
  let StructMember {..} = msParam
      name              = pretty . unName . msUnmarshalledName $ ms
  tyDoc' <- toHsTypePrec 10 smType
  tellImport "Foreign.Ptr" "plusPtr"
  let offset =
        parens
          $   "p `plusPtr`"
          <+> pretty (show smOffset)
          <+> "::"
          <+> "Ptr"
          <+> tyDoc'
  case msPokes of
    [WrappedPokePure value', SimplePoke] -> do
      value <- value'
      tyDoc <- toHsType smType
      pure $ PokeSet []
                     []
                     []
                     ["poke" <+> offset <+> parens (value <+> "::" <+> tyDoc)]
    _ -> flip foldMap msPokes $ \case
      SimplePoke -> do
        tellImport "Foreign.Storable" "poke"
        pure $ PokeSet [] [] [] ["poke" <+> offset <+> name]
      ComplexPoke d     -> PokeSet [] [] [] . pure <$> d offset
      ContTPoke   poke' -> do
        poke <- poke'
        pure $ PokeSet [poke] [] [] []
      IOPoke poke' -> do
        poke <- poke'
        pure $ PokeSet [] [poke] [] []
      WrappedPokeContT wrapFun' -> do
        wrapFun <- wrapFun'
        tellImport "Control.Monad.Trans.Cont" "ContT(..)"
        pure $ PokeSet [name <+> "<- ContT $" <+> wrapFun] [] [] []
      WrappedPokeIO value' -> do
        value <- value'
        pure $ PokeSet [] [name <+> "<-" <+> value] [] []
      WrappedPokePure value' -> do
        value <- value'
        pure $ PokeSet [] [] ["let" <+> name <+> "=" <+> value] []

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

getSizedWith :: Param a => Name Unmarshalled -> [a] -> [a]
getSizedWith lengthName ps = filter isVector ps
  where
    isVector v = case lengths v of
      Just (NamedLength len : _) | Name len == lengthName -> True
      -- ^ TODO: Change this to [NamedLength len] and think about handling
      _ -> False

allWithOthers :: (a -> [a] -> b) -> [a] -> [b]
allWithOthers f xs =
  [ f b (as ++ bs)
  | n <- [0 .. length xs - 1]
  , let (as, b : bs) = splitAt n xs
  ]

getMarshalledName :: Name Unmarshalled -> Name Marshalled
getMarshalledName (Name n) =
  Name (unReservedWord . T.lowerCaseFirst . dropPointer $ n)

unPtrType :: Type -> Type
unPtrType = \case
  Ptr _ t -> t
  t       -> t

isByteArrayElem :: Type -> Bool
isByteArrayElem = \case
  Void               -> True
  Char               -> True
  TypeName "uint8_t" -> True
  _                  -> False

-- TODO: Change for 32 bit library
pointerAlignment :: Word
pointerAlignment = 8

