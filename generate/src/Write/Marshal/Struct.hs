{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Write.Marshal.Struct
  ( structWrapper
  , vkStructWriteElement
  ) where

import           Control.Arrow                            ((&&&))
import           Control.Bool
import           Control.Category                         ((>>>))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer.Strict              hiding ((<>))
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.List                                (partition)
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Monoid                              (Endo (..))
import qualified Data.MultiMap                            as MultiMap
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Traversable
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import qualified Spec.Savvy.Type.Haskell                  as H

import           Documentation
import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Marshal.Monad
import           Write.Marshal.Util
import           Write.Marshal.Wrap
import           Write.Struct
import           Write.Util

structWrapper
  :: (Text -> Bool)
  -- ^ Is this the name of a handle type
  -> (Text -> Bool)
  -- ^ Is this the name of a bitmask type
  -> (Text -> Bool)
  -- ^ Is this the name of a struct type
  -> Struct
  -> Either [SpecError] WriteElement
structWrapper isHandle isBitmask isStruct struct = do
  let weName = sName struct T.<+> "wrapper"
      isDefaultable t = maybe
        False
        (isHandle <||> isBitmask <||> isDefaultableForeignType)
        (simpleTypeName t)
      isStructType t = maybe False isStruct (simpleTypeName t)
  (weDoc, (weImports, weProvides, weDepends, weExtensions, _)) <- either
    (throwError . fmap (WithContext (sName struct)))
    (pure . first (fmap vcat . sequence))
    (runWrap $ wrapStruct isDefaultable isStructType struct)
  pure WriteElement {..}

vkStructWriteElement :: WriteElement
vkStructWriteElement =
  let
    weName = "ToCStruct class declaration"
    weImports =
      [ Import "Foreign.Storable"      ["Storable", "poke", "pokeElemOff", "peek", "peekElemOff"]
      , Import "Foreign.Ptr"           ["Ptr"]
      , Import "Foreign.Marshal.Alloc" ["alloca"]
      , Import "Foreign.Marshal.Array" ["allocaArray"]
      , Import "Data.Vector"           ["Vector", "ifoldr"]
      , QualifiedImport "Data.Vector.Generic"
                        ["length", "take", "replicate", "fromList", "Vector", "(++)"]
      , QualifiedImport "Data.Vector.Generic.Sized.Internal" ["Vector(Vector)"]
      , QualifiedImport "Data.Vector.Generic.Sized" ["Vector"]
      , QualifiedImport "Data.Vector.Sized"         ["Vector"]
      , Import          "Data.Proxy"                ["Proxy(Proxy)"]
      , Import          "Data.ByteString"
                        ["ByteString", "take", "unpack"]
      , Import "GHC.TypeNats" ["natVal", "KnownNat", "type (<=)"]
      , Import "Foreign.C.Types" ["CChar(..)"]
      , Import "Data.Word" ["Word8"]
      , QualifiedImport "Data.Vector.Generic"
                        ["cons", "empty"]
      ]
    weProvides = [Unguarded $ WithConstructors $ WE.TypeName "ToCStruct"]
    weDepends  = []
    weExtensions =
      [ "FunctionalDependencies"
      , "RankNTypes"
      , "ScopedTypeVariables"
      , "TypeApplications"
      ]
    weDoc = pure [qci|
      class ToCStruct marshalled c | marshalled -> c, c -> marshalled where
        withCStruct :: marshalled -> (c -> IO a) -> IO a
      class FromCStruct marshalled c | marshalled -> c, c -> marshalled where
        fromCStruct :: c -> IO marshalled

      withCStructPtr :: (Storable c, ToCStruct a c) => a -> (Ptr c -> IO b) -> IO b
      withCStructPtr a f = withCStruct a (\c -> alloca (\p -> poke p c *> f p))

      fromCStructPtr :: (Storable c, FromCStruct a c) => Ptr c -> IO a
      fromCStructPtr p = fromCStruct =<< peek p

      fromCStructPtrElem :: (Storable c, FromCStruct a c) => Ptr c -> Int -> IO a
      fromCStructPtrElem p o = fromCStruct =<< peekElemOff p o

      withArray
        :: forall a b d
         . (a -> (b -> IO d) -> IO d)
        -> Vector a
        -> (Vector b -> IO d)
        -> IO d
      withArray alloc v cont =
        let go :: Int -> a -> (Vector b -> IO d) -> (Vector b -> IO d)
            go index x complete v = alloc x (\b -> complete (Data.Vector.Generic.cons b v))
        in  ifoldr go cont v (Data.Vector.Generic.empty)
        -- | Pad or trucate a vector so that it has the required size

      withVec
        :: forall a b d
         . Storable b
        => (a -> (b -> IO d) -> IO d)
        -> Vector a
        -> (Ptr b -> IO d)
        -> IO d
      withVec alloc v cont = allocaArray (Data.Vector.length v) $ \p ->
        let go :: Int -> a -> IO d -> IO d
            go index x complete = alloc x (\b -> pokeElemOff p index b *> complete)
        in  ifoldr go (cont p) v
        -- | Pad or trucate a vector so that it has the required size

      -- | Pad or trucate a vector so that it has the required size
      padSized
        :: forall n a v
         . (KnownNat n, Data.Vector.Generic.Vector v a)
        => a
        -- ^ The value with which to pad if the given vector is too short
        -> v a
        -- ^ The vector to pad or truncate
        -> Data.Vector.Generic.Sized.Vector v n a
      padSized p v = Data.Vector.Generic.Sized.Internal.Vector padded
        where
          padded :: v a
          padded =
            let n = fromIntegral (natVal (Proxy @n))
                m = Data.Vector.Generic.length v
            in  case m `compare` n of
                  LT -> v Data.Vector.Generic.++ (Data.Vector.Generic.replicate (n - m) p)
                  EQ -> v
                  GT -> Data.Vector.Generic.take n v

      -- | Convert a bytestring to a null terminated sized vector. If the bytestring
      -- is too long it will be truncated.
      byteStringToNullTerminatedSizedVector
        :: forall n v
         . (KnownNat n, 1 <= n, Data.Vector.Generic.Vector v CChar)
        => ByteString
        -> Data.Vector.Generic.Sized.Vector v n CChar
      byteStringToNullTerminatedSizedVector bs = padSized
        (CChar 0)
        (byteStringToVector (Data.ByteString.take predN bs))
        where
          predN = pred (fromIntegral (natVal (Proxy @n)))
          byteStringToVector =
            Data.Vector.Generic.fromList . fmap fromIntegral . Data.ByteString.unpack

      -- | Convert a bytestring to a sized vector. If the bytestring is too
      -- long it will be truncated. If it is too short it will be zero padded
      byteStringToSizedVector
        :: forall n v
         . (KnownNat n, Data.Vector.Generic.Vector v Word8)
        => ByteString
        -> Data.Vector.Generic.Sized.Vector v n Word8
      byteStringToSizedVector bs = padSized
        0
        (byteStringToVector (Data.ByteString.take n bs))
        where
          n                  = fromIntegral (natVal (Proxy @n))
          byteStringToVector = Data.Vector.Generic.fromList . Data.ByteString.unpack

    |]
  in WriteElement{..}

----------------------------------------------------------------
-- The wrapping commands
----------------------------------------------------------------

wrapStruct
  :: (Type -> Bool)
  -- ^ Is a defaultable type
  -> (Type -> Bool)
  -- ^ Is this a struct
  -> Struct
  -> WrapM [DocMap -> Doc ()]
wrapStruct isDefaultable isStruct s = do
  marshalled    <- marshallStruct isStruct isDefaultable s
  toCStructDoc  <- writeToCStructInstance marshalled
  fromCStructDoc  <- writeFromCStructInstance marshalled
  marshalledDoc <- writeMarshalled marshalled
  tellExtension "DuplicateRecordFields"
  let weDoc docMap = vsep [marshalledDoc docMap, toCStructDoc, fromCStructDoc]
  pure [weDoc]

----------------------------------------------------------------
--
----------------------------------------------------------------

data MarshalledStruct = MarshalledStruct
  { msName          :: Text
  , msMembers       :: [MemberUsage MarshalledMember]
  , msStructOrUnion :: StructOrUnion
  }

data MarshalledMember = MarshalledMember
  { mmName :: Text
  , mmType :: Doc ()
  }
  deriving (Show)

data MemberUsage a
  = Univalued Text
    -- ^ This is a type which is allowed just one possible value, so we elide
    -- it
  | Length Text
    -- ^ This is bound to be the length of an array
  | OptionalLength Text
    -- ^ This is bound to be the length of an optional array
  | FixedArrayValidCount Text
    -- ^ This is the number of valid elements in a fixed length array
  | FixedArray
      Text --- ^ Length member
      Text --- ^ Data member
      ArraySize --- ^ max size
      Type --- ^ elem type
      (Doc ()) --- ^ allocator
      (Doc ()) --- ^ dummy
    -- ^ An array with a fixed size but with a certain number of valid elements
    -- Contains the code for allocating a member and the code for generating a
    -- dummy member
  | FixedArrayNullTerminated
      Text --- ^ member name
      ArraySize --- ^ max array size
    -- ^ An array with a fixed size but with null terminated @char@ data
  | FixedArrayZeroPadByteString Text ArraySize
    -- ^ An array with a fixed size but with zero padded @char@ data
  | FixedArrayZeroPad
      Text --- ^ member name
      ArraySize --- ^ length
      Type --- ^ element type
    -- ^ An array with a fixed size but with zero padded data
  | FixedArrayTuple
      Text --- ^ member name
      Word --- ^ length
      Type --- ^ element type
    -- ^ A fixed size array represented as a tuple
  | Preserved a
    -- ^ This is preserved without changes
  | PreservedMarshalled
      Text --- ^ member name
      Text --- ^ member type
    -- ^ An ordinary member, using the marshalled struct
  | NextPointer Text
    -- ^ This is the pNext pointer (include the member name for completeness)
  | ByteString Text
    -- ^ A const char with member name
  | ByteStringData
      Text --- ^ length member name
      Text --- ^ data member name
    -- ^ A void pointer to some data with member name
  | ByteStringLength Text
    -- ^ The length of a bytestring
  | MaybeByteString Text
    -- ^ An optional const char with member name
  | Vector
      Text --- ^ The name of the member in the C struct which contains the
           -- length
      Text --- ^ member name
      Type --- ^ element type
      (Doc ()) --- ^ allocator
    -- ^ A vector with name and of type with the alloc function for the members
  | OptionalVector
      Text --- ^ The name of the member in the C struct which contains the
           -- length
      Text --- ^ member name
      Type --- ^ element type
      (Doc ()) --- ^ allocator
    -- ^ An optional vector with name and type and allocator, represented with
    -- Maybe.
  | OptionalPtr Text Type (Doc ())
    -- ^ An optional value with name and type and allocator, represented with
    -- Maybe.
  | NonOptionalPtr Text Type (Doc ())
    -- ^ An value with name and type and allocator
  | EnabledFlag Text
    -- ^ A VkBool32 type which is true iff another value is not Nothing
  | OptionalZero
      Text --- ^ member name
      Type --- ^ type
    -- ^ An optional value with name and type which is written as 0 if it is not present
  | SiblingVectorMaster Text [(Type, Doc ())]
    -- ^ A vector which shares its length with other vectors, represented as a
    -- vector of tuples here
  | SiblingVectorSlave Text
    -- ^ A vector which shares its length with other vectors, stored in the
    -- master vector as a tuple
  | MinLength [Text] [Text]
    -- ^ The minimum length of several vectors and several optional vectors
  deriving (Functor, Foldable, Traversable, Show)

----------------------------------------------------------------
-- Changing the member type
----------------------------------------------------------------

marshallStruct :: (Type -> Bool) -> (Type -> Bool) -> Struct -> WrapM MarshalledStruct
marshallStruct isStruct isDefaultable s@Struct {..} = do
  let lengthRelation = getLengthRelation s sMembers
  MarshalledStruct (dropVkType sName)
    <$> traverse (marshallMember isStruct isDefaultable lengthRelation s) sMembers
    <*> pure sStructOrUnion

-- This may
marshallMember
  :: (Type -> Bool)
  -- Is this a struct type
  -> (Type -> Bool)
  -- Is this a defaultable type
  -> [(StructMember, [StructMember])]
  -- ^ The relation between lengths and arrays (pointers)
  -> Struct
  -- ^ The struct this member inhabits
  -> StructMember
  -> WrapM (MemberUsage MarshalledMember)
marshallMember isStruct isDefaultable lengthRelation struct m = do
  let
    -- Go from a length name to a list of vectors having that length
    -- TODO: Rename
      lengthMap :: Text -> Maybe [StructMember]
      lengthMap =
        (`Map.lookup` Map.fromList [ (smName l, v) | (l, v) <- lengthRelation ])

      findLengthMember :: Text -> Maybe StructMember
      findLengthMember =
        (`Map.lookup` Map.fromList [ (smName l, l) | (l, _) <- lengthRelation ])

  ty <- case smType m of
    _ | Just [v] <- smValues m, Nothing <- smIsOptional m -> pure (Univalued v)
    t | isNonMarshalledType t -> Preserved <$> toHsType (smType m)
    Ptr _ t | isPassAsPointerType t, Nothing <- smIsOptional m ->
      Preserved <$> toHsType (smType m)
    Ptr cv Void
      | -- The pointer to the next element in the struct chain
        smName m == "pNext", Nothing <- smLengths m
      -> pure (NextPointer (smName m))
      | -- A void pointer consumed by the user and not Vulkan
        Nothing <- smLengths m
      -> Preserved <$> toHsType (smType m)
      | -- A pointer to data to be consumed by Vulkan (has a length)
        Nothing <- smIsOptional m, Just [length] <- smLengths m, Const <- cv, Just lengthMember <- findLengthMember length
      -> pure $ ByteStringData (smName lengthMember)
                               (smName m)
    Ptr Const Char
      | Nothing <- smIsOptional m, Just ["null-terminated"] <- smLengths m
      -> pure $ ByteString (smName m)
    Ptr Const Char
      | Just [True] <- smIsOptional m, Just ["null-terminated"] <- smLengths m
      -> pure $ MaybeByteString (smName m)
    Ptr _ t
      | -- An array
        Nothing <- smIsOptional m
      , Just [length] <- smLengths m
      , Just lengthMember <- findLengthMember length
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ Vector length
                           (smName m)
                           (TypeName (dropVkType tyName))
                           "withCStruct"
        else pure $ Vector (smName lengthMember) (smName m) t "(flip ($))"
      | -- An optional array
        Just [True] <- smIsOptional m
      , Just [length] <- smLengths m
      , Just tyName <- simpleTypeName t
      , Just lengthMember <- findLengthMember length
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ OptionalVector (smName lengthMember)
                                   (smName m)
                                   (TypeName (dropVkType tyName))
                                   "withCStruct"
        else pure
          $ OptionalVector (smName lengthMember) (smName m) t "(flip ($))"
      | -- An array of strings
        Nothing <- smIsOptional m
      , Just [length, "null-terminated"] <- smLengths m
      , Ptr Const Char <- t
      -> do
        tellImport "Data.ByteString" "useAsCString"
        tellImport "Data.ByteString" "ByteString"
        pure $ Vector length (smName m) (TypeName "ByteString") "useAsCString"
      | -- An optional pointer to a single value (no length)
        Just [True] <- smIsOptional m
      , Nothing <- smLengths m
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ OptionalPtr (smName m)
                                (TypeName (dropVkType tyName))
                                "withCStructPtr"
        else do
          tellImport "Foreign.Marshal.Utils" "with"
          pure $ OptionalPtr (smName m) t "with"
      | -- An pointer to a single value (no length)
        Nothing <- smIsOptional m
      , Nothing <- smLengths m
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ NonOptionalPtr (smName m)
                                   (TypeName (dropVkType tyName))
                                   "withCStructPtr"
        else do
          tellImport "Foreign.Marshal.Utils" "with"
          pure $ NonOptionalPtr (smName m) t "with"
    (Array _ size t)
      | SymbolicArraySize sizeSymbol <- size
      , sizeSymbol `elem` ["VK_UUID_SIZE", "VK_LUID_SIZE"]
      , TypeName "uint8_t" <- t
      , Nothing <- smIsOptional m
      , Nothing <- smLengths m
      , sReturnedOnly struct
      -> pure $ FixedArrayZeroPadByteString (smName m) size
      | SymbolicArraySize sizeSymbol <- size
      , (sName struct, smName m) `elem` zeroPadMembers
      , isSimpleType t
      , Nothing <- smIsOptional m
      , Nothing <- smLengths m
      , sReturnedOnly struct
      -> pure $ FixedArrayZeroPad (smName m) size t
      | isSimpleType t
      , Nothing <- smIsOptional m
      , Nothing <- smLengths m
      , [countName] <-
        [ countName
        | (structName, countName, vecName) <- fixedArrayLengths
        , structName == sName struct
        , vecName == smName m
        ]
      , Just tyName <- simpleTypeName t
      , Just countMember <- findLengthMember countName
      -> let alloc = if isStruct t
               then -- If this is a struct, use the marshalled version
                    "withCStruct"
               else "(flip ($))"
             ty = if isStruct t then (TypeName (dropVkType tyName)) else t
         in  FixedArray (smName countMember) (smName m) size ty alloc <$> dummyMember t
      | Char <- t
      , Nothing <- smIsOptional m
      , Nothing <- smLengths m
      , sReturnedOnly struct
      , [()] <-
        [ ()
        | (structName, vecName) <- nullTerminatedFixedArrays
        , structName == sName struct
        , vecName == smName m
        ]
      -> pure $ FixedArrayNullTerminated (smName m) size
      | NumericArraySize sizeNumber <- size
      , isSimpleType t
      , Nothing <- smLengths m
      , sizeNumber >= 2
      -> pure $ FixedArrayTuple (smName m) sizeNumber t
    t
      | -- An enabled flag
        [enabledName] <-
        [ enabledName
        | (structName, enableName, enabledName) <- enableRelation
        , structName == sName struct
        , enableName == smName m
        ]
      , Nothing <- smIsOptional m
      , TypeName "VkBool32" <- t
      -> pure $ EnabledFlag enabledName
      | -- An enabler
        [_] <-
        [ ()
        | (structName, enableName, enabledName) <- enableRelation
        , structName == sName struct
        , enabledName == smName m
        ]
      , Just [True] <- smIsOptional m
      , isSimpleType t
      -> pure $ OptionalZero (smName m) t
      | -- The number of valid elements in a fixed length array
        [vecName] <-
        [ vecName
        | (structName, countName, vecName) <- fixedArrayLengths
        , structName == sName struct
        , countName == smName m
        ]
      -> pure $ FixedArrayValidCount vecName
      | -- The length of a non-optional bytestring
        isSimpleType t
      , Just [v] <- lengthMap (smName m)
      , Ptr _ Void <- smType v
      , Nothing <- smIsOptional v
      -> pure $ ByteStringLength (smName v)
      | -- The length of an optional vector
        isSimpleType t
      , Just [v] <- lengthMap (smName m)
      , Just [True] <- smIsOptional v
      -> pure $ OptionalLength (smName v)
      | -- The minimum length of some vectors and optional vectors
        isSimpleType t
      , Just vs <- lengthMap (smName m)
      , all (isPtrType . smType) vs
      -> let (optionalVectors, nonOptionalVectors) =
               partition (maybe False head . smIsOptional) vs
         in  pure $ MinLength (smName <$> nonOptionalVectors)
                              (smName <$> optionalVectors)
      | -- The length of a void ptr
        isSimpleType t
      , Just [v] <- lengthMap (smName m)
      , Ptr _ Void <- smType v
      , Nothing <- smIsOptional v
      -> pure $ ByteStringLength (smName v)
      | isSimpleType t
      , Nothing <- lengthMap (smName m)
      , isNothing (smIsOptional m)
        ||     isDefaultable t
        ||     (sName struct, smName m)
        `elem` ignoredOptionalAttrs
      , Nothing <- smLengths m
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ PreservedMarshalled (smName m) (dropVkType tyName)
        else Preserved <$> toHsType t
    _ -> throwError
      [ Other
          (     "Couldn't convert struct member"
          T.<+> smName m
          T.<+> ":"
          T.<+> T.tShow (smType m)
          )
      ]
  pure $ MarshalledMember (smName m) <$> ty

writeToCStructInstance
  :: MarshalledStruct -> WrapM (Doc ())
writeToCStructInstance MarshalledStruct{..} = do
  tellDepends
    [Unguarded (WE.TypeName ("Vk" <> msName)), Unguarded (TermName ("Vk" <> msName))]
  tellExtension "InstanceSigs"
  case msStructOrUnion of
    AStruct -> do
      wrapped <- wrap "cont" ("Vk" <> msName) msName "from" msMembers
      pure [qci|
        instance ToCStruct {msName} Vk{msName} where
          withCStruct :: {msName} -> (Vk{msName} -> IO a) -> IO a
          withCStruct from cont = {wrapped}
        |]
    AUnion -> do
      wrappedAlts <- for msMembers $ \case
        -- These only work with single member sums
        m@(FixedArrayTuple n _ _) ->
          pure [qci|{T.upperCaseFirst n} x -> cont ({"Vk" <> T.upperCaseFirst n} (fromTuple x))|]
        m@(PreservedMarshalled n _) ->
          pure [qci|{T.upperCaseFirst n} x -> withCStruct x (cont . {"Vk" <> T.upperCaseFirst n})|]
        m -> throwError [Other ("Unhandled union member for wrapping:" T.<+> T.tShow m)]

      pure [qci|
        instance ToCStruct {msName} Vk{msName} where
          withCStruct :: {msName} -> (Vk{msName} -> IO a) -> IO a
          withCStruct from cont = case from of
        {indent 4 $ vcat wrappedAlts}
      |]

wrap
  :: Text
  -- ^ The name of the continuation
  -> Text
  -- ^ The C struct constructor name
  -> Text
  -- ^ The marshalled struct type
  -> Text
  -- ^ The marshalled struct name
  -> [MemberUsage MarshalledMember]
  -- ^ The struct members
  -> WrapM (Doc ())
wrap contName con fromType from members = do
  wrappers <- traverse (memberWrapper fromType from) members
  let applyCont :: Wrapper
      applyCont = \cont e -> cont [qci|{contName} ({e}|] <> ")"
  pure $ foldWrappers (applyCont : wrappers) id (pretty con)

memberWrapper
  :: Text
  -> Text
  -> MemberUsage MarshalledMember
  -> WrapM Wrapper
memberWrapper fromType from =
  let accessMember :: Text -> Doc ()
      accessMember memberName = [qci|({toMemberName memberName} ({from} :: {fromType}))|]
  in \case
  Univalued value  -> do
    tellExtension "PatternSynonyms"
    tellDepend (Unguarded (PatternName value))
    pure $ \cont e -> cont [qci|{e} {value}|]
  Length vec       -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (Data.Vector.length {accessMember vec}))|]
  MinLength [] _       -> do
    throwError [Other "MinLength has no non-optional vectors to examine"]
  MinLength [vec] []       -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (Data.Vector.length {accessMember vec}))|]
  MinLength nonOptionalVectors [] -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    tellImports [Import "Data.List" ["minimum"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (minimum ({list (("Data.Vector.length" <+>) . accessMember <$> nonOptionalVectors)})))|]
  MinLength nonOptionalVectors optionalVectors -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    tellImports [Import "Data.List" ["minimum"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (minimum ({list (("Data.Vector.length" <+>) . accessMember <$> nonOptionalVectors)} ++ [Data.Vector.length v | Just v <- {list (accessMember <$> optionalVectors)}])))|]
  OptionalLength vec       -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    tellImport "Data.Maybe" "maybe"
    pure $ \cont e -> cont [qci|{e} (maybe 0 (fromIntegral . Data.Vector.length) {accessMember vec})|]
  FixedArrayValidCount vec -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (Data.Vector.length {accessMember vec}))|]
  EnabledFlag enabled       -> do
    tellImports [QualifiedImport "Data.Maybe" ["maybe"]]
    tellDepends (Unguarded <$> [PatternName "VK_FALSE", PatternName "VK_TRUE"])
    pure $ \cont e -> cont [qci|{e} (maybe VK_FALSE (const VK_TRUE) {accessMember enabled})|]
  OptionalZero memberName _ -> do
    tellImport "Data.Maybe" "fromMaybe"
    pure $ \cont e -> cont [qci|{e} (fromMaybe 0 {accessMember memberName})|]
  FixedArray _ memberName _ _ alloc dummy -> do
    tellQualifiedImport "Data.Vector.Generic.Sized" "convert"
    -- This assumes that this is a vector of handles or pointers
    pure $ \cont e ->
      let paramName = pretty (dropPointer memberName)
          with = [qci|withArray {alloc} {accessMember memberName} (\\{paramName} -> {e} (Data.Vector.Generic.Sized.convert (padSized {dummy} {paramName}))|]
      in [qci|{cont with})|]
  FixedArrayTuple memberName _ _ -> do
    tellImport "Data.Vector.Generic.Sized" "fromTuple"
    pure $ \cont e -> cont [qci|{e} (fromTuple {accessMember memberName})|]
  FixedArrayNullTerminated memberName _ ->
    -- This truncates oversized bytestrings
    pure $ \cont e -> cont [qci|{e} (byteStringToNullTerminatedSizedVector {accessMember memberName})|]
  FixedArrayZeroPadByteString memberName _ ->
    -- This truncates oversized bytestrings
    pure $ \cont e -> cont [qci|{e} (byteStringToSizedVector {accessMember memberName})|]
  FixedArrayZeroPad memberName _ _ ->
    -- This truncates oversized vectors
    pure $ \cont e -> cont [qci|{e} (Data.Vector.Generic.Sized.convert (padSized 0 {accessMember memberName}))|]
  Preserved member -> pure $ \cont e -> cont [qci|{e} {accessMember (mmName member)}|]
  PreservedMarshalled memberName _ ->
    pure $ \cont e ->
      let paramName = pretty (dropPointer memberName)
          with  = [qci|withCStruct {accessMember memberName} (\\{paramName} -> {e} {paramName}|]
      in  [qci|{cont with})|]
  -- TODO: proper pointer names
  NextPointer memberName -> pure $ \cont e ->
    let withPtr =
          [qci|({accessMember memberName}) (\\{ptrName memberName} -> {e} {ptrName memberName}|]
    in [qci|{cont withPtr})|]
  ByteString memberName -> do
    tellImport "Data.ByteString" "useAsCString"
    pure $ \cont e ->
      let paramPtr = pretty (unKeyword $ ptrName (dropPointer memberName))
          withPtr  = [qci|useAsCString {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  ByteStringData _ memberName -> do
    tellImport "Data.ByteString.Unsafe" "unsafeUseAsCString"
    tellImport "Foreign.Ptr" "castPtr"
    pure $ \cont e ->
      let paramPtr = pretty (unKeyword $ ptrName (dropPointer memberName))
          withPtr  = [qci|unsafeUseAsCString {accessMember memberName} (\\{paramPtr} -> {e} (castPtr {paramPtr})|]
      in  [qci|{cont withPtr})|]
  ByteStringLength bs       -> do
    tellImports [QualifiedImport "Data.ByteString" ["length"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (Data.ByteString.length {accessMember bs}))|]
  MaybeByteString memberName -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    pure $ \cont e ->
      let paramPtr = pretty (unKeyword $ ptrName (dropPointer memberName))
          withPtr  = [qci|maybeWith useAsCString {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  Vector _ memberName elemType alloc -> pure $ \cont e ->
    let paramPtr = pretty (ptrName (dropPointer memberName))
        withPtr  = [qci|withVec {alloc} {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|{cont withPtr})|]
  OptionalVector _ memberName elemType alloc -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    pure $ \cont e ->
      let paramPtr = pretty (ptrName (dropPointer memberName))
          withPtr  = [qci|maybeWith (withVec {alloc}) {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  OptionalPtr memberName elemType alloc -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    pure $ \cont e ->
      let paramPtr = pretty (ptrName (dropPointer memberName))
          withPtr  = [qci|maybeWith {alloc} {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  NonOptionalPtr memberName elemType alloc -> do
    pure $ \cont e ->
      let paramPtr = pretty (ptrName (dropPointer memberName))
          withPtr  = [qci|{alloc} {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]

----------------------------------------------------------------
-- FromCStruct
----------------------------------------------------------------

writeFromCStructInstance
  :: MarshalledStruct -> WrapM (Doc ())
writeFromCStructInstance MarshalledStruct{..} = do
  tellDepends
    [Unguarded (WE.TypeName ("Vk" <> msName)), Unguarded (TermName ("Vk" <> msName))]
  tellExtension "InstanceSigs"
  case msStructOrUnion of
    AStruct -> do
      members <- traverse (fromCStructMember "c" (pretty $ "Vk" <> msName)) msMembers
      pure [qci|
        instance FromCStruct {msName} Vk{msName} where
          fromCStruct :: Vk{msName} -> IO {msName}
          fromCStruct c = {msName} <$> {indent (-4) . vsep $ (intercalatePrependEither "<*>" $ members)}

        |]
    AUnion -> pure "-- No FromCStruct instance for sum types"
      -- throwError [Other "Can't go from a c struct to a union"]

fromCStructMember
  :: (Doc ())
  -- ^ The name of the c struct we are translating
  -> (Doc ())
  -- ^ The type of the c struct we are translating
  -> MemberUsage MarshalledMember
  -> WrapM (Either (Doc ()) (Doc ()))
fromCStructMember from fromType =
  let accessMember :: Text -> Doc ()
      accessMember memberName =
        [qci|({toVkMemberName memberName} ({from} :: {fromType}))|]
  in  \case
        Univalued _        -> pure $ Left "-- Univalued Member elided"
        Length    _        -> pure $ Left "-- Length valued member elided"
        MinLength _ _      -> pure $ Left "-- Length valued member elided"
        OptionalLength   _ -> pure $ Left "-- Optional length valued member elided"
        ByteStringLength _ -> pure $ Left "-- Bytestring length valued member elided"
        FixedArrayValidCount _ ->
          pure $ Left "-- Fixed array valid count member elided"
        EnabledFlag _ -> pure $ Left "-- enable flag member elided"
        Preserved   m -> pure $ Right [qci|pure {accessMember (mmName m)}|]
        PreservedMarshalled memberName _ ->
          pure $ Right [qci|(fromCStructPtr {accessMember memberName})|]
        FixedArrayNullTerminated memberName _ -> do
          tellQualifiedImport "Data.Vector.Storable" "unsafeWith"
          tellQualifiedImport "Data.Vector.Storable.Sized" "fromSized"
          tellImport "Data.ByteString" "packCString"
          pure $ Right [qci|Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized {accessMember memberName}) packCString|]
        FixedArrayZeroPadByteString memberName arraySize -> do
          tellQualifiedImport "Data.Vector.Storable" "unsafeWith"
          tellQualifiedImport "Data.Vector.Storable.Sized" "fromSized"
          tellImport "Data.ByteString" "packCStringLen"
          let len = case arraySize of
                NumericArraySize n  -> pretty (show n)
                SymbolicArraySize s -> pretty s
          pure $ Right [qci|Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized {accessMember memberName}) (\p -> packCStringLen (p, {len}))|]
        NextPointer memberName -> pure $ Right [qci|pure ($ {accessMember memberName})|]
        ByteString memberName -> do
          tellImport "Data.ByteString" "packCString"
          pure $ Right [qci|packCString {accessMember memberName}|]
        MaybeByteString memberName -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          tellImport "Data.ByteString" "packCString"
          pure $ Right [qci|maybePeek packCString {accessMember memberName}|]
        Vector lenMemberName memberName type' _alloc -> do
          tellQualifiedImport "Data.Vector" "generateM"
          pure $ Right [qci|(Data.Vector.generateM (fromIntegral {accessMember lenMemberName}) (fromCStructPtrElem {accessMember memberName}))|]
        OptionalVector lenMemberName memberName type' _alloc -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          tellQualifiedImport "Data.Vector" "generateM"
          pure $ Right [qci|maybePeek (\p -> Data.Vector.generateM (fromIntegral {accessMember lenMemberName}) (fromCStructPtrElem p)) {accessMember memberName}|]
        ByteStringData lenMemberName memberName -> do
          tellImport "Data.ByteString" "packCStringLen"
          pure $ Right [qci|packCStringLen ({accessMember memberName}, fromIntegral {accessMember lenMemberName})|]
        OptionalPtr memberName _ _ -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          pure $ Right [qci|maybePeek (fromCStructPtr {accessMember memberName})|]
        NonOptionalPtr memberName _ _ ->
          pure $ Right [qci|fromCStructPtr {accessMember memberName}|]
        FixedArray validCountMemberName memberName maxArraySize _elemType _alloc _dummy -> do
          tellQualifiedImport "Data.Vector.Storable.Sized" "fromSized"
          tellQualifiedImport "Data.Vector.Generic" "convert"
          tellQualifiedImport "Data.Vector" "take"
          pure $ Right [qci|Data.Vector.take {accessMember validCountMemberName} (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized {accessMember memberName}))|]
        FixedArrayZeroPad memberName _ _ -> do
          tellQualifiedImport "Data.Vector.Storable.Sized" "fromSized"
          tellQualifiedImport "Data.Vector.Generic" "convert"
          tellQualifiedImport "Data.Vector" "take"
          pure $ Right [qci|Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized {accessMember memberName})|]
        FixedArrayTuple memberName length _ -> do
          tellQualifiedImport "Data.Vector.Storable.Sized" "unsafeIndex"
          let tuple = tupled ((\i -> [qci|Data.Vector.Storable.Sized.unsafeIndex {accessMember memberName} {i}|]) <$> [0 .. pred length])
          pure $ Right [qci|pure (let x = {accessMember memberName} in {tuple})|]
        OptionalZero memberName _ -> do
          pure $ Right [qci|pure (let x = {accessMember memberName} in if x == 0 then Nothing else Just x)|]

        m -> throwError [Other (T.tShow m)]
        -- m -> pure $ Right (pretty ("_ -- unhandled" T.<+> T.tShow m))
        -- m -> pure $ Right "_"


----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Returns (struct member containing length, name of member representing length,
-- vector struct members)
getLengthRelation :: Struct -> [StructMember] -> [(StructMember, [StructMember])]
getLengthRelation struct structMembers
  = let
      -- A list of all const arrays with lengths
      arrays :: [(Text, StructMember)]
      arrays =
        [ (l, m)
        | m            <- structMembers
        , -- TODO: Think if it's a good idea to just take the first element here
          Just (l : _) <- pure $ smLengths m
        , Ptr _ _      <- pure $ smType m
        ]

      lengthPairs :: [(Text, [StructMember])]
      lengthPairs =
        [ (length, ps)
        | (length, ps) <- MultiMap.assocs (MultiMap.fromList arrays)
        ]

      validCountPairs :: [(Text, [StructMember])]
      validCountPairs =
        [ (count, [array])
        | (structName, count, arrayName) <- fixedArrayLengths
        , structName == sName struct
        , Just array <- pure $ structmemberMap arrayName
        ]

      -- A map from name to structmembers
      structmemberMap =
        (`Map.lookup` Map.fromList ((smName &&& id) <$> structMembers))
    in
      -- Only return pairs where the length is another structmember
      mapMaybe (\(length, p) -> (, p) <$> structmemberMap length)
               (lengthPairs ++ validCountPairs)

----------------------------------------------------------------
-- Writing marshalled structs
----------------------------------------------------------------

writeMarshalled :: MarshalledStruct -> WrapM (DocMap -> Doc ())
writeMarshalled MarshalledStruct {..} = do
  case msStructOrUnion of
    AStruct -> do
      memberDocs <- traverse
        (writeMarshalledMember msName)
        msMembers
      pure $ \getDoc -> [qci|
      {document getDoc (TopLevel msName)}
      data {msName} = {msName}
        \{ {indent (-2) . vsep $
           intercalatePrependEither "," (($ getDoc) <$> memberDocs)
          }
        }
      |]
    AUnion -> do
      memberDocs <- traverse
        (writeMarshalledUnionMember msName)
        msMembers
      pure $ \getDoc -> [qci|
      {document getDoc (TopLevel msName)}
      data {msName}
        = {indent (-2) . vsep $
           intercalatePrependEither "|" (($ getDoc) <$> memberDocs)}
      |]

writeMarshalledMember
  :: Text
  -> MemberUsage MarshalledMember
  -> WrapM (DocMap -> Either (Doc ()) (Doc ()))
writeMarshalledMember parentName = \case
  Univalued            _ -> pure $ \_ -> Left "-- Univalued Member elided"
  Length               _ -> pure $ \_ -> Left "-- Length valued member elided"
  MinLength          _ _ -> pure $ \_ -> Left "-- Length valued member elided"
  OptionalLength       _ -> pure $ \_ -> Left "-- Optional length valued member elided"
  ByteStringLength     _ -> pure $ \_ -> Left "-- Bytestring length valued member elided"
  FixedArrayValidCount _ -> pure $ \_ -> Left "-- Fixed array valid count member elided"
  EnabledFlag          _ -> pure $ \_ -> Left "-- enable flag member elided"
  NextPointer memberName -> do
    tellImport "Foreign.Ptr" "Ptr"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: forall a. (Ptr () -> IO a) -> IO a
    |]
  ByteString memberName -> do
    tellImport "Data.ByteString" "ByteString"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: ByteString
    |]
  ByteStringData _ memberName -> do
    tellImport "Data.ByteString" "ByteString"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: ByteString
    |]
  FixedArrayNullTerminated memberName _ -> do
    tellImport "Data.ByteString" "ByteString"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: ByteString
    |]
  FixedArrayZeroPadByteString memberName _ -> do
    tellImport "Data.ByteString" "ByteString"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: ByteString
    |]
  FixedArrayZeroPad memberName _ t -> do
    tellImport "Data.Vector" "Vector"
    tyName <- toHsType t
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Vector {tyName}
    |]
  -- TODO: Abstract over Maybe here
  MaybeByteString memberName -> do
    tellImport "Data.ByteString" "ByteString"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe ByteString
    |]
  Vector _ memberName t _ -> do
    tyName <- toHsType t
    tellImport "Data.Vector" "Vector"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Vector {tyName}
    |]
  FixedArray _ memberName _ t _ _ -> do
    tyName <- toHsType t
    tellImport "Data.Vector" "Vector"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Vector {tyName}
    |]
  FixedArrayTuple memberName len t -> do
    tyName <- toHsType t
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: {tupled (replicate (fromIntegral len) tyName)}
    |]
  OptionalVector _ memberName t _ -> do
    tyName <- toHsType t
    tellImport "Data.Vector" "Vector"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe (Vector {tyName})
    |]
  Preserved MarshalledMember{..} ->
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName mmName)}
      {pretty (toMemberName mmName)} :: {mmType}
    |]
  PreservedMarshalled memberName memberType ->
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: {pretty memberType}
    |]
  OptionalPtr memberName t _ -> do
    tyName <- toHsType t
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe {tyName}
    |]
  NonOptionalPtr memberName t _ -> do
    tyName <- toHsType t
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: {tyName}
    |]
  OptionalZero memberName t -> do
    tyName <- toHsType t
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe {tyName}
    |]

writeMarshalledUnionMember
  :: Text
  -> MemberUsage MarshalledMember
  -> WrapM (DocMap -> Either (Doc ()) (Doc ()))
writeMarshalledUnionMember parentName = \case
  FixedArrayTuple n len t -> do
    tyName <- toHsType t
    pure $ \getDoc -> Right [qci|
      {T.upperCaseFirst n} {tupled (replicate (fromIntegral len) tyName)}
    |]
  PreservedMarshalled n t ->
    pure $ \getDoc -> Right [qci|
      {T.upperCaseFirst n} {t}
    |]
  m -> throwError [Other ("Unhandled union member:" T.<+> T.tShow m)]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

isUnivalued :: MemberUsage a -> Bool
isUnivalued = \case
  Univalued _ -> True
  _ -> False

isLength :: MemberUsage a -> Bool
isLength = \case
  Length _ -> True
  _ -> False

toMemberName :: Text -> Text
toMemberName = ("vk" <>) . T.upperCaseFirst

toVkMemberName :: Text -> Text
toVkMemberName = ("vk" <>) . T.upperCaseFirst

dropVkType :: Text -> Text
dropVkType = T.dropPrefix' "Vk"

----------------------------------------------------------------
-- Quirks
----------------------------------------------------------------

-- | A list of (Struct name, length member name, array member name) for fixed
-- sized arrays where only the first 'length member name' elements are valid'
fixedArrayLengths :: [(Text, Text, Text)]
fixedArrayLengths =
  [ ( "VkPhysicalDeviceGroupProperties"
    , "physicalDeviceCount"
    , "physicalDevices"
    )
  , ("VkPhysicalDeviceMemoryProperties", "memoryTypeCount", "memoryTypes")
  , ("VkPhysicalDeviceMemoryProperties", "memoryHeapCount", "memoryHeaps")
  ]

nullTerminatedFixedArrays :: [(Text, Text)]
nullTerminatedFixedArrays =
  [ ("VkPhysicalDeviceProperties", "deviceName")
  , ("VkExtensionProperties"     , "extensionName")
  , ("VkLayerProperties"         , "layerName")
  , ("VkLayerProperties"         , "description")
  ]

isDefaultableForeignType :: Text -> Bool
isDefaultableForeignType =
  (`elem` [ "HANDLE"
          , "DWORD"
          , "PFN_vkInternalAllocationNotification"
          , "PFN_vkInternalFreeNotification"
          , "PFN_vkAllocationFunction"
          , "PFN_vkReallocationFunction"
          , "PFN_vkFreeFunction"
          ]
  )

-- | Some flags are optional and if they are present a bool must be VK_TRUE
--
-- This is a triple of (Struct name, enable member name, enabled member name)
enableRelation :: [(Text, Text, Text)]
enableRelation =
  [ ( "VkPipelineCoverageToColorStateCreateInfoNV"
    , "coverageToColorEnable"
    , "coverageToColorLocation"
    )
  ]

zeroPadMembers :: [(Text, Text)]
zeroPadMembers = [("VkDeviceGroupPresentCapabilitiesKHR", "presentMask")]

-- | Generate a dummy member for padding (assume something is a handle if we
-- don't have a bespoke filler for it)
--
-- TODO: be a bit less lazy about this
dummyMember :: Type -> WrapM (Doc ())
dummyMember = \case
  TypeName "VkMemoryHeap" -> do
    tellImport "Data.Bits" "zeroBits"
    tellDepend (Unguarded (WE.TermName "VkMemoryHeap"))
    pure $ "(VkMemoryHeap 0 zeroBits)"
  TypeName "VkMemoryType" -> do
    tellImport "Data.Bits" "zeroBits"
    tellDepend (Unguarded (WE.TermName "VkMemoryType"))
    pure $ "(VkMemoryType zeroBits 0)"
  _ -> do
    tellImport "Foreign.Ptr" "nullPtr"
    pure "nullPtr"

-- | These are attributes marked optional which we leave to the user to sort out
ignoredOptionalAttrs :: [(Text, Text)]
ignoredOptionalAttrs =
  [ ("VkDebugUtilsMessengerCallbackDataEXT", "messageIdNumber")
  , ("VkCmdProcessCommandsInfoNVX"         , "sequencesCountOffset")
  , ("VkCmdProcessCommandsInfoNVX"         , "sequencesIndexOffset")
  ]

isNonMarshalledType :: Type -> Bool
isNonMarshalledType = \case
  Ptr _ (TypeName t) ->
    t `elem` ["SECURITY_ATTRIBUTES", "Display", "wl_surface", "MirSurface"]
  -- TODO: Marshal windows strings
  TypeName t -> t `elem` ["LPCWSTR"]
  _          -> False

