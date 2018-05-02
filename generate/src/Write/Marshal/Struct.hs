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
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Char                                (isUpper, toUpper)
import           Data.Closure
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.List                                (partition)
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Monoid                              (Endo (..))
import qualified Data.MultiMap                            as MultiMap
import qualified Data.Set                                 as Set
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
  -> [Struct]
  -- ^ A list of all structs
  -> Struct
  -> Either [SpecError] WriteElement
structWrapper isHandle isBitmask isStruct allStructs struct = do
  let weName = sName struct T.<+> "wrapper"
      isDefaultable t = maybe
        False
        (isHandle <||> isBitmask <||> isDefaultableForeignType)
        (simpleTypeName t)
      isStructType t = maybe False isStruct (simpleTypeName t)
      containsUnion = doesStructContainUnion allStructs
  (weDoc, (weImports, weProvides, weDepends, weExtensions, _)) <- either
    (throwError . fmap (WithContext (sName struct)))
    (pure . first (fmap vcat . sequence))
    (runWrap $ wrapStruct isDefaultable isStructType (containsUnion (sName struct)) struct)
  pure WriteElement {..}

vkStructWriteElement :: [Struct] -> WriteElement
vkStructWriteElement structs =
  let
    containsUnion = doesStructContainUnion structs

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
      , Import          "Data.Proxy"                ["Proxy(Proxy)"]
      , Import          "Data.ByteString"
                        ["ByteString", "take", "unpack"]
      , Import "GHC.TypeNats" ["natVal", "KnownNat", "type (<=)"]
      , Import "Foreign.C.Types" ["CChar(..)"]
      , Import "Data.Word" ["Word8"]
      , Import "Data.Typeable" ["Typeable", "cast", "eqT"]
      , Import "Data.Type.Equality" ["(:~:)(Refl)"]
      , Import "Control.Exception" ["throwIO"]
      , Import "Control.Applicative" ["(<|>)"]
      , Import "GHC.IO.Exception" ["IOException(..)", "IOErrorType(InvalidArgument)"]
      , QualifiedImport "Data.Vector.Generic"
                        ["snoc", "empty"]
      ]
    weProvides = Unguarded <$> [ WithConstructors $ WE.TypeName "ToCStruct"
                               , WithConstructors $ WE.TypeName "FromCStruct"
                               , WithConstructors $ WE.TypeName "SomeVkStruct"
                               , Term "SomeVkStruct"
                               , Term "withCStructPtr"
                               , Term "fromCStructPtr"
                               , Term "fromCStructPtrElem"
                               , Term "fromSomeVkStruct"
                               , Term "fromSomeVkStructChain"
                               , Term "peekVkStruct"
                               , Term "withSomeVkStruct"
                               , Term "withVec"
                               ]



    -- TODO: This also depends on all the patterns here, and all the type names
    weDepends  = [Unguarded (WE.TypeName "VkStructureType")]
    weExtensions =
      [ "FunctionalDependencies"
      , "DataKinds"
      , "ExplicitNamespaces"
      , "FlexibleContexts"
      , "GADTs"
      , "LambdaCase"
      , "RankNTypes"
      , "ScopedTypeVariables"
      , "StandaloneDeriving"
      , "TypeApplications"
      , "TypeOperators"
      ]
    weDoc = pure [qci|
      class ToCStruct marshalled c | marshalled -> c, c -> marshalled where
        withCStruct :: marshalled -> (c -> IO a) -> IO a
      class FromCStruct marshalled c | marshalled -> c, c -> marshalled where
        fromCStruct :: c -> IO marshalled
      class HasPNext a where
        getPNext :: a -> Maybe SomeVkStruct

      data SomeVkStruct where
        SomeVkStruct
          :: (ToCStruct a b, Storable b, Show a, Eq a, Typeable a, HasPNext a)
          => a
          -> SomeVkStruct

      instance HasPNext SomeVkStruct where
        getPNext (SomeVkStruct a) = getPNext a

      deriving instance Show SomeVkStruct

      instance Eq SomeVkStruct where
        SomeVkStruct (a :: a) == SomeVkStruct (b :: b) = case eqT @a @b of
          Nothing   -> False
          Just Refl -> a == b

      fromSomeVkStruct :: Typeable a => SomeVkStruct -> Maybe a
      fromSomeVkStruct (SomeVkStruct a) = cast a

      fromSomeVkStructChain :: Typeable a => SomeVkStruct -> Maybe a
      fromSomeVkStructChain a =
        fromSomeVkStruct a <|> (getPNext a >>= fromSomeVkStructChain)

      withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a
      withSomeVkStruct (SomeVkStruct a) f = withCStructPtr a (f . castPtr)

      -- | Read the @sType@ member of a Vulkan struct and marshal the struct into
      -- a 'SomeVkStruct'
      --
      -- Make sure that you only pass this a pointer to a Vulkan struct with a
      -- @sType@ member at offset 0 otherwise the behaviour is undefined.
      --
      -- - Throws an 'InvalidArgument' 'IOException' if given a pointer to a
      --   struct with an unrecognised @sType@ member.
      -- - Throws an 'InvalidArgument' 'IOException' if given a pointer to a
      --   struct which can't be marshalled (those containing union types)
      peekVkStruct :: Ptr SomeVkStruct -> IO SomeVkStruct
      peekVkStruct p = do
        peek (castPtr p :: Ptr VkStructureType) >>= \case
          {indent 0 . vcat . mapMaybe (writeSomeStructPeek containsUnion) $ structs}
          t -> throwIO (IOError Nothing InvalidArgument "" ("Unknown VkStructureType: " ++ show t) Nothing Nothing)

      withCStructPtr :: (Storable c, ToCStruct a c) => a -> (Ptr c -> IO b) -> IO b
      withCStructPtr a f = withCStruct a (\c -> alloca (\p -> poke p c *> f p))

      fromCStructPtr :: (Storable c, FromCStruct a c) => Ptr c -> IO a
      fromCStructPtr p = fromCStruct =<< peek p

      fromCStructPtrElem :: (Storable c, FromCStruct a c) => Ptr c -> Int -> IO a
      fromCStructPtrElem p o = fromCStruct =<< peekElemOff p o

      packCStringElemOff :: Ptr (Ptr CChar) -> Int -> IO ByteString
      packCStringElemOff p o = packCString =<< peekElemOff p o

      withArray
        :: forall a b d
         . (a -> (b -> IO d) -> IO d)
        -> Vector a
        -> (Vector b -> IO d)
        -> IO d
      withArray alloc v cont =
        let go :: a -> (Vector b -> IO d) -> (Vector b -> IO d)
            go x complete bs = alloc x (\b -> complete (Data.Vector.Generic.snoc bs b))
        in  foldr go cont v (Data.Vector.Generic.empty)

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

      -- | Pad or truncate a vector so that it has the required size
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
          padded = let n = fromIntegral (natVal (Proxy @n))
                   in padVector p n v

      -- | Make sure a vector is at least a certain length
      padVector
        :: (Data.Vector.Generic.Vector v a)
        => a
        -> Int
        -> v a
        -> v a
      padVector p n v =
        let m = Data.Vector.Generic.length v
        in case m `compare` n of
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

writeSomeStructPeek
  :: (Text -> Bool)
  -- ^ Does a struct contain a union
  -> Struct
  -> Maybe (Doc ())
writeSomeStructPeek containsUnion Struct{..}
  = do
    StructMember {smName = "sType", smValues = Just [enum]} : _ <- pure sMembers
    pure $ if containsUnion sName
      then [qci|
             -- We are not able to marshal this type back into Haskell as we don't know which union component to use
             {enum} -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing unions: " ++ show {enum}) Nothing Nothing)
           |]
      else [qci|{enum} -> SomeVkStruct <$> fromCStructPtr (castPtr p :: Ptr {sName})|]

----------------------------------------------------------------
-- The wrapping commands
----------------------------------------------------------------

wrapStruct
  :: (Type -> Bool)
  -- ^ Is a defaultable type
  -> (Type -> Bool)
  -- ^ Is this a struct
  -> Bool
  -- ^ Does this struct contain a union
  -> Struct
  -> WrapM [DocMap -> Doc ()]
wrapStruct isDefaultable isStruct containsUnion s = do
  marshalled     <- marshallStruct isStruct isDefaultable s
  toCStructDoc   <- writeToCStructInstance marshalled
  fromCStructDoc <- writeFromCStructInstance containsUnion marshalled
  hasPNextDoc    <- writeHasPNextInstance marshalled
  marshalledDoc  <- writeMarshalled marshalled
  tellExtension "DuplicateRecordFields"
  tellExport (Unguarded (WithConstructors (WE.TypeName (dropVkType (sName s)))))
  tellExport (Unguarded (Term (dropVkType (sName s))))
  let weDoc docMap = vsep [marshalledDoc docMap, toCStructDoc, fromCStructDoc, hasPNextDoc]
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
  | LengthMultiple
      Text --- ^ The vector this is the size of
      Int  --- ^ The size of each element
    -- ^ This is bound to be the length of an array in bytes
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
      (Maybe (Doc ())) --- ^ From C representation
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
      Int  --- ^ the length is a fraction of the length specifier in the C Struct (usually 1)
      Text --- ^ member name
      Type --- ^ element type
      (Doc ()) --- ^ allocator
      (Doc ()) --- ^ peekElemOff
    -- ^ A vector with name and of type with the alloc function for the members
  | BitMaskVector
      Text --- ^ The name of the member in the C struct which contains the
           -- length in bits
      Int  --- ^ The bit size of the elements
      Text --- ^ member name
      Type --- ^ element type
      (Doc ()) --- ^ allocator
    -- ^ A vector of masks, where the total bit length equals some other member
  | OptionalVector
      Text --- ^ The name of the member in the C struct which contains the
           -- length
      Text --- ^ member name
      Type --- ^ element type
      (Doc ()) --- ^ allocator
      (Doc ()) --- ^ peekElemOff
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
    -- vector of tuples here, Unused at the moment
  | SiblingVectorSlave Text
    -- ^ A vector which shares its length with other vectors, stored in the
    -- master vector as a tuple, Unused at the moment
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
        smName m == "pNext"
      , Nothing <- smLengths m
      -> pure (NextPointer (smName m))
      | -- A void pointer consumed by the user and not Vulkan
        Nothing <- smLengths m
      -> Preserved <$> toHsType (smType m)
      | -- A pointer to data to be consumed by Vulkan (has a length)
        Nothing <- smIsOptional m
      , Just [length] <- smLengths m
      , Const <- cv
      , Just lengthMember <- findLengthMember length
      -> pure $ ByteStringData (smName lengthMember) (smName m)
    Ptr Const Char
      | Nothing <- smIsOptional m, Just ["null-terminated"] <- smLengths m
      -> pure $ ByteString (smName m)
    Ptr Const Char
      | Just [True] <- smIsOptional m, Just ["null-terminated"] <- smLengths m
      -> pure $ MaybeByteString (smName m)
    Ptr _ t
      | -- An array with a length multiple
        Nothing <- smIsOptional m
      , -- TODO: Don't hardcode this
        Just ["codeSize / 4"] <- smLengths m
      , [(countName, multiple)] <-
        [ (countName, size)
        | (structName, countName, vecName, size) <- lengthMultipleRelation
        , structName == sName struct
        , vecName == smName m
        ]
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ Vector countName
                           multiple
                           (smName m)
                           (TypeName (dropVkType tyName))
                           "withCStruct"
                           "fromCStructPtrElem"
        else pure $ Vector countName multiple (smName m) t "(flip ($))" "peekElemOff"
      | -- An array with a length specified in bits
        -- TODO: Don't hardcode this
        Just ["(rasterizationSamples + 31) / 32"] <- smLengths m
      , [(countName, elemBitSize)] <-
        [ (countName, size)
        | (structName, countName, vecName, size) <- bitMaskLengthRelation
        , structName == sName struct
        , vecName == smName m
        ]
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ BitMaskVector countName
                                  elemBitSize
                                  (smName m)
                                  (TypeName (dropVkType tyName))
                                  "withCStruct"
        else pure $ BitMaskVector countName elemBitSize (smName m) t "(flip ($))"
      | -- An array
        Nothing <- smIsOptional m
      , Just [length] <- smLengths m
      , Just lengthMember <- findLengthMember length
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ Vector length
                           1
                           (smName m)
                           (TypeName (dropVkType tyName))
                           "withCStruct"
                           "fromCStructPtrElem"
        else pure $ Vector (smName lengthMember) 1 (smName m) t "(flip ($))" "peekElemOff"
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
                                   "fromCStructPtrElem"
        else pure
          $ OptionalVector (smName lengthMember) (smName m) t "(flip ($))" "peekElemOff"
      | -- An array of strings
        Nothing <- smIsOptional m
      , Just [length, "null-terminated"] <- smLengths m
      , Ptr Const Char <- t
      -> do
        tellImport "Data.ByteString" "useAsCString"
        tellImport "Data.ByteString" "ByteString"
        pure $ Vector length 1 (smName m) (TypeName "ByteString") "useAsCString" "packCStringElemOff"
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
             fromCRep = if isStruct t
               then Just "fromCStruct"
               else Nothing
             ty = if isStruct t then (TypeName (dropVkType tyName)) else t
         in  FixedArray (smName countMember) (smName m) size ty alloc
               <$> dummyMember t <*> pure fromCRep
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
      | -- The length of a vector in bytes
        [(vecName, size)] <-
        [ (vecName, size)
        | (structName, countName, vecName, size) <- lengthMultipleRelation
        , structName == sName struct
        , countName == smName m
        ]
      , Nothing <- smIsOptional m
      -> pure $ LengthMultiple vecName size
      | -- The length of a vector in bits, preserved as we can't extract enough
        -- information
        [()] <-
        [ ()
        | (structName, countName, vecName, size) <- bitMaskLengthRelation
        , structName == sName struct
        , countName == smName m
        ]
      , Nothing <- smIsOptional m
      -> Preserved <$> toHsType t
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
  LengthMultiple vec mul -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    pure $ \cont e -> cont [qci|{e} ({mul} * fromIntegral (Data.Vector.length {accessMember vec}))|]
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
    tellImport "Data.Maybe" "maybe"
    tellDepends (Unguarded <$> [PatternName "VK_FALSE", PatternName "VK_TRUE"])
    pure $ \cont e -> cont [qci|{e} (maybe VK_FALSE (const VK_TRUE) {accessMember enabled})|]
  OptionalZero memberName _ -> do
    tellImport "Data.Maybe" "fromMaybe"
    pure $ \cont e -> cont [qci|{e} (fromMaybe 0 {accessMember memberName})|]
  FixedArray _ memberName _ _ alloc dummy _ -> do
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
  NextPointer memberName -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    pure $ \cont e ->
      let withPtr =
            [qci|maybeWith withSomeVkStruct {accessMember memberName} (\\{ptrName memberName} -> {e} {ptrName memberName}|]
      in  [qci|{cont withPtr})|]
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
  Vector _ _ memberName elemType alloc _ -> pure $ \cont e ->
    let paramPtr = pretty (ptrName (dropPointer memberName))
        withPtr  = [qci|withVec {alloc} {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|{cont withPtr})|]
  BitMaskVector lengthMemberName bitSize memberName elemType alloc -> do
    tellImport "Data.Bits" "zeroBits"
    tellImport "Data.Coerce" "coerce"
    tellDepend (Unguarded (WE.TypeName "VkFlags"))
    pure $ \cont e ->
      let paramPtr = pretty (ptrName (dropPointer memberName))
          withPtr  = [qci|withVec {alloc} (padVector zeroBits (fromIntegral ((coerce {accessMember lengthMemberName} :: VkFlags) + {pred bitSize}) `quot` {bitSize}) {accessMember memberName}) (\\{paramPtr} -> {e} {paramPtr}|]
      in [qci|{cont withPtr})|]
  OptionalVector _ memberName elemType alloc _ -> do
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
  :: Bool
  -- ^ Does this struct contain a union
  -> MarshalledStruct
  -> WrapM (Doc ())
writeFromCStructInstance containsUnion MarshalledStruct{..} = do
  tellDepends
    [Unguarded (WE.TypeName ("Vk" <> msName)), Unguarded (TermName ("Vk" <> msName))]
  tellExtension "InstanceSigs"
  case msStructOrUnion of
    AStruct ->
      if containsUnion
        then pure "-- No FromCStruct instance for types containing unions"
        else do
          members <- traverse (fromCStructMember "c" (pretty $ "Vk" <> msName)) msMembers
          pure [qci|
            instance FromCStruct {msName} Vk{msName} where
              fromCStruct :: Vk{msName} -> IO {msName}
              fromCStruct c = {msName} <$> {indent (-4) . vsep $ (intercalatePrependEither "<*>" $ members)}

            |]
    AUnion -> pure "-- No FromCStruct instance for sum types"
      -- throwError [Other "Can't go from a c struct to a union"]

-- | Returns the names of all structs containing unions
doesStructContainUnion :: [Struct] -> Text -> Bool
doesStructContainUnion structs =
  let
    unionNames = [ sName s | s <- structs, sStructOrUnion s == AUnion ]

    -- The list of struct names which contain this type
    contains :: Text -> [Text]
    contains = (`MultiMap.lookup` m)
      where
        m = MultiMap.fromList
          [ (containee, sName container)
          | container <- structs
          , WE.TypeName containee <- typeDepends . smType =<< sMembers container
          ]

    structWithUnions = close contains unionNames
  in (`Set.member` Set.fromList structWithUnions)

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
        LengthMultiple _ _ -> pure $ Left "-- Length multiple valued member elided"
        MinLength _ _      -> pure $ Left "-- Length valued member elided"
        OptionalLength   _ -> pure $ Left "-- Optional length valued member elided"
        ByteStringLength _ -> pure $ Left "-- Bytestring length valued member elided"
        FixedArrayValidCount _ ->
          pure $ Left "-- Fixed array valid count member elided"
        EnabledFlag _ -> pure $ Left "-- enable flag member elided"
        Preserved   m -> pure $ Right [qci|pure {accessMember (mmName m)}|]
        PreservedMarshalled memberName _ ->
          pure $ Right [qci|(fromCStruct {accessMember memberName})|]
        FixedArrayNullTerminated memberName _ -> do
          tellQualifiedImport "Data.Vector.Storable" "unsafeWith"
          tellQualifiedImport "Data.Vector.Storable.Sized" "fromSized"
          tellImport "Data.ByteString" "packCString"
          pure $ Right [qci|Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized {accessMember memberName}) packCString|]
        FixedArrayZeroPadByteString memberName arraySize -> do
          tellQualifiedImport "Data.Vector.Storable" "unsafeWith"
          tellQualifiedImport "Data.Vector.Storable.Sized" "fromSized"
          tellImport "Data.ByteString" "packCStringLen"
          tellImport "Foreign.Ptr" "castPtr"
          len <- case arraySize of
            NumericArraySize n  -> pure $ pretty (show n)
            SymbolicArraySize s -> do
              tellDepend (Unguarded (PatternName s))
              pure $ pretty s
          pure $ Right [qci|Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized {accessMember memberName}) (\p -> packCStringLen (castPtr p, {len}))|]
        NextPointer memberName -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          tellImport "Foreign.Ptr" "castPtr"
          pure $ Right [qci|maybePeek peekVkStruct (castPtr {accessMember memberName})|]
        ByteString memberName -> do
          tellImport "Data.ByteString" "packCString"
          pure $ Right [qci|packCString {accessMember memberName}|]
        MaybeByteString memberName -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          tellImport "Data.ByteString" "packCString"
          pure $ Right [qci|maybePeek packCString {accessMember memberName}|]
        Vector lenMemberName 1 memberName type' _alloc peekElemOff -> do
          tellQualifiedImport "Data.Vector" "generateM"
          pure $ Right [qci|(Data.Vector.generateM (fromIntegral {accessMember lenMemberName}) ({peekElemOff} {accessMember memberName}))|]
        Vector lenMemberName multiple memberName type' _alloc peekElemOff -> do
          tellQualifiedImport "Data.Vector" "generateM"
          pure $ Right [qci|(Data.Vector.generateM (fromIntegral {accessMember lenMemberName} `quot` {multiple}) ({peekElemOff} {accessMember memberName}))|]
        BitMaskVector lenMemberName bitSize memberName type' _alloc -> do
          tellQualifiedImport "Data.Vector" "generateM"
          tellImport "Data.Coerce" "coerce"
          tellDepend (Unguarded (WE.TypeName "VkFlags"))
          pure $ Right [qci|(Data.Vector.generateM (fromIntegral (((coerce {accessMember lenMemberName} :: VkFlags) + {pred bitSize}) `quot` {bitSize})) (peekElemOff {accessMember memberName}))|]
        OptionalVector lenMemberName memberName type' _alloc peekElemOff -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          tellQualifiedImport "Data.Vector" "generateM"
          pure $ Right [qci|maybePeek (\p -> Data.Vector.generateM (fromIntegral {accessMember lenMemberName}) ({peekElemOff} p)) {accessMember memberName}|]
        ByteStringData lenMemberName memberName -> do
          tellImport "Data.ByteString" "packCStringLen"
          tellImport "Foreign.Ptr" "castPtr"
          pure $ Right [qci|packCStringLen (castPtr {accessMember memberName}, fromIntegral {accessMember lenMemberName})|]
        OptionalPtr memberName _ _ -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          pure $ Right [qci|maybePeek fromCStructPtr {accessMember memberName}|]
        NonOptionalPtr memberName _ _ ->
          pure $ Right [qci|fromCStructPtr {accessMember memberName}|]
        FixedArray validCountMemberName memberName maxArraySize _elemType _alloc _dummy fromCRep -> do
          tellQualifiedImport "Data.Vector.Storable.Sized" "fromSized"
          tellQualifiedImport "Data.Vector.Generic" "convert"
          tellQualifiedImport "Data.Vector" "take"
          let e :: Doc ()
              e = [qci|Data.Vector.take (fromIntegral {accessMember validCountMemberName}) (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized {accessMember memberName}))|]
          pure . Right $ case fromCRep of
            Nothing -> [qci|pure ({e})|]
            Just f  -> [qci|traverse {f} ({e})|]
        FixedArrayZeroPad memberName _ _ -> do
          tellQualifiedImport "Data.Vector.Storable.Sized" "fromSized"
          tellQualifiedImport "Data.Vector.Generic" "convert"
          tellQualifiedImport "Data.Vector" "take"
          pure $ Right [qci|pure (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized {accessMember memberName}))|]
        FixedArrayTuple memberName length _ -> do
          tellQualifiedImport "Data.Vector.Storable.Sized" "unsafeIndex"
          let tuple = tupled ((\i -> [qci|Data.Vector.Storable.Sized.unsafeIndex x {i}|]) <$> [0 .. pred length])
          pure $ Right [qci|pure (let x = {accessMember memberName} in {tuple})|]
        OptionalZero memberName _ -> do
          pure $ Right [qci|pure (let x = {accessMember memberName} in if x == 0 then Nothing else Just x)|]
        m -> throwError [Other (T.tShow m)]

----------------------------------------------------------------
-- HasPNext instance
----------------------------------------------------------------

writeHasPNextInstance :: MarshalledStruct -> WrapM (Doc ())
writeHasPNextInstance MarshalledStruct{..} =
  case [n | NextPointer n  <- msMembers] of
    [] -> pure [qci|-- This struct doesn't have a pNext member"|]
    [n] ->
      pure $ [qci|
        instance HasPNext {msName} where
          getPNext a = vkPNext (a :: {msName})
      |]
    _ -> throwError [Other ("Struct with more than one pNext member:" T.<+> T.tShow msName)]

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
        deriving (Show, Eq)
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
        deriving (Show, Eq)
      |]

writeMarshalledMember
  :: Text
  -> MemberUsage MarshalledMember
  -> WrapM (DocMap -> Either (Doc ()) (Doc ()))
writeMarshalledMember parentName = \case
  Univalued            _ -> pure $ \_ -> Left "-- Univalued Member elided"
  Length               _ -> pure $ \_ -> Left "-- Length valued member elided"
  LengthMultiple     _ _ -> pure $ \_ -> Left "-- Length multiple valued member elided"
  MinLength          _ _ -> pure $ \_ -> Left "-- Length valued member elided"
  OptionalLength       _ -> pure $ \_ -> Left "-- Optional length valued member elided"
  ByteStringLength     _ -> pure $ \_ -> Left "-- Bytestring length valued member elided"
  FixedArrayValidCount _ -> pure $ \_ -> Left "-- Fixed array valid count member elided"
  EnabledFlag          _ -> pure $ \_ -> Left "-- enable flag member elided"
  NextPointer memberName -> do
    tellImport "Foreign.Ptr" "Ptr"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe SomeVkStruct
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
  Vector _ _ memberName t _ _ -> do
    tyName <- toHsType t
    tellImport "Data.Vector" "Vector"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Vector {tyName}
    |]
  BitMaskVector _ _ memberName t _ -> do
    tyName <- toHsType t
    tellImport "Data.Vector" "Vector"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Vector {tyName}
    |]
  FixedArray _ memberName _ t _ _ _ -> do
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
  OptionalVector _ memberName t _ _ -> do
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

toMemberName :: Text -> Text
toMemberName = ("vk" <>) . T.upperCaseFirst

-- | drop the first word if it is just @p@s and @s@s
upperCaseHungarian :: Text -> Text
upperCaseHungarian t = case T.break isUpper t of
  (firstWord, remainder) | T.all ((== 'p') <||> (== 's')) firstWord ->
    T.map toUpper firstWord <> remainder
  _ -> t

toVkMemberName :: Text -> Text
toVkMemberName = ("vk" <>) . T.upperCaseFirst . upperCaseHungarian

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

-- The length member is the length in bytes of the vector
-- TODO: Extract this from the spec!
lengthMultipleRelation :: [(Text, Text, Text, Int)]
lengthMultipleRelation = [("VkShaderModuleCreateInfo", "codeSize", "pCode", 4)]

bitMaskLengthRelation :: [(Text, Text, Text, Int)]
bitMaskLengthRelation =
  [ ( "VkPipelineMultisampleStateCreateInfo"
    , "rasterizationSamples"
    , "pSampleMask"
    , 32
    )
  ]

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

