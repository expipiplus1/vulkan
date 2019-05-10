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
  , toMemberName
  ) where

import           Control.Arrow                            ((&&&))
import           Control.Bool
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Char                                (isUpper, toUpper)
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.List                                (partition)
import qualified Data.Map                                 as Map
import           Data.Maybe
import qualified Data.MultiMap                            as MultiMap
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Traversable
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import           Spec.Savvy.Handle
import           Documentation
import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Monad
import           Write.Marshal.Util
import           Write.Marshal.Struct.Utils
import           Write.Marshal.Wrap
import           Write.Util

import qualified Write.Marshal.Marshal as M


{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

structWrapper
  :: (Text -> Maybe Handle)
  -- ^ Is this a handle
  -> (Text -> Bool)
  -- ^ Is this the name of a bitmask type
  -> (Text -> Bool)
  -- ^ Is this the name of a struct type
  -> [Struct]
  -- ^ A list of all structs
  -> Struct
  -> Write [WriteElement]
structWrapper getHandle isBitmask isStruct allStructs struct = do
  let
    isNonDispatchableHandle n = case getHandle n of
      Just h  -> hHandleType h == NonDispatchable
      Nothing -> True
    isMarshalledNonDispatchableHandle = isNonDispatchableHandle . ("Vk" <>)
    isMarshalledBitmask               = isBitmask . ("Vk" <>)
    isMarshalledStruct                = isStruct . ("Vk" <>)
    isDefaultable t = maybe
      False
      (    isNonDispatchableHandle
      <||> isMarshalledNonDispatchableHandle
      <||> isBitmask
      <||> isDefaultableForeignType
      <||> isMarshalledBitmask
      )
      (simpleTypeName t)
    isStructType t =
      maybe False (isStruct <||> isMarshalledStruct) (simpleTypeName t)
    getTypeHandle h = getHandle . ("Vk" <>) =<< simpleTypeName h
    containsUnion              = doesStructContainUnion allStructs
    containsDispatchableHandle = doesStructContainDispatchableHandle
      (getHandle <=< simpleTypeName)
      allStructs
  (we, aliasWriteElements) <-
    runWE' (sName struct T.<+> "wrapper") $ wrapStruct
      isDefaultable
      isStructType
      getTypeHandle
      (containsUnion (sName struct))
      (containsDispatchableHandle (sName struct))
      struct
  pure (we : aliasWriteElements)

----------------------------------------------------------------
-- The wrapping commands
----------------------------------------------------------------

wrapStruct
  :: (Type -> Bool)
  -- ^ Is a defaultable type
  -> (Type -> Bool)
  -- ^ Is this a struct
  -> (Type -> Maybe Handle)
  -- ^ Is this a handle
  -> Bool
  -- ^ Does this struct contain a union
  -> Maybe Handle
  -- ^ Does this struct contain a dispatchable Handle
  -> Struct
  -> WE (DocMap -> Doc (), [WriteElement])
  -- ^ Returns the Docs for this struct, and any aliases
wrapStruct isDefaultable isStruct getHandle containsUnion containsDispatchableHandle s = do
  marshalled     <- marshallStruct isStruct isDefaultable getHandle s
  toCStructDoc   <- writeToCStructInstance s marshalled
  fromCStructDoc <- writeFromCStructInstance containsUnion containsDispatchableHandle marshalled
  zeroDoc        <- writeZeroInstance marshalled
  marshalledDoc  <- writeMarshalled s marshalled
  tellExtension "DuplicateRecordFields"
  tellExport (WithConstructors (WE.TypeName (dropVkType (sName s))))
  tellExport (Term (dropVkType (sName s)))
  let weDoc docMap =
        vcatPara [marshalledDoc docMap, toCStructDoc, fromCStructDoc, zeroDoc]
  aliases <- liftWrite $ traverseV (writeAlias (msMembers marshalled) s) (sAliases s)
  pure (weDoc, aliases)

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
      (Maybe (Doc ())) --- ^ allocator
      (Maybe (Doc ())) --- ^ from C representation
    -- ^ A fixed size array represented as a tuple
  | Preserved a
    -- ^ This is preserved without changes
  | PreservedMarshalled
      Text --- ^ member name
      Type --- ^ member type
    -- ^ An ordinary member, using the marshalled struct
  | DispatchableHandle
      Text --- ^ member name
      Handle --- ^ member type
    -- ^ A dispatchable handle
  | Bool
      Text --- ^ member name
    -- ^ This is a bool member
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
      (WE (Doc ())) --- ^ allocator
      (WE (Doc ())) --- ^ peekElemOff
    -- ^ A vector with name and of type with the alloc function for the members
  | BitMaskVector
      Text --- ^ The name of the member in the C struct which contains the
           -- length in bits
      Text --- ^ The type of length member
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
      (WE (Doc ())) --- ^ allocator
      (WE (Doc ())) --- ^ peekElemOff
    -- ^ An optional vector with name and type and allocator, represented with
    -- Maybe.
  | OptionalPtr Text Type (WE (Doc ())) (WE (Doc ()))
    -- ^ An optional value with name and type and allocator and fromCStructPtr,
    -- represented with Maybe.
  | NonOptionalPtr Text Type (WE (Doc ())) (WE (Doc ()))
    -- ^ An value with name and type and allocator and fromCStructPtr
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
  deriving (Functor, Foldable, Traversable)

instance Show (MemberUsage a) where
  show = \case
    Univalued {} -> "Univalued {}"
    Length {} -> "Length {}"
    LengthMultiple {} -> "LengthMultiple {}"
    OptionalLength {} -> "OptionalLength {}"
    FixedArrayValidCount {} -> "FixedArrayValidCount {}"
    FixedArray {} -> "FixedArray {}"
    FixedArrayNullTerminated {} -> "FixedArrayNullTerminated {}"
    FixedArrayZeroPadByteString {} -> "FixedArrayZeroPadByteString {}"
    FixedArrayZeroPad {} -> "FixedArrayZeroPad {}"
    FixedArrayTuple {} -> "FixedArrayTuple {}"
    Preserved {} -> "Preserved {}"
    PreservedMarshalled {} -> "PreservedMarshalled {}"
    DispatchableHandle {} -> "DispatchableHandle {}"
    Bool {} -> "Bool {}"
    NextPointer {} -> "NextPointer {}"
    ByteString {} -> "ByteString {}"
    ByteStringData {} -> "ByteStringData {}"
    ByteStringLength {} -> "ByteStringLength {}"
    MaybeByteString {} -> "MaybeByteString {}"
    Vector {} -> "Vector {}"
    BitMaskVector {} -> "BitMaskVector {}"
    OptionalVector {} -> "OptionalVector {}"
    OptionalPtr {} -> "OptionalPtr {}"
    NonOptionalPtr {} -> "NonOptionalPtr {}"
    EnabledFlag {} -> "EnabledFlag {}"
    OptionalZero {} -> "OptionalZero {}"
    SiblingVectorMaster {} -> "SiblingVectorMaster {}"
    SiblingVectorSlave {} -> "SiblingVectorSlave {}"
    MinLength {} -> "MinLength {}"

----------------------------------------------------------------
-- Changing the member type
----------------------------------------------------------------

marshallStruct
  :: (Type -> Bool)
  -> (Type -> Bool)
  -> (Type -> Maybe Handle)
  -> Struct -> WE MarshalledStruct
marshallStruct isStruct isDefaultable getHandle s@Struct {..} = do
  let lengthRelation = getLengthRelation s sMembers
  MarshalledStruct (dropVkType sName)
    <$> traverse
          (marshallMember isStruct isDefaultable getHandle lengthRelation s)
          sMembers
    <*> pure sStructOrUnion

-- This may
marshallMember
  :: (Type -> Bool)
  -- Is this a struct type
  -> (Type -> Bool)
  -- Is this a defaultable type
  -> (Type -> Maybe Handle)
  -- Get a handle if possible
  -> [(StructMember, [StructMember])]
  -- ^ The relation between lengths and arrays (pointers)
  -> Struct
  -- ^ The struct this member inhabits
  -> StructMember
  -> WE (MemberUsage MarshalledMember)
marshallMember isStruct isDefaultable getHandle lengthRelation struct m = do
  let
    -- Go from a length name to a list of vectors having that length
    -- TODO: Rename
    lengthMap :: Text -> Maybe [StructMember]
    lengthMap =
      (`Map.lookup` Map.fromList [ (smName l, v) | (l, v) <- lengthRelation ])

    findLengthMember :: Text -> Maybe StructMember
    findLengthMember =
      (`Map.lookup` Map.fromList [ (smName l, l) | (l, _) <- lengthRelation ])

    marshalledType :: Type -> Type
    marshalledType t
      | Just tyName <- simpleTypeName t
      , isStruct t
      = TypeName (dropVkType tyName)
      | Just tyName <- simpleTypeName t
      , tyName /= "VkBool32"
      , Just n <- T.dropPrefix "Vk" tyName
      = TypeName n
      | otherwise
      = t

    deepMarshalledType :: Type -> Type
    deepMarshalledType = \case
      Ptr cv t        -> Ptr cv (deepMarshalledType t)
      Array cv size t -> Array cv size (deepMarshalledType t)
      Proto t ps' ->
        Proto (deepMarshalledType t) (second deepMarshalledType <$> ps')
      t -> marshalledType t

    -- For a type get the "withCStruct" and "peekElemOff" for that type
    structWithPeekElem :: Type -> WE (WE (Doc ()), WE (Doc ()))
    structWithPeekElem t = do
      unless (isStruct t) $ throwError
        ("getting struct peek elem for non struct type:" T.<+> T.tShow t)
      tyName <- case simpleTypeName t of
        Nothing -> throwError
          ("getting struct peek elem for non simple type:" T.<+> T.tShow t)
        Just tyName -> pure tyName
      pure
        ( do
          tellDepend (TermName ("withCStruct" <> tyName))
          pure . pretty $ "withCStruct" <> tyName
        , do
          tellDepend (TermName ("fromCStruct" <> tyName))
          tellImport "Control.Monad"    "(<=<)"
          tellImport "Foreign.Storable" "peekElemOff"
          pure . pretty $ "((fromCStruct" <> tyName <> " <=<) . peekElemOff)"
        )

    nonStructWithPeekElem :: Type -> WE (WE (Doc ()), WE (Doc ()))
    nonStructWithPeekElem t = case getHandle t of
      Just h
        | Dispatchable <- hHandleType h
        -> let accessHandle =
                 pretty
                   . (<> "Handle")
                   . T.lowerCaseFirst
                   . dropVkType
                   . hName
                   $ h
           in
             case hLevel h of
               Nothing -> throwError
                 "stripping command table for a dispatchable handle without a level"
               Just _ -> pure
                 ( tellImport "Data.Function" "(&)"
                 $> "((&) . "
                 <> accessHandle
                 <> ")"
                 , do
                   tellImport "Foreign.Storable" "peekElemOff"
                   tyName <- toHsType t
                   pure
                     $  "(\\p i -> flip "
                     <> tyName
                     <> " commandTable <$> peekElemOff p i)"
                 )
      _ -> pure
        ( tellImport "Data.Function"    "(&)" $> "(&)"
        , tellImport "Foreign.Storable" "peekElemOff" $> "peekElemOff"
        )

    structWithPtr :: Type -> WE (WE (Doc ()), WE (Doc ()))
    structWithPtr t = do
      unless (isStruct t) $ throwError
        ("getting struct withPtr for non struct type:" T.<+> T.tShow t)
      tyName <- case simpleTypeName t of
        Nothing -> throwError
          ("getting struct withPtr for non simple type:" T.<+> T.tShow t)
        Just tyName -> pure tyName
      pure
        ( do
          tellDepend (TermName ("withCStruct" <> tyName))
          tellImport "Foreign.Marshal.Utils" "with"
          pure [qci|(\\a -> withCStruct{tyName} a . flip with)|]
        , do
          tellDepend (TermName ("fromCStruct" <> tyName))
          tellImport "Foreign.Storable" "peek"
          tellImport "Control.Monad"    "(<=<)"
          pure [qci|(fromCStruct{tyName} <=< peek)|]
        )


  ty <- case deepMarshalledType (smType m) of
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
      , Just [lengthName] <- smLengths m
      , Const <- cv
      , Just lengthMember <- findLengthMember lengthName
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
      , isSimpleType t
      -> do
        (with, peekElem) <- if isStruct t
          then structWithPeekElem t
          else nonStructWithPeekElem t
        pure $ Vector countName multiple (smName m) t with peekElem
      | -- An array with a length specified in bits
        -- TODO: Don't hardcode this
        Just ["(rasterizationSamples + 31) / 32"] <- smLengths m
      , [(countName, countType, elemBitSize)] <-
        [ (countName, countType, size)
        | (structName, countName, countType, vecName, size) <-
          bitMaskLengthRelation
        , structName == sName struct
        , vecName == smName m
        ]
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then do
          tellDepend (TermName ("withCStruct" <> tyName))
          -- If this is a struct, use the marshalled version
          pure $ BitMaskVector countName
                               countType
                               elemBitSize
                               (smName m)
                               (TypeName tyName)
                               (pretty $ "withCStruct" <> tyName)
        else pure $ BitMaskVector countName
                                  countType
                                  elemBitSize
                                  (smName m)
                                  t
                                  "(flip ($))"
      | -- An array
        Nothing <- smIsOptional m
      , Just [lengthName] <- smLengths m
      , Just _ <- findLengthMember lengthName
      , isSimpleType t
      -> do
        (with, peekElem) <- if isStruct t
          then structWithPeekElem t
          else nonStructWithPeekElem t
        pure $ Vector lengthName 1 (smName m) t with peekElem
      | -- An optional array
        Just opts <- smIsOptional m
      , opts == [True] || opts == [True, False]
      , Just [lengthName] <- smLengths m
      , Just tyName <- simpleTypeName t
      , Just lengthMember <- findLengthMember lengthName
      -> if isStruct t
        then do
          (with, peekElem) <- structWithPeekElem t
          -- If this is a struct, use the marshalled version
          pure $ OptionalVector (smName lengthMember)
                                (smName m)
                                (TypeName (dropVkType tyName))
                                with
                                peekElem
        else pure $ OptionalVector
          (smName lengthMember)
          (smName m)
          t
          (tellImport "Data.Function" "(&)" $> "(&)")
          (tellImport "Foreign.Storable" "peekElemOff" $> "peekElemOff")
      | -- An array of strings
        Nothing <- smIsOptional m
      , Just [lengthName, "null-terminated"] <- smLengths m
      , Ptr Const Char <- t
      -> do
        tellImport "Data.ByteString" "ByteString"
        pure $ Vector
          lengthName
          1
          (smName m)
          (TypeName "ByteString")
          (tellImport "Data.ByteString" "useAsCString" $> "useAsCString")
          (  tellDepend (TermName "packCStringElemOff")
          $> "packCStringElemOff"
          )
      | -- An optional pointer to a single value (no length)
        Just [True] <- smIsOptional m
      , Nothing <- smLengths m
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then do
          (withPtr, fromPtr) <- structWithPtr t
          -- If this is a struct, use the marshalled version
          pure $ OptionalPtr (smName m)
                             (TypeName (dropVkType tyName))
                             withPtr
                             fromPtr
        else pure $ OptionalPtr
          (smName m)
          t
          (tellImport "Foreign.Marshal.Utils" "with" $> "with")
          (tellImport "Foreign.Storable" "peek" $> "peek")
      | -- An pointer to a single value (no length)
        Nothing <- smIsOptional m
      , Nothing <- smLengths m
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then do
          (withPtr, fromPtr) <- structWithPtr t
          -- If this is a struct, use the marshalled version
          pure $ NonOptionalPtr (smName m)
                                (TypeName (dropVkType tyName))
                                withPtr
                                fromPtr
        else pure $ NonOptionalPtr
          (smName m)
          t
          (tellImport "Foreign.Marshal.Utils" "with" $> "with")
          (tellImport "Foreign.Storable" "peek" $> "peek")

    (Array _ size t)
      | SymbolicArraySize sizeSymbol <- size
      , sizeSymbol `elem` ["VK_UUID_SIZE", "VK_LUID_SIZE"]
      , TypeName "uint8_t" <- t
      , Nothing <- smIsOptional m
      , Nothing <- smLengths m
      , sReturnedOnly struct
      -> pure $ FixedArrayZeroPadByteString (smName m) size
      | SymbolicArraySize _sizeSymbol <- size
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
      -> do
        alloc <- if isStruct t
          then do
            tellDepend (TermName ("withCStruct" <> tyName))
            -- If this is a struct, use the marshalled version
            pure (pretty $ "withCStruct" <> tyName)
          else fst =<< nonStructWithPeekElem t
        let
          fromCRep = if isStruct t
            then Just (pretty $ "fromCStruct" <> tyName)
            else case getHandle t of
              Just h | Dispatchable <- hHandleType h ->
                let con = pretty . dropVkType . hName $ h
                in  Just (parens $ "\\p -> pure $ " <> con <> " p commandTable")
              _ -> Nothing
          ty = if isStruct t then TypeName (dropVkType tyName) else t
        FixedArray (smName countMember) (smName m) size ty alloc
          <$> dummyMember t
          <*> pure fromCRep
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
      , Just tyName <- simpleTypeName t
      -> pure $ if isStruct t
        then FixedArrayTuple (smName m)
                             sizeNumber
                             t
                             (Just (pretty $ "withCStruct" <> tyName))
                             (Just (pretty $ "fromCStruct" <> tyName))
        else FixedArrayTuple (smName m) sizeNumber t Nothing Nothing
      | -- A fixed length char array
        Char <- t
      , SymbolicArraySize _sizeSymbol <- size
      , Nothing <- smIsOptional m
      , Nothing <- smLengths m
      , sReturnedOnly struct
      -> pure $ FixedArrayNullTerminated (smName m) size
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
        | (structName, countName, _countType, _vecName, _size) <-
          bitMaskLengthRelation
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
        | (structName, _enableName, enabledName) <- enableRelation
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
      , Just opts <- smIsOptional v
      , opts == [True] || opts == [True, False]
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
      | -- A Bool
        Just "VkBool32" <- simpleTypeName t
      , Nothing <- lengthMap (smName m)
      , isNothing (smIsOptional m)
      , Nothing <- smLengths m
      -> pure $ Bool (smName m)
      | -- A boring normal type
        isSimpleType t
      , Nothing <- lengthMap (smName m)
      , isNothing (smIsOptional m)
        ||     isDefaultable t
        ||     (sName struct, smName m)
        `elem` ignoredOptionalAttrs
      , Nothing <- smLengths m
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ PreservedMarshalled (smName m) (TypeName tyName)
        else case getHandle t of
          Just h | Dispatchable <- hHandleType h ->
            pure $ DispatchableHandle (smName m) h
          _ -> Preserved <$> toHsType t
    _ -> throwError
          (     "Couldn't convert struct member"
          T.<+> smName m
          T.<+> ":"
          T.<+> T.tShow (smType m)
          )
  pure $ MarshalledMember (smName m) <$> ty

writeToCStructInstance
  :: Struct -> MarshalledStruct -> WE (Doc ())
writeToCStructInstance s MarshalledStruct{..} = do
  tellDepend (WE.TypeName ("Vk" <> msName))
  tellExport (Term ("withCStruct" <> msName))
  let marshalledName = "marshalled"
      size = pretty . show . sSize $ s
      align = pretty . show . sAlignment $ s
  case msStructOrUnion of
    AStruct -> do
      wrapped <- wrap "cont" ("Vk" <> msName) msName marshalledName msMembers
      schemes <- liftWrite $ M.marshalStructMembers s
      tellImport "Control.Monad.IO.Class" "liftIO"
      M.PokeSet {..} <- foldMap M.renderStructMemberPoke schemes
      pure [qci|
        -- | A function to temporarily allocate memory for a 'Vk{msName}' and
        -- marshal a '{msName}' into it. The 'Vk{msName}' is only valid inside
        -- the provided computation and must not be returned out of it.
        withCStruct{msName} :: {msName} -> (Vk{msName} -> IO a) -> IO a
        withCStruct{msName} {marshalledName} cont = {wrapped}

        withCStruct{msName}' :: {msName} -> (Ptr Vk{msName} -> IO a) -> IO a
        withCStruct{msName}' {msName}\{..} = runContT $ do
        {indent 2 . vsep $ ("p <- allocaBytesAligned" <+> size <+> align) : pokeSetContT}
          liftIO $ do
        {indent 4 . vsep $ pokeSetIO <> pokeSetPure <> pokeSetPoke}
          pure p
        |]
    AUnion -> do
      wrappedAlts <- for msMembers $ \case
        -- These only work with single member sums
        FixedArrayTuple n _ _ Nothing _ ->
          pure [qci|{T.upperCaseFirst n} t -> cont ({"Vk" <> T.upperCaseFirst n} (fromTuple t))|]
        PreservedMarshalled n t
          | Just tyName <- simpleTypeName t
          -> do tellDepend (TermName ("withCStruct" <> tyName))
                pure [qci|{T.upperCaseFirst n} s -> withCStruct{tyName} s (cont . {"Vk" <> T.upperCaseFirst n})|]
        m -> throwError ("Unhandled union member for wrapping:" T.<+> T.tShow m)

      pure [qci|
        -- | A function to temporarily allocate memory for a 'Vk{msName}' and
        -- marshal a '{msName}' into it. The 'Vk{msName}' is only valid inside
        -- the provided computation and must not be returned out of it.
        withCStruct{msName} :: {msName} -> (Vk{msName} -> IO a) -> IO a
        withCStruct{msName} {marshalledName} cont = case {marshalledName} of
        {indent 2 $ vcat wrappedAlts}
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
  -> WE (Doc ())
wrap contName con fromType from members = do
  wrappers <- traverse (memberWrapper fromType from) members
  let applyCont :: Wrapper
      applyCont cont e = cont [qci|{contName} ({e}|] <> ")"
  pure $ foldWrappers (applyCont : wrappers) id (pretty con)

memberWrapper
  :: Text
  -> Text
  -> MemberUsage MarshalledMember
  -> WE Wrapper
memberWrapper fromType from =
  let accessMember :: Text -> Doc ()
      accessMember memberName = [qci|({toMemberName memberName} ({from} :: {fromType}))|]
      makeParam = pretty . (<> "'")
  in \case
  Univalued value  -> do
    tellExtension "PatternSynonyms"
    tellDepend (PatternName value)
    pure $ \cont e -> cont [qci|{e} {value}|]
  Length vec       -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (Data.Vector.length {accessMember vec}))|]
  LengthMultiple vec mul -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    pure $ \cont e -> cont [qci|{e} ({mul} * fromIntegral (Data.Vector.length {accessMember vec}))|]
  MinLength [] _       ->
    throwError "MinLength has no non-optional vectors to examine"
  MinLength [vec] []       -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (Data.Vector.length {accessMember vec}))|]
  MinLength nonOptionalVectors [] -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    tellImports [Import "Data.List" ["minimum"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (minimum ({showQ $ list (("Data.Vector.length" <+>) . accessMember <$> nonOptionalVectors)})))|]
  MinLength nonOptionalVectors optionalVectors -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    tellImports [Import "Data.List" ["minimum"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (minimum ({showQ $ list (("Data.Vector.length" <+>) . accessMember <$> nonOptionalVectors)} ++ [Data.Vector.length v | Just v <- {list (accessMember <$> optionalVectors)}])))|]
  OptionalLength vec       -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    tellImport "Data.Maybe" "maybe"
    pure $ \cont e -> cont [qci|{e} (maybe 0 (fromIntegral . Data.Vector.length) {accessMember vec})|]
  FixedArrayValidCount vec -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (Data.Vector.length {accessMember vec}))|]
  EnabledFlag enabled       -> do
    tellImport "Data.Maybe" "maybe"
    tellDepends [PatternName "VK_FALSE", PatternName "VK_TRUE"]
    pure $ \cont e -> cont [qci|{e} (maybe VK_FALSE (const VK_TRUE) {accessMember enabled})|]
  OptionalZero memberName _ -> do
    tellImport "Data.Maybe" "fromMaybe"
    pure $ \cont e -> cont [qci|{e} (fromMaybe 0 {accessMember memberName})|]
  FixedArray _ memberName _ _ alloc dummy _ -> do
    tellQualifiedImport "Data.Vector.Generic.Sized" "convert"
    tellDepend (TermName "withArray")
    tellDepend (TermName "padSized")
    -- This assumes that this is a vector of handles or pointers
    pure $ \cont e ->
      let paramName = makeParam (ptrName memberName)
          with = [qci|withArray {alloc} {accessMember memberName} (\\{paramName} -> {e} (Data.Vector.Generic.Sized.convert (padSized {dummy} {paramName}))|]
      in [qci|{cont with})|]
  FixedArrayTuple memberName _ _ Nothing _ -> do
    tellImport "Data.Vector.Generic.Sized" "fromTuple"
    pure $ \cont e -> cont [qci|{e} (fromTuple {accessMember memberName})|]
  FixedArrayTuple memberName _ _ (Just alloc) _ -> do
    tellImport "Data.Vector.Generic.Sized" "fromTuple"
    tellImport "Data.Vector.Generic.Sized" "convert"
    tellDepend (TermName "withSizedArray")
    pure $ \cont e ->
      let paramName = makeParam memberName
          with = [qci|withSizedArray {alloc} (fromTuple {accessMember memberName}) (\\{paramName} -> {e} (Data.Vector.Generic.Sized.convert {paramName})|]
      in [qci|{cont with})|]
  FixedArrayNullTerminated memberName _ -> do
    tellDepend (TermName "byteStringToNullTerminatedSizedVector")
    -- This truncates oversized bytestrings
    pure $ \cont e -> cont [qci|{e} (byteStringToNullTerminatedSizedVector {accessMember memberName})|]
  FixedArrayZeroPadByteString memberName _ -> do
    tellDepend (TermName "byteStringToSizedVector")
    -- This truncates oversized bytestrings
    pure $ \cont e -> cont [qci|{e} (byteStringToSizedVector {accessMember memberName})|]
  FixedArrayZeroPad memberName _ _ -> do
    tellDepend (TermName "padSized")
    tellQualifiedImport "Data.Vector.Generic.Sized" "convert"
    -- This truncates oversized vectors
    pure $ \cont e -> cont [qci|{e} (Data.Vector.Generic.Sized.convert (padSized 0 {accessMember memberName}))|]
  Preserved member -> pure $ \cont e -> cont [qci|{e} {accessMember (mmName member)}|]
  DispatchableHandle member h -> do
    let accessHandle = pretty . (<> "Handle") . T.lowerCaseFirst . dropVkType . hName $ h
    pure $ \cont e -> cont [qci|{e} ({accessHandle} {accessMember member})|]
  PreservedMarshalled memberName t
    | Just tyName <- simpleTypeName t
    -> do tellDepend (TermName ("withCStruct" <> tyName))
          pure $ \cont e ->
            let paramName = makeParam $ memberName <> "'"
                with  = [qci|withCStruct{tyName} {accessMember memberName} (\\{paramName} -> {e} {paramName}|]
            in  [qci|{cont with})|]
    | otherwise
    -> throwError "PreservedMarshalled given non simple type"
  Bool memberName -> do
    tellDepend (TermName "boolToBool32")
    pure $ \cont e -> cont [qci|{e} (boolToBool32 {accessMember memberName})|]
  -- TODO: proper pointer names
  NextPointer memberName -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    tellSourceDepend (TermName "withSomeVkStruct")
    pure $ \cont e ->
      let withPtr =
            [qci|maybeWith withSomeVkStruct {accessMember memberName} (\\{ptrName memberName} -> {e} {ptrName memberName}|]
      in  [qci|{cont withPtr})|]
  ByteString memberName -> do
    tellImport "Data.ByteString" "useAsCString"
    pure $ \cont e ->
      let paramPtr = pretty (unReservedWord $ ptrName memberName)
          withPtr  = [qci|useAsCString {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  ByteStringData _ memberName -> do
    tellImport "Data.ByteString.Unsafe" "unsafeUseAsCString"
    tellImport "Foreign.Ptr" "castPtr"
    pure $ \cont e ->
      let paramPtr = pretty (unReservedWord $ ptrName memberName)
          withPtr  = [qci|unsafeUseAsCString {accessMember memberName} (\\{paramPtr} -> {e} (castPtr {paramPtr})|]
      in  [qci|{cont withPtr})|]
  ByteStringLength bs       -> do
    tellImports [QualifiedImport "Data.ByteString" ["length"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (Data.ByteString.length {accessMember bs}))|]
  MaybeByteString memberName -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    pure $ \cont e ->
      let paramPtr = pretty (unReservedWord $ ptrName memberName)
          withPtr  = [qci|maybeWith useAsCString {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  Vector _ _ memberName _elemType alloc _ -> do
    tellDepend (TermName "withVec")
    a <- alloc
    pure $ \cont e ->
      let paramPtr = pretty (ptrName memberName)
          withPtr  = [qci|withVec {a} {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  BitMaskVector lengthMemberName lengthMemberType bitSize memberName _elemType alloc -> do
    tellImport "Data.Bits" "zeroBits"
    tellImport "Data.Coerce" "coerce"
    tellDepend (WE.TypeName "VkFlags")
    tellDepend (TermName "withVec")
    tellDepend (TermName "padVector")
    tellDepend (WE.TypeName lengthMemberType)
    pure $ \cont e ->
      let paramPtr = pretty (ptrName memberName)
          withPtr  = [qci|withVec {alloc} (padVector zeroBits (fromIntegral ((coerce {accessMember lengthMemberName} :: VkFlags) + {pred bitSize}) `quot` {bitSize}) {accessMember memberName}) (\\{paramPtr} -> {e} {paramPtr}|]
      in [qci|{cont withPtr})|]
  OptionalVector _ memberName _elemType alloc _ -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    tellDepend (TermName "withVec")
    a <- alloc
    pure $ \cont e ->
      let paramPtr = pretty (ptrName memberName)
          withPtr  = [qci|maybeWith (withVec {a}) {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  OptionalPtr memberName _elemType alloc _ -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    a <- alloc
    pure $ \cont e ->
      let paramPtr = pretty (ptrName memberName)
          withPtr  = [qci|maybeWith {a} {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  NonOptionalPtr memberName _elemType alloc _ -> do
    a <- alloc
    pure $ \cont e ->
      let paramPtr = pretty (ptrName memberName)
          withPtr  = [qci|{a} {accessMember memberName} (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  SiblingVectorMaster {} -> error "Sibling vectors unimplemented"
  SiblingVectorSlave  {} -> error "Sibling vectors unimplemented"

----------------------------------------------------------------
-- FromCStruct
----------------------------------------------------------------

writeFromCStructInstance
  :: Bool
  -- ^ Does this struct contain a union
  -> Maybe Handle
  -- ^ Does this struct contain a dispatchable handle
  -> MarshalledStruct
  -> WE (Doc ())
writeFromCStructInstance containsUnion containsDispatchableHandle MarshalledStruct {..}
  = do
  tellDepend (WE.TypeName ("Vk" <> msName))
  case msStructOrUnion of
    AStruct ->
      if containsUnion
        then pure "-- No fromCStruct function for types containing unions"
        else case containsDispatchableHandle of
               Just h -> do
                  members <- traverse (fromCStructMember "c" (pretty $ "Vk" <> msName)) msMembers
                  tellExport (Term ("fromCStruct" <> msName))
                  cmdTableType <- case hLevel h of
                    Just Instance       -> do
                      tellDepend (WE.TypeName "InstanceCmds")
                      pure ("InstanceCmds" :: Doc ())
                    Just PhysicalDevice -> do
                      tellDepend (WE.TypeName "InstanceCmds")
                      pure "InstanceCmds"
                    Just Device         -> do
                      tellDepend (WE.TypeName "DeviceCmds")
                      pure "DeviceCmds"
                    Nothing             -> throwError "Dispatchable handle with no level"
                  pure [qci|
                    -- | A function to read a 'Vk{msName}' and all additional
                    -- structures in the pointer chain into a '{msName}'.
                    fromCStruct{msName} :: {cmdTableType} -> Vk{msName} -> IO {msName}
                    fromCStruct{msName} commandTable c = {msName} <$> {indent (-4) . vsep $ (intercalatePrependEither "<*>" $ members)}
                    |]
               Nothing -> do
                  members <- traverse (fromCStructMember "c" (pretty $ "Vk" <> msName)) msMembers
                  tellExport (Term ("fromCStruct" <> msName))
                  pure [qci|
                    -- | A function to read a 'Vk{msName}' and all additional
                    -- structures in the pointer chain into a '{msName}'.
                    fromCStruct{msName} :: Vk{msName} -> IO {msName}
                    fromCStruct{msName} c = {msName} <$> {indent (-4) . vsep $ (intercalatePrependEither "<*>" $ members)}
                    |]
    AUnion -> pure "-- No FromCStruct function for sum types"

fromCStructMember
  :: Doc ()
  -- ^ The name of the c struct we are translating
  -> Doc ()
  -- ^ The type of the c struct we are translating
  -> MemberUsage MarshalledMember
  -> WE (Either (Doc ()) (Doc ()))
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
        DispatchableHandle member h ->
          let con = dropVkType . hName $ h
          in pure $ Right [qci|pure ({con} {accessMember member} commandTable)|]
        PreservedMarshalled memberName t
          | Just tyName <- simpleTypeName t
          -> do tellDepend (TermName ("fromCStruct" <> tyName))
                pure $ Right [qci|(fromCStruct{tyName} {accessMember memberName})|]
        Bool memberName -> do
          tellDepend (TermName "bool32ToBool")
          pure $ Right [qci|pure (bool32ToBool {accessMember memberName})|]
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
              tellDepend (PatternName s)
              pure $ pretty s
          pure $ Right [qci|Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized {accessMember memberName}) (\p -> packCStringLen (castPtr p, {len}))|]
        NextPointer memberName -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          tellImport "Foreign.Ptr" "castPtr"
          tellSourceDepend (TermName "peekVkStruct")
          pure $ Right [qci|maybePeek peekVkStruct (castPtr {accessMember memberName})|]
        ByteString memberName -> do
          tellImport "Data.ByteString" "packCString"
          pure $ Right [qci|packCString {accessMember memberName}|]
        MaybeByteString memberName -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          tellImport "Data.ByteString" "packCString"
          pure $ Right [qci|maybePeek packCString {accessMember memberName}|]
        Vector lenMemberName 1 memberName _ _alloc peekElemOff -> do
          tellQualifiedImport "Data.Vector" "generateM"
          p <- peekElemOff
          pure $ Right [qci|(Data.Vector.generateM (fromIntegral {accessMember lenMemberName}) ({p} {accessMember memberName}))|]
        Vector lenMemberName multiple memberName _ _alloc peekElemOff -> do
          tellQualifiedImport "Data.Vector" "generateM"
          p <- peekElemOff
          pure $ Right [qci|(Data.Vector.generateM (fromIntegral {accessMember lenMemberName} `quot` {multiple}) ({p} {accessMember memberName}))|]
        BitMaskVector lenMemberName lenMemberType bitSize memberName _ _alloc -> do
          tellQualifiedImport "Data.Vector" "generateM"
          tellImport "Data.Coerce" "coerce"
          tellImport "Foreign.Storable" "peekElemOff"
          tellDepend (WE.TypeName "VkFlags")
          tellDepend (WE.TypeName lenMemberType)
          pure $ Right [qci|(Data.Vector.generateM (fromIntegral (((coerce {accessMember lenMemberName} :: VkFlags) + {pred bitSize}) `quot` {bitSize})) (peekElemOff {accessMember memberName}))|]
        OptionalVector lenMemberName memberName _ _alloc peekElemOff -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          tellQualifiedImport "Data.Vector" "generateM"
          p <- peekElemOff
          pure $ Right [qci|maybePeek (\p -> Data.Vector.generateM (fromIntegral {accessMember lenMemberName}) ({p} p)) {accessMember memberName}|]
        ByteStringData lenMemberName memberName -> do
          tellImport "Data.ByteString" "packCStringLen"
          tellImport "Foreign.Ptr" "castPtr"
          pure $ Right [qci|packCStringLen (castPtr {accessMember memberName}, fromIntegral {accessMember lenMemberName})|]
        OptionalPtr memberName _ _ fromPtr -> do
          tellImport "Foreign.Marshal.Utils" "maybePeek"
          f <- fromPtr
          pure $ Right [qci|maybePeek {f} {accessMember memberName}|]
        NonOptionalPtr memberName _ _ fromPtr -> do
          f <- fromPtr
          pure $ Right [qci|{f} {accessMember memberName}|]
        FixedArray validCountMemberName memberName _maxArraySize _elemType _alloc _dummy fromCRep -> do
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
          pure $ Right [qci|pure (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized {accessMember memberName}))|]
        FixedArrayTuple memberName length' _ _ Nothing -> do
          tellQualifiedImport "Data.Vector.Storable.Sized" "unsafeIndex"
          let tmp = "v" :: Doc ()
              tuple = tupled ((\i -> [qci|Data.Vector.Storable.Sized.unsafeIndex {tmp} {i}|]) <$> [0 .. pred length'])
          pure $ Right [qci|pure (let {tmp} = {accessMember memberName} in {tuple})|]
        FixedArrayTuple memberName length' _ _ (Just fromCStruct) -> do
          tellQualifiedImport "Data.Vector.Storable.Sized" "unsafeIndex"
          let tmp = "v" :: Doc ()
              cs :: [Doc ()]
              cs = (\i -> [qci|{fromCStruct} (Data.Vector.Storable.Sized.unsafeIndex {tmp} {i})|]) <$> [0 .. pred length']
              tuple :: Doc ()
              tuple = [qci|{tupled (replicate (fromIntegral length') mempty)} <$> {
                            indent (-4) . vsep $ (intercalatePrepend "<*>" $ cs)}
              |]
          pure $ Right [qci|(let {tmp} = {accessMember memberName} in {tuple})|]
        OptionalZero memberName _ ->
          pure $ Right [qci|pure (let nz = {accessMember memberName} in if nz == 0 then Nothing else Just nz)|]
        m -> throwError (T.tShow m)

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
        [ (length', ps)
        | (length', ps) <- MultiMap.assocs (MultiMap.fromList arrays)
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
      mapMaybe (\(length', p) -> (, p) <$> structmemberMap length')
               (lengthPairs ++ validCountPairs)

----------------------------------------------------------------
-- Writing marshalled structs
----------------------------------------------------------------

writeMarshalled :: Struct -> MarshalledStruct -> WE (DocMap -> Doc ())
writeMarshalled s MarshalledStruct {..} =
  case msStructOrUnion of
    AStruct -> do
      memberDocs <- traverse
        (writeMarshalledMember msName)
        msMembers
      mParams <- liftWrite $ M.marshalStructMembers s
      mDocs <- sequenceA . mapMaybe (writeMarshalledParam msName) $ mParams
      pure $ \getDoc -> [qci|
      {document getDoc (TopLevel ("Vk" <> msName))}
      data {msName} = {msName}
        \{ {
            indent (-2) . vsep . intercalatePrepend "," . fmap ($ getDoc) $ mDocs}
        }
        deriving (Show, Eq)
      |]
    AUnion -> do
      memberDocs <- traverse
        writeMarshalledUnionMember
        msMembers
      pure $ \getDoc -> [qci|
      {document getDoc (TopLevel msName)}
      data {msName}
        = {indent (-2) . vsep $
           intercalatePrependEither "|" (($ getDoc) <$> memberDocs)}
        deriving (Show, Eq)
      |]

writeMarshalledParam
  :: Text -> M.MarshalScheme StructMember -> Maybe (WE (DocMap -> Doc ()))
writeMarshalledParam parentName p = M.msMarshalledType p <&> \t -> do
  type' <- t
  pure $ \getDoc -> [qci|
    {document getDoc (Nested parentName (M.unName (M.msUnmarshalledName p)))}
    {M.unName (M.msMarshalledName p)} :: {type'}
  |]

writeMarshalledMember
  :: Text
  -> MemberUsage MarshalledMember
  -> WE (DocMap -> Either (Doc ()) (Doc ()))
writeMarshalledMember parentName = \case
  Univalued            _ -> pure $ \_ -> Left "-- Univalued member elided"
  Length               _ -> pure $ \_ -> Left "-- Length valued member elided"
  LengthMultiple     _ _ -> pure $ \_ -> Left "-- Length multiple valued member elided"
  MinLength          _ _ -> pure $ \_ -> Left "-- Length valued member elided"
  OptionalLength       _ -> pure $ \_ -> Left "-- Optional length valued member elided"
  ByteStringLength     _ -> pure $ \_ -> Left "-- Bytestring length valued member elided"
  FixedArrayValidCount _ -> pure $ \_ -> Left "-- Fixed array valid count member elided"
  EnabledFlag          _ -> pure $ \_ -> Left "-- enable flag member elided"
  NextPointer memberName -> do
    tellSourceDepend (WE.TypeName "SomeVkStruct")
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
  BitMaskVector _ _ _ memberName t _ -> do
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
  FixedArrayTuple memberName len t _ _-> do
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
  DispatchableHandle memberName h ->
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: {dropVkType (hName h)}
    |]
  PreservedMarshalled memberName memberType -> do
    tyName <- toHsType memberType
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: {tyName}
    |]
  Bool memberName ->
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Bool
    |]
  OptionalPtr memberName t _ _ -> do
    tyName <- toHsType t
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe {tyName}
    |]
  NonOptionalPtr memberName t _ _ -> do
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
  SiblingVectorMaster {} -> error "Sibling vectors unimplemented"
  SiblingVectorSlave  {} -> error "Sibling vectors unimplemented"

writeMarshalledUnionMember
  :: MemberUsage MarshalledMember
  -> WE (DocMap -> Either (Doc ()) (Doc ()))
writeMarshalledUnionMember = \case
  FixedArrayTuple n len t _ _-> do
    tyName <- toHsType t
    pure $ \_ -> Right [qci|
      {T.upperCaseFirst n} {tupled (replicate (fromIntegral len) tyName)}
    |]
  PreservedMarshalled n t -> do
    tyName <- toHsType t
    pure $ \_ -> Right [qci|
      {T.upperCaseFirst n} {tyName}
    |]
  m -> throwError ("Unhandled union member:" T.<+> T.tShow m)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

toMemberName :: Text -> Text
-- toMemberName = ("vk" <>) . T.upperCaseFirst
toMemberName = unReservedWord . T.lowerCaseFirst . dropPointer

-- | drop the first word if it is just @p@s and @s@s
upperCaseHungarian :: Text -> Text
upperCaseHungarian t = case T.break isUpper t of
  (firstWord, remainder) | T.all ((== 'p') <||> (== 's')) firstWord ->
    T.map toUpper firstWord <> remainder
  _ -> t

toVkMemberName :: Text -> Text
toVkMemberName = ("vk" <>) . T.upperCaseFirst . upperCaseHungarian

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
zeroPadMembers =
  [ ("VkDeviceGroupPresentCapabilitiesKHR"      , "presentMask")
  , ("VkPhysicalDeviceMemoryBudgetPropertiesEXT", "heapBudget")
  , ("VkPhysicalDeviceMemoryBudgetPropertiesEXT", "heapUsage")
  ]

-- The length member is the length in bytes of the vector
-- TODO: Extract this from the spec!
lengthMultipleRelation :: [(Text, Text, Text, Int)]
lengthMultipleRelation = [("VkShaderModuleCreateInfo", "codeSize", "pCode", 4)]

-- TODO: lol, "Haskell is strongly typed"
bitMaskLengthRelation :: [(Text, Text, Text, Text, Int)]
bitMaskLengthRelation =
  [ ( "VkPipelineMultisampleStateCreateInfo"
    , "rasterizationSamples"
    , "VkSampleCountFlagBits"
    , "pSampleMask"
    , 32
    )
  ]

-- | Generate a dummy member for padding (assume something is a handle if we
-- don't have a bespoke filler for it)
--
-- TODO: be a bit less lazy about this
dummyMember :: Type -> WE (Doc ())
dummyMember = \case
  TypeName "MemoryHeap" -> do
    tellImport "Data.Bits" "zeroBits"
    tellDepend (WE.TermName "VkMemoryHeap")
    pure "(VkMemoryHeap 0 zeroBits)"
  TypeName "MemoryType" -> do
    tellImport "Data.Bits" "zeroBits"
    tellDepend (WE.TermName "VkMemoryType")
    pure "(VkMemoryType zeroBits 0)"
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
  TypeName t -> t == "LPCWSTR"
  _          -> False

----------------------------------------------------------------
-- Aliases
----------------------------------------------------------------

writeAlias
  :: [MemberUsage MarshalledMember]
  -- ^ The types
  -> Struct
  -- ^ The original Struct
  -> Text
  -- ^ The alias name
  -> Write WriteElement
writeAlias _members Struct{..} name =
  runWE (name T.<+> "alias" T.<+> sName) $ do
    tellExport (TypeAlias (dropVkType name))
    tellDepend (WE.TypeName (dropVkType sName))
    tellExtension "PatternSynonyms"
    pure $ \_ -> [qci|
      type {dropVkType name} = {dropVkType sName}
      -- TODO: Pattern constructor alias)
    |]

----------------------------------------------------------------
-- Zero instance
----------------------------------------------------------------

writeZeroInstance :: MarshalledStruct -> WE (Doc ())
writeZeroInstance MarshalledStruct{..} =
  if hasNoZeroInstance msName
    then pure ""
    else do
      tellDepend (WE.TypeName "Zero")
      case msStructOrUnion of
        AStruct -> do
          zeros <- catMaybes <$> traverse writeMarshalledMemberZero msMembers
          pure [qci|
            instance Zero {msName} where
              zero = {msName} {indent 0 $ vsep zeros}
          |]
        AUnion -> do
          zero <- writeMarshalledMemberZero (head msMembers) >>= \case
            Nothing -> throwError ("Unhandled union member for Zero instance:" T.<+> T.tShow (head msMembers))
            Just z -> pure z
          n <- case head msMembers of
            FixedArrayTuple n _ _ Nothing _ ->
              pure n
            PreservedMarshalled n _ ->
              pure n
            m -> throwError ("Unhandled union member for Zero instance:" T.<+> T.tShow m)
          pure [qci|
            instance Zero {msName} where
              zero = {T.upperCaseFirst n} {zero}
          |]


writeMarshalledMemberZero
  :: MemberUsage MarshalledMember
  -> WE (Maybe (Doc ()))
writeMarshalledMemberZero = \case
  Univalued            _ -> pure Nothing
  Length               _ -> pure Nothing
  LengthMultiple     _ _ -> pure Nothing
  MinLength          _ _ -> pure Nothing
  OptionalLength       _ -> pure Nothing
  ByteStringLength     _ -> pure Nothing
  FixedArrayValidCount _ -> pure Nothing
  EnabledFlag          _ -> pure Nothing
  NextPointer _ ->
    pure $ Just "Nothing"
  ByteString _ -> do
    tellQualifiedImport "Data.ByteString" "empty"
    pure $ Just "Data.ByteString.empty"
  ByteStringData _ _ -> do
    tellQualifiedImport "Data.ByteString" "empty"
    pure $ Just "Data.ByteString.empty"
  FixedArrayNullTerminated _ _ -> do
    tellQualifiedImport "Data.ByteString" "empty"
    pure $ Just "Data.ByteString.empty"
  FixedArrayZeroPadByteString _memberName _ -> do
    tellQualifiedImport "Data.ByteString" "empty"
    pure $ Just "Data.ByteString.empty"
  FixedArrayZeroPad _memberName _ _t -> do
    tellQualifiedImport "Data.Vector" "empty"
    pure $ Just "Data.Vector.empty"
  MaybeByteString _memberName ->
    pure $ Just "Nothing"
  Vector{} -> do
    tellQualifiedImport "Data.Vector" "empty"
    pure $ Just "Data.Vector.empty"
  BitMaskVector{} -> do
    tellQualifiedImport "Data.Vector" "empty"
    pure $ Just "Data.Vector.empty"
  FixedArray{} -> do
    tellQualifiedImport "Data.Vector" "empty"
    pure $ Just "Data.Vector.empty"
  FixedArrayTuple _ len _t _ _->
    pure . Just $ tupled (replicate (fromIntegral len) "zero")
  OptionalVector _ _memberName _t _ _ ->
    pure $ Just "Nothing"
  Preserved MarshalledMember{..} ->
    pure $ Just "zero"
    -- | show mmType `elem` ([ t <> show w | t <- ["Int", "Word"], w <- [8,16,32,64] ] ++ ["CFloat"]) ->
    --   pure $ Just "0"
    -- | otherwise
    -- -> error (show (3,mmName, mmType))
  DispatchableHandle _memberName _h ->
    pure $ Just "zero"
  PreservedMarshalled _memberName _memberType ->
    pure $ Just "zero"
  Bool _memberName ->
    pure $ Just "False"
  OptionalPtr _memberName _t _ _ ->
    pure $ Just "Nothing"
  NonOptionalPtr _memberName _t _ _ ->
    pure $ Just "zero"
  OptionalZero _memberName _t ->
    pure $ Just "Nothing"
  SiblingVectorMaster {} -> error "Sibling vectors unimplemented"
  SiblingVectorSlave  {} -> error "Sibling vectors unimplemented"

----------------------------------------------------------------
-- Bespoke stuff
----------------------------------------------------------------

hasNoZeroInstance :: Text -> Bool
hasNoZeroInstance = (`elem` ["CmdProcessCommandsInfoNVX"])
