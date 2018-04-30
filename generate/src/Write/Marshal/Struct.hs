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
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Monoid                              (Endo (..))
import qualified Data.MultiMap                            as MultiMap
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
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
      isDefaultable t =
        maybe False (isHandle <||> isBitmask) (simpleTypeName t)
      isStructType t =
        maybe False isStruct (simpleTypeName t)
  (weDoc, (weImports, weProvides, weDepends, weExtensions, _)) <- either
    (throwError . fmap (WithContext (sName struct)))
    (pure . first (fmap vcat . sequence))
    (runWrap $ wrapStruct isDefaultable isStructType struct)
  pure WriteElement {..}

vkStructWriteElement :: WriteElement
vkStructWriteElement =
  let weName = "VkStruct class declaration"
      weImports = [ Import "Foreign.Storable" ["Storable", "poke", "pokeElemOff"]
                  , Import "Foreign.Ptr" ["Ptr"]
                  , Import "Foreign.Marshal.Alloc" ["alloca"]
                  , Import "Foreign.Marshal.Array" ["allocaArray"]
                  , Import "Data.Vector" ["Vector", "ifoldr"]
                  , QualifiedImport "Data.Vector" ["length"]
                  ]
      weProvides =
        [Unguarded $ WithConstructors $ WE.TypeName "VkStruct"]
      weDepends = []
      weExtensions = ["FunctionalDependencies", "RankNTypes"]
      weDoc = pure [qci|
        class Storable c => VkStruct marshalled c | marshalled -> c, c -> marshalled where
          withCStruct :: marshalled -> (c -> IO a) -> IO a

        withCStructPtr :: VkStruct a c => a -> (Ptr c -> IO b) -> IO b
        withCStructPtr a f = withCStruct a (\c -> alloca (\p -> poke p c *> f p))

        -- withVec
        --   :: forall a b c
        --    . VkStruct a c
        --   => Vector a
        --   -> (Ptr c -> IO b)
        --   -> IO b
        -- withVec v cont = allocaArray (Data.Vector.length v) $ \p ->
        --   let go :: Int -> ((b -> IO d) -> IO d) -> IO d -> IO d
        --       go index a complete = withCStruct a (\b -> pokeElemOff p index b *> complete)
        --   in  ifoldr go (cont p) v

        withVec
          :: forall a b d
           . Storable b
          => (forall c . a -> (b -> IO c) -> IO c)
          -> Vector a
          -> (Ptr b -> IO d)
          -> IO d
        withVec alloc v cont = allocaArray (Data.Vector.length v) $ \p ->
          let go :: Int -> a -> IO d -> IO d
              go index x complete = alloc x (\b -> pokeElemOff p index b *> complete)
          in  ifoldr go (cont p) v
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
  toCStructDoc  <- writeVkStructInstance marshalled
  marshalledDoc <- writeMarshalled marshalled
  tellExtension "DuplicateRecordFields"
  let weDoc docMap = marshalledDoc docMap <> line <> toCStructDoc
  pure [weDoc]

----------------------------------------------------------------
--
----------------------------------------------------------------

data MarshalledStruct = MarshalledStruct
  { msName    :: Text
  , msMembers :: [MemberUsage MarshalledMember]
  }

data MarshalledMember = MarshalledMember
  { mmName :: Text
  , mmType :: Doc ()
  }

data MemberUsage a
  = Univalued Text
    -- ^ This is a type which is allowed just one possible value, so we elide
    -- it
  | Length Text
    -- ^ This is bound to be the length of an array
  | OptionalLength Text
    -- ^ This is bound to be the length of an optional array
  | Preserved a
    -- ^ This is preserved without changes
  | NextPointer Text
    -- ^ This is the pNext pointer (include the member name for completeness)
  | ByteString Text
    -- ^ A const char with member name
  | MaybeByteString Text
    -- ^ An optional const char with member name
  | Vector Text Type (Doc ())
    -- ^ A vector with name and of type with the alloc function for the members
  | OptionalVector Text Type (Doc ())
    -- ^ An optional vector with name and type and allocator, represented with
    -- Maybe.
  | OptionalPtr Text Type (Doc ())
    -- ^ An optional value with name and type and allocator, represented with
    -- Maybe.
  deriving (Functor, Foldable, Traversable)

----------------------------------------------------------------
-- Changing the member type
----------------------------------------------------------------

marshallStruct :: (Type -> Bool) -> (Type -> Bool) -> Struct -> WrapM MarshalledStruct
marshallStruct isStruct isDefaultable Struct {..} = do
  let lengthRelation = getLengthRelation sMembers
  when (sStructOrUnion == AUnion) $ throwError [Other "Unions not supported"]
  MarshalledStruct (dropVkType sName)
    <$> traverse (marshallMember isStruct isDefaultable lengthRelation) sMembers

-- This may
marshallMember
  :: (Type -> Bool)
  -- Is this a struct type
  -> (Type -> Bool)
  -- Is this a defaultable type
  -> [(StructMember, StructMember)]
  -- ^ The relation between lengths and arrays (pointers)
  -> StructMember
  -> WrapM (MemberUsage MarshalledMember)
marshallMember isStruct isDefaultable lengthRelation m = do
  let
    -- Go from a length name to a list of vectors having that length
    -- TODO: Rename
      lengthMap :: Text -> Maybe StructMember
      lengthMap =
        (`Map.lookup` Map.fromList [ (smName l, v) | (l, v) <- lengthRelation ])

  ty <- case smType m of
    Ptr _ Void | smName m == "pNext" -> pure (NextPointer (smName m))
    Ptr _ t | isPassAsPointerType t, Nothing <- smIsOptional m ->
      Preserved <$> toHsType (smType m)
    _ | Just [v] <- smValues m, Nothing <- smIsOptional m -> pure (Univalued v)
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
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure
          $ Vector (smName m) (TypeName (dropVkType tyName)) "withCStruct"
        else pure $ Vector (smName m) t "(flip ($))"
      | -- An optional array
        Just [True] <- smIsOptional m
      , Just [length] <- smLengths m
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ OptionalVector (smName m)
                                   (TypeName (dropVkType tyName))
                                   "withCStruct"
        else pure $ OptionalVector (smName m) t "(flip ($))"
      | -- An array of strings
        Nothing <- smIsOptional m
      , Just [length, "null-terminated"] <- smLengths m
      , Ptr Const Char <- t
      -> do
        tellImport "Data.ByteString" "useAsCString"
        tellImport "Data.ByteString" "ByteString"
        pure $ Vector (smName m) (TypeName "ByteString") "useAsCString"
      | -- An optional pointer to a single value (no length)
        Just [True] <- smIsOptional m
      , Nothing <- smLengths m
      , Just tyName <- simpleTypeName t
      -> if isStruct t
        then -- If this is a struct, use the marshalled version
             pure $ OptionalPtr (smName m)
                                (TypeName (dropVkType tyName))
                                "withCStruct"
        else pure $ OptionalPtr (smName m) t "(flip ($))"
    -- (Array _ _ t)
    --   | isSimpleType t, Nothing <- smIsOptional m, Nothing <- smLengths m
    --   -> Preserved <$> toHsType (smType m)
    t
      | -- The length of a non-optional vector
        isSimpleType t
      , Just v <- lengthMap (smName m)
      , Nothing <- smIsOptional v
      -> pure $ Length (smName v)
      | -- The length of an optional vector
        isSimpleType t
      , Just v <- lengthMap (smName m)
      , Just [True] <- smIsOptional v
      -> pure $ OptionalLength (smName v)
      | isSimpleType t
      , Nothing <- lengthMap (smName m)
      , (isNothing (smIsOptional m) || isDefaultable t)
      , Nothing <- (smLengths m)
      -> do
        Preserved <$> toHsType t
    _ -> do
      throwError
        [ Other
            (     "Couldn't convert struct member"
            T.<+> (smName m)
            T.<+> ":"
            T.<+> T.tShow (smType m)
            )
        ]
  pure $ MarshalledMember (smName m) <$> ty

writeVkStructInstance
  :: MarshalledStruct -> WrapM (Doc ())
writeVkStructInstance MarshalledStruct{..} = do
  tellDepends
    [Unguarded (WE.TypeName ("Vk" <> msName)), Unguarded (TermName ("Vk" <> msName))]
  tellExtension "InstanceSigs"
  wrapped <- wrap "cont" ("Vk" <> msName) msName "from" msMembers
  pure [qci|
    instance VkStruct {msName} Vk{msName} where
      withCStruct :: {msName} -> (Vk{msName} -> IO a) -> IO a
      withCStruct from cont = {wrapped}
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
      accessMember memberName = [qci|{toMemberName memberName} ({from} :: {fromType})|]
  in \case
  Univalued value  -> do
    tellExtension "PatternSynonyms"
    tellDepend (Unguarded (PatternName value))
    pure $ \cont e -> cont [qci|{e} {value}|]
  Length vec       -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    pure $ \cont e -> cont [qci|{e} (fromIntegral (Data.Vector.length {accessMember vec}))|]
  OptionalLength vec       -> do
    tellImports [QualifiedImport "Data.Vector" ["length"]]
    tellImport "Data.Maybe" "maybe"
    pure $ \cont e -> cont [qci|{e} (maybe 0 (fromIntegral . Data.Vector.length) ({accessMember vec}))|]
  Preserved member -> pure $ \cont e -> cont [qci|{e} ({accessMember (mmName member)})|]
  -- TODO: proper pointer names
  NextPointer memberName -> pure $ \cont e ->
    let withPtr =
          [qci|({accessMember memberName}) (\\{ptrName memberName} -> {e} {ptrName memberName}|]
    in [qci|{cont withPtr})|]
  ByteString memberName -> do
    tellImport "Data.ByteString" "useAsCString"
    pure $ \cont e ->
      let paramPtr = pretty (unKeyword $ ptrName (dropPointer memberName))
          withPtr  = [qci|useAsCString ({accessMember memberName}) (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  MaybeByteString memberName -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    pure $ \cont e ->
      let paramPtr = pretty (unKeyword $ ptrName (dropPointer memberName))
          withPtr  = [qci|maybeWith useAsCString ({accessMember memberName}) (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  Vector memberName elemType alloc -> pure $ \cont e ->
    let paramPtr = pretty (ptrName (dropPointer memberName))
        withPtr  = [qci|withVec {alloc} ({accessMember memberName}) (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|{cont withPtr})|]
  OptionalVector memberName elemType alloc -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    pure $ \cont e ->
      let paramPtr = pretty (ptrName (dropPointer memberName))
          withPtr  = [qci|maybeWith (withVec {alloc}) ({accessMember memberName}) (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]
  OptionalPtr memberName elemType alloc -> do
    tellImport "Foreign.Marshal.Utils" "maybeWith"
    pure $ \cont e ->
      let paramPtr = pretty (ptrName (dropPointer memberName))
          withPtr  = [qci|maybeWith {alloc} ({accessMember memberName}) (\\{paramPtr} -> {e} {paramPtr}|]
      in  [qci|{cont withPtr})|]

-- | Returns (struct member containing length, name of member representing length,
-- vector struct members)
getLengthRelation :: [StructMember] -> [(StructMember, StructMember)]
getLengthRelation structMembers
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

      -- We have a pair if the length is determined by exactly one array
      uniqueLengthPairs :: [(Text, StructMember)]
      uniqueLengthPairs =
        [ (length, p)
        | (length, [p]) <- MultiMap.assocs (MultiMap.fromList arrays)
        ]
      -- A map from name to structmembers
      structmemberMap =
        (`Map.lookup` Map.fromList ((smName &&& id) <$> structMembers))
    in
      -- Only return pairs where the length is another structmember
      mapMaybe (\(length, p) -> (, p) <$> structmemberMap length)
               uniqueLengthPairs

----------------------------------------------------------------
-- Writing marshalled structs
----------------------------------------------------------------

writeMarshalled :: MarshalledStruct -> WrapM (DocMap -> Doc ())
writeMarshalled MarshalledStruct {..} = do
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

writeMarshalledMember :: Text -> MemberUsage MarshalledMember -> WrapM (DocMap -> Either (Doc ()) (Doc ()))
writeMarshalledMember parentName = \case
  Univalued      _ -> pure $ \_ -> Left "-- Univalued Member elided"
  Length         _ -> pure $ \_ -> Left "-- Length valued member elided"
  OptionalLength _ -> pure $ \_ -> Left "-- Optional length valued member elided"
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
  -- TODO: Abstract over Maybe here
  MaybeByteString memberName -> do
    tellImport "Data.ByteString" "ByteString"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe ByteString
    |]
  Vector memberName t _ -> do
    tyName <- toHsType t
    tellImport "Data.Vector" "Vector"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Vector {tyName}
    |]
  OptionalVector memberName t _ -> do
    tyName <- toHsType t
    tellImport "Data.Vector" "Vector"
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe (Vector {tyName})
    |]
  Preserved MarshalledMember{..} -> do
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName mmName)}
      {pretty (toMemberName mmName)} :: {mmType}
    |]
  OptionalPtr memberName t _ -> do
    tyName <- toHsType t
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe {tyName}
    |]

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

dropVkType :: Text -> Text
dropVkType = T.dropPrefix' "Vk"

{-
import           Data.Vector           as V
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

   -}
