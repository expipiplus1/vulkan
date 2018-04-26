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
  -> Struct
  -> Either [SpecError] WriteElement
structWrapper isHandle isBitmask struct = do
  let weName = sName struct T.<+> "wrapper"
      isDefaultable t =
        maybe False (isHandle <||> isBitmask) (simpleTypeName t)
  (weDoc, (weImports, weProvides, weDepends, weExtensions, _)) <- either
    (throwError . fmap (WithContext (sName struct)))
    (pure . first (fmap vcat . sequence))
    (runWrap $ wrapStruct isDefaultable struct)
  pure WriteElement {..}

----------------------------------------------------------------
-- The wrapping commands
----------------------------------------------------------------

wrapStruct
  :: (Type -> Bool)
  -- ^ Is a defaultable type
  -> Struct
  -> WrapM [DocMap -> Doc ()]
wrapStruct isDefaultable s = do
  marshalled    <- marshallStruct isDefaultable s
  toCStructDoc  <- writeToCStruct marshalled
  marshalledDoc <- writeMarshalled marshalled
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
  | Preserved a
    -- ^ This is preserved without changes
  | NextPointer Text
    -- ^ This is the pNext pointer (include the member name for completeness)
  | ByteString Text
    -- ^ A const char with member name
  | MaybeByteString Text
    -- ^ An optional const char with member name
  | Vector Text Type
    -- ^ A vector with name and of type
  deriving (Functor, Foldable, Traversable)

----------------------------------------------------------------
-- Changing the member type
----------------------------------------------------------------

marshallStruct :: (Type -> Bool) -> Struct -> WrapM MarshalledStruct
marshallStruct isDefaultable Struct {..} =
  let lengthRelation = getLengthRelation sMembers
  in  MarshalledStruct sName
        <$> traverse (marshallMember isDefaultable lengthRelation) sMembers

-- This may
marshallMember
  :: (Type -> Bool)
  -- Is this a defaultable type
  -> [(StructMember, StructMember)]
  -- ^ The relation between lengths and arrays (pointers)
  -> StructMember
  -> WrapM (MemberUsage MarshalledMember)
marshallMember isDefaultable lengthRelation m = do
  let
    -- Go from a length name to a list of vectors having that length
    -- TODO: Rename
      lengthMap :: Text -> Maybe StructMember
      lengthMap =
        (`Map.lookup` Map.fromList [ (smName l, v) | (l, v) <- lengthRelation ])

  ty <- case smType m of
    Ptr _ Void | smName m == "pNext" -> pure (NextPointer (smName m))
    Ptr _ t | isPassAsPointerType t, Nothing <- smIsOptional m ->
      Preserved <$> toHsType t
    _ | Just [v] <- smValues m, Nothing <- smIsOptional m -> pure (Univalued v)
    Ptr Const Char
      | Nothing <- smIsOptional m, Just ["null-terminated"] <- smLengths m
      -> pure $ ByteString (smName m)
    Ptr Const Char
      | Just [True] <- smIsOptional m, Just ["null-terminated"] <- smLengths m
      -> pure $ MaybeByteString (smName m)
    Ptr _ t
      | Nothing <- smIsOptional m, Just [length] <- smLengths m, isSimpleType t
      -> pure $ Vector (smName m) t
    (Array _ _ t)
      | isSimpleType t, Nothing <- smIsOptional m, Nothing <- smLengths m
      -> Preserved <$> toHsType (smType m)
    t
      | isSimpleType t
      , Just v <- lengthMap (smName m)
      -> pure $ Length (smName v)
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

writeToCStruct :: MarshalledStruct -> WrapM (Doc ())
writeToCStruct MarshalledStruct{..} = pure $ [qci|
  withCStruct :: {msName} -> (C.{msName} -> IO a) -> IO a
  withCStruct from cont = {wrap "cont" ("C." <> msName) "from" msMembers}
  |]

wrap
  :: Text
  -- ^ The name of the continuation
  -> Text
  -- ^ The C struct constructor name
  -> Text
  -- ^ The marshalled struct name
  -> [MemberUsage MarshalledMember]
  -- ^ The struct members
  -> Doc ()
wrap contName con from members =
  let wrappers :: [Wrapper]
      wrappers = memberWrapper from <$> members
      applyCont :: Wrapper
      applyCont = \cont e -> cont [qci|({contName} ({e})|] <> ")"
  in  foldWrappers (applyCont : wrappers) id (pretty con)

memberWrapper :: Text -> MemberUsage MarshalledMember -> Wrapper
memberWrapper from = \case
  Univalued value  -> \cont e -> cont [qci|{e} {value}|]
  Length vec       -> \cont e -> cont [qci|{e} (fromIntegral (length {vec}))|]
  Preserved member -> \cont e -> cont [qci|{e} ({mmName member} {from})|]
  -- TODO: proper pointer names
  NextPointer memberName -> \cont e ->
    let withPtr =
          [qci|({memberName} {from}) (\\{ptrName memberName} -> {e} {ptrName memberName}|]
    in [qci|{cont withPtr})|]
  ByteString memberName -> \cont e ->
    let paramPtr = pretty (unKeyword $ ptrName (dropPointer memberName))
        withPtr  = [qci|useAsCString ({memberName} {from}) (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|{cont withPtr})|]
  MaybeByteString memberName -> \cont e ->
    let paramPtr = pretty (unKeyword $ ptrName (dropPointer memberName))
        withPtr  = [qci|maybeWith useAsCString ({memberName} {from}) (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|{cont withPtr})|]
  Vector memberName elemType -> \cont e ->
    let paramPtr = pretty (ptrName (dropPointer memberName))
        withPtr  = [qci|todoWithVector ({memberName} {from}) (\\{paramPtr} -> {e} {paramPtr}|]
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
  Univalued _ -> pure $ \_ -> Left "-- Univalued Member elided"
  Length    _ -> pure $ \_ -> Left "-- Length valued member elided"
  NextPointer memberName ->
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: forall a. (Ptr () -> IO a) -> IO a
    |]
  ByteString memberName ->
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: ByteString
    |]
  -- TODO: Abstract over Maybe here
  MaybeByteString memberName ->
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Maybe ByteString
    |]
  Vector memberName t -> do
    tyName <- toHsType t
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName memberName)}
      {pretty (toMemberName memberName)} :: Vector {tyName}
    |]
  Preserved MarshalledMember{..} -> do
    pure $ \getDoc -> Right [qci|
      {document getDoc (Nested parentName mmName)}
      {pretty (toMemberName mmName)} :: {mmType}
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

{-
import           Data.Vector           as V
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

withVec
  :: forall a b d
   . Storable b
  => (forall c . a -> (b -> IO c) -> IO c)
  -> Vector a
  -> (Ptr b -> IO d)
  -> IO d
withVec alloc v cont = allocaArray (V.length v) $ \p ->
  let go :: Int -> ((b -> IO d) -> IO d) -> IO d -> IO d
      go index with complete = with (\b -> pokeElemOff p index b *> complete)
  in  ifoldr go (cont p) (fmap alloc v)

   -}
