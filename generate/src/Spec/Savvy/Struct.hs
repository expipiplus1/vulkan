{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

module Spec.Savvy.Struct
  ( Struct(..)
  , StructMember(..)
  , StructOrUnion(..)
  , specStructs
  ) where

import           Control.Monad.Fix.Extra
import           Data.Closure
import           Data.Either.Validation
import           Data.Maybe
import qualified Data.MultiMap           as MultiMap
import           Data.Semigroup
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Traversable
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Spec               as P
import qualified Spec.Type               as P

data Struct = Struct
  { sName          :: Text
  , sMembers       :: [StructMember]
  , sSize          :: Word -- Keep this lazy
  , sAlignment     :: Word -- Keep this lazy
  , sComment       :: Maybe Text
  , sAliases       :: [Text]
    -- ^ The closure of struct aliases, doesn't include aliases from extensions
  , sStructOrUnion :: StructOrUnion
  }
  deriving (Show)

data StructMember = StructMember
  { smName      :: Text
  , smType      :: Type
  , smOffset    :: Word -- Keep this lazy
  , smSize      :: Word -- Keep this lazy
  , smAlignment :: Word -- Keep this lazy
  , smValues    :: [Text]
  , smComment   :: Maybe Text
  }
  deriving (Show)

data StructOrUnion
  = AStruct
  | AUnion
  deriving (Show, Eq)

specStructs :: TypeContext -> P.Spec -> Validation [SpecError] [Struct]
specStructs tc P.Spec {..}
  = let
      parsedStructs     = [ s | P.AStructType s <- sTypes ]
      parsedUnions      = [ u | P.AUnionType u <- sTypes ]
      parsedStructNames = (P.stName <$> parsedStructs) ++ (P.utName <$> parsedUnions)

      structAliases :: [(Text, Text)]
      structAliases =
        [ (taAlias, taName)
        | P.AnAlias P.TypeAlias {..} <- sTypes
        , taCategory == "struct"
        ]
      aliasMap = MultiMap.fromList structAliases
      getAliases s = closeNonReflexive (`MultiMap.lookup` aliasMap) [s]
    in
      eitherToValidation $ fixLookupM parsedStructNames sName $ \getStruct ->
        let structTypeContext =
              extendTypeContext getStructSize getStructAlignment tc
            getField f = \case
              TypeName n | Just s <- getStruct n -> pure (f s)
              n -> Left [UnknownType (T.pack (show n))]

            getStructSize      = getField sSize
            getStructAlignment = getField sAlignment
        in  (<>)
            <$> for parsedStructs (specStruct getAliases structTypeContext)
            <*> validationToEither
                  (for parsedUnions (specUnion getAliases structTypeContext))

----------------------------------------------------------------
-- Structs
----------------------------------------------------------------

specStruct
  :: (Text -> [Text])
  -> TypeContext
  -> P.StructType
  -> Either [SpecError] Struct
specStruct getAliases tc@TypeContext {..} P.StructType {..} = do
  rec sMembers <- validationToEither $ do
        let offsets = memberOffsets sMembers
        for (zipSameLength stMembers offsets) (uncurry (specStructMember tc))
  let
    sName          = stName
    sAliases       = getAliases sName
    sComment       = stComment
    sAlignment     = foldl1 lcm (smAlignment <$> sMembers)
    lastMember     = last sMembers
    sSize = nextAlignment sAlignment (smOffset lastMember + smSize lastMember)
    sStructOrUnion = AStruct
  pure Struct {..}

specStructMember
  :: TypeContext
  -> P.StructMember
  -> Word
  -- ^ The offset in the struct
  -> Validation [SpecError] StructMember
specStructMember tc P.StructMember {..} smOffset = eitherToValidation $ do
  type'       <- stringToTypeExpected (tcParseContext tc) smName smType
  smAlignment <- getTypeAlignment tc type'
  smSize      <- getTypeSize tc type'
  vals        <- pure (maybeToList smValues)
  pure StructMember {smType = type', smValues = vals, ..}

----------------------------------------------------------------
-- Unions
----------------------------------------------------------------

specUnion
  :: (Text -> [Text])
  -> TypeContext
  -> P.UnionType
  -> Validation [SpecError] Struct
specUnion getAliases tc@TypeContext {..} P.UnionType {..} = do
  sMembers <- for utMembers (\m -> specStructMember tc m 0)
  -- ApplicativeDo for this weirdness
  pure
    $ let
        sName          = utName
        sAliases       = getAliases sName
        sComment       = utComment
        sAlignment     = foldl1 lcm (smAlignment <$> sMembers)
        sSize = nextAlignment sAlignment (maximum (0 : (smSize <$> sMembers)))
        sStructOrUnion = AUnion
      in
        Struct {..}

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

memberOffsets :: [StructMember] -> [Word]
memberOffsets = tail . fmap fst . scanl go (0, 0)
  where
    go
      :: (Word, Word)
      -- ^ The beginning and end of the previous struct
      -> StructMember
      -- ^ The struct to determine the offset for
      -> (Word, Word)
    go (_, e) m =
      let b = nextAlignment (smAlignment m) e
      in (b, b + smSize m)

-- | Find the next multiple of an alignment
nextAlignment
  :: Word
  -- ^ The alignment
  -> Word
  -- ^ The value to align
  -> Word
  -- ^ The next multiple of alignment
nextAlignment alignment value =
  alignment * ((value + (alignment - 1)) `quot` alignment)

-- | zipSameLength is lazy in its second argument
zipSameLength :: [a] -> [b] -> [(a,b)]
zipSameLength []     _bs     = []
zipSameLength (a:as) ~(b:bs) = (a,b) : zipSameLength as bs
