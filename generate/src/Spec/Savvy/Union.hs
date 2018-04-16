{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Spec.Savvy.Union
  ( Union(..)
  , UnionMember(..)
  , specUnions
  ) where

import           Control.Monad.Fix.Extra
import           Data.Either.Validation
import           Data.List               (find)
import           Data.Monoid             (Endo (..))
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Traversable
import           Spec.Savvy.Error
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import qualified Spec.Spec               as P
import qualified Spec.Type               as P

data Union = Union
  { sName         :: Text
  , sAlternatives :: [StructMember]
  , sSize         :: Word -- Keep this lazy
  , sAlignment    :: Word -- Keep this lazy
  , sComment      :: Maybe Text
  }
  deriving (Show)

specUnions :: TypeContext -> P.Spec -> Validation [SpecError] [Union]
specUnions tc P.Spec {..} =
  let specUnions     = [ s | P.AUnionType s <- sTypes ]
      specUnionNames = P.stName <$> specUnions
  in  eitherToValidation $ fixLookupM specUnionNames sName $ \getUnion ->
        let unionTypeContext =
              extendTypeContext getUnionSize getUnionAlignment tc
            getField f = \case
              TypeName n | Just s <- getUnion n -> pure (f s)
              n -> Left [UnknownType (T.pack (show n))]

            getUnionSize      = getField sSize
            getUnionAlignment = getField sAlignment
        in  for specUnions (specUnion unionTypeContext)

specUnion :: TypeContext -> P.UnionType -> Either [SpecError] Union
specUnion tc@TypeContext {..} P.UnionType {..} = do
  rec sMembers <- validationToEither $ do
        let offsets = memberOffsets sMembers
        for (zipSameLength stMembers offsets) (uncurry (specUnionMember tc))
  let
    sName      = stName
    sComment   = stComment
    sAlignment = foldl1 lcm (smAlignment <$> sMembers)
    lastMember = last sMembers
    sSize = nextAlignment sAlignment (smOffset lastMember + smSize lastMember)
  pure Union {..}

specUnionMember
  :: TypeContext
  -> P.UnionMember
  -> Word
  -- ^ The offset in the union
  -> Validation [SpecError] UnionMember
specUnionMember tc P.UnionMember {..} smOffset = eitherToValidation $ do
  smType      <- stringToTypeExpected (tcParseContext tc) smName smType
  smAlignment <- getTypeAlignment tc smType
  smSize      <- getTypeSize tc smType
  pure UnionMember {..}

memberOffsets :: [UnionMember] -> [Word]
memberOffsets = tail . fmap fst . scanl go (0, 0)
  where
    go
      :: (Word, Word)
      -- ^ The beginning and end of the previous union
      -> UnionMember
      -- ^ The union to determine the offset for
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

