{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Spec.Savvy.Struct
  ( Struct(..)
  , StructMember(..)
  , specStructs
  ) where

import           Control.Monad.Fix.Extra
import           Data.Either.Validation
import           Data.List               (find)
import           Data.Monoid             (Endo (..))
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Traversable
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Spec               as P
import qualified Spec.Type               as P

data Struct = Struct
  { sName      :: Text
  , sMembers   :: [StructMember]
  , sSize      :: Word -- Keep this lazy
  , sAlignment :: Word -- Keep this lazy
  , sComment   :: Maybe Text
  }
  deriving (Show)

data StructMember = StructMember
  { smName      :: Text
  , smType      :: Type
  , smOffset    :: Word -- Keep this lazy
  , smSize      :: Word -- Keep this lazy
  , smAlignment :: Word -- Keep this lazy
  , smComment   :: Maybe Text
  }
  deriving (Show)

specStructs :: TypeContext -> P.Spec -> Validation [SpecError] [Struct]
specStructs tc P.Spec {..} =
  let specStructs     = [ s | P.AStructType s <- sTypes ]
      specStructNames = P.stName <$> specStructs
  in  eitherToValidation $ fixLookupM specStructNames sName $ \getStruct ->
        let structTypeContext =
              extendTypeContext getStructSize getStructAlignment tc
            getField f = \case
              TypeName n | Just s <- getStruct n -> pure (f s)
              n -> Left [UnknownType (T.pack (show n))]

            getStructSize      = getField sSize
            getStructAlignment = getField sAlignment
        in  for specStructs (specStruct structTypeContext)

specStruct :: TypeContext -> P.StructType -> Either [SpecError] Struct
specStruct tc@TypeContext {..} P.StructType {..} = do
  rec sMembers <- validationToEither $ do
        let offsets = memberOffsets sMembers
        for (zipSameLength stMembers offsets) (uncurry (specStructMember tc))
  let
    sName      = stName
    sComment   = stComment
    sAlignment = foldl1 lcm (smAlignment <$> sMembers)
    lastMember = last sMembers
    sSize = nextAlignment sAlignment (smOffset lastMember + smSize lastMember)
  pure Struct {..}

specStructMember
  :: TypeContext
  -> P.StructMember
  -> Word
  -- ^ The offset in the struct
  -> Validation [SpecError] StructMember
specStructMember tc P.StructMember {..} smOffset = eitherToValidation $ do
  smType      <- stringToTypeExpected (tcParseContext tc) smName smType
  smAlignment <- getTypeAlignment tc smType
  smSize      <- getTypeSize tc smType
  pure StructMember {..}

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

