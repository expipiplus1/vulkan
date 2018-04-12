{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Savvy.FuncPointer
  ( FuncPointer(..)
  , specFuncPointers
  ) where

import           Data.Either.Validation
import           Data.Text
import           Data.Traversable
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Spec              as P
import qualified Spec.Type              as P

data FuncPointer = FuncPointer
  { fpName :: Text
  , fpType :: Type
  }
  deriving (Show)

specFuncPointers
  :: TypeParseContext -> P.Spec -> Validation [SpecError] [FuncPointer]
specFuncPointers pc P.Spec {..} =
  let fpts = [ fp | P.AFuncPointerType fp <- sTypes ]
  in  for fpts $ \P.FuncPointerType {..} -> eitherToValidation $ do
        let fpName = fptName
        -- Use the type string without the name as the name has already been
        -- declared as a type and this confuses the parser.
        fpType <- stringToTypeExpected pc fpName fptTypeWithoutName
        pure FuncPointer {..}
