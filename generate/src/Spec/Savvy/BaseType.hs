{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.BaseType
  ( BaseType(..)
  , specBaseTypes
  ) where

import           Data.Either.Validation
import qualified Data.HashSet           as HashSet
import           Data.Text
import           Language.C.Types.Parse
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Spec              as P
import qualified Spec.Type              as P

data BaseType = BaseType
  { btName :: Text
  , btType :: Type
  }
  deriving (Show)

specBaseTypes :: TypeParseContext -> P.Spec -> Validation [SpecError] [BaseType]
specBaseTypes pc P.Spec {..} =
  let parsedBaseTypes = [ h | P.ABaseType h <- sTypes ]
      basetypeNames = [ btName | P.BaseType {..} <- parsedBaseTypes ]
      pc'         = CParserContext
        (cpcIdentName pc)
        (HashSet.filter ((`notElem` basetypeNames) . pack . unCIdentifier)
                        (cpcTypeNames pc)
        )
        (cpcParseIdent pc)
        (cpcIdentToString pc)
  in  sequenceA
        [ eitherToValidation
          $   BaseType btName
          <$> stringToTypeExpected pc' btName btType
        | P.BaseType {..} <- parsedBaseTypes
        ]
