{-# LANGUAGE RecordWildCards #-}

module Spec.Savvy.TypeAlias
  ( TypeAlias(..)
  , specTypeAliases
  ) where

import           Data.Either.Validation
import           Data.Text              (Text)
import qualified Spec.Constant          as P
import           Spec.Savvy.Error
import qualified Spec.Spec              as P
import qualified Spec.Type              as P

data TypeAlias = TypeAlias
  { taName  :: Text
  , taAlias :: Text
  }
  deriving (Show)

specTypeAliases :: P.Spec -> Validation [SpecError] [TypeAlias]
specTypeAliases P.Spec {..} = pure
  (  [ TypeAlias {..} | P.AnAlias P.TypeAlias {..} <- sTypes ]
  ++ [ TypeAlias {..}
     | P.ABaseType P.BaseType {..} <- sTypes
     , let taName  = btName
           taAlias = btType
     ]
  ++ [ TypeAlias {..}
     | P.Constant {..} <- sConstants
     , Left P.ConstantAlias{..} <- [cValue]
     , let taName  = cName
           taAlias = unConstantAlias
     ]
  )
