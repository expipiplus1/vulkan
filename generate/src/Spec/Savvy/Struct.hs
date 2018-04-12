{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Savvy.Struct
  ( Struct(..)
  , StructMember(..)
  , specStructs
  ) where

import           Data.Either.Validation
import           Data.Text
import           Data.Traversable
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Spec              as P
import qualified Spec.Type              as P

data Struct = Struct
  { sName    :: Text
  , sMembers :: [StructMember]
  , sComment :: Maybe Text
  }
  deriving (Show)

data StructMember = StructMember
  { smName    :: Text
  , smType    :: Type
  , smComment :: Maybe Text
  }
  deriving (Show)

specStructs :: TypeParseContext -> P.Spec -> (Validation [SpecError] [Struct])
specStructs pc P.Spec{..} =
  let specStructs = [s | P.AStructType s <- sTypes]
  in for specStructs $ \P.StructType{..} -> do
       let sName = stName
           sComment = stComment
       sMembers <- for stMembers $ \P.StructMember{..} -> do
         smType <- eitherToValidation $ stringToTypeExpected pc smName smType
         pure StructMember{..}
       pure Struct{..}

