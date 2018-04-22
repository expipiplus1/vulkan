{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Handle
  ( Handle(..)
  , specHandles
  ) where

import           Data.Closure
import           Data.Either.Validation
import qualified Data.HashSet           as HashSet
import qualified Data.MultiMap          as MultiMap
import           Data.Text
import           Language.C.Types.Parse
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Spec              as P
import qualified Spec.Type              as P

data Handle = Handle
  { hName    :: Text
  , hType    :: Type
  , hAliases :: [Text]
  , hParents :: [Text]
  }
  deriving (Show)

specHandles
  :: (Text -> Either [SpecError] Text)
  -- ^ Preprocessor
  -> TypeParseContext
  -> P.Spec
  -> Validation [SpecError] [Handle]
specHandles preprocess pc P.Spec {..} =
  let parsedHandles = [ h | P.AHandleType h <- sTypes ]
      handleNames   = [ htName | P.HandleType {..} <- parsedHandles ]
      pc'           = CParserContext
        (cpcIdentName pc)
        (HashSet.filter ((`notElem` handleNames) . pack . unCIdentifier)
                        (cpcTypeNames pc)
        )
        (cpcParseIdent pc)
        (cpcIdentToString pc)
      handleAliases :: [(Text, Text)]
      handleAliases =
        [ (taAlias, taName)
        | P.AnAlias P.TypeAlias {..} <- sTypes
        , taCategory == "handle"
        ]
      aliasMap = MultiMap.fromList handleAliases
      getAliases s = closeNonReflexive (`MultiMap.lookup` aliasMap) [s]
  in  sequenceA
        [ eitherToValidation
          $   Handle htName
          <$> (stringToTypeExpected pc' htName =<< preprocess htType)
          <*> pure (getAliases htName)
          <*> pure htParents
        | P.HandleType {..} <- parsedHandles
        ]


