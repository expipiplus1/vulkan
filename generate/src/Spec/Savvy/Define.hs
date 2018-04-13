{-# LANGUAGE RecordWildCards #-}

module Spec.Savvy.Define
  ( Define(..)
  , specDefines
  ) where

import           Data.Text
import qualified Spec.Spec as P
import qualified Spec.Type as P

data Define = Define
  { dName :: Text
  , dText :: Text
  }
  deriving (Show)

specDefines :: P.Spec -> [Define]
specDefines P.Spec {..} =
  [ Define {..} | P.ADefine P.Define {..} <- sTypes ]
