{-# LANGUAGE OverloadedStrings #-}

module Spec.Savvy.HeaderVersion
  ( specHeaderVersion
  ) where

import           Control.Monad.Except
import           Data.Either.Validation
import           Data.Text.Extra

import           Spec.Savvy.Error

specHeaderVersion
  :: (Text -> Either [SpecError] Text) -> Validation [SpecError] Word
specHeaderVersion preprocess = eitherToValidation $ do
  t <- preprocess "VK_HEADER_VERSION"
  case readMaybe t of
    Nothing -> throwError [UnableToParseHeaderVersion t]
    Just w  -> pure w
