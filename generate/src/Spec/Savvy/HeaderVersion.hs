{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.HeaderVersion
  ( specHeaderVersion
  ) where

import           Control.Monad.Except
import           Data.Either.Validation
import           Data.Text
import           Text.Read

import           Spec.Savvy.Error

specHeaderVersion :: (Text -> Either [SpecError] Text) -> Validation [SpecError] Word
specHeaderVersion preprocess = eitherToValidation $ do
  t <- preprocess "VK_HEADER_VERSION"
  case readMaybe (unpack t) of
    Nothing -> throwError [UnableToParseHeaderVersion t]
    Just w  -> pure w
