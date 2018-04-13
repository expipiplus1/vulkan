{-# LANGUAGE OverloadedStrings #-}

module Spec.Savvy.Preprocess
  ( createPreprocessor
  ) where

import           Control.Monad.Trans.Except
import           Data.Maybe
import           Data.Text
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Hpp
import           Spec.Savvy.Define
import           Spec.Savvy.Error

initialState :: HppState
initialState = fromMaybe (error "Preprocessor definition did not parse")
                         (addDefinition "__x86_64__" "1" emptyHppState)

createPreprocessor :: [Define] -> Either [SpecError] (Text -> Either [SpecError] Text)
createPreprocessor = fmap preprocessText . createPreprocessState

createPreprocessState :: [Define] -> Either [SpecError] HppState
createPreprocessState ds =
  case
      runExcept
        (expand initialState
                (Hpp.preprocess (T.encodeUtf8 <$> (T.lines . dText =<< ds)))
        )
    of
      Left  e      -> Left [DefineError (T.pack (show e))]
      Right (_, s) -> pure s

preprocessText :: HppState -> Text -> Either [SpecError] Text
preprocessText s t =
  case runExcept (expand s (Hpp.preprocess [T.encodeUtf8 t])) of
    Left e       -> Left [PreprocessorError (T.pack (show e))]
    Right (o, _) -> pure . T.strip . T.unlines $ fmap T.decodeUtf8 (hppOutput o)

