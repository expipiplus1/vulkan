{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Exception
  ( vkExceptionWriteElement
  ) where

import           Data.Either.Extra
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Extra as T
import           Data.Text.Prettyprint.Doc
import           Prelude hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented
import           Text.Pandoc

import           Documentation
import           Spec.Savvy.Enum
import           Write.Element

vkExceptionWriteElement :: (Documentee -> Maybe Documentation) -> Enum -> WriteElement
vkExceptionWriteElement getDocumentation vkResultEnum =
  let
    resultPatterns = (eeName <$> eElements vkResultEnum)
                  ++ (exName <$> eExtensions vkResultEnum)

    weName       = "VulkanException declaration"
    weImports    = [Import "Control.Exception" ["Exception(..)"]]
    weProvides   = Unguarded <$>
                     [ TypeConstructor "VulkanException"
                     , Term "VulkanException"
                     , Term "resultString"
                     ]
    weUndependableProvides = []
    weSourceDepends        = []
    weBootElement          = Nothing
    weDepends    = Unguarded <$> TypeName "VkResult"
                               : (PatternName <$> resultPatterns)
    weExtensions = [ "LambdaCase" ]
    weDoc = pure [qci|
      -- | This exception is thrown from calls to marshalled vulkan commands
      -- which return a negative VkResult.
      newtype VulkanException = VulkanException \{ vulkanExceptionResult :: VkResult }
        deriving (Eq, Ord, Read, Show)

      instance Exception VulkanException where
        displayException (VulkanException r) = show r ++ ": " ++ resultString r

      -- | A human understandable message for each VkResult
      resultString :: VkResult -> String
      resultString = \case
        {indent 0 . vcat . fmap (displayExceptionCase getDocumentation) $ resultPatterns}
        r -> show r
    |]
  in WriteElement{..}

displayExceptionCase :: (Documentee -> Maybe Documentation) -> Text -> Doc ()
displayExceptionCase getDocumentation pat = [qci|
  {pat} -> {fromMaybe ("show" <+> pretty pat) (documentationToString =<< getDocumentation (Nested "VkResult" pat))}
  |]

-- | Get a string expression from some documentation
documentationToString :: Documentation -> Maybe (Doc ())
documentationToString Documentation {..} =
  let writerOptions = def
  in  pretty . show . fixupResultDescription <$> eitherToMaybe
        (runPure (writePlain writerOptions (prepareForPlain dDocumentation)))

-- |
-- - Keep only the first sentence
-- - Drop the first word (it's the enum name)
fixupResultDescription :: Text -> Text
fixupResultDescription =
  T.takeWhile (/= '.') . T.unwords . tailSafe . T.words . T.replace "\8217" "'"

tailSafe :: [a] -> [a]
tailSafe = \case
  [] -> []
  _:xs -> xs

prepareForPlain :: Pandoc -> Pandoc
prepareForPlain = topDown removeEmph
  where
    removeEmph :: [Inline] -> [Inline]
    removeEmph is = removeEmphInline =<< is
    removeEmphInline :: Inline -> [Inline]
    removeEmphInline = \case
      Emph is -> is
      i -> [i]
