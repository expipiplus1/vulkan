{-# language QuasiQuotes #-}

module Render.VkException
  ( vkExceptionRenderElement
  ) where

import           Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Data.Vector                   as V
import           Polysemy.Input
import           Relude                  hiding ( ask )
import           Text.InterpolatedString.Perl6.Unindented
import           Text.Pandoc

import           Control.Exception

import qualified Data.Text.Extra               as T
import           Documentation
import           Error
import           Haskell                       as H
import           Haskell.Name                   ( )
import           Render.Element
import           Render.SpecInfo
import           Render.Type
import           Spec.Types

vkExceptionRenderElement
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => SpecFlavor
  -> (Documentee -> Maybe Documentation)
  -> Enum'
  -> Sem r RenderElement
vkExceptionRenderElement specFlavor getDocumentation vkResultEnum =
  genRe "Exception declaration" $ do
    tellExplicitModule =<< mkModuleName ["Exception"]
    tellNotReexportable
    RenderParams {..} <- input
    tellImportWithAll ''Control.Exception.Exception
    vkResultTyDoc <- renderType =<< cToHsType DoNotPreserve successCodeType
    tellImportWithAll (mkTyName (eName vkResultEnum))
    tellExport (EData exceptionTypeName)
    tellExport (ETerm (TermName "resultString"))
    let resultPatterns = evName <$> eValues vkResultEnum
    let lower, upper :: Doc ()
        (lower, upper) = case specFlavor of
          SpecVk -> ("vulkan", "Vulkan")
          SpecXr -> ("openxr", "OpenXR")
    cases <- V.mapMaybe id <$> forV
      resultPatterns
      (displayExceptionCase specFlavor getDocumentation)
    tellDoc [qci|
        -- | This exception is thrown from calls to marshalled {upper} commands
        -- which return a negative 'Result'.
        newtype {exceptionTypeName} = {exceptionTypeName} \{ {lower}ExceptionResult :: {vkResultTyDoc} }
          deriving (Eq, Ord, Read, Show)

        instance Exception {exceptionTypeName} where
          displayException ({exceptionTypeName} r) = show r ++ ": " ++ resultString r

        -- | A human understandable message for each 'Result'
        resultString :: {vkResultTyDoc} -> String
        resultString = \case
        {indent 2 . vcat $ V.toList cases}
          r -> show r
      |]

displayExceptionCase
  :: HasRenderParams r
  => SpecFlavor
  -> (Documentee -> Maybe Documentation)
  -> CName
  -> Sem r (Maybe (Doc ()))
displayExceptionCase specFlavor getDocumentation pat = do
  RenderParams {..} <- input
  let pat' = mkPatternName pat
      res  = case specFlavor of
        SpecVk -> "VkResult"
        SpecXr -> "XrResult"
  pure $ fmap
    ((pretty pat' <+> "->") <+>)
    (documentationToString specFlavor =<< getDocumentation (Nested res pat))

-- | Get a string expression from some documentation
documentationToString :: SpecFlavor -> Documentation -> Maybe (Doc ())
documentationToString specFlavor Documentation {..} =
  let writerOptions = def
  in  viaShow . fixupResultDescription specFlavor <$> eitherToMaybe
        (runPure (writePlain writerOptions (prepareForPlain dDocumentation)))

-- |
-- Vulkan:
-- - Keep only the first sentence (vulkan only)
-- - Drop the first word (it's the enum name) (for vulkan)
--
-- OpenXR
-- - Swap the leading "The" for "A"
fixupResultDescription :: SpecFlavor -> Text -> Text
fixupResultDescription = \case
  SpecVk -> T.takeWhile (/= '.') . T.unwords . tailSafe . T.words . T.replace
    "\8217"
    "'"
  SpecXr ->
    T.upperCaseFirst
      . T.dropWhileEnd (== '.')
      . T.strip
      . replacePrefixSafe "The" "A"
      . T.unwords
      . T.words
      . T.replace "\8217" "'"

tailSafe :: [a] -> [a]
tailSafe = \case
  []     -> []
  _ : xs -> xs

replacePrefixSafe :: Text -> Text -> Text -> Text
replacePrefixSafe p r t =
  if p `T.isPrefixOf` t then r <> T.drop (T.length p) t else t

prepareForPlain :: Pandoc -> Pandoc
prepareForPlain = topDown removeEmph
 where
  removeEmph :: [Inline] -> [Inline]
  removeEmph is = removeEmphInline =<< is
  removeEmphInline :: Inline -> [Inline]
  removeEmphInline = \case
    Emph is -> is
    i       -> [i]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = \case
  Left  _ -> Nothing
  Right x -> Just x
