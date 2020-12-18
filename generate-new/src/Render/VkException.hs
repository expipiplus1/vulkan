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
  => (Documentee -> Maybe Documentation)
  -> Enum'
  -> Sem r RenderElement
vkExceptionRenderElement getDocumentation vkResultEnum =
  genRe "VulkanException declaration" $ do
    tellExplicitModule =<< mkModuleName ["Exception"]
    tellNotReexportable
    RenderParams {..} <- input
    tellImportWithAll ''Control.Exception.Exception
    vkResultTyDoc <- renderType =<< cToHsType DoNotPreserve successCodeType
    tellImportWithAll (mkTyName (eName vkResultEnum))
    tellExport (EData exceptionTypeName)
    tellExport (ETerm (TermName "resultString"))
    let resultPatterns = evName <$> eValues vkResultEnum
    cases <- V.mapMaybe id
      <$> forV resultPatterns (displayExceptionCase getDocumentation)
    tellDoc [qci|
        -- | This exception is thrown from calls to marshalled Vulkan commands
        -- which return a negative VkResult.
        newtype {exceptionTypeName} = {exceptionTypeName} \{ vulkanExceptionResult :: {vkResultTyDoc} }
          deriving (Eq, Ord, Read, Show)

        instance Exception {exceptionTypeName} where
          displayException ({exceptionTypeName} r) = show r ++ ": " ++ resultString r

        -- | A human understandable message for each VkResult
        resultString :: {vkResultTyDoc} -> String
        resultString = \case
        {indent 2 . vcat $ V.toList cases}
          r -> show r
      |]

displayExceptionCase
  :: HasRenderParams r
  => (Documentee -> Maybe Documentation)
  -> CName
  -> Sem r (Maybe (Doc ()))
displayExceptionCase getDocumentation pat = do
  RenderParams {..} <- input
  let pat' = mkPatternName pat
  pure $ fmap
    ((pretty pat' <+> "->") <+>)
    (documentationToString =<< getDocumentation (Nested "VkResult" pat))

-- | Get a string expression from some documentation
documentationToString :: Documentation -> Maybe (Doc ())
documentationToString Documentation {..} =
  let writerOptions = def
  in  viaShow . fixupResultDescription <$> eitherToMaybe
        (runPure (writePlain writerOptions (prepareForPlain dDocumentation)))

-- |
-- - Keep only the first sentence
-- - Drop the first word (it's the enum name)
fixupResultDescription :: Text -> Text
fixupResultDescription =
  T.takeWhile (/= '.') . T.unwords . tailSafe . T.words . T.replace "\8217" "'"

tailSafe :: [a] -> [a]
tailSafe = \case
  []     -> []
  _ : xs -> xs

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
