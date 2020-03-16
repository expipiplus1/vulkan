{-# language QuasiQuotes #-}

module Render.VkException
  ( vkExceptionRenderElement)
  where

import           Relude                  hiding ( ask )
import           Polysemy.Reader
import           Data.Text.Prettyprint.Doc
import           Text.InterpolatedString.Perl6.Unindented

import           Control.Exception

import           Spec.Types
import           Error
import           Haskell                       as H
import           Render.Element
import           Render.SpecInfo
import           Render.Type

vkExceptionRenderElement
  :: (HasErr r, HasRenderParams r, HasSpecInfo r) =>
  -- (Documentee -> Maybe Documentation) ->
                                                     Enum' -> Sem r RenderElement
vkExceptionRenderElement -- getDocumentation
                         vkResultEnum = do
  let getDocumentation = const Nothing
  genRe "VulkanException declaration" $ do
    tellExplicitModule (ModName "Graphics.Vulkan.Exception")
    RenderParams {..} <- ask
    tellImportWithAll ''Control.Exception.Exception
    vkResultTyDoc <- renderType =<< cToHsType DoNotPreserve successCodeType
    tellImportWithAll (TyConName (mkTyName (eName vkResultEnum)))
    tellExport (EData exceptionTypeName)
    let resultPatterns = evName <$> eValues vkResultEnum
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
        {indent 2 . vcat . fmap (displayExceptionCase getDocumentation) $ toList resultPatterns}
          r -> show r
      |]

data Documentee
data Documentation

displayExceptionCase :: (Documentee -> Maybe Documentation) -> Text -> Doc ()
displayExceptionCase getDocumentation pat = [qci|
  {pat} -> {fromMaybe ("show" <+> pretty pat) Nothing}
  |]
  -- {pat} -> {fromMaybe ("show" <+> pretty pat) (documentationToString =<< getDocumentation (Nested "VkResult" pat))}

-- -- | Get a string expression from some documentation
-- documentationToString :: Documentation -> Maybe (Doc ())
-- documentationToString Documentation {..} =
--   let writerOptions = def
--   in  pretty . show . fixupResultDescription <$> eitherToMaybe
--         (runPure (writePlain writerOptions (prepareForPlain dDocumentation)))

-- -- |
-- -- - Keep only the first sentence
-- -- - Drop the first word (it's the enum name)
-- fixupResultDescription :: Text -> Text
-- fixupResultDescription =
--   T.takeWhile (/= '.') . T.unwords . tailSafe . T.words . T.replace "\8217" "'"

-- tailSafe :: [a] -> [a]
-- tailSafe = \case
--   [] -> []
--   _:xs -> xs

-- prepareForPlain :: Pandoc -> Pandoc
-- prepareForPlain = topDown removeEmph
--   where
--     removeEmph :: [Inline] -> [Inline]
--     removeEmph is = removeEmphInline =<< is
--     removeEmphInline :: Inline -> [Inline]
--     removeEmphInline = \case
--       Emph is -> is
--       i -> [i]
