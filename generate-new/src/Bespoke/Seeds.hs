module Bespoke.Seeds
  ( specSeeds
  )
where

import           Relude                  hiding ( runReader
                                                , uncons
                                                )
import           Polysemy
import           Data.Version
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import           Data.Text.Extra                ( upperCaseFirst )

import           Error
import           Spec.Parse
import           Write.Segment
import           Render.Element
import           Haskell.Name

specSeeds :: HasErr r => Spec -> Sem r (V.Vector (SegmentGroup ModName HName))
specSeeds Spec {..} = do
  let

    featureSeeds =
      (\Feature {..} ->
          SegmentGroup (ModName (featureModulePrefix fVersion <> ".Extra"))
            . fmap
                (\re ->
                  SegmentSeed (featureCommentToModuleName fVersion (rComment re))
                    .  seedFilter
                    $  rCommandNames re
                    <> rTypeNames re
                    <> rEnumValueNames re
                )
            . V.filter ((/= Just "Header boilerplate") . rComment)
            $ fRequires
        )
        <$> specFeatures

    extSeeds =
      SegmentGroup (ModName (extensionModulePrefix <> ".Extra"))
        $   (\Extension {..} ->
              SegmentSeed (extensionNameToModuleName exName)
                . V.concatMap
                    (\re ->
                      rCommandNames re <> rTypeNames re <> rEnumValueNames re
                    )
                $ exRequires
            )
        <$> specExtensions
  pure $ featureSeeds <> V.singleton extSeeds

featureModulePrefix :: Version -> Text
featureModulePrefix v =
  "Graphics.Vulkan.Core" <> foldMap show (versionBranch v)

featureCommentToModuleName :: Version -> Maybe Text -> ModName
featureCommentToModuleName v = \case
  Nothing -> ModName $ featureModulePrefix v
  Just t ->
    ModName
      . ((featureModulePrefix v <> ".") <>)
      . mconcat
      . fmap (upperCaseFirst . replaceSymbols)
      . dropLast "commands"
      . T.words
      . T.takeWhile (/= ',')
      . removeParens
      $ t

extensionModulePrefix :: Text
extensionModulePrefix = "Graphics.Vulkan.Extensions"

extensionNameToModuleName :: Text -> ModName
extensionNameToModuleName = ModName . ((extensionModulePrefix <> ".") <>)

removeParens :: Text -> Text
removeParens t =
  let (x, y) = T.breakOn "(" t in x <> T.takeWhileEnd (/= ')') y

replaceSymbols :: Text -> Text
replaceSymbols = \case
  "+" -> "And"
  t   -> t

dropLast :: Eq a => a -> [a] -> [a]
dropLast x l = case nonEmpty l of
  Nothing -> []
  Just xs -> if last xs == x then init xs else toList xs

seedFilter :: V.Vector HName -> V.Vector HName
seedFilter = fmap
  (\case
    TyConName "VK_NULL_HANDLE" -> ConName "VK_NULL_HANDLE"
    n                          -> n
  )
