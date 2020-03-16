module Bespoke.Seeds
  ( specSeeds
  , ModulePlacement(..)
  ) where

import           Relude                  hiding ( runReader
                                                , uncons
                                                )
import           Polysemy
import           Data.Version
import qualified Data.Map                      as Map
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import qualified Data.Text                     as T
import           Data.Text.Extra                ( upperCaseFirst )
import           Algebra.Graph.AdjacencyIntMap

import           Error
import           Spec.Parse
import           Write.Segment
import           Render.Element
import           Haskell.Name

-- specSeeds
--   :: HasErr r
--   => Spec
--   -> Vector RenderElement
--   -> Sem r (V.Vector (SegmentGroup ModName HName))
-- specSeeds _ elements = do
--   (lookupElement, lookupProvider, relation) <- buildRelation elements
--   let featureSeeds = V.singleton (SegmentGroup (ModName "FixMe") mempty)
--       extSeeds     = V.singleton (SegmentGroup (ModName "FixMe") mempty)
--       bespokeSeeds = V.singleton (SegmentGroup (ModName "FixMe") mempty)
--   pure $ featureSeeds <> extSeeds <> bespokeSeeds


data ModulePlacement
  = CoreMod Version Text
  | ExtensionMod Text
  | BespokeMod Text
  deriving(Show)

specSeeds
  :: HasErr r => Spec -> Sem r (V.Vector (SegmentGroup ModulePlacement HName))
specSeeds Spec {..} = do
  let
    featureSeeds =
      (\Feature {..} ->
          SegmentGroup (CoreMod fVersion "Extra")
            . fmap
                (\re ->
                  SegmentSeed (featureCommentToModulePlacement fVersion (rComment re))
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
      (\(groupName, es) ->
          SegmentGroup (ExtensionMod groupName)
            $   (\Extension {..} ->
                  SegmentSeed (ExtensionMod exName)
                    . V.concatMap
                        (\re ->
                          rCommandNames re <> rTypeNames re <> rEnumValueNames re
                        )
                    $ exRequires
                )
            <$> es
        )
        <$> groupExtensions specExtensions
  pure $ featureSeeds <> extSeeds <> bespokeSeeds

bespokeSeeds :: V.Vector (SegmentGroup ModulePlacement HName)
bespokeSeeds = V.fromList
  [ SegmentGroup
      (BespokeMod "Dynamic")
      (V.fromList
        [ SegmentSeed
            (BespokeMod "Dynamic")
            (V.fromList [TyConName "DeviceCmds", TyConName "InstanceCmds"])
        ]
      )
  ]

----------------------------------------------------------------
--
----------------------------------------------------------------

-- TODO: Remove this
--
-- It doesn't work because it can pull in things which are present in other
-- later groups.
--
-- We really want a way of collecting names unique to a group
groupExtensions :: V.Vector Extension -> V.Vector (Text, V.Vector Extension)
groupExtensions es =
  V.fromList
    . fmap (\((_, n), e) -> (n, e))
    . Map.toAscList
    . Map.fromListWith (<>)
    $ [ (extensionGroup e, V.singleton e) | e <- toList es ]

-- | Instead of putting any leftovers here in the "Extra" module, put them in
-- this grouped module, lower priority comes first
extensionGroup :: Extension -> (Int, Text)
extensionGroup Extension {..} = case exName of
  "VK_KHR_display"   -> (0, "Surface")
  "VK_KHR_surface"   -> (0, "Surface")
  "VK_KHR_swapchain" -> (1, "SwapChain")
  _                  -> (999, "Extra")

----------------------------------------------------------------
-- Module name making
----------------------------------------------------------------

featureCommentToModulePlacement :: Version -> Maybe Text -> ModulePlacement
featureCommentToModulePlacement v = \case
  Nothing -> CoreMod v "Unnamed"
  Just t ->
    CoreMod v
      . mconcat
      . fmap (upperCaseFirst . replaceSymbols)
      . dropLast "commands"
      . T.words
      . T.replace "Promoted from" "Promoted_From_"
      . T.replace "Originally based on" "Originally_Based_On_"
      . T.takeWhile (/= ',')
      . removeParens
      . featureCommentMap
      $ t

featureCommentMap :: Text -> Text
featureCommentMap = \case
  "These types are part of the API and should always be defined, even when no enabled features require them."
    -> "OtherTypes"
  t -> t

extensionModulePrefix :: Text
extensionModulePrefix = "Graphics.Vulkan.Extensions"

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
