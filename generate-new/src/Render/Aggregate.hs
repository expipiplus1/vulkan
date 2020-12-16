module Render.Aggregate where

import           Data.List.Extra                ( (\\)
                                                , inits
                                                , nubOrd
                                                )
import qualified Data.Map                      as Map
import           Data.Text                      ( intercalate
                                                , splitOn
                                                )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Polysemy
import           Prelude

import           Render.Element
import           Write.Segment

mergeElements
  :: HasRenderParams r
  => [(ModName, Vector RenderElement)]
  -> Sem r [Segment ModName RenderElement]
mergeElements ss = do
  noAggregateModules <- noAggregateModules
  let unpackedSegments = [ (m, rs) | (m, rs) <- ss, not (V.null rs) ] -- Don't write empty segments
      initialModNames  = nubOrd . fmap fst $ unpackedSegments
      allModNames      = nubOrd
        [ ancestorOrSelf
        | m              <- initialModNames
        , -- tail for (ModName "")
          ancestorOrSelf <-
          fmap (ModName . intercalate ".")
          . tail
          . inits
          . splitOn "."
          . unModName
          $ m
        ]

      allReexportedModNames = allModNames \\ noAggregateModules
      aggregates            = makeAggregateRenderElements allReexportedModNames

      -- Merge segments with the same module
      segments =
        fmap (uncurry Segment)
          . Map.toList
          . Map.fromListWith (<>)
          $ (unpackedSegments <> (fmap V.singleton <$> aggregates))
  pure segments

makeAggregateRenderElements :: [ModName] -> [(ModName, RenderElement)]
makeAggregateRenderElements ms =
  [ (parent, re)
  | m <- ms
  , let parent = ModName . intercalate "." . init . splitOn "." . unModName $ m
  , let re     = run . genRe "aggregate" $ tellReexportMod m
  ]

noAggregateModules :: HasRenderParams r => Sem r [ModName]
noAggregateModules = traverse
  mkModuleName
  [["CStruct", "Utils"], ["CStruct", "Extends"], ["Dynamic"], ["Exception"], []]
