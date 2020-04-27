module Render.Aggregate
  where

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
  :: [(ModName, Vector RenderElement)] -> [Segment ModName RenderElement]
mergeElements ss =
  let unpackedSegments = [ (m, rs) | (m, rs) <- ss, not (V.null rs) ] -- Don't write empty segments
      initialModNames  = nubOrd . fmap fst $ unpackedSegments
      allModNames      = nubOrd
        [ ancestorOrSelf
        | m              <- initialModNames
        , -- drop 2 for (ModName "") and (ModName "Graphics")
          ancestorOrSelf <-
          fmap (ModName . intercalate ".")
          . drop 2
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
  in  segments

makeAggregateRenderElements :: [ModName] -> [(ModName, RenderElement)]
makeAggregateRenderElements ms =
  [ (parent, re)
  | m <- ms
  , let parent = ModName . intercalate "." . init . splitOn "." . unModName $ m
  , let re     = run . genRe "aggregate" $ tellReexportMod m
  ]

noAggregateModules :: [ModName]
noAggregateModules =
  [ ModName "Graphics.Vulkan.CStruct.Utils"
  , ModName "Graphics.Vulkan.CStruct.Extends"
  , ModName "Graphics.Vulkan.Dynamic"
  , ModName "Graphics.Vulkan.Exception"
  , ModName "Graphics.Vulkan"
  , ModName "Graphics"
  ]
