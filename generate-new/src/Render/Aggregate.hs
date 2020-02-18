module Render.Aggregate
  where

import           Prelude
import           Polysemy
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.Map                      as Map
import           Data.Text                      ( splitOn
                                                , intercalate
                                                )
import           Data.List.Extra                ( nubOrd
                                                , inits
                                                )

import           Render.Element
import           Write.Segment

mergeElements
  :: Vector (SegmentedGroup ModName RenderElement)
  -> [Segment ModName RenderElement]
mergeElements groups =
  let unpackedSegments =
          [ (m, rs)
          | SegmentedGroup xs x  <- V.toList groups
          , Segment        m  rs <- x : V.toList xs
          , not (V.null rs) -- Don't write empty segments
          ]
      allModNames = fst <$> unpackedSegments
      aggregates  = makeAggregateRenderElements allModNames

      -- Merge segments with the same module
      segments =
          fmap (uncurry Segment)
            . Map.toList
            . Map.fromListWith (<>)
            $ (unpackedSegments <> (fmap V.singleton <$> aggregates))
  in
  segments

makeAggregateRenderElements :: [ModName] -> [(ModName, RenderElement)]
makeAggregateRenderElements ms =
  [ let re = run . genRe "aggregate" $ tellReexport m
    in  (ModName (intercalate "." ancestor), re)
  | m        <- nubOrd ms
  , -- drop 2 for empty name and "Graphics"
    -- init so we don't rexport the current module
    ancestor <- drop 2 . init . inits . splitOn "." . unModName $ m
  ]

