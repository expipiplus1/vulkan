{-# LANGUAGE RecordWildCards #-}

module Spec.Partition where

--
-- A module to partition the spec into smaller modules
-- 

import Spec.Section
import Spec.Graph
import Data.Foldable as F
import Data.HashMap.Lazy as M
import Data.HashSet as S

data PartitionedSpec = PartitionedSpec{ coreNames :: S.HashSet String
                                      , exclusiveSectionNames :: [(String, S.HashSet String)]
                                      , otherNames :: S.HashSet String
                                      }
  deriving(Show)

partitionSpec :: [Section] -> SpecGraph -> PartitionedSpec
partitionSpec sections graph = 
  let lookupName name = 
        case M.lookup name (gNameVertexMap graph) of
          Nothing -> error ("Depended upon name not in spec: " ++ name)
          Just vertex -> vertex
      sectionDependencies = (\section -> allReachable (lookupName <$> 
                                                       allSectionNames section)) 
                            <$> sections
      -- allNames = S.fromMap (gNameVertexMap graph) -- uncomment when we have uc >= 0.2.6
      allNames = S.fromList (vName <$> gVertices graph)
      coreNames = getCoreNames sectionDependencies
      exclusiveSectionNames = zip (sComment <$> sections) (getExclusiveSectionNames sectionDependencies)
      otherNames = allNames `S.difference` (S.unions (coreNames:(snd <$> exclusiveSectionNames)))
  in PartitionedSpec{..}

getCoreNames :: [S.HashSet String] -> S.HashSet String
getCoreNames = intersections

intersections :: [HashSet String] -> HashSet String
intersections = F.foldl' S.intersection mempty

getExclusiveSectionNames :: [HashSet String] -> [S.HashSet String]
getExclusiveSectionNames = reverse . go mempty []
  where go _ acc [] = acc
        go left acc (x:xs) = let others = S.unions (xs ++ [left])
                                 a = x `S.difference` others
                             in go (left `S.union` x) (a:acc) xs

