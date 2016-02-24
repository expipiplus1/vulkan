{-# LANGUAGE RecordWildCards #-}

module Spec.Partition where

--
-- A module to partition the spec into smaller modules
--

import Data.Foldable as F
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Maybe(catMaybes, isJust)
import Spec.Graph
import Spec.Section
import Write.Utils
import Data.List(isPrefixOf, sortOn)

data PartitionedSpec =
  PartitionedSpec{ sectionNames :: M.HashMap String (S.HashSet String)
                 , otherNames :: S.HashSet String
                 }
  deriving(Show)

partitionSpec :: [Section] -> SpecGraph -> PartitionedSpec
partitionSpec sections graph =
  let -- A helper to find a named vertex in the graph
      lookupName name =
        case M.lookup name (gNameVertexMap graph) of
          Nothing -> error ("Depended upon name not in spec: " ++ name)
          Just vertex -> vertex

      -- The set of all names reachable from each section
      sectionDependencies = (\section -> allReachable (lookupName <$>
                                                       allSectionNames section))
                            <$> sections

      -- The names of every vertex
      -- allNames = S.fromMap (gNameVertexMap graph) -- uncomment for uc >= 0.2.6
      allNames = S.fromList (vName <$> gVertices graph)

      -- The names depended on by exactly one section
      sectionExclusiveNames =
        M.fromList $ zip (sectionNameToModuleBaseName . sComment <$> sections)
                         (getExclusiveSectionNames sectionDependencies)

      -- All names which are used by more than one section
      otherNames = allNames `S.difference`
                   (S.unions (M.elems sectionExclusiveNames))

      -- The module base names
      moduleBaseNames = (sectionNameToModuleBaseName . sComment) <$> sections

      -- Names inserted into the section modules by 'partitionBespokeNames'
      sectionBespokeNames =
        partitionBespokeNames moduleBaseNames (S.toList otherNames)

      -- The complete section names
      sectionNames = M.unionWith S.union
                                 sectionExclusiveNames
                                 sectionBespokeNames
  in PartitionedSpec{..}

partitionBespokeNames :: [String] -> [String]
                      -> M.HashMap String (S.HashSet String)
partitionBespokeNames moduleNames names =
  M.fromListWith S.union
    [(moduleName, S.singleton name) | (Just moduleName, name) <- zip nameModules names]
  where nameModules = assignNameToModule moduleNames <$> names


assignNameToModule :: [String] -> String -> Maybe String
assignNameToModule moduleNames name
  | any (`isPrefixOf` dropVK name) moduleNames =
    Just (longest . filter (`isPrefixOf` (dropVK name)) $ moduleNames)
  | "vk_platform" == name = Nothing
  | otherwise = error ("No rule to assign this name to module: " ++ name)

longest :: [[a]] -> [a]
longest = last . sortOn length

getCoreNames :: [S.HashSet String] -> S.HashSet String
getCoreNames = intersections

intersections :: [S.HashSet String] -> S.HashSet String
intersections = F.foldl1 S.intersection

getExclusiveSectionNames :: [S.HashSet String] -> [S.HashSet String]
getExclusiveSectionNames = reverse . go mempty []
  where go _ acc [] = acc
        go left acc (x:xs) = let others = S.unions (xs ++ [left])
                                 a = x `S.difference` others
                             in go (left `S.union` x) (a:acc) xs

