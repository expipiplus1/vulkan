{-# LANGUAGE RecordWildCards #-}

module Spec.Partition
  ( partitionSpec
  , PartitionedSpec(..)
  , ignoredNames
  ) where

--
-- A module to partition the spec into smaller modules
--

import           Control.Exception (assert)
import           Data.Foldable     as F
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet      as S
import           Data.List         (isPrefixOf, isSuffixOf)
import           Data.Maybe        (fromMaybe)
import           Safe              (assertNote)
import           Spec.Extension
import           Spec.ExtensionTag
import           Spec.Graph
import           Spec.Section
import           Spec.Spec
import           Write.Utils

data PartitionedSpec =
  PartitionedSpec{ moduleExports :: M.HashMap ModuleName [SourceEntity]
                 }
  deriving(Show)

-- | A spec is partitioned into modules based on the names mentioned in
-- extensions and section. There is one module per extension or section. An
-- entity ends up in a module iff it is reachable from an entity mentioned in
-- that module's section or extension and it is not reachable from any other
-- extension or module or it is placed into that module by
-- 'bespokeModuleExports' or inferredModuleExports.
partitionSpec :: Spec -> PartitionedSpec
partitionSpec spec =
  let graph = getSpecGraph spec
      extensionModuleExports = M.fromList $
        (\extension -> ( extensionNameToModuleName . eName $ extension
                       , S.fromList (allExtensionNames extension)
                       )) <$> sExtensions spec

      sectionModuleExports = M.fromList $
        (\section -> ( sectionNameToModuleName . sComment $ section
                     , S.fromList (allSectionNames section)
                     )) <$> sSections spec

      explicitCoreModuleExports = foldr' (M.unionWith S.union) mempty
                                    [ extensionModuleExports
                                    , sectionModuleExports
                                    , bespokeModuleExports
                                    ]

      inferredCoreModuleExports =
        inferModuleExports spec allEntityNames explicitCoreModuleExports

      coreModuleExports = M.unionWith S.union explicitCoreModuleExports
                                              inferredCoreModuleExports

      moduleExportNames = calculateModuleExports graph coreModuleExports
      moduleExports = (fmap (vSourceEntity . requiredLookup graph) . toList) <$>
        moduleExportNames

      allEntityNames = S.fromList (M.keys (gNameVertexMap graph))
                       `S.difference` ignoredNames
      allBespokeNames = S.unions (M.elems bespokeModuleExports)
      allExportedNames = S.unions (M.elems moduleExportNames)
      -- unexportedEntities = allEntityNames `S.difference` allExportedNames
      missingBespokeNames = allBespokeNames `S.difference` allEntityNames
      exportedIgnoredNames = ignoredNames `S.intersection` allExportedNames
  in -- TODO: put this is a better error handling scheme
     assertNote (unlines
       [ "Error, some bespoke names were not found in the spec"
       , "Bespoke exports not found:"
       ] ++ show missingBespokeNames) (S.null missingBespokeNames) $
     assertNote (unlines
       [ "Error, some ignored names are being exported"
       , "Ignored names exported:"
       ] ++ show exportedIgnoredNames) (S.null exportedIgnoredNames)
     -- assertNote (unlines
     --   [ "Error, some entites are not exported by any module."
     --   , "Manually place them in a module by updating 'bespokeModuleExports'"
     --   , "Unexported names:"
     --   ] ++ show unexportedEntities) (S.null unexportedEntities) $
     PartitionedSpec{..}

inferModuleExports :: Spec
                   -> S.HashSet String
                      -- ^ Every name in the spec
                   -> M.HashMap ModuleName (S.HashSet String)
                      -- ^ The core names for modules
                   -> M.HashMap ModuleName (S.HashSet String)
inferModuleExports spec allExports explicitExports =
  M.filter (not . S.null) (M.mapWithKey updateModule explicitExports)
  where updateModule moduleName exports =
          let exportList = S.toList exports
              moduleAssociations = possibleModuleAssociations moduleName
          in allExports `S.intersection`
             S.fromList (transitiveClosure (possibleEntityAssociations spec)
                                           (==)
                                           (exportList ++
                                            moduleAssociations))

possibleEntityAssociations :: Spec -> String -> [String]
possibleEntityAssociations spec name
  | "vkCreate" `isPrefixOf` name
  = let Just baseObjectName = swapPrefix "vkCreate" "Vk" baseName
        objectName = baseObjectName ++ tagString
        createInfoStructName = baseObjectName ++ "CreateInfo" ++ tagString
        createFlagsName = baseObjectName ++ "CreateFlags" ++ tagString
        usageFlagsName = baseObjectName ++ "UsageFlags" ++ tagString
        typeEnumName = baseObjectName ++ "Type" ++ tagString
    in [ objectName
       , createInfoStructName
       , createFlagsName
       , usageFlagsName
       , typeEnumName
       ]
  | "Flags" `isSuffixOf` baseName
  = let Just newBaseName = swapSuffix "Flags" "FlagBits" baseName
    in [newBaseName ++ tagString]
  | "FlagBits" `isSuffixOf` baseName
  = let Just newBaseName = swapSuffix "FlagBits" "Flags" baseName
    in [newBaseName ++ tagString]
  | otherwise
  = []
  where (baseName, tag) = breakNameTag (getSpecExtensionTags spec) name
        tagString = fromMaybe "" (unExtensionTag <$> tag)

possibleModuleAssociations :: ModuleName -> [String]
possibleModuleAssociations moduleName
  = ["Vk" ++ getModuleBaseName moduleName]

bespokeModuleExports :: M.HashMap ModuleName (S.HashSet String)
bespokeModuleExports = M.fromList
  [ ( ModuleName "Graphics.Vulkan.Constants"
    , S.fromList [ "VK_MAX_PHYSICAL_DEVICE_NAME_SIZE"
                 , "VK_MAX_MEMORY_TYPES"
                 , "VK_MAX_MEMORY_HEAPS"
                 , "VK_MAX_EXTENSION_NAME_SIZE"
                 , "VK_MAX_DESCRIPTION_SIZE"
                 , "VK_UUID_SIZE"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Memory"
    , S.fromList [ "VkDeviceMemory"
                 , "VkSystemAllocationScope"
                 , "VkInternalAllocationType"
                 , "VkAllocationCallbacks"
                 , "PFN_vkFreeFunction"
                 , "PFN_vkReallocationFunction"
                 , "PFN_vkAllocationFunction"
                 , "PFN_vkInternalAllocationNotification"
                 , "PFN_vkInternalFreeNotification"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Pass"
    , S.fromList [ "VkDependencyFlags"
                 , "VkDependencyFlagBits"
                 , "VkRenderPass"
                 , "VkFramebuffer"
                 , "VkAccessFlags"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Core"
    , S.fromList [ "VkDeviceSize"
                 , "VkFormat"
                 , "VkFlags"
                 , "VkResult"
                 , "VkOffset2D"
                 , "VkOffset3D"
                 , "VkRect2D"
                 , "VkRect3D"
                 , "VkExtent2D"
                 , "VkExtent3D"
                 , "VkBool32"
                 , "VkStructureType"
                 , "VkSharingMode"
                 , "VkViewport"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Sampler"
    , S.fromList [ "VkFilter"
                 , "VkCompareOp"
                 , "VkSampleCountFlagBits"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Image"
    , S.fromList [ "VkImageAspectFlags"
                 , "VkImageTiling"
                 , "VkImageLayout"
                 , "VkImageSubresource"
                 , "VkImageSubresourceRange"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.DescriptorSet"
    , S.fromList [ "VkDescriptorSetLayout"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Query"
    , S.fromList [ "VkQueryControlFlags"
                 , "VkQueryPipelineStatisticFlags"
                 , "VkQueryResultFlags"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Device"
    , S.fromList [ "VkPhysicalDeviceFeatures"
                 , "VkPhysicalDevice"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Shader"
    , S.fromList [ "VkShaderStageFlagBits"
                 , "VkShaderStageFlags"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Pipeline"
    , S.fromList [ "VkPipelineBindPoint"
                 , "VkPipelineStageFlagBits"
                 , "VkPipelineStageFlags"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.KHR.Surface"
    , S.fromList [ "VkSurfaceTransformFlagBitsKHR"
                 , "VkSurfaceTransformFlagsKHR"
                 , "VkColorSpaceKHR"
                 , "VkCompositeAlphaFlagBitsKHR"
                 , "VkPresentModeKHR"
                 , "VkSurfaceKHR"
                 ]
    )
  , ( ModuleName "Graphics.Vulkan.Version"
    , S.fromList [ "VK_MAKE_VERSION"
                 ]
    )
  ]

ignoredNames :: S.HashSet String
ignoredNames = S.fromList [ "uint64_t"
                          , "vulkan.h"
                          , "uint32_t"
                          , "size_t"
                          , "void"
                          , "float"
                          , "int32_t"
                          , "char"
                          , "VK_DEFINE_HANDLE"
                          , "VK_DEFINE_NON_DISPATCHABLE_HANDLE"
                          , "VK_NULL_HANDLE"
                          ]

calculateModuleExports :: SpecGraph
                          -- | The core module exports
                       -> M.HashMap ModuleName (S.HashSet String)
                       -> M.HashMap ModuleName (S.HashSet String)
calculateModuleExports graph coreModuleExports =
  let coreAndReachableModuleExports =
        M.map (\coreExports ->
                (coreExports,
                 allReachableFromNames graph (S.toList coreExports)))
              coreModuleExports
      coreAndReachableList = M.toList coreAndReachableModuleExports
      removeIgnored ns = ns `S.difference` ignoredNames
  in M.fromListWith (error "Overlapping module names")
       (zip (fst <$> coreAndReachableList)
            (fmap removeIgnored . getExclusiveReachable $ snd <$> coreAndReachableList))

-- | Given a list of (Core Exports, Reachable Names) return a list of
-- (Reachable names which are not in any other core names).
getExclusiveReachable :: [(S.HashSet String, S.HashSet String)]
                      -> [S.HashSet String]
getExclusiveReachable = reverse . go mempty []
  where go _ acc [] = acc
        go (leftCore, leftReachable) acc ((c,r):xs) =
          let otherCoreExports = S.unions (leftCore : (fst <$> xs))
              otherReachable = S.unions (leftReachable : (snd <$> xs))
              otherReachableSubThisCore = otherReachable `S.difference` c
              a = r `S.difference` otherCoreExports
                    `S.difference` otherReachableSubThisCore
          in -- Check that none of the cores overlap
             assert (S.null (c `S.intersection` otherCoreExports)) $
             go (c `S.union` leftCore, r `S.union` leftReachable)
                (a:acc)
                xs

