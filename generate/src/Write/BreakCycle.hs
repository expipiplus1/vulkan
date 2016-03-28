module Write.CycleBreak
  ( writeHsBootFiles
  ) where

import           Data.HashMap.Strict as M

import           Control.Monad       (void)
import           Spec.Graph
import           Write.Module
import           Write.Utils

cycleBreakers :: HashMap ModuleName [String]
cycleBreakers = M.fromList [ (ModuleName "Graphics.Vulkan.Device", ["VkDevice"])
                           , (ModuleName "Graphics.Vulkan.Pass", ["VkRenderPass"])
                           ]

writeHsBootFiles :: FilePath -> SpecGraph -> NameLocations -> IO ()
writeHsBootFiles root graph nameLocations =
  void $ M.traverseWithKey (writeHsBootFile root graph nameLocations)
                           cycleBreakers

writeHsBootFile :: FilePath      -- ^ The source root
                -> SpecGraph     -- ^ The specification graph
                -> NameLocations -- ^ The map of names to modules
                -> ModuleName    -- ^ The module name we're writing
                -> [String]      -- ^ The symbols to export
                -> IO ()
writeHsBootFile root graph nameLocations moduleName exports = do
  createModuleDirectory root moduleName
  let moduleString = writeModule graph nameLocations moduleName exports
  writeFile (moduleNameToFile root moduleName) moduleString

