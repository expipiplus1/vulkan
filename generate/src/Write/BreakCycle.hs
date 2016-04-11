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

writeHsBootFiles :: FilePath -> SpecGraph -> IO ()
writeHsBootFiles root graph =
  void $ M.traverseWithKey (writeHsBootFile root graph)
                           cycleBreakers

writeHsBootFile :: FilePath      -- ^ The source root
                -> SpecGraph     -- ^ The specification graph
                -> ModuleName    -- ^ The module name we're writing
                -> [String]      -- ^ The symbols to export
                -> IO ()
writeHsBootFile root graph moduleName exports = do
  createModuleDirectory root moduleName
  let moduleString = writeModule graph moduleName exports
  writeFile (moduleNameToFile root moduleName) moduleString

