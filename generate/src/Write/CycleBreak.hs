module Write.CycleBreak where

import           Control.Monad       (void)
import           Data.HashMap.Strict as M
import           Spec.Graph
import           Spec.Partition
import           Write.Module
import           Write.Quirks
import           Write.Utils
import           Write.WriteMonad

writeHsBootFiles :: FilePath -> SpecGraph -> PartitionedSpec -> IO ()
writeHsBootFiles root graph part =
  void $ M.traverseWithKey (writeHsBootFile root graph part)
    (fmap (vSourceEntity . requiredLookup graph) <$> cycleBreakers)

writeHsBootFile :: FilePath      -- ^ The source root
                -> SpecGraph     -- ^ The specification graph
                -> PartitionedSpec
                -> ModuleName    -- ^ The module name we're writing
                -> [SourceEntity]      -- ^ The symbols to export
                -> IO ()
writeHsBootFile root graph part moduleName exports = do
  createModuleDirectory root moduleName
  let moduleString = writeModule graph part Boot moduleName exports
  writeFile (moduleNameToFile root moduleName ++ "-boot") moduleString

