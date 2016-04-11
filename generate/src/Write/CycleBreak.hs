module Write.CycleBreak where

import           Control.Monad       (void)
import           Data.HashMap.Strict as M
import           Spec.Graph
import           Write.Module
import           Write.Quirks
import           Write.Utils
import           Write.WriteMonad

writeHsBootFiles :: FilePath -> SpecGraph -> IO ()
writeHsBootFiles root graph =
  void $ M.traverseWithKey (writeHsBootFile root graph)
    (fmap (vSourceEntity . requiredLookup graph) <$> cycleBreakers)

writeHsBootFile :: FilePath      -- ^ The source root
                -> SpecGraph     -- ^ The specification graph
                -> ModuleName    -- ^ The module name we're writing
                -> [SourceEntity]      -- ^ The symbols to export
                -> IO ()
writeHsBootFile root graph moduleName exports = do
  createModuleDirectory root moduleName
  let moduleString = writeModule graph Boot moduleName exports
  writeFile (moduleNameToFile root moduleName ++ "-boot") moduleString

