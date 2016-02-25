module Write.CycleBreak where

import Control.Monad(void)
import Data.HashMap.Strict as M
import Spec.Graph
import Write.Module
import Write.Quirks
import Write.Utils
import Write.WriteMonad

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
  let moduleString = writeModule graph nameLocations Boot moduleName exports 
  writeFile (moduleNameToFile root moduleName ++ "-boot") moduleString

