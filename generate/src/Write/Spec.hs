{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Write.Spec
  ( writeSpecModules
  ) where

import Data.Maybe(catMaybes)
import Spec.Spec
import Spec.Type
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text ((<+>), vcat, Doc, indent)
import Write.Constant
import Write.Enum
import Write.Header as H hiding (ModuleName)
import Write.Type.Base
import Write.Type.Bitmask
import Write.Type.FuncPointer
import Write.Type.Handle
import Write.Type.Struct
import Write.TypeConverter
import Write.Command

import Spec.Graph
import Control.Arrow(second)
import Spec.Partition
import Write.Module
import Write.Utils
import Write.CycleBreak
import Write.WriteMonad
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeDirectory)
import Data.List.Split(splitOn)
import Data.List(foldl1', sort)
import Data.Foldable(traverse_)
import Data.String

writeSpecModules :: FilePath -> Spec -> IO ()
writeSpecModules root spec = do
  let graph = getSpecGraph spec
      partitions = second S.toList <$> M.toList (moduleExports (partitionSpec spec graph))
      locations = M.unions (uncurry exportMap <$> partitions)
      moduleNames = fst <$> partitions
      moduleStrings = uncurry (writeModule graph locations Normal) <$> 
                      partitions
      modules = zip moduleNames moduleStrings
  traverse_ (createModuleDirectory root) (fst <$> modules)
  mapM_ (uncurry (writeModuleFile root)) modules
  writeHsBootFiles root graph locations
  writeModuleFile root (ModuleName "Graphics.Vulkan")
                       (writeParentModule moduleNames)

writeModuleFile :: FilePath -> ModuleName -> String -> IO ()
writeModuleFile root moduleName = 
  writeFile (moduleNameToFile root moduleName)

exportMap :: ModuleName -> [String] -> M.HashMap String ModuleName
exportMap moduleName exports = M.fromList ((,moduleName) <$> exports)

writeParentModule :: [ModuleName] -> String
writeParentModule names = show moduleDoc
  where nameStrings = fmap fromString . sort . fmap unModuleName $ names
        moduleDoc :: Doc
        moduleDoc = [qc|module Graphics.Vulkan
  ( {indent (-2) . vcat $ intercalatePrepend (fromString ",") ((fromString "module" <+>) <$> nameStrings)}
  ) where

{vcat $ (fromString "import" <+>) <$> nameStrings}|]

