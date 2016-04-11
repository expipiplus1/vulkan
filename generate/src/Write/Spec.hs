{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}

module Write.Spec
  ( writeSpecModules
  ) where

import           Spec.Spec
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  (Doc, indent, vcat, (<+>))

import           Data.Foldable                 (traverse_)
import qualified Data.HashMap.Strict           as M
import           Data.List                     (sort)
import           Data.String
import           Spec.Graph
import           Spec.Partition

import           Write.CycleBreak
import           Write.Module
import           Write.Utils
import           Write.WriteMonad

writeSpecModules :: FilePath -> Spec -> IO ()
writeSpecModules root spec = do
  let graph = getSpecGraph spec
      part = partitionSpec spec
      partitions = M.toList $ moduleExports part
      moduleNames = fst <$> partitions
      moduleStrings = uncurry (writeModule graph part Normal) <$>
                      partitions
      modules = zip moduleNames moduleStrings
  traverse_ (createModuleDirectory root) (fst <$> modules)
  mapM_ (uncurry (writeModuleFile root)) modules
  writeHsBootFiles root graph part
  writeModuleFile root (ModuleName "Graphics.Vulkan")
                       (writeParentModule moduleNames)

writeModuleFile :: FilePath -> ModuleName -> String -> IO ()
writeModuleFile root moduleName =
  writeFile (moduleNameToFile root moduleName)

writeParentModule :: [ModuleName] -> String
writeParentModule names = show moduleDoc
  where nameStrings = fmap fromString . sort . fmap unModuleName $ names
        moduleDoc :: Doc
        moduleDoc = [qc|module Graphics.Vulkan
  ( {indent (-2) . vcat $ intercalatePrepend (fromString ",") ((fromString "module" <+>) <$> nameStrings)}
  ) where

{vcat $ (fromString "import" <+>) <$> nameStrings}|]

