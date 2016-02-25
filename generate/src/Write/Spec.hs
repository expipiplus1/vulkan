{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Write.Spec
  ( haskellize
  , writeSpecModules
  ) where

import Data.Maybe(catMaybes)
import Spec.Spec
import Spec.Type
import Text.InterpolatedString.Perl6
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
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeDirectory)
import Data.List.Split(splitOn)
import Data.List(foldl1')
import Data.Foldable(traverse_)

writeSpecModules :: FilePath -> Spec -> IO ()
writeSpecModules root spec = do
  let graph = getSpecGraph spec
      partitions = second S.toList <$> M.toList (moduleExports (partitionSpec spec graph))
      locations = M.unions (uncurry exportMap <$> partitions)
      moduleNames = fst <$> partitions
      moduleStrings = uncurry (writeModule graph locations) <$> partitions
      modules = zip moduleNames moduleStrings
  traverse_ (createModuleDirectory root) (fst <$> modules)
  mapM_ (uncurry (writeModuleFile root)) modules
  writeHsBootFiles root graph locations

writeModuleFile :: FilePath -> ModuleName -> String -> IO ()
writeModuleFile root moduleName = 
  writeFile (moduleNameToFile root moduleName)

exportMap :: ModuleName -> [String] -> M.HashMap String ModuleName
exportMap moduleName exports = M.fromList ((,moduleName) <$> exports)

partitionExclusiveModule :: (ModuleName, S.HashSet String) -> (ModuleName, [String])
partitionExclusiveModule = second S.toList

haskellize :: Spec -> String
haskellize spec = let -- TODO: Remove
                      typeConverter = cTypeToHsTypeString
                      typeEnv = buildTypeEnvFromSpec spec
                  in [qc|{writeExtensions allExtensions }
module Vulkan where

{writeImports allImports}

{writeConstants (sConstants spec)}
{writeBaseTypes typeConverter 
                (catMaybes . fmap typeDeclToBaseType . sTypes $ spec)}
{writeHandleTypes typeConverter 
                  (catMaybes . fmap typeDeclToHandleType . sTypes $ spec)}
{writeFuncPointerTypes typeConverter 
                       (catMaybes . fmap typeDeclToFuncPointerType . sTypes $ 
                         spec)}
{writeBitmaskTypes typeConverter 
                   (sBitmasks spec) 
                   (catMaybes . fmap typeDeclToBitmaskType . sTypes $ spec)}
{writeEnums (sEnums spec)}
{writeStructTypes typeEnv 
                  (catMaybes . fmap typeDeclToStructType . sTypes $ spec)}
{writeUnionTypes typeEnv 
                  (catMaybes . fmap typeDeclToUnionType . sTypes $ spec)}
{writeCommands (sCommands spec)}
|]

allExtensions :: [Extension]
allExtensions = [ Extension "DataKinds"
                , Extension "DuplicateRecordFields"
                , Extension "ForeignFunctionInterface"
                , Extension "GeneralizedNewtypeDeriving"
                , Extension "PatternSynonyms"
                , Extension "Strict"
                ]

allImports :: [H.Import]
allImports = [ H.Import "Data.Bits" ["Bits", "FiniteBits"]
             , H.Import "Data.Int" ["Int32"]
             , H.Import "Data.Vector.Fixed.Storable" ["Vec"]
             , H.Import "Data.Vector.Fixed.Cont" ["ToPeano"]
             , H.Import "Data.Void" ["Void"]
             , H.Import "Data.Word" ["Word8", "Word32", "Word64"]
             , H.Import "Foreign.C.Types" ["CChar", "CFloat(..)", "CSize(..)"]
             , H.Import "Foreign.Ptr" ["Ptr", "FunPtr", "plusPtr", "castPtr"]
             , H.Import "Foreign.Storable" ["Storable(..)"]
             ]

