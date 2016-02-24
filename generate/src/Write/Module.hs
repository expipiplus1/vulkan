{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Write.Module where

import Spec.Graph
import Control.Monad.Writer
import Data.HashSet as S
import Data.HashMap.Strict as M
import Data.Hashable
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Text.InterpolatedString.Perl6
import GHC.Generics(Generic)
import Write.Utils
import Data.String

data RequiredName = ExternalName ModuleName String
                  | InternalName String
  deriving(Eq, Generic)

instance Hashable RequiredName

type WriteMonad = Writer (HashSet RequiredName)

type NameLocations = HashMap String ModuleName

writeModule :: SpecGraph -> NameLocations -> ModuleName -> [String] -> String
writeModule graph nameLocations (ModuleName n) names = moduleString
  where (moduleString, extraRequiredNames) = runWriter moduleWriter
        extensions = []
        getEntity name = 
          case M.lookup name (gNameVertexMap graph) of
            Nothing -> error ("exported name missing from spec graph " ++ name)
            Just e -> e
        moduleEntities = getEntity <$> names
        isIncludeName = isIncludeVertex . getEntity
        requiredNames = extraRequiredNames `S.union` 
                        S.map nameToRequiredName (S.filter (not . isIncludeName) (allReachable moduleEntities) `S.difference` (S.fromList names))
        imports = vcat (getImportDeclarations nameLocations requiredNames)
        definitions = names
        moduleWriter = do
          pure [qc|{unlines extensions}module {n} where
{imports}
{unlines definitions}
|]

nameToRequiredName :: String -> RequiredName
nameToRequiredName name =
  case name of
    "uint32_t" -> ExternalName (ModuleName "Data.Word") "Word32"
    "uint64_t" -> ExternalName (ModuleName "Data.Word") "Word64"
    "size_t"   -> ExternalName (ModuleName "Foreign.C.Types") "CSize"
    "void"     -> ExternalName (ModuleName "Data.Void") "Void"
    _ -> InternalName name

tellSingle :: (Eq a, Hashable a, Monad m) => a -> WriterT (HashSet a) m ()
tellSingle = tell . S.singleton

getImportDeclarations :: NameLocations -> HashSet RequiredName -> [Doc]
getImportDeclarations nameLocations names = fmap writeImport . mergeImports $ imports
  where imports = getImportDeclaration <$> S.toList names
        getImportDeclaration rn = 
          case rn of
            ExternalName moduleName name -> Import moduleName [name]
            InternalName name -> 
              case M.lookup name nameLocations of
                Nothing -> error ("Imported name not in any module: " ++ name)
                Just moduleName -> Import moduleName [name]
            
data Import = Import ModuleName [String]

writeImport :: Import -> Doc
writeImport (Import (ModuleName moduleName) imports) = 
  let 
  in [qc|import {moduleName}( {indent (-2) (vcat ((intercalatePrepend "," (fromString <$> imports) ++ [")"])))}|]

mergeImports :: [Import] -> [Import]
mergeImports is = fmap (uncurry Import) . M.toList . M.fromListWith (++) $ [(name, imports) | Import name imports <- is]
