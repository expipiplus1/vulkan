{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Write.Module
  ( writeModule
  )where

import Data.HashMap.Strict as M
import Data.HashSet as S
import Data.String
import Spec.Graph
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.Quirks
import Write.Utils
import Write.WriteMonad

import Data.Char(isUpper, isAlphaNum)

writeModule :: SpecGraph -> NameLocations -> ModuleName -> [String] -> String
writeModule graph nameLocations (ModuleName n) names = moduleString
  where (moduleString, extraRequiredNames) = runWrite moduleWriter
        extensions = []
        getEntity name = 
          case M.lookup name (gNameVertexMap graph) of
            Nothing -> error ("exported name missing from spec graph " ++ name)
            Just e -> e
        moduleEntities = getEntity <$> names
        isIncludeName = isIncludeVertex . getEntity
        requiredNames = extraRequiredNames `S.union` 
                        S.map nameToRequiredName (S.filter (not . isIncludeName) (allReachable moduleEntities) `S.difference` (S.fromList names))
        imports = vcat (getImportDeclarations (ModuleName n) nameLocations requiredNames)
        definitions = (\name -> if isUpper (head name)
                                  then "data " ++ name
                                  else name ++ " = undefined") <$> Prelude.filter isValid names
        moduleWriter = 
          pure [qc|{unlines extensions}module {n} where
{imports}
{unlines definitions}
|]

isValid :: String -> Bool
isValid = all (\c -> isAlphaNum c || c == '_')

nameToRequiredName :: String -> RequiredName
nameToRequiredName name =
  case name of
    "int32_t"  -> ExternalName (ModuleName "Data.Int")  "Int32"
    "uint32_t" -> ExternalName (ModuleName "Data.Word") "Word32"
    "uint64_t" -> ExternalName (ModuleName "Data.Word") "Word64"
    "size_t"   -> ExternalName (ModuleName "Foreign.C.Types") "CSize"
    "void"     -> ExternalName (ModuleName "Data.Void") "Void"
    _ -> InternalName name

getImportDeclarations :: ModuleName -> NameLocations -> HashSet RequiredName -> [Doc]
getImportDeclarations importingModule nameLocations names = 
    fmap (writeImport . (makeImportSourcy importingModule)) . 
      mergeImports $ imports
  where imports = getImportDeclaration <$> S.toList names
        getImportDeclaration rn = 
          case rn of
            ExternalName moduleName name -> Import NotSource moduleName [name]
            InternalName name -> 
              case M.lookup name nameLocations of
                Nothing -> error ("Imported name not in any module: " ++ name)
                Just moduleName -> Import NotSource moduleName [name]
            
data Import = Import Source ModuleName [String]

data Source = NotSource
            | Source

writeImport :: Import -> Doc
writeImport (Import source (ModuleName moduleName) imports) = 
  let sourceDoc :: Doc
      sourceDoc = fromString $ case source of 
                                 NotSource -> ""
                                 Source -> "{-# SOURCE #-} "
  in [qc|import {sourceDoc}{moduleName}( {indent (-2) (vcat ((intercalatePrepend "," (fromString <$> imports) ++ [")"])))}|]

mergeImports :: [Import] -> [Import]
mergeImports is = fmap (uncurry (Import NotSource)) . 
                  M.toList . M.fromListWith (++) $ 
                  [(name, imports) | Import _ name imports <- is]

makeImportSourcy :: ModuleName -> Import -> Import 
makeImportSourcy importingModule (Import source moduleName names) 
  | Just sourceImported <- M.lookup importingModule sourceImports
  , elem moduleName sourceImported 
  = Import Source moduleName names
  | otherwise 
  = Import source moduleName names
