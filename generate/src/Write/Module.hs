{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE QuasiQuotes       #-}

module Write.Module
  ( writeModule
  )where

import           Data.HashMap.Strict           as M
import           Data.HashSet                  as S
import           Data.Maybe                    (catMaybes)
import           Data.String
import           Spec.Graph
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.Quirks
import           Write.TypeConverter           (buildTypeEnvFromSpecGraph)
import           Write.Utils
import           Write.Vertex
import           Write.WriteMonad

writeModule :: SpecGraph
            -> FileType
            -> ModuleName
            -> [SourceEntity]
            -> String
writeModule graph boot (ModuleName n) entities = moduleString
  where typeEnv = buildTypeEnvFromSpecGraph graph
        (moduleString, (extraRequiredNames, extensions)) =
          runWrite typeEnv boot moduleWriter
        extensionDocs = getExtensionDoc <$> S.toList extensions
        requiredNames = extraRequiredNames
        imports = vcat (getImportDeclarations (ModuleName n) requiredNames)
        moduleWriter = do
          definitions <- writeVertices entities
          pure [qc|{vcat extensionDocs}
module {n} where

{imports}

{definitions}
|]

getExtensionDoc :: String -> Doc
getExtensionDoc e = let ed = fromString e :: Doc
                       in [qc|\{-# LANGUAGE {ed} #-}|]

-- nameToRequiredName :: SpecGraph -> String -> RequiredName
-- nameToRequiredName graph name =
--   case name of
--     "int32_t"  -> ExternalName (ModuleName "Data.Int")  "Int32"
--     "uint8_t"  -> ExternalName (ModuleName "Data.Word") "Word8"
--     "uint32_t" -> ExternalName (ModuleName "Data.Word") "Word32"
--     "uint64_t" -> ExternalName (ModuleName "Data.Word") "Word64"
--     "size_t"   -> ExternalName (ModuleName "Foreign.C.Types") "CSize(..)"
--     "void"     -> ExternalName (ModuleName "Data.Void") "Void"
--     "float"    -> ExternalName (ModuleName "Foreign.C.Types") "CFloat(..)"
--     _ -> if isTypeConstructor (requiredLookup graph name)
--            then InternalName WildCard name
--            else InternalName NoWildCard name

getImportDeclarations :: ModuleName -> HashSet RequiredName -> [Doc]
getImportDeclarations importingModule names =
    fmap (writeImport . makeImportSourcy importingModule) .
      mergeImports $ imports
  where imports = catMaybes (getImportDeclaration <$> S.toList names)
        getImportDeclaration rn =
          case rn of
            ExternalName moduleName name ->
              Just (Import NotSource moduleName [name])
            _ -> Nothing

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
  , moduleName `elem` sourceImported
  = Import Source moduleName names
  | otherwise
  = Import source moduleName names
