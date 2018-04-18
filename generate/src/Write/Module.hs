{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Write.Module
  ( Module(..)
  , writeModules
  )where

import           Control.Applicative
import           Control.Arrow                            ((&&&))
import           Control.Bool
import           Data.Char
import           Data.Either.Validation
import           Data.Functor.Extra
import           Data.List.Extra
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Text                                (Text)
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import qualified Data.Text                                as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Alias
import           Spec.Savvy.APIConstant
import           Spec.Savvy.Command
import           Spec.Savvy.Define
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Error
import           Spec.Savvy.Extension
import           Spec.Savvy.Feature
import           Spec.Savvy.FuncPointer
import           Spec.Savvy.Handle
import           Spec.Savvy.HeaderVersion
import           Spec.Savvy.Preprocess
import           Spec.Savvy.Struct
import           Spec.Savvy.Struct
import           Spec.Savvy.Type.Haskell
import           Spec.Savvy.Type.Haskell
import           Spec.Savvy.Type.Packing
import           Spec.Savvy.TypeAlias

import           Write.Element
import           Write.Struct
import           Write.Util

data Module = Module
  { mName          :: Text
  , mWriteElements :: [WriteElement]
  , mReexports     :: [Export]
  }
  deriving (Show)

writeModules :: [Module] -> [Doc ()]
writeModules ms =
  ms <&> writeModule (findModule ms)

writeModule :: (HaskellName -> Maybe (Text, Export)) -> Module -> Doc ()
writeModule getModule m@Module{..} = [qci|
  \{-# language Strict #-}
  {vcat $ moduleExtensions m}

  module {mName}
    ( {indent (-2) $ separatedSections "," [(Nothing, moduleExports m), (Just "-- Reexports", moduleReexports m)]}
    ) where

  {vcat $ moduleImports m}

  {vcat $ moduleInternalImports getModule m}

  {vcatPara $ weDoc <$> mWriteElements}
  |]

moduleExports :: Module -> [Doc ()]
moduleExports Module {..} =
  mapMaybe exportHaskellName (weProvides =<< mWriteElements)

moduleReexports :: Module -> [Doc ()]
moduleReexports Module {..} =
  mapMaybe exportHaskellName mReexports

exportHaskellName :: Export -> Maybe (Doc ())
exportHaskellName e =
  let s = case unExport e of
        TypeName n -> Just (pretty n)
        TermName n | isConstructor n -> Nothing
                   | otherwise       -> Just (pretty n)
        PatternName n -> Just ("pattern" <+> pretty n)
  in  case e of
        WithConstructors    _ -> (<> "(..)") <$> s
        WithoutConstructors _ -> s

isConstructor = \case
  Cons x _ | isUpper x -> True
  _                    -> False

moduleImports :: Module -> [Doc ()]
moduleImports Module{..} =
  let importMap = Map.fromListWith union ((iModule &&& iImports) <$> (weImports =<< mWriteElements))
  in  Map.assocs importMap <&> \(mod, is) -> [qci|
        import {pretty mod}
          ( {indent (-2) . vcat . intercalatePrepend "," $ pretty <$> is}
          )
|]

findModule :: [Module] -> HaskellName -> Maybe (Text, Export)
findModule ms =
  let nameMap = Map.fromList
        [ (unExport e, (mName m, e)) | m <- ms, we <- mWriteElements m, e <- weProvides we ]
  in  (`Map.lookup` nameMap)

moduleInternalImports
  :: (HaskellName -> Maybe (Text, Export))
  -- ^ which module is this name from
  -> Module
  -> [Doc ()]
moduleInternalImports nameModule Module {..} =
  let depends = Map.fromListWith
        (<>)
        [ (m, [e])
        | d      <- nubOrd (weDepends =<< mWriteElements)
        , Just (m, e) <- [nameModule d]
        , d `notElem` (unExport <$> (weProvides =<< mWriteElements))
        ]
  in  Map.assocs depends <&> \(mod, is) -> [qci|
        import {pretty mod}
          ( {indent (-2) . vcat . intercalatePrepend "," $ mapMaybe exportHaskellName is}
          )
|]

pattern Cons :: Char -> Text -> Text
pattern Cons c cs <- (T.uncons -> Just (c, cs))
  where Cons c cs = T.cons c cs

moduleExtensions :: Module -> [Doc ()]
moduleExtensions Module{..} =
  let es = nubOrd $ weExtensions =<< mWriteElements
  in es <&> \e -> [qci|\{-# language {e} #-}|]

{-
import           Data.HashMap.Strict           as M
import           Data.HashSet                  as S
import           Data.Maybe                    (catMaybes)
import           Data.String
import           Spec.Graph
import           Spec.Partition
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.Quirks
import           Write.TypeConverter           (buildTypeEnvFromSpecGraph)
import           Write.Utils
import           Write.Vertex
import           Write.WriteMonad

writeModule :: SpecGraph
            -> NameLocations
            -> FileType
            -> ModuleName
            -> [String]
            -> String
writeModule graph nameLocations boot (ModuleName n) names = moduleString
  where typeEnv = buildTypeEnvFromSpecGraph graph
        (moduleString, (extraRequiredNames, extensions)) =
          runWrite typeEnv boot moduleWriter
        extensionDocs = getExtensionDoc <$> S.toList extensions
        getEntity name = let err = error ("exported name missing from spec graph " ++ name)
                         in M.lookupDefault err name (gNameVertexMap graph)
        moduleEntities = getEntity <$> names
        isIncludeName = isIncludeVertex . getEntity
        requiredNames = extraRequiredNames `S.union`
                        S.map (nameToRequiredName graph)
                              (S.filter (not . isIncludeName)
                                        (allReachable moduleEntities)
                                        `S.difference` S.fromList names)
        imports = vcat (getImportDeclarations (ModuleName n) nameLocations requiredNames)
        moduleWriter = do
          definitions <- writeVertices (requiredLookup graph <$> names)
          pure [qc|{vcat extensionDocs}
module {n} where

{imports}

{definitions}
|]

getExtensionDoc :: String -> Doc
getExtensionDoc e = let ed = fromString e :: Doc
                       in [qc|\{-# LANGUAGE {ed} #-}|]

nameToRequiredName :: SpecGraph -> String -> RequiredName
nameToRequiredName graph name =
  case name of
    "int32_t"  -> ExternalName (ModuleName "Data.Int")  "Int32"
    "uint8_t"  -> ExternalName (ModuleName "Data.Word") "Word8"
    "uint32_t" -> ExternalName (ModuleName "Data.Word") "Word32"
    "uint64_t" -> ExternalName (ModuleName "Data.Word") "Word64"
    "size_t"   -> ExternalName (ModuleName "Foreign.C.Types") "CSize(..)"
    "void"     -> ExternalName (ModuleName "Data.Void") "Void"
    "float"    -> ExternalName (ModuleName "Foreign.C.Types") "CFloat(..)"
    _ -> if isTypeConstructor (requiredLookup graph name)
           then InternalName WildCard name
           else InternalName NoWildCard name

getImportDeclarations :: ModuleName -> NameLocations -> HashSet RequiredName -> [Doc]
getImportDeclarations importingModule nameLocations names =
    fmap (writeImport . makeImportSourcy importingModule) .
      mergeImports $ imports
  where imports = catMaybes (getImportDeclaration <$> S.toList names)
        getImportDeclaration rn =
          case rn of
            ExternalName moduleName name ->
              Just (Import NotSource moduleName [name])
            InternalName wildCard name ->
              if S.member name ignoredNames
                then Nothing
                else case M.lookup name nameLocations of
                       Nothing ->
                         error ("Imported name not in any module: " ++ name)
                       Just moduleName ->
                         case wildCard of
                           WildCard ->
                             Just $ Import NotSource moduleName [name ++ "(..)"]
                           NoWildCard ->
                             Just $ Import NotSource moduleName [name]

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
  -}
