{-# LANGUAGE QuasiQuotes #-}

module Write.Header where

import Data.List.Extra(nubOrd, intercalate)
import Text.InterpolatedString.Perl6

data Import = Import ModuleName [String]
            | ImportQualified ModuleName Alias [String]

newtype Extension = Extension String
  deriving (Eq, Ord)

type ModuleName = String

type Alias = String

writeExtensions :: [Extension] -> String
writeExtensions es = let es' = nubOrd es
                     in unlines (languagePragma <$> es')
  where languagePragma (Extension e) = "{-# LANGUAGE " ++ e ++ " #-}"
                      
writeImports :: [Import] -> String
writeImports = unlines . fmap importDecl
  where importDecl (Import mn is) = 
          [qc|import {mn} ({intercalate ", " is})|]
        importDecl (ImportQualified mn alias is) = 
          [qc|import qualified {mn} as {alias} ({intercalate "," is})|]

