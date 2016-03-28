{-# LANGUAGE Arrows #-}

module Parse.CType
  ( SpecParseState
  , ParseArrow
  , initialSpecParseState
  , addTypeName
  , parseCType
  , preprocessTypeString
  ) where

import qualified Control.Arrow               as A
import           Data.List                   (isPrefixOf)
import           Language.C.Types
import           Language.Preprocessor.Cpphs
import           Parse.State
import           Parse.Utils
import           Spec.Type                   (CType)
import           Text.XML.HXT.Core           hiding (first, second)

preprocessTypeString :: ParseArrow String String
preprocessTypeString = let process (symbolTable, string) =
                             macroPass symbolTable
                                       (preProcessorBoolOptions
                                          PreprocessingTypes)
                                       [(newfile "type string", string)]
                       in proc unPreprocessed -> do
  symbolTable <- getSymbolTable -< unPreprocessed
  arrIO process -< (symbolTable, unPreprocessed)


parseCType :: ParseArrow String CType
parseCType = proc typeString -> do
  parseContext <- getTypeNames -< typeString
  let fixedTypeString = typeStringWorkarounds typeString
      parseResult = runCParser (cCParserContext parseContext)
                               "typeString"
                               fixedTypeString
                               parseType
      failureString = "Failed to parse C type \"" ++
                      typeString ++ "\""
  cType <- fst ^<< (A.first fromRightShowA) `orElse`
                   (failString <<^ snd) -< (parseResult, failureString)
  returnA -< cType

typeStringWorkarounds :: String -> String
typeStringWorkarounds s
    -- There is no space in the between the type and the name.
  | s == "VkBufferViewCreateFlagsflags" = "VkBufferViewCreateFlags flags"
  | s == "VkSparseMemoryBindFlagsflags" = "VkSparseMemoryBindFlags flags"
    -- The struct
  | "struct " `isPrefixOf` s = strip . drop (length "struct") $ s
  | otherwise = s
