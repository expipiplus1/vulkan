{-# LANGUAGE Arrows #-}

module Parse.State
  ( ParseArrow
  , initialSpecParseState
  , getTypeNames
  , getSymbolTable
  , getSymbolTableFromDefineText
  , SpecParseState
  , parseIdentifier
  , addTypeName
  , addDefines
  , preProcessorBoolOptions
  , Stage(..)
  ) where

import qualified Data.HashSet                as HashSet
import           Language.C.Types            (CIdentifier, TypeNames)
import           Language.Preprocessor.Cpphs
import           Parse.Utils
import           Spec.Type
import           Text.XML.HXT.Core

type ParseArrow a b = IOStateArrow SpecParseState a b

type SymbolTable = [(String, String)]

data SpecParseState = SpecParseState { -- | A set of all type names we've seen
                                       spsTypeNames   :: TypeNames
                                     , -- | The macro symbol table generated
                                       -- from all the "define" typedecls
                                       spsSymbolTable :: SymbolTable
                                     }

initialSpecParseState :: SpecParseState
initialSpecParseState = SpecParseState{ spsTypeNames = HashSet.empty
                                      , spsSymbolTable = [("VKAPI_PTR", "")]
                                      }

getTypeNames :: ParseArrow b TypeNames
getTypeNames = getUserState >>^ spsTypeNames

getSymbolTable :: ParseArrow b SymbolTable
getSymbolTable = getUserState >>^ spsSymbolTable

insertTypeName :: CIdentifier -> SpecParseState -> SpecParseState
insertTypeName n s = s{ spsTypeNames = HashSet.insert n (spsTypeNames s)
                      }

insertSymbolTable :: SymbolTable -> SpecParseState -> SpecParseState
insertSymbolTable t s = s{ spsSymbolTable = spsSymbolTable s ++ t
                         }

addSymbolTable :: ParseArrow SymbolTable SymbolTable
addSymbolTable = changeUserState insertSymbolTable

data Stage = AddingDefines
           | PreprocessingTypes

preProcessorBoolOptions :: Stage -> BoolOptions
preProcessorBoolOptions s = defaultBoolOptions{ macros = True
                                              , locations = False
                                              , hashline = False
                                              , pragma = False
                                              , stripEol = True
                                              , stripC89 = True
                                                -- How's that for boolean
                                                -- blindness! (False means the
                                                -- language isn't Haskell)
                                              , lang = False
                                                -- This is particularly
                                                -- horrible... Done to
                                                -- avoid execiting the (##)
                                                -- operator too early.
                                              , ansi =
                                                  case s of
                                                    AddingDefines -> False
                                                    PreprocessingTypes -> True
                                              , layout = False
                                              , literate = False
                                              , warnings = True
                                              }

addDefines :: ParseArrow [Define] [Define]
addDefines = proc defineDecls -> do
  allDefines <- arr (stripLines . unlines . fmap dText) -< defineDecls
  symbolTable <- arrIO getSymbolTableFromDefineText -< allDefines
  addSymbolTable -< symbolTable
  returnA -< defineDecls

getSymbolTableFromDefineText :: String -> IO [(String, String)]
getSymbolTableFromDefineText = fmap snd .
                                 runCpphsReturningSymTab options "define text"
  where options = defaultCpphsOptions{ boolopts =
                                         preProcessorBoolOptions AddingDefines
                                     }

addTypeName :: ParseArrow String CIdentifier
addTypeName = parseIdentifier >>> changeUserState insertTypeName

