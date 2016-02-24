{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Write.Utils where

import Data.Char(toUpper)
import Data.Hashable
import Data.List(intersperse, isPrefixOf)
import Numeric
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

-- | A newtype for module names
newtype ModuleName = ModuleName{ unModuleName :: String }
  deriving(Eq, Show, Hashable)

comment :: String -> String
comment "" = ""
comment c = let ls = lines c
                brokenLines = intersperse "" ls
            in removeTrailingNewlines $ unlines (("-- " ++) <$> brokenLines)

docComment :: Char -> String -> String
docComment docSymbol c = case comment c of
                 "" -> ""
                 ('-':'-':s) -> "-- " ++ docSymbol : s
                 _ -> error "'comment' didn't return a comment"

postdocComment :: String -> String
postdocComment = docComment '^'

predocComment :: String -> String
predocComment = docComment '|'

removeTrailingNewlines :: String -> String
removeTrailingNewlines = reverse . dropWhile (=='\n') . reverse

isEmpty :: Doc -> Bool
isEmpty = null . show

showHex' :: (Show a, Integral a) => a -> String
showHex' n = sign ++ "0x" ++ showHex n ""
  where sign = if n < 0 
                 then "-"
                 else ""

-- | Mame the first letter of the word uppercase
upperFirst :: String -> String
upperFirst "" = ""
upperFirst (x:xs) = toUpper x : xs

-- | 'intercalatePrepend d (x:xs)' will prepend with a space d to xs 
intercalatePrepend :: Doc -> [Doc] -> [Doc]
intercalatePrepend _ [] = []
intercalatePrepend i (m:ms) = m : ((i <+>) <$> ms)

-- | Remove any 'vk' prefix (if any)
dropVK :: String -> String
dropVK name = 
  if "VK" `isPrefixOf` (toUpper <$> name) 
    then drop 2 name
    else name

-- | Concatenate words in the string and make the first letter of each one
-- uppercase
pascalCase :: String -> String
pascalCase = concatMap upperFirst . words

-- | Create a module name from a section name
sectionNameToModuleName :: String -> ModuleName
sectionNameToModuleName sectionName = 
    ModuleName ("Graphics.Vulkan." ++ baseName)
  where baseName = sectionNameToModuleBaseName sectionName

sectionNameToModuleBaseName :: String -> String
sectionNameToModuleBaseName sectionName = pascalCase moduleNameSpaces
  where moduleNameSpaces = unwords . filter isAllowed . words $ sectionName
        isAllowed n = notElem n forbiddenWords
        forbiddenWords = ["commands", "API"]
