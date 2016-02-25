{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Write.Utils where

import Data.Char(toUpper)
import Data.Hashable
import Data.List(intersperse, isPrefixOf, isSuffixOf, find)
import Data.List.Split(splitOn)
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

-- | 'swapPrefix prefix replacement string' will return 'string' with 'prefix'
-- replaced by 'replacement' if 'prefix' is a prefix of 'string' otherwise
-- Nothing.
swapPrefix :: String -> String -> String -> Maybe String
swapPrefix prefix replacement string =
  if prefix `isPrefixOf` string
    then Just $ replacement ++ drop (length prefix) string 
    else Nothing

-- | 'swapSuffix suffix replacement string' will return 'string' with 'suffix'
-- replaced by 'replacement' if 'suffix' is a suffix of 'string' otherwise
-- Nothing.
swapSuffix :: String -> String -> String -> Maybe String
swapSuffix suffix replacement string =
  if suffix `isSuffixOf` string
    then Just $ take (length string - length suffix) string ++ replacement
    else Nothing

breakNameTag :: [String] -> String -> (String, String)
breakNameTag tags name = 
  case find (`isSuffixOf` name) tags of
    Nothing -> (name, "")
    Just tag -> (take (length name - length tag) name, tag)

-- | Concatenate words in the string and make the first letter of each one
-- uppercase
pascalCase :: String -> String
pascalCase = concatMap upperFirst . words

getModuleBaseName :: ModuleName -> String 
getModuleBaseName (ModuleName moduleName) =
  last . splitOn "." $ moduleName

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

extensionNameToModuleName :: String -> ModuleName
extensionNameToModuleName extensionName 
  | "VK":category:n:ns <- splitOn "_" extensionName
  = ModuleName $ "Graphics.Vulkan." ++ 
                 pascalCase category ++ "." ++
                 pascalCase (unwords (n:ns))
  | otherwise
  = error ("extension name in unexpected format: " ++ extensionName)

-- | From ghc Util
transitiveClosure :: (a -> [a])         -- Successor function
                  -> (a -> a -> Bool)   -- Equality predicate
                  -> [a]
                  -> [a]                -- The transitive closure

transitiveClosure succ eq xs
 = go [] xs
 where
   go done []                      = done
   go done (x:xs) | x `is_in` done = go done xs
                  | otherwise      = go (x:done) (succ x ++ xs)

   _ `is_in` []                 = False
   x `is_in` (y:ys) | eq x y    = True
                    | otherwise = x `is_in` ys
