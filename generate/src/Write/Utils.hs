{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Write.Utils where

import           Data.Char                    (toLower, toUpper)
import           Data.Hashable
import           Data.HashMap.Strict          (HashMap)
import           Data.List                    (find, foldl', intersperse,
                                               isPrefixOf, isSuffixOf)
import           Data.List.Split              (splitOn)
import           Data.Maybe                   (fromMaybe)
import           Numeric
import           Spec.ExtensionTag
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              (takeDirectory, (<.>), (</>))
import           Text.PrettyPrint.Leijen.Text hiding (string, (<$>), (</>))

-- | A newtype for module names
newtype ModuleName = ModuleName{ unModuleName :: String }
  deriving(Eq, Show, Hashable)

type NameLocations = HashMap String (ModuleName, String)

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

swapSuffixUnderTag :: [ExtensionTag] -- ^ All the known extension tags
                   -> String -- ^ suffix
                   -> String -- ^ replacement
                   -> String -- ^ string
                   -> Maybe String
swapSuffixUnderTag tags suffix replacement string = (++ tagOrEmpty) <$> swappedBase
  where (baseName, tag) = breakNameTag tags string
        tagOrEmpty = fromMaybe "" (unExtensionTag <$> tag)
        swappedBase = swapSuffix suffix replacement baseName

breakNameTag :: [ExtensionTag] -> String -> (String, Maybe ExtensionTag)
breakNameTag tags name =
  case find ((`isSuffixOf` name) . unExtensionTag) tags of
    Nothing -> (name, Nothing)
    Just tag -> ( take (length name - length (unExtensionTag tag)) name
                , Just tag)

-- | Concatenate words in the string and make the first letter of each one
-- uppercase.
pascalCase :: String -> String
pascalCase = concatMap upperFirst . words

-- | Concatenate words separated by underscores in the string and make the
-- first letter of each one uppercase.
pascalCase_ :: String -> String
pascalCase_ = concatMap upperFirst . splitOn "_"

-- | Concatenate words separated by underscores in the string and make the
-- first letter of each one but the first uppercase.
camelCase_ :: String -> String
camelCase_ "" = ""
camelCase_ s = let w:ws = splitOn "_" . fmap toLower $ s
               in w ++ concatMap upperFirst ws

getModuleBaseName :: ModuleName -> String
getModuleBaseName (ModuleName moduleName) =
  last . splitOn "." $ moduleName

-- | Create a module name from a section name
sectionNameToModuleName :: String -> ModuleName
sectionNameToModuleName sectionName =
    ModuleName ("Graphics.Vulkan." ++ baseName)
  where baseName = sectionNameToModuleBaseName sectionName

sectionNameToModuleBaseName :: String -> String
sectionNameToModuleBaseName "Types not directly used by the API" = "OtherTypes"
sectionNameToModuleBaseName "Header boilerplate" = "OtherTypes"
sectionNameToModuleBaseName sectionName = pascalCase moduleNameSpaces
  where moduleNameSpaces = unwords . filter isAllowed . words $ sectionName
        isAllowed n = n `notElem` forbiddenWords
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
transitiveClosure successor eq = go []
 where
   go done []                      = done
   go done (x:xs) | x `is_in` done = go done xs
                  | otherwise      = go (x:done) (successor x ++ xs)

   _ `is_in` []                 = False
   x `is_in` (y:ys) | eq x y    = True
                    | otherwise = x `is_in` ys

-- | Create the directory that this module sits in under the given root
createModuleDirectory :: FilePath -> ModuleName -> IO ()
createModuleDirectory root moduleName =
  let moduleDirectory = takeDirectory . moduleNameToFile root $ moduleName
      createParents = True
  in createDirectoryIfMissing createParents moduleDirectory

-- | Returns the filename for the specified module under the given root
moduleNameToFile :: FilePath -> ModuleName -> FilePath
moduleNameToFile root (ModuleName moduleName) =
  let pathComponents = splitOn "." moduleName
  in foldl' (</>) root pathComponents <.> "hs"
