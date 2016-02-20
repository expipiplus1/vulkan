module Write.Utils 
  ( comment
  , predocComment
  , postdocComment
  , isEmpty
  ) where

import Data.List(intersperse)
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

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
