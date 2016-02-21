module Write.Utils 
  ( comment
  , predocComment
  , postdocComment
  , isEmpty
  , showHex'
  , upperFirst
  ) where

import Data.List(intersperse)
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Numeric
import Data.Char(toUpper)

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

upperFirst :: String -> String
upperFirst "" = ""
upperFirst (x:xs) = toUpper x : xs

