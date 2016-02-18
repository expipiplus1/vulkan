{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse.Utils 
  ( optional
  , optionalAttrValue
  , requiredAttrValue
  , commaSepList
  , optionalCommaSepListAttr
  , requiredRead
  , failA
  , arrF
  , traceShowA
  , oneRequired
  , onlyChildWithName
  , getAllText
  ) where

import Text.XML.HXT.Core
import Data.Foldable (toList)
import Safe(readMay)
import Data.List.Split(splitOn)

optionalAttrValue :: ArrowXml a => String -> a XmlTree (Maybe String)
optionalAttrValue s = optional (getAttrValue0 s)

optional :: ArrowIf a => a b a1 -> a b (Maybe a1)
optional x = (x >>^ Just) `orElse` constA Nothing

requiredAttrValue :: String -> IOStateArrow s XmlTree String
requiredAttrValue s = getAttrValue0 s `orElse` 
                      failA ("Missing required attribute: " ++ s)

requiredRead :: Read a => IOStateArrow s String a
requiredRead = arrF readMay `orElse` failA "Failed to read "

arrF :: (ArrowList a, Foldable t) => (b -> t c) -> a b c
arrF f = arrL (toList . f)

failA :: String -> IOStateArrow s b c
failA s = issueErr s >>> none

traceShowA :: Show a => IOStateArrow s a a 
traceShowA = traceValue 0 show

oneRequired :: String -> IOStateArrow s a c -> IOStateArrow s a c
oneRequired e a = listA a 
                  >>> 
                  (isA (not . null) `orElse`
                    failA ("Arrow returned no results (expected one): " ++ e))
                  >>>
                  ((isA isSingleton >>> arr head) `orElse`
                    failA ("Arrow returned more than one result: " ++ e))
  where isSingleton [_] = True
        isSingleton _ = False

onlyChildWithName :: String -> IOStateArrow s XmlTree XmlTree
onlyChildWithName s = oneRequired s (hasName s <<< getChildren)

commaSepList :: String -> [String]
commaSepList = splitOn ","

optionalCommaSepListAttr :: ArrowXml a => String -> a XmlTree (Maybe [String])
optionalCommaSepListAttr n = fmap commaSepList ^<< optionalAttrValue n

getAllText :: ArrowXml a => a XmlTree [Char]
getAllText = deep getText >. concat
