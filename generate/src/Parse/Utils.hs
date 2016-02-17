module Parse.Utils 
  ( optionalAttrValue
  , requiredAttrValue
  , requiredRead
  , failA
  , arrF
  , traceShowA
  , oneRequired
  ) where

import Text.XML.HXT.Core
import Data.Foldable (toList)
import Safe(readMay)

optionalAttrValue :: ArrowXml a => String -> a XmlTree (Maybe String)
optionalAttrValue s = (getAttrValue0 s >>^ Just) `orElse` constA Nothing

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
oneRequired e a = listA a >>> ((isA isSingleton >>> arr head) `orElse` 
                    failA ("Arrow didn't return exactly one result: " ++ e))
  where isSingleton [_] = True
        isSingleton _ = False

