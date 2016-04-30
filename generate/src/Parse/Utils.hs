{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse.Utils
  ( optional
  , optionalAttrValue
  , requiredAttrValue
  , commaSepList
  , optionalCommaSepListAttr
  , commaSepListAttr
  , requiredRead
  , failA
  , failString
  , arrF
  , traceShowA
  , oneRequired
  , onlyChildWithName
  , getAllText
  , oneOf
  , extractFields
  , getNameAttrOrChildText
  , getNameChildText
  , parseValidityBlock
  , traverseMaybeA
  , mapA
  , parseBool
  , boolAttrDefault
  , fromRightA
  , fromRightShowA
  , strip
  , stripL
  , stripR
  , stripLines
  , parseIdentifier
  ) where

import           Data.Char         (isSpace)
import           Data.Foldable     (foldr', toList)
import           Data.List.Split   (splitOn)
import           Language.C.Types  (CIdentifier, cIdentifierFromString)
import           Safe              (readMay)
import           Text.XML.HXT.Core

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

failString :: IOStateArrow s String c
failString = applyA (arr issueErr) >>> none

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

-- | When the attribute is not present this returns the empty list
commaSepListAttr :: ArrowXml a => String -> a XmlTree [String]
commaSepListAttr n = commaSepList ^<< getAttrValue n

optionalCommaSepListAttr :: ArrowXml a => String -> a XmlTree (Maybe [String])
optionalCommaSepListAttr n = fmap commaSepList ^<< optionalAttrValue n

getAllText :: ArrowXml a => a XmlTree String
getAllText = deep getText >. concat

-- | Try each option in turn until one of them succeeds. State altering arrows
-- should be handled with care.
oneOf :: (Show b, Foldable f) => f (IOStateArrow s b c) -> IOStateArrow s b c
oneOf = foldr' orElse
        ((failA "None of the choices matched" &&& traceShowA) >>> arr fst)

-- | 'extractFields name predicate extract' runs extract if the predicate
-- holds and fails if extract doesn't return exactly one value
extractFields :: String -> IOStateArrow s a b -> IOStateArrow s b c -> IOStateArrow s a c
extractFields name predicate extract =
  predicate >>>
  oneRequired name
    (extract `orElse` failA ("Failed to extract fields for " ++ name))

-- | Get either the attribute called "name" or the text between the "name"
-- tags.
getNameAttrOrChildText :: IOStateArrow s XmlTree String
getNameAttrOrChildText =
  oneOf [ getAttrValue0 "name"
        , getNameChildText
        ]

-- | Get all the text between the child with name "name"
getNameChildText :: IOStateArrow s XmlTree String
getNameChildText = getAllText <<< hasName "name" <<< getChildren

parseValidityBlock :: ArrowXml a => a XmlTree [String]
parseValidityBlock = hasName "validity" >>>
                     listA (getChildren >>> parseUsageString)
  where parseUsageString = hasName "usage" >>> getAllText

traverseMaybeA :: ArrowIf a => a b c -> a (Maybe b) (Maybe c)
traverseMaybeA a = (arrF id >>> a >>^ Just) `orElse` constA Nothing

mapA :: ArrowList a => a b c -> a [b] [c]
mapA a = listA (a <<< unlistA)

parseBool :: IOStateArrow s String Bool
parseBool = arrF go `orElse` failA "Failed to parse bool"
  where go "true"  = Just True
        go "false" = Just False
        go _       = Nothing

boolAttrDefault :: String -> Bool -> IOStateArrow s XmlTree Bool
boolAttrDefault attrName def =
  (parseBool <<< getAttrValue0 attrName) `orElse`
  constA def

-- | Take the right element of either or failA with the error on the Left
fromRightA :: IOStateArrow s (Either String a) a
fromRightA = ifP isRight
                 (arr $ \(Right x) -> x)
                 (applyA (arr $ \(Left e) -> failA e))
  where isRight (Right _) = True
        isRight (Left _)  = False

-- | Take the right element of either or failA with the error on the Left
fromRightShowA :: Show e => IOStateArrow s (Either e a) a
fromRightShowA = ifP isRight
                 (arr $ \(Right x) -> x)
                 (arr (\(Left e) -> show e) ^>> failString)
  where isRight (Right _) = True
        isRight (Left _)  = False

strip :: String -> String
strip = stripL . stripR

stripL :: String -> String
stripL = dropWhile isSpace

stripR :: String -> String
stripR = reverse . stripL . reverse

stripLines :: String -> String
stripLines = unlines . fmap strip . lines

parseIdentifier :: IOStateArrow s String CIdentifier
parseIdentifier = (not . isReservedIdentifier) `guardsP`
                   (cIdentifierFromString ^>> fromRightA)

isReservedIdentifier :: String -> Bool
isReservedIdentifier i = i `elem` reservedIdentifiers
  where reservedIdentifiers = [ "void"
                              , "char"
                              , "float"
                              ]
