{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse.Utils
  ( optional
  , optionalAttrValue
  , requiredAttrValue
  , commaSepList
  , optionalCommaSepListAttr
  , commaSepListAttr
  , requiredRead
  , required
  , failA
  , failString
  , arrF
  , traceShowA
  , oneRequired
  , onlyChildWithName
  , getAllText
  , getAllNonCommentText
  , oneOf
  , oneOf'
  , allChildren
  , extractFields
  , getAttrOrChildText
  , getChildText
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
  ) where

import           Data.Char         (isSpace)
import           Data.Foldable     (foldr', toList)
import           Data.List.Split   (splitOn)
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
requiredRead = required "parse with Read" readMay

-- | Try to process input with the given function, fail if Nothing is returned
required
  :: String
  -- ^ What is required to succeed, used in diagnostics
  -> (a -> Maybe b)
  -- ^ The predicate
  -> IOStateArrow s a b
required s p = arrF p `orElse` failA ("Failed with requirement: " ++ s)

arrF :: (ArrowList a, Foldable t) => (b -> t c) -> a b c
arrF f = arrL (toList . f)

failA :: String -> IOStateArrow s b c
failA s = issueErr s >>> none

failString :: IOStateArrow s String c
failString = applyA (arr issueErr) >>> none

traceShowA :: Show a => IOStateArrow s a a
traceShowA = traceValue 0 show

-- | Succeed only if the given arrow returns exactly one match
oneRequired
  :: String
  -- ^ What are we trying to get one of (Used for error messages)
  -> IOStateArrow s a c
  -> IOStateArrow s a c
oneRequired e a = listA a
                  >>>
                  (isA (not . null) `orElse`
                    failA ("Arrow returned no results (expected one): " ++ e))
                  >>>
                  ((isA isSingleton >>> arr head) `orElse`
                    failA ("Arrow returned more than one result: " ++ e))
  where isSingleton [_] = True
        isSingleton _   = False

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

getAllNonCommentText :: ArrowXml a => a XmlTree String
-- getAllNonCommentText = deepest (neg (hasName "comment") >>> getText) >. concat
getAllNonCommentText = processTopDown (neg (hasName "comment")) >>> getAllText

-- | Try each option in turn until one of them succeeds. State altering arrows
-- should be handled with care.
oneOf :: (Show b, Foldable f) => f (IOStateArrow s b c) -> IOStateArrow s b c
oneOf = foldr' orElse
        ((failA "None of the choices matched" &&& traceShowA) >>> arr fst)

-- | The same as 'oneOf' but with error message customization
oneOf'
  :: (Show b, Foldable f)
  => IOStateArrow s b String
  -- ^ Generate an error string
  -> f (IOStateArrow s b c)
  -> IOStateArrow s b c
oneOf' matchDiag = foldr'
  orElse
  (matchDiag >>> failString)

-- | Parse all children of a tree with the given parsers, raising an error
-- should a child fail to parse
allChildren
  :: IOStateArrow s XmlTree String
  -- ^ Generate an erorr string
  -> [IOStateArrow s XmlTree b]
  -> IOStateArrow s XmlTree [b]
allChildren matchDiag parsers =
  listA (oneOf' matchDiag parsers <<< neg isText <<< getChildren)

-- | 'extractFields name predicate extract' runs extract if the predicate
-- holds and fails if extract doesn't return exactly one value
extractFields :: String -> IOStateArrow s a b -> IOStateArrow s b c -> IOStateArrow s a c
extractFields name predicate extract =
  predicate >>>
  oneRequired name
    (extract `orElse` failA ("Failed to extract fields for " ++ name))

getAttrOrChildText
  :: String
  -- ^ The attribute or child name to get
  -> IOStateArrow s XmlTree String
getAttrOrChildText s = oneOf [getAttrValue0 s, getChildText s]

-- | Get all the text between the child with the given name
getChildText
  :: String
  -- ^ The name of the child to get the text of
  -> IOStateArrow s XmlTree String
getChildText s = getAllText <<< hasName s <<< getChildren

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
