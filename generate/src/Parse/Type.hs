{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Parse.Type
  ( parseTypes
  ) where

import           Control.Bool      ((<||>))
import           Data.Char         (isAlpha, isDigit)
import           Data.List         (isPrefixOf)
import           Data.Maybe        (fromMaybe, isNothing)
import           Parse.CType
import           Parse.State
import           Parse.Utils
import           Spec.Type
import           Text.Regex.TDFA   ((=~))
import           Text.XML.HXT.Core

parseTypes :: ParseArrow XmlTree [TypeDecl]
parseTypes = extractFields "type decls" (hasName "types") extract
  where extract = proc typeDeclBlock -> do
          -- Annoyingly although platform types appear after base types in the
          -- spec, the contain definitions which the base types need. Hence
          -- having to parse them out of order here. The same goes for the
          -- defines and a few of the struct names.
          typeDecls <- allChildren typeFailDiag typeParsers -< typeDeclBlock
          returnA -< typeDecls
          -- (defineDeclTrees, nonDefineTypeDeclTrees) <-
          --   partitionA (inCategory "define") -< typeDecls
          -- (platformTypeDeclTrees, ordinaryTypeDeclTrees) <-
          --   partitionA isPlatformTypeDecl
          --   -< nonDefineTypeDeclTrees

          -- defineDecls <-
          --   fmap ADefine ^<< addDefines <<< mapA parseDefine
          --   -< defineDeclTrees

          -- platformTypeDecls <-
          --   mapA (addTypeDeclToState <<< parseType) -< platformTypeDeclTrees

          -- mapA (addTypeName <<< peekStructOrUnionName) -< ordinaryTypeDeclTrees

          -- ordinaryTypeDecls <-
          --   mapA (addTypeDeclToState <<< parseType) -< ordinaryTypeDeclTrees

          -- returnA -< platformTypeDecls ++ defineDecls ++ ordinaryTypeDecls

typeParsers :: [IOStateArrow s XmlTree TypeDecl]
typeParsers =
  [ -- It's important to alias to go at the top to scoop out out all elements
    -- with an category et which don't actually match the schema for that
    -- category. This means that later when we see a category we can give more
    -- precise errors if things go wrong.
    AnAlias ^<< typeAlias
  , APlatformHeader ^<< platformHeader
  , ARequirement ^<< requirement
  , ADefine ^<< define
  , ABaseType ^<< baseType
  , ABaseType ^<< intType
  , ABitmaskType ^<< bitmask
  , AHandleType ^<< handle
  , AnEnumType ^<< enum
  , AFuncPointerType ^<< funcPointer
  , AStructType ^<< struct
  , AUnionType ^<< union
  , ASectionComment ^<< sectionComment
  ]

typeFailDiag :: IOStateArrow s XmlTree String
typeFailDiag = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  category <- optionalAttrValue "category" -< t
  returnA -< "Failed to parse type"
          ++ maybe "" (" named " ++) name
          ++ maybe "" (" in category " ++) category


----------------------------------------------------------------
-- The Type parsers themselves
--
-- These were created by reading the 'types' section of the spec and adding any
-- unhndled cases.
----------------------------------------------------------------

platformHeader :: IOStateArrow s XmlTree PlatformHeader
platformHeader = proc t -> do
  inCategory "include" -< t
  phName <- requiredAttrValue "name" -< t
  returnA -< PlatformHeader{..}

requirement :: IOStateArrow s XmlTree Requirement
requirement = proc t -> do
  -- There is no guard for requirements, the only thing to do is fail silently
  rHeader <- getAttrValue0 "requires" -< t
  rName <- getAttrValue0 "name" -< t
  returnA -< Requirement{..}

-- | Parse a preprocessor define
define :: IOStateArrow s XmlTree Define
define = proc t -> do
  inCategory "define" -< t
  dName <- getAttrOrChildText "name" -< t
  -- TODO: Figure out what to do with deprecated and conditional defines
  -- dText <- isA ("#define" `isPrefixOf`) <<< getAllText -< t
  dText <- getAllText -< t
  returnA -< Define{..}

-- | Parse a type in the "basetype" category
baseType :: IOStateArrow s XmlTree BaseType
baseType = proc t -> do
  inCategory "basetype" -< t
  btName <- getChildText "name" -< t
  btType <- getChildText "type" -< t
  returnA -< BaseType{..}

-- | Parse the "int" declaration
intType :: IOStateArrow s XmlTree BaseType
intType = proc t -> do
  btName <- isA (== "int") <<< getAttrValue0 "name" -< t
  let btType = btName
  returnA -< BaseType{..}

-- | Parse a type in the "basetype" category
bitmask :: IOStateArrow s XmlTree BitmaskType
bitmask = proc t -> do
  inCategory "bitmask" -< t
  isA ("typedef " `isPrefixOf`) <<< getAllText -< t
  bmtName <- getChildText "name" -< t
  bmtType <- getChildText "type" -< t
  bmtRequires <- optionalAttrValue "requires" -< t
  returnA -< BitmaskType{..}

-- | Parse a type in the "handle" category
handle :: IOStateArrow s XmlTree HandleType
handle = proc t -> do
  inCategory "handle" -< t
  htParents <- commaSepListAttr "parent" -< t
  htName <- getChildText "name" -< t
  htType <- getChildText "type" -< t
  returnA -< HandleType{..}

-- | Parse an enum type declaration
enum :: IOStateArrow s XmlTree EnumType
enum = proc t -> do
  inCategory "enum" -< t
  etName <- getAttrValue0 "name" -< t
  isA isNothing <<< optionalAttrValue "alias" -< t
  returnA -< EnumType{..}

-- | Parse a function pointer type declaration
funcPointer :: IOStateArrow s XmlTree FuncPointerType
funcPointer = proc t -> do
  inCategory "funcpointer" -< t
  fptName <- getChildText "name" -< t
  fptTypeString <- getAllText -< t
  returnA -< FuncPointerType{..}

struct :: IOStateArrow s XmlTree StructType
struct = proc t -> do
  inCategory "struct" -< t
  stName <- getAttrValue0 "name" -< t
  stComment <- optionalAttrValue "comment" -< t
  -- stMembers <- allChildren (structMemberFail a) [structMember] -< t
  (_metaMemberComments, stMembers) <-
    partitionEither ^<< app
    -< (allChildren (structMemberFail stName) [
         Right ^<< structMember, Left ^<< metaMemberComment
       ], t)
  stIsReturnedOnly <- boolAttrDefault "returnedonly" False -< t
  returnA -< StructType{..}

-- TODO: better location info
structMemberFail
  :: String
  --- ^ Struct name
  -> IOStateArrow s XmlTree String
structMemberFail n = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  type' <- optional (getAttrOrChildText "type") -< t
  returnA -< ("Failed to parse member of struct " ++ n)
          ++ maybe "" (" named " ++) name
          ++ maybe "" (" : " ++) type'

structMember :: IOStateArrow s XmlTree StructMember
structMember = proc m -> do
  hasName "member" -< m
  smName <- oneRequired "struct member name" (getChildText "name") -< m
  smType <- oneRequired "struct member type text" getAllNonCommentText -< m
  smValues <- optionalAttrValue "values" -< m
  smNoAutoValidity <- optional (parseBool <<< getAttrValue0 "noautovalidity") -< m
  smIsOptional <-
    traverseMaybeA (mapA parseBool) <<<
    optionalCommaSepListAttr "optional" -< m
  -- TODO: comma separation might be too basic here
  smLengths <- optionalCommaSepListAttr "len" -< m
  smAltLengths <- optionalCommaSepListAttr "altlen" -< m
  smComment <- optional (getChildText "comment") -< m
  returnA -< StructMember{..}

--- | Comments which group the members (discarded at the moment)
metaMemberComment :: IOStateArrow s XmlTree SectionComment
metaMemberComment = sectionComment

union :: IOStateArrow s XmlTree UnionType
union = proc t -> do
  inCategory "union" -< t
  utName <- getAttrValue0 "name" -< t
  utComment <- optionalAttrValue "comment" -< t
  (_metaMemberComments, utMembers) <-
    partitionEither ^<< app
    -< (allChildren (structMemberFail utName) [
         Right ^<< structMember, Left ^<< metaMemberComment
       ], t)
  utIsReturnedOnly <- boolAttrDefault "returnedonly" False -< t
  returnA -< UnionType{..}

typeAlias :: IOStateArrow s XmlTree TypeAlias
typeAlias = proc t -> do
  taName <- getAttrValue0 "name" -< t
  taAlias <- getAttrValue0 "alias" -< t
  taCategory <- getAttrValue0 "category" -< t
  returnA -< TypeAlias{..}

sectionComment :: IOStateArrow s XmlTree SectionComment
sectionComment = SectionComment ^<< getAllText <<< hasName "comment"

-- parseMember :: ParseArrow XmlTree StructMember
-- parseMember = extractFields "struct member"
--                             (hasName "member")
--                             extract
--   where extract = proc member -> do
--           smName <- memberNameWorkarounds ^<< getNameChildText -< member
--           smType <- preprocessTypeString <<< getAllText -< member
--           smCType <- parseCType -< smTypeString
--           smNoAutoValidity <-
--             boolAttrDefault "noautovalidity" False -< member
--           smIsOptional <- traverseMaybeA (mapA parseBool) <<<
--                           optionalCommaSepListAttr "optional" -< member
--           smLengths <- optionalCommaSepListAttr "len" -< member
--           -- TODO: comments
--           smComment <- constA Nothing -< member
--           returnA -< StructMember{..}

-- parseStructType :: ParseArrow XmlTree StructType
-- parseStructType = extractFields "struct type"
--                                 (inCategory "struct")
--                                 extract
--   where extract = proc structType -> do
--           stName <- getAttrValue0 "name" -< structType
--           stComment <- optionalAttrValue "comment" -< structType
--           stMembers <- listA (parseMember <<< getChildren) -< structType
--           stUsage <- ((parseValidityBlock <<< getChildren) `orElse`
--                       constA []) -< structType
--           stIsReturnedOnly <-
--             boolAttrDefault "returnedonly" False -< structType
--           returnA -< StructType{..}

-- |


-- parseDefine = extractFields "define"
--                             (inCategory "define")
--                             extract
--   where extract = proc define -> do
--           dName <- getNameAttrOrChildText -< define
--           dText <- getAllText -< define
--           dSymTab <- arrIO getSymbolTableFromDefineText <<^
--                      addTrailingNewline -< dText
--           returnA -< Define{..}

-- parseInclude :: ParseArrow XmlTree Include
-- parseInclude = extractFields "include"
--                              (inCategory "include")
--                              extract
--   where extract = proc include -> do
--           iName <- getNameAttrOrChildText -< include
--           iFilename <- parseIncludeFilename <<< getAllText -< include
--           returnA -< Include{..}

{-

peekStructOrUnionName :: ArrowXml a => a XmlTree String
peekStructOrUnionName = hasName "type" >>>
                        hasAttrValue "category"
                                     (flip elem ["struct", "union"]) >>>
                        getAttrValue0 "name"

addTypeDeclToState :: ParseArrow TypeDecl TypeDecl
addTypeDeclToState = perform (typeDeclTypeName ^>>
                              traverseMaybeA addTypeName)

parseType :: ParseArrow XmlTree TypeDecl
parseType = hasName "type" >>>
            (extract `orElse` failA "Failed to extract type fields")
  where extract = oneOf [ AnInclude        ^<< parseInclude
                          -- Defines are handled on their own earlier because we
                          -- need to add them to the state.
                          --, ADefine          ^<< parseDefine
                        , ABaseType        ^<< parseBaseType
                        , APlatformType    ^<< parsePlatformType
                        , ABitmaskType     ^<< parseBitmaskType
                        , AHandleType      ^<< parseHandleType
                        , AnEnumType       ^<< parseEnumType
                        , AFuncPointerType ^<< parseFuncPointerType
                        , AStructType      ^<< parseStructType
                        , AUnionType       ^<< parseUnionType
                        ]

parseInclude :: ParseArrow XmlTree Include
parseInclude = extractFields "include"
                             (inCategory "include")
                             extract
  where extract = proc include -> do
          iName <- getNameAttrOrChildText -< include
          iFilename <- parseIncludeFilename <<< getAllText -< include
          returnA -< Include{..}

parseIncludeFilename :: ParseArrow String String
parseIncludeFilename = oneRequired "include filename parse" (arrF go)
  where go s = let matches = s =~ "#include[[:space:]]*[\"<](.*)[\">]"
               in case matches of
                    [[_, filename]] -> Just filename
                    _               -> Nothing

parseDefine :: ParseArrow XmlTree Define
parseDefine = extractFields "define"
                            (inCategory "define")
                            extract
  where extract = proc define -> do
          dName <- getNameAttrOrChildText -< define
          dText <- getAllText -< define
          dSymTab <- arrIO getSymbolTableFromDefineText <<^
                     addTrailingNewline -< dText
          returnA -< Define{..}

addTrailingNewline :: String -> String
addTrailingNewline = (++ "\n")

parseBaseType :: ParseArrow XmlTree BaseType
parseBaseType = extractFields "base type"
                              (inCategory "basetype")
                              extract
  where extract = proc baseType -> do
          btName <- getAllText <<< hasName "name" <<< getChildren -< baseType
          btTypeString <- preprocessTypeString <<< getAllText -< baseType
          btCType <- parseCType -< btTypeString
          returnA -< BaseType{..}

parsePlatformType :: ParseArrow XmlTree PlatformType
parsePlatformType = extractFields "platform type"
                                  isPlatformTypeDecl
                                  extract
  where extract = proc platformType -> do
          ptName <- getAttrValue0 "name" -< platformType
          ptRequires <- getAttrValue0 "requires" -< platformType
          returnA -< PlatformType{..}

isPlatformTypeDecl :: ParseArrow XmlTree XmlTree
isPlatformTypeDecl = (hasAttr "requires" >>> neg (hasAttr "category"))

parseHandleType :: ParseArrow XmlTree HandleType
parseHandleType = extractFields "handle type"
                                (inCategory "handle")
                                extract
  where extract = proc handleType -> do
          htName <- getNameChildText -< handleType
          htParents <- commaSepListAttr "parent" -< handleType
          htTypeString <- preprocessTypeString <<< getAllText -< handleType
          -- TODO: need to do defines here
          htCType <- parseCType -< htTypeString
          returnA -< HandleType{..}

parseEnumType :: ParseArrow XmlTree EnumType
parseEnumType = extractFields "enum type"
                              (inCategory "enum")
                              extract
  where extract = proc enumType -> do
          etName <- getAttrValue0 "name" -< enumType
          returnA -< EnumType{..}

parseFuncPointerType :: ParseArrow XmlTree FuncPointerType
parseFuncPointerType = extractFields "funcpointer type"
                                     (inCategory "funcpointer")
                                     extract
  where extract = proc funcPointerType -> do
          fptName <- getNameChildText -< funcPointerType
          fptTypeString <- preprocessTypeString <<< getAllText -< funcPointerType
          fptCType <- parseCType -< fptTypeString
          returnA -< FuncPointerType{..}

parseStructType :: ParseArrow XmlTree StructType
parseStructType = extractFields "struct type"
                                (inCategory "struct")
                                extract
  where extract = proc structType -> do
          stName <- getAttrValue0 "name" -< structType
          stComment <- optionalAttrValue "comment" -< structType
          stMembers <- listA (parseMember <<< getChildren) -< structType
          stUsage <- ((parseValidityBlock <<< getChildren) `orElse`
                      constA []) -< structType
          stIsReturnedOnly <-
            boolAttrDefault "returnedonly" False -< structType
          returnA -< StructType{..}

parseUnionType :: ParseArrow XmlTree UnionType
parseUnionType = extractFields "union type"
                               (inCategory "union")
                               extract
  where extract = proc unionType -> do
          utName <- getAttrValue0 "name" -< unionType
          utComment <- optionalAttrValue "comment" -< unionType
          utMembers <- listA (parseMember <<< getChildren) -< unionType
          utUsage <- ((parseValidityBlock <<< getChildren) `orElse`
                      constA []) -< unionType
          utIsReturnedOnly <-
            boolAttrDefault "returnedonly" False -< unionType
          returnA -< UnionType{..}

parseMember :: ParseArrow XmlTree StructMember
parseMember = extractFields "struct member"
                            (hasName "member")
                            extract
  where extract = proc member -> do
          smName <- memberNameWorkarounds ^<< getNameChildText -< member
          smTypeString <- preprocessTypeString <<< getAllText -< member
          smCType <- parseCType -< smTypeString
          smNoAutoValidity <-
            boolAttrDefault "noautovalidity" False -< member
          smIsOptional <- traverseMaybeA (mapA parseBool) <<<
                          optionalCommaSepListAttr "optional" -< member
          smLengths <- optionalCommaSepListAttr "len" -< member
          -- TODO: comments
          smComment <- constA Nothing -< member
          returnA -< StructMember{..}

-- In VkImageBlit "srcOffsets" is "srcOffsets[2]" and "dstOffsets" is
-- "dstOffsets[2]"
memberNameWorkarounds :: String -> String
memberNameWorkarounds = takeWhile (isAlpha <||> isDigit <||> (=='_'))

-}

-- | Succeed if the category attribute matches the given string
inCategory
  :: ArrowXml a
  => String
  -- ^ The category to match on
  -> a XmlTree XmlTree
inCategory s = hasAttrValue "category" (==s)
