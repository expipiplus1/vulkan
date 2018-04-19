{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parse.Type
  ( parseTypes
  ) where

import           Data.Maybe        (isNothing)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Parse.Utils
import           Spec.Type
import           Text.XML.HXT.Core

parseTypes :: IOStateArrow s XmlTree [TypeDecl]
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
  , ARequirement ^<< intType
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
  phName <- requiredAttrValueT "name" -< t
  returnA -< PlatformHeader{..}

requirement :: IOStateArrow s XmlTree Requirement
requirement = proc t -> do
  -- There is no guard for requirements, the only thing to do is fail silently
  rHeader <- getAttrValue0T "requires" -< t
  rName <- getAttrValue0T "name" -< t
  returnA -< Requirement{..}

-- | Parse a preprocessor define
define :: IOStateArrow s XmlTree Define
define = proc t -> do
  inCategory "define" -< t
  dName <- getAttrOrChildTextT "name" -< t
  -- TODO: Figure out what to do with deprecated and conditional defines
  -- dText <- isA ("#define" `isPrefixOf`) <<< getAllText -< t
  dText <- getAllTextT -< t
  returnA -< Define{..}

-- | Parse a type in the "basetype" category
baseType :: IOStateArrow s XmlTree BaseType
baseType = proc t -> do
  inCategory "basetype" -< t
  btName <- getChildTextT "name" -< t
  btType <- getChildTextT "type" -< t
  returnA -< BaseType{..}

intType :: IOStateArrow s XmlTree Requirement
intType = proc t -> do
  rName <- isA (== "int") <<< getAttrValue0T "name" -< t
  returnA -< Requirement{rHeader="builtin", ..}

-- | Parse a type in the "basetype" category
bitmask :: IOStateArrow s XmlTree BitmaskType
bitmask = proc t -> do
  inCategory "bitmask" -< t
  isA ("typedef " `T.isPrefixOf`) <<< getAllTextT -< t
  bmtName <- getChildTextT "name" -< t
  bmtType <- getChildTextT "type" -< t
  bmtRequires <- optionalAttrValueT "requires" -< t
  returnA -< BitmaskType{..}

-- | Parse a type in the "handle" category
handle :: IOStateArrow s XmlTree HandleType
handle = proc t -> do
  inCategory "handle" -< t
  htParents <- commaSepListAttrT "parent" -< t
  htName <- getChildTextT "name" -< t
  htMacro <- getChildTextT "type" -< t
  htType <- getAllTextT -< t
  returnA -< HandleType{..}

-- | Parse an enum type declaration
enum :: IOStateArrow s XmlTree EnumType
enum = proc t -> do
  inCategory "enum" -< t
  etName <- getAttrValue0T "name" -< t
  isA isNothing <<< optionalAttrValueT "alias" -< t
  returnA -< EnumType{..}

-- | Parse a function pointer type declaration
funcPointer :: IOStateArrow s XmlTree FuncPointerType
funcPointer = proc t -> do
  inCategory "funcpointer" -< t
  fptName <- getChildTextT "name" -< t
  fptType <- getAllTextT -< t
  fptTypeWithoutName <- oneRequired "funcPointer type text" getAllNonCommentNonNameText -< t
  returnA -< FuncPointerType{..}

struct :: IOStateArrow s XmlTree StructType
struct = proc t -> do
  inCategory "struct" -< t
  stName <- getAttrValue0T "name" -< t
  stComment <- optionalAttrValueT "comment" -< t
  (_metaMemberComments, stMembers) <-
    partitionEither ^<< app
    -< (allChildren (structMemberFail stName) [
         Right ^<< structMember, Left ^<< metaMemberComment
       ], t)
  stIsReturnedOnly <- boolAttrDefault "returnedonly" False -< t
  returnA -< StructType{..}

structMemberFail
  :: Text
  --- ^ Struct name
  -> IOStateArrow s XmlTree String
structMemberFail n = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  type' <- optional (getAttrOrChildText "type") -< t
  returnA -< ("Failed to parse member of struct " ++ T.unpack n)
          ++ maybe "" (" named " ++) name
          ++ maybe "" (" : " ++) type'

structMember :: IOStateArrow s XmlTree StructMember
structMember = proc m -> do
  hasName "member" -< m
  smName <- oneRequired "struct member name" (getChildTextT "name") -< m
  -- smType <- oneRequired "struct member type text" getAllNonCommentText -< m
  smType <-
    oneRequired "struct member type text" getAllNonCommentText <<<
    -- Insert a space between the "type" and "name" it doesn't exist sometimes
    (insertChildrenAt 1 (txt " "))
    -< m
  smTypeWithoutName <- oneRequired "struct member type text" getAllNonCommentNonNameText -< m
  smValues <- optionalAttrValueT "values" -< m
  smNoAutoValidity <- optional (parseBool <<< getAttrValue0 "noautovalidity") -< m
  smIsOptional <-
    traverseMaybeA (mapA parseBoolT) <<<
    optionalCommaSepListAttrT "optional" -< m
  -- TODO: comma separation might be too basic here
  smLengths <- optionalCommaSepListAttrT "len" -< m
  smAltLengths <- optionalCommaSepListAttrT "altlen" -< m
  smComment <- optional (getChildTextT "comment") -< m
  returnA -< StructMember{..}

--- | Comments which group the members (discarded at the moment)
metaMemberComment :: IOStateArrow s XmlTree SectionComment
metaMemberComment = sectionComment

union :: IOStateArrow s XmlTree UnionType
union = proc t -> do
  inCategory "union" -< t
  utName <- getAttrValue0T "name" -< t
  utComment <- optionalAttrValueT "comment" -< t
  (_metaMemberComments, utMembers) <-
    partitionEither ^<< app
    -< (allChildren (structMemberFail utName) [
         Right ^<< structMember, Left ^<< metaMemberComment
       ], t)
  utIsReturnedOnly <- boolAttrDefault "returnedonly" False -< t
  returnA -< UnionType{..}

typeAlias :: IOStateArrow s XmlTree TypeAlias
typeAlias = proc t -> do
  taName <- getAttrValue0T "name" -< t
  taAlias <- getAttrValue0T "alias" -< t
  taCategory <- getAttrValue0T "category" -< t
  returnA -< TypeAlias{..}

sectionComment :: IOStateArrow s XmlTree SectionComment
sectionComment = SectionComment ^<< getAllTextT <<< hasName "comment"

-- | Succeed if the category attribute matches the given string
inCategory
  :: ArrowXml a
  => String
  -- ^ The category to match on
  -> a XmlTree XmlTree
inCategory s = hasAttrValue "category" (==s)
