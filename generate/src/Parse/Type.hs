{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Parse.Type
  ( parseTypes
  ) where

import           Control.Bool                 ((<||>))
import           Data.Char                    (isAlpha, isDigit)
import qualified Language.Haskell.Exts.Syntax as HS
import           Parse.CType
import           Parse.State
import           Parse.Utils
import           Spec.Type
import           Text.Regex.TDFA              ((=~))
import           Text.XML.HXT.Core

parseTypes :: ParseArrow XmlTree [TypeDecl]
parseTypes = extractFields "type decls" (hasName "types") extract
  where extract = proc typeDeclBlock -> do
          -- Annoyingly although platform types appear after base types in the
          -- spec, the contain definitions which the base types need. Hence
          -- having to parse them out of order here. The same goes for the
          -- defines and a few of the struct names.
          typeDecls <- listA getChildren -< typeDeclBlock
          (defineDeclTrees, nonDefineTypeDeclTrees) <-
            partitionA (inCategory "define") -< typeDecls
          (platformTypeDeclTrees, ordinaryTypeDeclTrees) <-
            partitionA isPlatformTypeDecl
            -< nonDefineTypeDeclTrees

          defineDecls <-
            fmap ADefine ^<< addDefines <<< mapA parseDefine
            -< defineDeclTrees

          platformTypeDecls <-
            mapA (addTypeDeclToState <<< parseType) -< platformTypeDeclTrees

          mapA (addTypeName <<< peekStructOrUnionName) -< ordinaryTypeDeclTrees

          ordinaryTypeDecls <-
            mapA (addTypeDeclToState <<< parseType) -< ordinaryTypeDeclTrees

          returnA -< platformTypeDecls ++ defineDecls ++ ordinaryTypeDecls

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
                    _ -> Nothing

parseDefine :: ParseArrow XmlTree Define
parseDefine = extractFields "define"
                            (inCategory "define")
                            extract
  where extract = proc define -> do
          dName <- getNameAttrOrChildText -< define
          let dHsName = dName
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
          let btHsName = btName
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

parseBitmaskType :: ParseArrow XmlTree BitmaskType
parseBitmaskType = extractFields "bitmask type"
                                 (inCategory "bitmask")
                                 extract
  where extract = proc bitmaskType -> do
          bmtName <- getNameChildText -< bitmaskType
          let bmtHsName = bmtName
          bmtTypeString <- preprocessTypeString <<< getAllText -< bitmaskType
          bmtRequires <- optionalAttrValue "requires" -< bitmaskType
          bmtCType <- parseCType -< bmtTypeString
          returnA -< BitmaskType{..}

parseHandleType :: ParseArrow XmlTree HandleType
parseHandleType = extractFields "handle type"
                                (inCategory "handle")
                                extract
  where extract = proc handleType -> do
          htName <- getNameChildText -< handleType
          let htHsName = htName
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
          let etHsName = etName
          returnA -< EnumType{..}

parseFuncPointerType :: ParseArrow XmlTree FuncPointerType
parseFuncPointerType = extractFields "funcpointer type"
                                     (inCategory "funcpointer")
                                     extract
  where extract = proc funcPointerType -> do
          fptName <- getNameChildText -< funcPointerType
          let fptHsName = fptName
          fptTypeString <- preprocessTypeString <<< getAllText -< funcPointerType
          fptCType <- parseCType -< fptTypeString
          returnA -< FuncPointerType{..}

parseStructType :: ParseArrow XmlTree StructType
parseStructType = extractFields "struct type"
                                (inCategory "struct")
                                extract
  where extract = proc structType -> do
          stName <- getAttrValue0 "name" -< structType
          let stHsName = stName
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
          let utHsName = utName
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
          let smHsName = smName
          smTypeString <- preprocessTypeString <<< getAllText -< member
          smCType <- parseCType -< smTypeString
          let smHsType = HS.TyCon $ HS.UnQual $ HS.Ident "FOO"
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

inCategory :: ArrowXml a => String -> a XmlTree XmlTree
inCategory s = hasAttrValue "category" (==s)
