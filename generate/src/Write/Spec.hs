{-# LANGUAGE QuasiQuotes #-}

module Write.Spec
  ( haskellize
  ) where

import Data.Maybe(catMaybes)
import Spec
import Spec.Type
import Text.InterpolatedString.Perl6
import Write.Enum
import Write.Header
import Write.Type.Base
import Write.Type.Bitmask
import Write.Type.FuncPointer
import Write.Type.Handle
import Write.Type.Struct
import Write.TypeConverter

haskellize :: Spec -> String
haskellize spec = let typeConverter = cTypeToHsTypeString
                  in [qc|{writeExtensions allExtensions }
module Vulkan where

{writeImports allImports}

{writeBaseTypes typeConverter 
                (catMaybes . fmap typeDeclToBaseType . sTypes $ spec)}
{writeHandleTypes typeConverter 
                  (catMaybes . fmap typeDeclToHandleType . sTypes $ spec)}
{writeFuncPointerTypes typeConverter 
                       (catMaybes . fmap typeDeclToFuncPointerType . sTypes $ 
                         spec)}
{writeBitmaskTypes typeConverter 
                   (sBitmasks spec) 
                   (catMaybes . fmap typeDeclToBitmaskType . sTypes $ spec)}
{writeEnums (sEnums spec)}
{writeStructTypes typeConverter 
                  (catMaybes . fmap typeDeclToStructType . sTypes $ spec)}
{writeUnionTypes typeConverter 
                  (catMaybes . fmap typeDeclToUnionType . sTypes $ spec)}
|]

allExtensions :: [Extension]
allExtensions = [ Extension "PatternSynonyms"
                , Extension "GeneralizedNewtypeDeriving"
                , Extension "DuplicateRecordFields"
                , Extension "Strict"
                ]

allImports :: [Import]
allImports = [ Import "Data.Bits" ["Bits", "FiniteBits"]
             , Import "Data.Int" ["Int32"]
             , Import "Data.Vector.Storable" ["Vector"]
             , Import "Data.Void" ["Void"]
             , Import "Data.Word" ["Word8", "Word32", "Word64"]
             , Import "Foreign.C.Types" ["CChar", "CFloat", "CSize"]
             , Import "Foreign.Ptr" ["Ptr", "FunPtr", "plusPtr"]
             , Import "Foreign.Storable" ["Storable(..)"]
             ]

