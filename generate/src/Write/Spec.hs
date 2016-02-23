{-# LANGUAGE QuasiQuotes #-}

module Write.Spec
  ( haskellize
  ) where

import Data.Maybe(catMaybes)
import Spec.Spec
import Spec.Type
import Text.InterpolatedString.Perl6
import Write.Constant
import Write.Enum
import Write.Header
import Write.Type.Base
import Write.Type.Bitmask
import Write.Type.FuncPointer
import Write.Type.Handle
import Write.Type.Struct
import Write.TypeConverter
import Write.Command

import Spec.Graph
import Spec.Partition

haskellize :: Spec -> String
haskellize spec = let -- TODO: Remove
                      typeConverter = cTypeToHsTypeString
                      typeEnv = buildTypeEnvFromSpec spec
                  in [qc|{writeExtensions allExtensions }
module Vulkan where

{writeImports allImports}

{writeConstants (sConstants spec)}
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
{writeStructTypes typeEnv 
                  (catMaybes . fmap typeDeclToStructType . sTypes $ spec)}
{writeUnionTypes typeEnv 
                  (catMaybes . fmap typeDeclToUnionType . sTypes $ spec)}
{writeCommands (sCommands spec)}
|]

allExtensions :: [Extension]
allExtensions = [ Extension "DataKinds"
                , Extension "DuplicateRecordFields"
                , Extension "ForeignFunctionInterface"
                , Extension "GeneralizedNewtypeDeriving"
                , Extension "PatternSynonyms"
                , Extension "Strict"
                ]

allImports :: [Import]
allImports = [ Import "Data.Bits" ["Bits", "FiniteBits"]
             , Import "Data.Int" ["Int32"]
             , Import "Data.Vector.Fixed.Storable" ["Vec"]
             , Import "Data.Vector.Fixed.Cont" ["ToPeano"]
             , Import "Data.Void" ["Void"]
             , Import "Data.Word" ["Word8", "Word32", "Word64"]
             , Import "Foreign.C.Types" ["CChar", "CFloat(..)", "CSize(..)"]
             , Import "Foreign.Ptr" ["Ptr", "FunPtr", "plusPtr", "castPtr"]
             , Import "Foreign.Storable" ["Storable(..)"]
             ]

