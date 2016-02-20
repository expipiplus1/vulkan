{-# LANGUAGE PatternSynonyms #-}

module Write.TypeConverter
  ( TypeConverter
  , cTypeToHsTypeString
  , cTypeToHsType
  , cTypeToHsType'
  , cTypeNames
  , pattern TypeDef
  ) where

-- This file is gross TODO: clean

import Spec.Type
import Language.C.Types as C
import Language.Haskell.Exts.Syntax as HS
import Language.Haskell.Exts.Pretty(prettyPrint)
-- import Data.HashMap.Strict as M
import Data.Maybe(fromMaybe)
import Control.Monad.State

type TypeConverter = CType -> String

pattern TypeDef t = TypeSpecifier (Specifiers [TYPEDEF] [] []) t

-- TODO: Make this take a CIdentifier instead
-- | Usually returns (Left CType) for further evaluation, but returns a
-- haskell type for 'platform types'
-- type TypeEnv = String -> HS.Type

-- buildTypeEnv :: [TypeDecl] -> TypeEnv
-- buildTypeEnv typeDecls =
--   where baseTypes = catMaybes . fmap typeDeclToBaseType $ typeDecls
--         baseTypeAssocs = (btName &&& Left . btCType) <$> baseTypes
--         platformTypes = catMaybes . fmap typeDeclToPlatformType $ typeDecls
--         platformTypeAssocs = 
--           (ptName &&& Right . platformTypeToHsType) <$> platformTypes

platformType :: String -> Maybe HS.Type
platformType s = 
  case s of
    "void"     -> Just $ TyCon (Special UnitCon)
    "char"     -> Just $ simpleCon "CChar"
    "float"    -> Just $ simpleCon "CFloat"
    "uint8_t"  -> Just $ simpleCon "Word8"
    "uint32_t" -> Just $ simpleCon "Word32"
    "uint64_t" -> Just $ simpleCon "Word64"
    "int32_t"  -> Just $ simpleCon "Int32"
    "size_t"   -> Just $ simpleCon "CSize"
    _          -> Nothing

cTypeToHsTypeString :: TypeConverter
cTypeToHsTypeString = prettyPrint . cTypeToHsType'

cTypeToHsType' :: CType -> HS.Type
cTypeToHsType' t = evalState (cTypeToHsType t) initialTypeConvertState

cTypeToHsType :: CType -> TypeConvert HS.Type
cTypeToHsType cType = hsType
  where 
    hsType = case cType of
               TypeSpecifier _ Void 
                 -> pure $ TyCon (Special UnitCon)
               TypeSpecifier _ (C.Char Nothing) 
                 -> pure $ simpleCon "CChar"
               TypeSpecifier _ (C.Char (Just Signed)) 
                 -> pure $ simpleCon "CChar"
               TypeSpecifier _ (C.Char (Just Unsigned)) 
                 -> pure $ simpleCon "CUChar"
               TypeSpecifier _ Float 
                 -> pure $ simpleCon "CFloat"
               TypeSpecifier _ (TypeName t) 
                 -> pure $ cIdToHsType t
               TypeDef (TypeName t) 
                 -> pure $ cIdToHsType t
               TypeDef (Struct t) 
                 -> pure $ cIdToHsType t
               Ptr _ (TypeSpecifier _ Void)
                 -> pure $ simpleCon "Ptr" `TyApp` simpleCon "Void"
               Ptr _ (Proto ret parameters) 
                 -> (simpleCon "FunPtr" `TyApp`) <$> 
                      makeFunctionType ret parameters
               Ptr _ t 
                 -> (simpleCon "Ptr" `TyApp`) <$>
                      cTypeToHsType t
               Array _ t 
                 -> (simpleCon "Vector" `TyApp`) <$> cTypeToHsType t
               _ -> error ("Failed to convert C type:\n" ++ show cType)

cIdToHsType :: CIdentifier -> HS.Type
cIdToHsType i = let s = unCIdentifier i
                in fromMaybe (simpleCon s) (platformType s)

simpleCon :: String -> HS.Type
simpleCon = TyCon . UnQual . Ident

makeFunctionType :: CType -> [ParameterDeclaration CIdentifier] 
                 -> TypeConvert HS.Type
makeFunctionType ret [ParameterDeclaration _ (TypeSpecifier _ Void)] = makeFunctionType ret []
makeFunctionType ret parameters = 
  foldr TyFun <$>
        ((simpleCon "IO" `TyApp`) <$> cTypeToHsType ret) <*>
        (traverse (cTypeToHsType . parameterDeclarationType) parameters)

type TypeConvert = State TypeConvertState

data TypeConvertState = TypeConvertState{ nextVariable :: String
                                        }

initialTypeConvertState = TypeConvertState{ nextVariable = "a"
                                          }

getTypeVariable :: TypeConvert String
getTypeVariable = gets nextVariable <* modify incrementNextVariable

succLexicographic :: (Enum a, Ord a) => a -> a -> [a] -> [a]
succLexicographic lower _ [] = [lower]
succLexicographic lower upper (x:ys) = 
  if x >= upper 
    then lower : succLexicographic lower upper ys
    else succ x : ys

incrementNextVariable :: TypeConvertState -> TypeConvertState
incrementNextVariable (TypeConvertState n) = 
  TypeConvertState (succLexicographic 'a' 'z' n)

-- | cTypeNames returns the names of the C types this type depends on
cTypeNames :: CType -> [String]
cTypeNames cType = 
  case cType of 
    TypeSpecifier _ Void 
      -> ["void"]
    TypeSpecifier _ (C.Char Nothing) 
      -> ["char"]
    TypeSpecifier _ Float 
      -> ["float"]
    TypeSpecifier _ (TypeName t) 
      -> [unCIdentifier t]
    TypeDef (Struct t) 
      -> [unCIdentifier t]
    Ptr _ t
      -> cTypeNames t
    Array _ t 
      -> cTypeNames t
    Proto ret ps 
      -> cTypeNames ret ++ concatMap parameterTypeNames ps
    _ -> error ("Failed to get depended on names for C type:\n" ++ show cType)

parameterTypeNames :: ParameterDeclaration CIdentifier -> [String]
parameterTypeNames (ParameterDeclaration _ t) = cTypeNames t

