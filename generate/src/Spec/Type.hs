module Spec.Type where
  
import Language.C.Types (Type, CIdentifier)

type CType = Type CIdentifier

data TypeDecl = AnInclude Include
              | ADefine Define
              | ABaseType BaseType
              | APlatformType PlatformType
              | ABitmaskType BitmaskType
              | AHandleType HandleType
              | AnEnumType EnumType
              | AFuncPointerType FuncPointerType
              | AStructType StructType
              | AUnionType UnionType
  deriving (Show)

data Include = Include { iName :: String
                       , iFilename :: String
                       }
  deriving (Show)

data Define = Define { dName :: String
                     , dText :: String
                     }
  deriving (Show)

data BaseType = BaseType { btName :: String
                         , btTypeString :: String
                         , btCType :: CType
                         }
  deriving (Show)

data PlatformType = PlatformType { ptName :: String
                                 , ptRequires :: String
                                 }
  deriving (Show)

data BitmaskType = BitmaskType { bmtName :: String
                               , bmtTypeString :: String
                               , bmtRequires :: Maybe String
                               , bmtCType :: CType
                               }
  deriving (Show)

data HandleType = HandleType { htName :: String
                             , htParents :: [String]
                             , htTypeString :: String
                             , htCType :: CType
                             }
  deriving (Show)

data EnumType = EnumType { etName :: String
                         }
  deriving (Show)

data FuncPointerType = FuncPointerType { fptName :: String
                                       , fptTypeString :: String
                                       , fptCType :: CType
                                       }
  deriving (Show)

data StructType = StructType { stName :: String
                             , stComment :: Maybe String
                             , stMembers :: [StructMember]
                             , stUsage :: [String]
                             , stIsReturnedOnly :: Bool
                             }
  deriving (Show)

data StructMember = StructMember { smName :: String
                                 , smTypeString :: String
                                 , smCType :: CType
                                 , smNoAutoValidity :: Bool
                                 , smIsOptional :: Maybe [Bool]
                                 , smLengths :: Maybe [String]
                                 , smComment :: Maybe String
                                 }
  deriving (Show)

data UnionType = UnionType { utName :: String
                           , utComment :: Maybe String
                           , utMembers :: [StructMember]
                           , utUsage :: [String]
                           , utIsReturnedOnly :: Bool
                           }
  deriving (Show)

typeDeclTypeNames :: TypeDecl -> Maybe String
typeDeclTypeNames (AnInclude _)          = Nothing
typeDeclTypeNames (ADefine _)            = Nothing
typeDeclTypeNames (ABaseType bt)         = Just $ btName bt
typeDeclTypeNames (APlatformType pt)     = Just $ ptName pt
typeDeclTypeNames (ABitmaskType bmt)     = Just $ bmtName bmt
typeDeclTypeNames (AHandleType ht)       = Just $ htName ht
typeDeclTypeNames (AnEnumType et)        = Just $ etName et
typeDeclTypeNames (AFuncPointerType fpt) = Just $ fptName fpt
typeDeclTypeNames (AStructType st)       = Just $ stName st
typeDeclTypeNames (AUnionType ut)        = Just $ utName ut

typeDeclToInclude :: TypeDecl -> Maybe Include
typeDeclToInclude (AnInclude x) = Just x
typeDeclToInclude _ = Nothing

typeDeclToDefine :: TypeDecl -> Maybe Define
typeDeclToDefine (ADefine x) = Just x
typeDeclToDefine _ = Nothing

typeDeclToBaseType :: TypeDecl -> Maybe BaseType
typeDeclToBaseType (ABaseType x) = Just x
typeDeclToBaseType _ = Nothing

typeDeclToPlatformType :: TypeDecl -> Maybe PlatformType
typeDeclToPlatformType (APlatformType x) = Just x
typeDeclToPlatformType _ = Nothing

typeDeclToBitmaskType :: TypeDecl -> Maybe BitmaskType
typeDeclToBitmaskType (ABitmaskType x) = Just x
typeDeclToBitmaskType _ = Nothing

typeDeclToHandleType :: TypeDecl -> Maybe HandleType
typeDeclToHandleType (AHandleType x) = Just x
typeDeclToHandleType _ = Nothing

typeDeclToEnumType :: TypeDecl -> Maybe EnumType
typeDeclToEnumType (AnEnumType x) = Just x
typeDeclToEnumType _ = Nothing

typeDeclToFuncPointerType :: TypeDecl -> Maybe FuncPointerType
typeDeclToFuncPointerType (AFuncPointerType x) = Just x
typeDeclToFuncPointerType _ = Nothing

typeDeclToStructType :: TypeDecl -> Maybe StructType
typeDeclToStructType (AStructType x) = Just x
typeDeclToStructType _ = Nothing

typeDeclToUnionType :: TypeDecl -> Maybe UnionType
typeDeclToUnionType (AUnionType x) = Just x
typeDeclToUnionType _ = Nothing
