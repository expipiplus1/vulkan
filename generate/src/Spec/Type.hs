module Spec.Type where

import           Language.C.Types (CIdentifier, Type)

type CType = Type CIdentifier

data TypeDecl
  = -- | These aren't really types, they are #includes too
    APlatformHeader PlatformHeader
  | ARequirement Requirement
  | ADefine Define
  | ABaseType BaseType
  | APlatformType PlatformType
  | ABitmaskType BitmaskType
  | AHandleType HandleType
  | AnEnumType EnumType
  | AFuncPointerType FuncPointerType
  | AStructType StructType
  | AUnionType UnionType
  | -- | A comment separating type sections
    ASectionComment SectionComment
  | AnAlias TypeAlias

  | AnInclude Include
  deriving (Show, Eq)


data PlatformHeader = PlatformHeader { phName :: String }
  deriving (Show, Eq)

data Requirement = Requirement
  { rName   :: String
    -- ^ The name of the type requiring the header
  , rHeader :: String
    -- ^ The required header for this type
  }
  deriving (Show, Eq)


data Define = Define { dName :: String
                     , dText :: String
                     }
  deriving (Show, Eq)

data BaseType = BaseType { btName :: String
                         , btType :: String
                         }
  deriving (Show, Eq)

data BitmaskType = BitmaskType { bmtName     :: String
                               , bmtType     :: String
                               , bmtRequires :: Maybe String
                               }
  deriving (Show, Eq)

data HandleType = HandleType { htName    :: String
                             , htParents :: [String]
                             , htType    :: String
                             }
  deriving (Show, Eq)

data EnumType = EnumType { etName :: String
                         }
  deriving (Show, Eq)

data FuncPointerType = FuncPointerType { fptName       :: String
                                       , fptTypeString :: String
                                       }
  deriving (Show, Eq)

data StructType = StructType { stName           :: String
                             , stComment        :: Maybe String
                             , stMembers        :: [StructMember]
                             , stIsReturnedOnly :: Bool
                             }
  deriving (Show, Eq)

data StructMember = StructMember { smName           :: String
                                 , smType           :: String
                                 , smValues         :: Maybe String
                                 , smNoAutoValidity :: Maybe Bool
                                 , smIsOptional     :: Maybe [Bool]
                                 , smLengths        :: Maybe [String]
                                 , smAltLengths     :: Maybe [String]
                                 , smComment        :: Maybe String
                                 }
  deriving (Show, Eq)

data UnionType = UnionType { utName           :: String
                           , utComment        :: Maybe String
                           , utMembers        :: [StructMember]
                           , utIsReturnedOnly :: Bool
                           }
  deriving (Show, Eq)

newtype SectionComment = SectionComment { scText :: String }
  deriving (Show, Eq)

data TypeAlias = TypeAlias
  { taName     :: String
  , taAlias    :: String
  , taCategory :: String
  }
  deriving (Show, Eq)

----------------------------------------------------------------
-- old stuff
----------------------------------------------------------------

-- TODO: Remove
data Include = Include { iName     :: String
                       , iFilename :: String
                       }
  deriving (Show, Eq)


data PlatformType = PlatformType { ptName     :: String
                                 , ptRequires :: String
                                 }
  deriving (Show, Eq)



{-
typeDeclTypeName :: TypeDecl -> Maybe String
typeDeclTypeName (AnInclude _)          = Nothing
typeDeclTypeName (ADefine _)            = Nothing
typeDeclTypeName (ABaseType bt)         = Just $ btName bt
typeDeclTypeName (APlatformType pt)     = Just $ ptName pt
typeDeclTypeName (ABitmaskType bmt)     = Just $ bmtName bmt
typeDeclTypeName (AHandleType ht)       = Just $ htName ht
typeDeclTypeName (AnEnumType et)        = Just $ etName et
typeDeclTypeName (AFuncPointerType fpt) = Just $ fptName fpt
typeDeclTypeName (AStructType st)       = Just $ stName st
typeDeclTypeName (AUnionType ut)        = Just $ utName ut

typeDeclCType :: TypeDecl -> Maybe CType
typeDeclCType (AnInclude _)          = Nothing
typeDeclCType (ADefine _)            = Nothing
typeDeclCType (ABaseType bt)         = Just $ btCType bt
typeDeclCType (APlatformType _)      = Nothing
typeDeclCType (ABitmaskType bmt)     = Just $ bmtCType bmt
typeDeclCType (AHandleType ht)       = Just $ htCType ht
typeDeclCType (AnEnumType _)         = Nothing
typeDeclCType (AFuncPointerType fpt) = Just $ fptCType fpt
typeDeclCType (AStructType _)        = Nothing
typeDeclCType (AUnionType _)         = Nothing

typeDeclToInclude :: TypeDecl -> Maybe Include
typeDeclToInclude (AnInclude x) = Just x
typeDeclToInclude _             = Nothing

typeDeclToDefine :: TypeDecl -> Maybe Define
typeDeclToDefine (ADefine x) = Just x
typeDeclToDefine _           = Nothing

typeDeclToBaseType :: TypeDecl -> Maybe BaseType
typeDeclToBaseType (ABaseType x) = Just x
typeDeclToBaseType _             = Nothing

typeDeclToPlatformType :: TypeDecl -> Maybe PlatformType
typeDeclToPlatformType (APlatformType x) = Just x
typeDeclToPlatformType _                 = Nothing

typeDeclToBitmaskType :: TypeDecl -> Maybe BitmaskType
typeDeclToBitmaskType (ABitmaskType x) = Just x
typeDeclToBitmaskType _                = Nothing

typeDeclToHandleType :: TypeDecl -> Maybe HandleType
typeDeclToHandleType (AHandleType x) = Just x
typeDeclToHandleType _               = Nothing

typeDeclToEnumType :: TypeDecl -> Maybe EnumType
typeDeclToEnumType (AnEnumType x) = Just x
typeDeclToEnumType _              = Nothing

typeDeclToFuncPointerType :: TypeDecl -> Maybe FuncPointerType
typeDeclToFuncPointerType (AFuncPointerType x) = Just x
typeDeclToFuncPointerType _                    = Nothing

typeDeclToStructType :: TypeDecl -> Maybe StructType
typeDeclToStructType (AStructType x) = Just x
typeDeclToStructType _               = Nothing

typeDeclToUnionType :: TypeDecl -> Maybe UnionType
typeDeclToUnionType (AUnionType x) = Just x
typeDeclToUnionType _              = Nothing
-}
