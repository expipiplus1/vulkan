{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Type where

import           Data.Semigroup
import           Data.Text
import           Language.C.Types (CIdentifier, Type)

type CType = Type CIdentifier

data TypeDecl
  = -- | These aren't really types, they are #includes too
    APlatformHeader PlatformHeader
  | ARequirement Requirement
  | ADefine Define
  | ABaseType BaseType
  | ABitmaskType BitmaskType
  | AHandleType HandleType
  | AnEnumType EnumType
  | AFuncPointerType FuncPointerType
  | AStructType StructType
  | AUnionType UnionType
  | -- | A comment separating type sections
    ASectionComment SectionComment
  | AnAlias TypeAlias
  deriving (Show, Eq)


newtype PlatformHeader = PlatformHeader { phName :: Text }
  deriving (Show, Eq)

data Requirement = Requirement
  { rName   :: Text
    -- ^ The name of the type requiring the header
  , rHeader :: Text
    -- ^ The required header for this type
  }
  deriving (Show, Eq)


data Define = Define { dName :: Text
                     , dText :: Text
                     }
  deriving (Show, Eq)

data BaseType = BaseType { btName :: Text
                         , btType :: Text
                         }
  deriving (Show, Eq)

data BitmaskType = BitmaskType { bmtName     :: Text
                               , bmtType     :: Text
                               , bmtRequires :: Maybe Text
                               }
  deriving (Show, Eq)

data HandleType = HandleType { htName    :: Text
                             , htParents :: [Text]
                             , htMacro   :: Text
                             , htType    :: Text
                             }
  deriving (Show, Eq)

newtype EnumType = EnumType { etName :: Text
                            }
  deriving (Show, Eq)

data FuncPointerType = FuncPointerType { fptName            :: Text
                                       , fptType            :: Text
                                       , fptTypeWithoutName :: Text
                                       }
  deriving (Show, Eq)

data StructType = StructType { stName           :: Text
                             , stComment        :: Maybe Text
                             , stMembers        :: [StructMember]
                             , stIsReturnedOnly :: Bool
                             }
  deriving (Show, Eq)

data StructMember = StructMember { smName            :: Text
                                 , smType            :: Text
                                 , smTypeWithoutName :: Text
                                 , smValues          :: Maybe Text
                                 , smNoAutoValidity  :: Maybe Bool
                                 , smIsOptional      :: Maybe [Bool]
                                 , smLengths         :: Maybe [Text]
                                 , smAltLengths      :: Maybe [Text]
                                 , smComment         :: Maybe Text
                                 }
  deriving (Show, Eq)

data UnionType = UnionType { utName           :: Text
                           , utComment        :: Maybe Text
                           , utMembers        :: [StructMember]
                           , utIsReturnedOnly :: Bool
                           }
  deriving (Show, Eq)

newtype SectionComment = SectionComment { scText :: Text }
  deriving (Show, Eq)

data TypeAlias = TypeAlias
  { taName     :: Text
    -- ^ The new type name
  , taAlias    :: Text
    -- ^ What it is an alias of
  , taCategory :: Text
  }
  deriving (Show, Eq)

----------------------------------------------------------------
-- old stuff
----------------------------------------------------------------


typeDeclTypeName :: TypeDecl -> Maybe Text
typeDeclTypeName = \case
  (APlatformHeader _)    -> Nothing
  (ARequirement r)       -> Just $ rName r
  (ADefine d)            -> Just $ dName d
  (ABaseType bt)         -> Just $ btName bt
  (ABitmaskType bmt)     -> Just $ bmtName bmt
  (AHandleType ht)       -> Just $ htName ht <> "_T"
  (AnEnumType et)        -> Just $ etName et
  (AFuncPointerType fpt) -> Just $ fptName fpt
  (AStructType st)       -> Just $ stName st
  (AUnionType ut)        -> Just $ utName ut
  (ASectionComment _)    -> Nothing
  (AnAlias ta)           -> Just $ taName ta

--    APlatformHeader PlatformHeader
--  | ARequirement Requirement
--  | ADefine Define
--  | ABaseType BaseType
--  | ABitmaskType BitmaskType
--  | AHandleType HandleType
--  | AnEnumType EnumType
--  | AFuncPointerType FuncPointerType
--  | AStructType StructType
--  | AUnionType UnionType
--  | -- | A comment separating type sections
--    ASectionComment SectionComment
--  | AnAlias TypeAlias


{-
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
