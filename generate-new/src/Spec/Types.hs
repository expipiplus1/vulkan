{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
{-# language TypeFamilyDependencies #-}
module Spec.Types
  ( CName(..)
  , module Spec.Types
  , module Spec.Flavor
  ) where

import           Data.Vector                    ( Vector )
import           Data.Version
import           Relude                  hiding ( Handle )

import           CType
import qualified Marshal.Marshalable           as M
import           Spec.APIConstant
import           Spec.Name
import           Spec.Flavor

----------------------------------------------------------------
-- The spec
----------------------------------------------------------------

data Spec t = Spec
  { specHeaderVersion      :: SpecHeaderVersion t
  , specHandles            :: Vector Handle
  , specAtoms              :: Vector Atom
    -- ^ Not present in Vulkan specs
  , specFuncPointers       :: Vector FuncPointer
  , specStructs            :: Vector Struct
  , specUnions             :: Vector Union
  , specCommands           :: Vector Command
  , specEnums              :: Vector Enum'
  , specAliases            :: Vector Alias
  , specFeatures           :: Vector Feature
  , specExtensions         :: Vector Extension
  , specDisabledExtensions :: Vector Extension
  , specAPIConstants       :: Vector Constant
  , specExtensionConstants :: Vector Constant
  , specSPIRVExtensions    :: Vector SPIRVExtension
    -- ^ Only present in Vulkan specs
  , specSPIRVCapabilities  :: Vector SPIRVCapability
    -- ^ Only present in Vulkan specs
  }
  deriving Show

data SpecHeaderVersion (t :: SpecFlavor) where
  VkVersion :: Word -> SpecHeaderVersion SpecVk
  XrVersion :: Word -> Word -> Word -> SpecHeaderVersion SpecXr

deriving instance Show (SpecHeaderVersion t)

--
-- Features and Extensions
--

data Feature = Feature
  { fName :: CName
  , fVersion :: Version
  , fRequires :: Vector Require
  }
  deriving (Show)

data Extension = Extension
  { exName         :: Text
  , exNumber       :: Int
  , exRequiresCore :: Maybe Version
  , exRequires     :: Vector Require
  , exSupported    :: Text
  , exType         :: ExtensionType
  , exDependencies :: Vector Text
  }
  deriving Show

data ExtensionType = DeviceExtension | InstanceExtension | UnknownExtensionType
  deriving Show

data Require = Require
  { rComment        :: Maybe Text
  , rCommandNames   :: Vector CName
  , rTypeNames      :: Vector CName
  , rEnumValueNames :: Vector CName
  }
  deriving (Show)

--
-- Constants
--

data Constant = Constant
  { constName :: CName
  , constValue :: ConstantValue
  }
  deriving (Show)


--
-- Aliases
--

data Alias = Alias
  { aName :: CName
  , aTarget :: CName
  , aType :: AliasType
  }
  deriving (Show, Eq, Ord)

data AliasType
  = TypeAlias
  | TermAlias
  | PatternAlias
  deriving (Show, Eq, Ord)

--
-- Function Pointers
--

data FuncPointer = FuncPointer
  { fpName :: CName
  , fpType :: CType
  }
  deriving (Show)

--
-- Atoms
--

newtype Atom = Atom
  { atName :: CName
  }
  deriving (Show)

--
-- Handles
--

data Handle = Handle
  { hName :: CName
  , hDispatchable :: Dispatchable
  , hLevel :: HandleLevel
  }
  deriving (Show)

-- | The "level" of a handle, related to what it is descended from.
data HandleLevel
  = Instance
  | Device
  | NoHandleLevel
  deriving (Show, Eq, Ord)

data Dispatchable = Dispatchable | NonDispatchable
  deriving (Show, Eq)

--
-- Structs
--


type Struct = StructOrUnion AStruct 'WithSize 'WithChildren
type Union = StructOrUnion AUnion 'WithSize 'WithChildren

data StructOrUnionType = AStruct | AUnion

data WithSize = WithSize | WithoutSize

type family SizeType (a :: WithSize) = r | r -> a where
  SizeType 'WithSize = Int
  SizeType 'WithoutSize = ()

data WithChildren = WithChildren | WithoutChildren

type family ChildrenType (su :: StructOrUnionType) (a :: WithChildren) where
  ChildrenType AStruct 'WithChildren    = Vector CName
  ChildrenType _       'WithoutChildren = ()
  ChildrenType AUnion  _                = ()

data StructOrUnion (t :: StructOrUnionType) (s :: WithSize) (c :: WithChildren)
  = Struct
    { sName        :: CName
    , sMembers     :: Vector (StructMember' s)
    , sSize        :: SizeType s
    , sAlignment   :: SizeType s
    , sExtends     :: Vector CName
    , sExtendedBy  :: ChildrenType t c
    , sInherits    :: Vector CName
    , sInheritedBy :: ChildrenType t c
      -- ^ parentstruct stuff
    }

deriving instance Show (ChildrenType t 'WithChildren) => Show (StructOrUnion t 'WithSize 'WithChildren)

data StructMember' (s :: WithSize) = StructMember
  { smName :: CName
  , smType :: CType
  , smValues :: Vector Text
  , smLengths :: Vector M.ParameterLength
  , smIsOptional :: Vector Bool
  , smOffset :: SizeType s
  }

type StructMember = StructMember' 'WithSize

deriving instance Show StructMember
deriving instance Eq (StructMember' WithoutSize)

instance M.Marshalable StructMember where
  name       = smName
  type'      = smType
  values     = smValues
  lengths    = smLengths
  isOptional = smIsOptional

--
-- Commands
--

data Command = Command
  { cName         :: CName
  , cReturnType   :: CType
  , cParameters   :: Vector Parameter
  , cSuccessCodes :: Vector Text
  , cErrorCodes   :: Vector Text
  , cIsDynamic    :: Bool
  , cCanBlock     :: Bool
  }
  deriving (Show, Eq)

data Parameter = Parameter
  { pName       :: CName
  , pType       :: CType
  , pLengths    :: Vector M.ParameterLength
  , pIsOptional :: Vector Bool
  }
  deriving (Show, Eq, Ord)

instance M.Marshalable Parameter where
  name       = pName
  type'      = pType
  values     = const mempty
  lengths    = pLengths
  isOptional = pIsOptional

--
-- Enums
--

data Enum' = Enum
  { eName :: CName
  , eValues :: Vector EnumValue
  , eType :: EnumType
  }
  deriving (Show, Eq)

data EnumValue = EnumValue
  { evName        :: CName
  , evValue       :: Int64
  , evIsExtension :: Bool
  }
  deriving (Show, Eq, Ord)

data EnumType
  = AnEnum
  | ABitmask CName BitmaskWidth
  -- ^ Stores the name of the "Flags" type
  deriving (Show, Eq)

data BitmaskWidth = Bitmask32 | Bitmask64
  deriving (Show, Eq)

--
-- SPIR-V stuff
--

data SPIRVExtension = SPIRVExtension
  { spirvExtensionName :: Text
  , spirvExtensionReqs :: Vector SPIRVRequirement
  }
  deriving (Show, Eq)

data SPIRVCapability = SPIRVCapability
  { spirvCapabilityName :: Text
  , spirvCapabilityReqs :: Vector SPIRVRequirement
  }
  deriving (Show, Eq)

data SPIRVRequirement
  = SPIRVReqVersion Version
  | SPIRVReqExtension Text
  | SPIRVReqFeature CName CName (Vector Text)
    -- ^ Struct, feature, requires
  | SPIRVReqProperty CName CName CName (Vector Text)
    -- ^ Property, member, value, requires
  deriving (Show, Eq)
