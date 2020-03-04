module Spec.Types where

import           Relude                  hiding ( Handle )
import           Data.Vector                    ( Vector )
import           Data.Version

import           Spec.APIConstant
import           CType
import qualified Marshal.Marshalable           as M
import           Haskell.Name

data Spec = Spec
  { specHandles      :: Vector Handle
  , specFuncPointers :: Vector FuncPointer
  , specStructs      :: Vector Struct
  , specUnions       :: Vector Union
  , specCommands     :: Vector Command
  , specEnums        :: Vector Enum'
  , specAliases      :: Vector Alias
  , specFeatures     :: Vector Feature
  , specExtensions   :: Vector Extension
  , specConstants    :: Vector Constant
  }
  deriving (Show)

--
-- Features and Extensions
--

data Feature = Feature
  { fName :: Text
  , fVersion :: Version
  , fRequires :: Vector Require
  }
  deriving (Show)

data Extension = Extension
  { exName      :: Text
  , exNumber    :: Int
  , exRequires  :: Vector Require
  , exSupported :: Text
  }
  deriving (Show)

data Require = Require
  { rComment        :: Maybe Text
  , rCommandNames   :: Vector HName
  , rTypeNames      :: Vector HName
  , rEnumValueNames :: Vector HName
  }
  deriving (Show)

--
-- Constants
--

data Constant = Constant
  { constName :: Text
  , constValue :: ConstantValue
  }
  deriving (Show)


--
-- Aliases
--

data Alias = Alias
  { aName :: Text
  , aTarget :: Text
  , aType :: AliasType
  }
  deriving (Show)

data AliasType
  = TypeAlias
  | TermAlias
  | PatternAlias
  deriving (Show, Eq)

--
-- Function Pointers
--

data FuncPointer = FuncPointer
  { fpName :: Text
  , fpType :: CType
  }
  deriving (Show)

--
-- Handles
--

data Handle = Handle
  { hName :: Text
  , hDispatchable :: Dispatchable
  , hLevel :: HandleLevel
  }
  deriving (Show)

-- | The "level" of a handle, related to what it is descended from.
data HandleLevel
  = Instance
  | PhysicalDevice
  | Device
  | NoHandleLevel
  deriving (Show, Eq)

data Dispatchable = Dispatchable | NonDispatchable
  deriving (Show, Eq)

--
-- Structs
--


type Struct = StructOrUnion AStruct 'WithSize
type Union = StructOrUnion AUnion 'WithSize

data StructOrUnionType = AStruct | AUnion

data WithSize = WithSize | WithoutSize

type family SizeType (a :: WithSize) where
  SizeType 'WithSize = Int
  SizeType 'WithoutSize = ()

data StructOrUnion (t :: StructOrUnionType) (s :: WithSize) = Struct
  { sName :: Text
  , sMembers :: Vector (StructMember' s)
  , sSize :: SizeType s
  , sAlignment :: SizeType s
  }

deriving instance Show (StructOrUnion t 'WithSize)

data StructMember' (s :: WithSize) = StructMember
  { smName :: Text
  , smType :: CType
  , smValues :: Vector Text
  , smLengths :: Vector M.ParameterLength
  , smIsOptional :: Vector Bool
  , smOffset :: SizeType s
  }

type StructMember = StructMember' 'WithSize

deriving instance Show StructMember

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
  { cName :: Text
  , cReturnType :: CType
  , cParameters :: Vector Parameter
  -- , cCommandLevel :: HandleLevel
  , cSuccessCodes :: Vector Text
  , cErrorCodes :: Vector Text
  }
  deriving (Show, Eq)

data Parameter = Parameter
  { pName       :: Text
  , pType       :: CType
  , pLengths    :: Vector M.ParameterLength
  , pIsOptional :: Vector Bool
  }
  deriving (Show, Eq)

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
  { eName :: Text
  , eValues :: Vector EnumValue
  , eType :: EnumType
  }
  deriving (Show, Eq)

data EnumValue = EnumValue
  { evName        :: Text
  , evValue       :: Int64
  , evIsExtension :: Bool
  }
  deriving (Show, Eq, Ord)

data EnumType = AnEnum | ABitmask
  deriving (Show, Eq)

