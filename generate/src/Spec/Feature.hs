module Spec.Feature
  where

import           Data.Text

data Feature = Feature
  { fAPI      :: Text
    -- ^ API tag (e.g. 'gl', 'gles2', etc. - used internally, not
    -- necessarily an actual API name
  , fName     :: Text
    -- ^ version name (C preprocessor name, e.g. GL_VERSION_4_2)
  , fNumber   :: Rational
    -- ^ The schema says float here, but the spec has values such as "1.1"
    -- which is not representable as a Float
    --
    -- version number, e.g. 4.2
  , fProtect  :: Maybe Text
    -- ^ additional #ifdef symbol to place around the feature
  , fComment  :: Maybe Text
  , fElements :: [FeatureElement]
  }
  deriving(Show)

data FeatureElement
  = ARequirement FeatureRequirement
  | ARemoval -- TODO: Not in the spec so far
  deriving(Show)

data FeatureRequirement = FeatureRequirement
  { frProfile    :: Maybe Text
  , frExtension  :: Maybe Text
  , frComment    :: Maybe Text
  , frInterfaces :: [InterfaceElement]
  }
  deriving(Show)

data InterfaceElement
  = AnEnumName EnumName
  | ATypeName TypeName
  | ACommandName CommandName
  | AnEnumExtension EnumExtension
  | ABitmaskExtension BitmaskExtension
  | AnEnumAlias EnumAlias
  | AnEnumExtensionAbsolute EnumExtensionAbsolute
  | AnEnumValue EnumValue
  | AComment Text
  deriving(Show)

newtype EnumName = EnumName { unEnumName :: Text }
  deriving(Show)
newtype TypeName = TypeName { unTypeName :: Text }
  deriving(Show)
newtype CommandName = CommandName { unCommandName :: Text }
  deriving(Show)

data EnumExtension = EnumExtension
  { eexExtends   :: Text
  , eexExtNumber :: Maybe Int
  , eexOffset    :: Int
  , eexDirection :: Maybe Direction
  , eexName      :: Text
  , eexComment   :: Maybe Text
  }
  deriving(Show)

data Direction = Positive | Negative
  deriving(Show)

data BitmaskExtension = BitmaskExtension
  { bmxBitPos  :: Int
  , bmxExtends :: Text
  , bmxName    :: Text
  , bmxComment :: Maybe Text
  }
  deriving(Show)

data EnumAlias = EnumAlias
  { eaExtends :: Text
  , eaName    :: Text
  , eaAlias   :: Text
  , eaComment :: Maybe Text
  }
  deriving(Show)

data EnumExtensionAbsolute = EnumExtensionAbsolute
  { eexaValue     :: Int
  , eexaName      :: Text
  , eexaExtends   :: Text
  , eexaExtNumber :: Maybe Int
  , eexaComment   :: Maybe Text
  }
  deriving(Show)

data EnumValue = EnumValue
  { evValue   :: EnumValueType
  , evName    :: Text
  , evComment :: Maybe Text
  }
  deriving(Show)

data EnumValueType
  = EnumValueString Text
  | EnumValueInt Int
  | EnumValueAlias Text
  deriving(Show)

-- From the API schema
{- # Each <feature> defines the interface of an API version (e.g. OpenGL 1.2)
   #   api - API tag (e.g. 'gl', 'gles2', etc. - used internally, not
   #     necessarily an actual API name
   #   name - version name (C preprocessor name, e.g. GL_VERSION_4_2)
   #   number - version number, e.g. 4.2
   #   protect - additional #ifdef symbol to place around the feature
   #   <require> / <remove> contains features to require or remove in
   #                        this version
   #     profile - only require/remove when generated profile matches
   #     comment - unused
   Feature = element feature {
       attribute api { text } ,
       Name ,
       attribute number { xsd:float } ,
       attribute protect { text } ? ,
       Comment ? ,
       (
           element require {
               ProfileName ? ,
               ExtensionName ? ,
               Comment ? ,
               (
                   InterfaceElement |
                   element comment { text }
               ) *
           } |
           element remove {
               ProfileName ? ,
               Comment ? ,
               (
                   InterfaceElement |
                   element comment { text }
               ) *
           }
       ) *
   }
-}
