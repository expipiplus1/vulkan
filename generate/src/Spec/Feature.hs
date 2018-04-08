module Spec.Feature
  where

data Feature = Feature
  { fAPI      :: String
    -- ^ API tag (e.g. 'gl', 'gles2', etc. - used internally, not
    -- necessarily an actual API name
  , fName     :: String
    -- ^ version name (C preprocessor name, e.g. GL_VERSION_4_2)
  , fNumber   :: Rational
    -- ^ The schema says float here, but the spec has values such as "1.1"
    -- which is not representable as a Float
    --
    -- version number, e.g. 4.2
  , fProtect  :: Maybe String
    -- ^ additional #ifdef symbol to place around the feature
  , fComment  :: Maybe String
  , fElements :: [FeatureElement]
  }
  deriving(Show)

data FeatureElement
  = ARequirement FeatureRequirement
  | ARemoval -- TODO: Not in the spec so far
  deriving(Show)

data FeatureRequirement = FeatureRequirement
  { frProfile    :: Maybe String
  , frExtension  :: Maybe String
  , frComment    :: Maybe String
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
  | AComment String
  deriving(Show)

newtype EnumName = EnumName { unEnumName :: String }
  deriving(Show)
newtype TypeName = TypeName { unTypeName :: String }
  deriving(Show)
newtype CommandName = CommandName { unCommandName :: String }
  deriving(Show)

data EnumExtension = EnumExtension
  { eexExtends   :: String
  , eexExtNumber :: Maybe Int
  , eexOffset    :: Int
  , eexDirection :: Maybe Direction
  , eexName      :: String
  , eexComment   :: Maybe String
  }
  deriving(Show)

data Direction = Positive | Negative
  deriving(Show)

data BitmaskExtension = BitmaskExtension
  { bmxBitPos  :: Int
  , bmxExtends :: String
  , bmxName    :: String
  , bmxComment :: Maybe String
  }
  deriving(Show)

data EnumAlias = EnumAlias
  { eaExtends :: String
  , eaName    :: String
  , eaAlias   :: String
  , eaComment :: Maybe String
  }
  deriving(Show)

data EnumExtensionAbsolute = EnumExtensionAbsolute
  { eexaValue   :: String
  , eexaName    :: String
  , eexaExtends :: Maybe String
  , eexaComment :: Maybe String
  }
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
