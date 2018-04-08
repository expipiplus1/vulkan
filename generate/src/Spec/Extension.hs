module Spec.Extension where

import           Data.Int     (Int32)
import           Spec.Feature

data Extension = Extension
  { extName      :: String
    -- ^ extension name string
  , extNumber    :: Word
    -- ^ extension number (positive integer, should be unique)
  , extProtect   :: Maybe String
    -- ^ C preprocessor symbol to conditionally define the interface
  , extPlatform  :: Maybe String
  , extAuthor    :: Maybe String
    -- ^ name of the author (usually a company or project name)
  , extContact   :: Maybe String
    -- ^ contact responsible for the tag (name and contact information)
  , extType      :: Maybe ExtensionType
    -- ^ 'device' or 'instance', if present
  , extRequires  :: Maybe [String]
    -- ^ commas-separated list of extension names required by this
    --     extension
  , extSupported :: ExtensionSupport
    -- ^ profile name(s) supporting this extension, e.g. 'vulkan'
    --     or 'disabled' to never generate output.
  , extElements  :: [ExtensionElement]
  }
  deriving(Show)

data ExtensionType = Device | Instance
  deriving(Show)

data ExtensionSupport = Disabled | Profile String
  deriving(Show)

data ExtensionElement
  = AnExtensionRequirement ExtensionRequirement
  | AnExtensionRemoval
    -- ^ Unused in the spec: TODO
  deriving(Show)

data ExtensionRequirement = ExtensionRequirement
  { erAPI               :: Maybe String
  , erProfile           :: Maybe String
  , erExtension         :: Maybe String
  , erFeature           :: Maybe String
  , erComment           :: Maybe String
  , erInterfaceElements :: [InterfaceElement]
  }
  deriving(Show)

-- From the XML schema
{- # Defines the interface of an API <extension>. Like a <feature>
   # tag, but with slightly different attributes:
   #   api - regexp pattern matching one or more API tags, indicating
   #     which APIs the extension is known to work with. The only
   #     syntax supported is <name>{|<name>}* and each name must
   #     exactly match an API being generated (implicit ^$ surrounding).
   #   name - extension name string
   #   number - extension number (positive integer, should be unique)
   #   protect - C preprocessor symbol to conditionally define the interface
   #   author - name of the author (usually a company or project name)
   #   contact - contact responsible for the tag (name and contact information)
   #   type - 'device' or 'instance', if present
   #   requires - commas-separated list of extension names required by this
   #       extension
   #   supported - profile name(s) supporting this extension, e.g. 'vulkan'
   #       or 'disabled' to never generate output.
   # In addition, <require> / <remove> tags also support an api attribute:
   #     api - only require/remove these features for the matching API.
   #       Not a regular expression.
   Extension = element extension {
       Name ,
       attribute number { Integer } ? ,
       attribute protect { text } ? ,
       attribute platform { text } ? ,
       attribute author { text } ? ,
       attribute contact { text } ? ,
       attribute type { text } ? ,
       attribute requires { text } ? ,
       attribute supported { StringGroup } ? ,
       Comment ? ,
       (
           element require {
               attribute api { text } ? ,
               ProfileName ? ,
               ExtensionName ? ,
               FeatureName ? ,
               Comment ? ,
               (
                   InterfaceElement |
                   element comment { text }
               ) *
           } |
           element remove {
               attribute api { text } ? ,
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
