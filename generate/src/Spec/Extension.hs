module Spec.Extension where

import           Data.Int (Int32)

data Extension = Extension{ eName           :: String
                          , eNumber         :: Int
                          , eSupported      :: String
                          , eProtect        :: Maybe String
                          , eAuthor         :: Maybe String
                          , eContact        :: Maybe String
                          , eEnumExtensions :: [EnumExtension]
                          , eConstants      :: [ExtensionConstant]
                          , eCommandNames   :: [String]
                          , eTypeNames      :: [String]
                          }
  deriving(Show)

data Direction = Negative
               | Positive
  deriving(Show)

data EnumExtension = EnumExtension{ eeName      :: String
                                  , eeHsName    :: String
                                  , eeExtends   :: String
                                  , eeOffset    :: Int32
                                  , eeDirection :: Direction
                                  }
  deriving(Show)

data ExtensionConstant = ExtensionConstant{ ecName   :: String
                                          , ecHsName :: String
                                          , ecValue  :: Either String Integer
                                          }
  deriving(Show)

allExtensionNames :: Extension -> [String]
allExtensionNames e =
  -- TODO: Uncomment
  -- eeName <$> eEnumExtensions e ++
  -- evName <$> eConstants e ++
  eCommandNames e ++
  eTypeNames e
