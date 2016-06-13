module Spec.Extension where

data Extension = Extension{ eName         :: String
                          , eNumber       :: Int
                          , eSupported    :: String
                          , eProtect      :: Maybe String
                          , eAuthor       :: Maybe String
                          , eContact      :: Maybe String
                          , eEnums        :: [ExtensionEnum]
                          , eConstants    :: [ExtensionConstant]
                          , eBitmasks     :: [ExtensionBitmask]
                          , eCommandNames :: [String]
                          , eTypeNames    :: [String]
                          }
  deriving(Show)

data Direction = Negative
               | Positive
  deriving(Show)

data ExtensionEnum = ExtensionEnum{ eeName      :: String
                                  , eeExtends   :: String
                                  , eeOffset    :: Int
                                  , eeDirection :: Direction
                                  }
  deriving(Show)

data ExtensionConstant = ExtensionConstant{ ecName    :: String
                                          , ecValue   :: Either String Integer
                                          , ecExtends :: Maybe String
                                          }
  deriving(Show)

data ExtensionBitmask = ExtensionBitmask{ ebmName    :: String
                                        , ebmBitpos  :: Integer
                                        , ebmExtends :: Maybe String
                                        }
  deriving(Show)

allExtensionNames :: Extension -> [String]
allExtensionNames e =
  (eeName <$> eEnums e) ++
  -- evName <$> eConstants e ++
  eCommandNames e ++
  eTypeNames e
