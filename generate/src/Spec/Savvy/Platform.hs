{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Platform
  ( module Spec.Platform
  , PlatformGuardInfo(..)
  , getModuleGuardInfo
  ) where

import           Control.Arrow          ((&&&))
import           Data.Either.Validation
import qualified Data.Map               as Map
import           Data.Text
import qualified Data.Text              as T

import           Spec.Platform
import           Spec.Savvy.Error
import           Spec.Savvy.Extension

data PlatformGuardInfo = PlatformGuardInfo
  { pgiModuleName :: Text
  , pgiPlatform   :: Text
  , pgiGuard      :: Text
  }
  deriving(Eq, Ord)

getModuleGuardInfo
  :: [Extension] -> [Platform] -> Validation [SpecError] [PlatformGuardInfo]
  -- TODO: better representation
  -- ^ (moduleName, guard)
getModuleGuardInfo exts platforms =
  let guardMap = Map.fromList $ (pName &&& pProtect) <$> platforms
      platformToGuard :: Text -> Validation [SpecError] Text
      platformToGuard p = case Map.lookup p guardMap of
        Nothing -> Failure [UnknownPlatform p]
        Just g  -> pure g
  in  sequenceA
        [ PlatformGuardInfo (toModuleName "Extensions" extName) platform
            <$> platformToGuard platform
        | Extension {..} <- exts
        , Just platform  <- [extPlatform]
        ]

-- | TODO: Reduce duplication
toModuleName :: Text -> Text -> Text
toModuleName feature n = T.intercalate
  "."
  [ "Graphics"
  , "Vulkan"
  , feature
  , n
  ]

