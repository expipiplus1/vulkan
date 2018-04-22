{-# LANGUAGE OverloadedStrings #-}

module Write.Module.Aggregate
  ( makeAggregateModules
  ) where

import           Data.List.Extra
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Spec.Savvy.Platform
import           Write.Module

makeAggregateModules
  :: [PlatformGuardInfo]
  -> [Module]
  -- ^ Modules to make aggregates for
  -> [Module]
makeAggregateModules guards ms
  = let
      inputNames :: [[Text]]
      inputNames =
        nubOrd
          .   splitModuleName
          <$> (filter (`notElem` disallowedModules) . fmap mName $ ms)
      newNames :: [[Text]]
      newNames =
        nubOrd (filter (not . null) (inits =<< inputNames)) \\ inputNames
    in
      [ Module aggName [] [] reexports
      | nameComponents <- newNames
      , not . null $ nameComponents
      , let
        aggName       = unsplitModuleName nameComponents
        reexportNames = unsplitModuleName
          <$> filter ((nameComponents ==) . init) (inputNames ++ newNames)
        reexports =
          [ ReexportedModule
              n
              (pgiGuard <$> find ((== n) . pgiModuleName) guards)
          | n <- reexportNames
          ]
      , length reexports > 1
      ]

splitModuleName :: Text -> [Text]
splitModuleName = T.splitOn "."

unsplitModuleName :: [Text] -> Text
unsplitModuleName = T.intercalate "."

-- | Modules which shouldn't feature in any aggregate
disallowedModules :: [Text]
disallowedModules = ["Graphics.Vulkan.Dynamic"]
