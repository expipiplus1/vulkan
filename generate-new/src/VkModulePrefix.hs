module VkModulePrefix
  ( vulkanModule
  , vulkanModulePrefix
  )
where

import qualified Data.Text                     as T
import           Relude

import           Render.Element                 ( ModName(..) )

-- | Module prefix for generated source
vulkanModulePrefix :: Text
vulkanModulePrefix = "Vulkan"

vulkanModule :: [Text] -> ModName
vulkanModule = ModName . T.intercalate "." . (vulkanModulePrefix :)
