module Vulkan.JSON.FuncPointer (FuncPointer (..)) where

import           Data.Aeson.TH                  (deriveJSON)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)

import           Vulkan.JSON.FuncPointerParam   (FuncPointerParam)
import           Vulkan.JSON.Internal           (vkAesonOptions)

data FuncPointer = FuncPointer
  { name             :: Text
  , protect          :: Maybe Text
  , returnType       :: Text
  , requires         :: Maybe Text
  , params           :: [FuncPointerParam]
  , cFunctionPointer :: Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''FuncPointer)
