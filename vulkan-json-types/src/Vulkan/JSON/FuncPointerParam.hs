module Vulkan.JSON.FuncPointerParam (FuncPointerParam (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data FuncPointerParam = FuncPointerParam
  { name         :: Text
  , type'        :: Text
  , fullType     :: Text
  , cDeclaration :: Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''FuncPointerParam)
