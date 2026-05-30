module Vulkan.JSON.EnumField (EnumField (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data EnumField = EnumField
  { name                 :: Text
  , aliases              :: [Text]
  , parent               :: Text
  , protect              :: Maybe Text
  , negative             :: Bool
  , value                :: Integer
  , valueStr             :: Text
  , extensions           :: [Text]
  , extending            :: Bool
  , definingRequirements :: Map Text (Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''EnumField)
