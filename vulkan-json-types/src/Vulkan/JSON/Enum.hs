module Vulkan.JSON.Enum (Enum (..)) where

import           Prelude                  hiding (Enum)

import           Data.Aeson.TH            (deriveJSON)
import           Data.Map.Strict          (Map)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)

import           Vulkan.JSON.EnumField    (EnumField)
import           Vulkan.JSON.Internal     (vkAesonOptions)

data Enum = Enum
  { name                 :: Text
  , aliases              :: [Text]
  , protect              :: Maybe Text
  , bitWidth             :: Int
  , returnedOnly         :: Bool
  , fields               :: [EnumField]
  , extensions           :: [Text]
  , fieldExtensions      :: [Text]
  , definingRequirements :: Map Text (Maybe Text)
  , videoStdHeader       :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Enum)
