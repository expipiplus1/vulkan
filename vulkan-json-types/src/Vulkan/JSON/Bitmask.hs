module Vulkan.JSON.Bitmask (Bitmask (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Flag     (Flag)
import           Vulkan.JSON.Internal (vkAesonOptions)

data Bitmask = Bitmask
  { name                 :: Text
  , aliases              :: [Text]
  , flagName             :: Text
  , protect              :: Maybe Text
  , bitWidth             :: Int
  , returnedOnly         :: Bool
  , flags                :: [Flag]
  , extensions           :: [Text]
  , flagExtensions       :: [Text]
  , definingRequirements :: Map Text (Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Bitmask)
