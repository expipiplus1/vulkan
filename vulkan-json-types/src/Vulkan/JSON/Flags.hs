module Vulkan.JSON.Flags (Flags (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data Flags = Flags
  { name                 :: Text
  , aliases              :: [Text]
  , bitmaskName          :: Maybe Text
  , protect              :: Maybe Text
  , baseFlagsType        :: Text
  , bitWidth             :: Int
  , returnedOnly         :: Bool
  , extensions           :: [Text]
  , definingRequirements :: Map Text (Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Flags)
