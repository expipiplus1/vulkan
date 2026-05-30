module Vulkan.JSON.Flag (Flag (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data Flag = Flag
  { name                 :: Text
  , aliases              :: [Text]
  , parent               :: Text
  , protect              :: Maybe Text
  , value                :: Integer
  , valueStr             :: Text
  , bitpos               :: Maybe Int
  , multiBit             :: Bool
  , zero                 :: Bool
  , extensions           :: [Text]
  , extending            :: Bool
  , definingRequirements :: Map Text (Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Flag)
