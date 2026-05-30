module Vulkan.JSON.Handle (Handle (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)
import           Vulkan.JSON.Ref      (Ref)

data Handle = Handle
  { name                 :: Text
  , aliases              :: [Text]
  , type'                :: Text
  , protect              :: Maybe Text
  , parent               :: Maybe Ref
  , instance'            :: Bool
  , device               :: Bool
  , dispatchable         :: Bool
  , extensions           :: [Text]
  , definingRequirements :: Map Text (Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Handle)
