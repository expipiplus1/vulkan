module Vulkan.JSON.Struct (Struct (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)
import           Vulkan.JSON.Member   (Member)
import           Vulkan.JSON.Ref      (Ref)

data Struct = Struct
  { name                 :: Text
  , aliases              :: [Text]
  , extensions           :: [Text]
  , version              :: Maybe Ref
  , protect              :: Maybe Text
  , members              :: [Member]
  , union                :: Bool
  , returnedOnly         :: Bool
  , sType                :: Maybe Text
  , allowDuplicate       :: Bool
  , extends              :: [Text]
  , extendedBy           :: [Text]
  , definingRequirements :: Map Text (Maybe Text)
  , videoStdHeader       :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Struct)
