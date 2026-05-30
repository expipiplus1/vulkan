module Vulkan.JSON.Param (Param (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data Param = Param
  { name              :: Text
  , alias             :: Maybe Text
  , type'             :: Text
  , fullType          :: Text
  , noAutoValidity    :: Bool
  , const'            :: Bool
  , length            :: Maybe Text
  , nullTerminated    :: Bool
  , pointer           :: Bool
  , fixedSizeArray    :: [Text]
  , optional          :: Bool
  , optionalPointer   :: Bool
  , externSync        :: Text
  , externSyncPointer :: Maybe Text
  , cDeclaration      :: Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Param)
