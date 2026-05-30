module Vulkan.JSON.Member (Member (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data Member = Member
  { name            :: Text
  , type'           :: Text
  , fullType        :: Text
  , noAutoValidity  :: Bool
  , limitType       :: Maybe Text
  , const'          :: Bool
  , length          :: Maybe Text
  , nullTerminated  :: Bool
  , pointer         :: Bool
  , fixedSizeArray  :: [Text]
  , optional        :: Bool
  , optionalPointer :: Bool
  , externSync      :: Text
  , cDeclaration    :: Text
  , bitFieldWidth   :: Maybe Int
  , selector        :: Maybe Text
  , selection       :: [Text]
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Member)
