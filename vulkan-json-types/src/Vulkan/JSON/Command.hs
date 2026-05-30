module Vulkan.JSON.Command (Command (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)
import           Vulkan.JSON.Legacy   (Legacy)
import           Vulkan.JSON.Param    (Param)
import           Vulkan.JSON.Ref      (Ref)

data Command = Command
  { name                     :: Text
  , alias                    :: Maybe Text
  , protect                  :: Maybe Text
  , extensions               :: [Text]
  , version                  :: Maybe Ref
  , returnType               :: Text
  , params                   :: [Param]
  , instance'                :: Bool
  , device                   :: Bool
  , tasks                    :: [Text]
  , queues                   :: [Text]
  , allowNoQueues            :: Bool
  , successCodes             :: [Text]
  , errorCodes               :: [Text]
  , primary                  :: Bool
  , secondary                :: Bool
  , renderPass               :: Text
  , videoCoding              :: Text
  , implicitExternSyncParams :: [Text]
  , legacy                   :: Maybe Legacy
  , cPrototype               :: Text
  , cFunctionPointer         :: Text
  , definingRequirements     :: Map Text (Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Command)
