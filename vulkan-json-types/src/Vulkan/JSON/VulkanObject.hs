module Vulkan.JSON.VulkanObject (VulkanObject (..)) where

import           Prelude                     hiding (Enum)

import           Data.Aeson                  (Value)
import           Data.Aeson.TH               (deriveJSON)
import           Data.Map.Strict             (Map)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)

import           Vulkan.JSON.Bitmask         (Bitmask)
import           Vulkan.JSON.Command         (Command)
import           Vulkan.JSON.Constant        (Constant)
import           Vulkan.JSON.Enum            (Enum)
import           Vulkan.JSON.Extension       (Extension)
import           Vulkan.JSON.Flags           (Flags)
import           Vulkan.JSON.Format          (Format)
import           Vulkan.JSON.FuncPointer     (FuncPointer)
import           Vulkan.JSON.Handle          (Handle)
import           Vulkan.JSON.Internal        (vkAesonOptions)
import           Vulkan.JSON.Spirv           (Spirv)
import           Vulkan.JSON.Struct          (Struct)
import           Vulkan.JSON.SyncAccess      (SyncAccess)
import           Vulkan.JSON.SyncPipeline    (SyncPipeline)
import           Vulkan.JSON.SyncStage       (SyncStage)
import           Vulkan.JSON.Version         (Version)
import           Vulkan.JSON.VideoCodec      (VideoCodec)

type AliasRequirements = Map Text (Map Text (Maybe Text))

data VulkanObject = VulkanObject
  { headerVersion          :: Text
  , headerVersionComplete  :: Text
  , extensions             :: Map Text Extension
  , versions               :: Map Text Version
  , handles                :: Map Text Handle
  , commands               :: Map Text Command
  , structs                :: Map Text Struct
  , enums                  :: Map Text Enum
  , bitmasks               :: Map Text Bitmask
  , flags                  :: Map Text Flags
  , constants              :: Map Text Constant
  , formats                :: Map Text Format
  , funcPointers           :: Map Text FuncPointer
  , syncStage              :: [SyncStage]
  , syncAccess             :: [SyncAccess]
  , syncPipeline           :: [SyncPipeline]
  , spirv                  :: [Spirv]
  , platforms              :: Map Text Text
  , vendorTags             :: [Text]
  , aliasTypeRequirements  :: AliasRequirements
  , aliasFieldRequirements :: AliasRequirements
  , aliasFlagRequirements  :: AliasRequirements
  , videoCodecs            :: Map Text VideoCodec
  , videoStd               :: Maybe Value
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''VulkanObject)
