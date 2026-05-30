module Vulkan.JSON.Ref
  ( Ref (..)
  , RefType (..)
  ) where

import           Data.Aeson   (FromJSON (..), ToJSON (..), Value (String), object,
                               withObject, withText, (.:), (.=))
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics (Generic)

-- | The complete set of @__type__@ discriminators known to appear in the
-- Vulkan JSON dump. Used both as the payload of a 'Ref' (where the value
-- under @__ref__@ in the source is one of these names) and as a registry
-- of every concrete record type in this package.
data RefType
  = RefBitmask
  | RefCommand
  | RefConstant
  | RefEnum
  | RefEnumField
  | RefExtension
  | RefFeatureRequirement
  | RefFlag
  | RefFlags
  | RefFormat
  | RefFormatComponent
  | RefFormatPlane
  | RefFuncPointer
  | RefFuncPointerParam
  | RefHandle
  | RefLegacy
  | RefMember
  | RefParam
  | RefSpirv
  | RefSpirvEnables
  | RefStruct
  | RefSyncAccess
  | RefSyncEquivalent
  | RefSyncPipeline
  | RefSyncPipelineStage
  | RefSyncStage
  | RefSyncSupport
  | RefVersion
  | RefVideoCodec
  | RefVideoFormat
  | RefVideoProfileMember
  | RefVideoProfiles
  | RefVideoRequiredCapabilities
  | RefVulkanObject
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)

refTypeText :: RefType -> Text
refTypeText = \case
  RefBitmask                   -> "Bitmask"
  RefCommand                   -> "Command"
  RefConstant                  -> "Constant"
  RefEnum                      -> "Enum"
  RefEnumField                 -> "EnumField"
  RefExtension                 -> "Extension"
  RefFeatureRequirement        -> "FeatureRequirement"
  RefFlag                      -> "Flag"
  RefFlags                     -> "Flags"
  RefFormat                    -> "Format"
  RefFormatComponent           -> "FormatComponent"
  RefFormatPlane               -> "FormatPlane"
  RefFuncPointer               -> "FuncPointer"
  RefFuncPointerParam          -> "FuncPointerParam"
  RefHandle                    -> "Handle"
  RefLegacy                    -> "Legacy"
  RefMember                    -> "Member"
  RefParam                     -> "Param"
  RefSpirv                     -> "Spirv"
  RefSpirvEnables              -> "SpirvEnables"
  RefStruct                    -> "Struct"
  RefSyncAccess                -> "SyncAccess"
  RefSyncEquivalent            -> "SyncEquivalent"
  RefSyncPipeline              -> "SyncPipeline"
  RefSyncPipelineStage         -> "SyncPipelineStage"
  RefSyncStage                 -> "SyncStage"
  RefSyncSupport               -> "SyncSupport"
  RefVersion                   -> "Version"
  RefVideoCodec                -> "VideoCodec"
  RefVideoFormat               -> "VideoFormat"
  RefVideoProfileMember        -> "VideoProfileMember"
  RefVideoProfiles             -> "VideoProfiles"
  RefVideoRequiredCapabilities -> "VideoRequiredCapabilities"
  RefVulkanObject              -> "VulkanObject"

textRefType :: Text -> Maybe RefType
textRefType = \case
  "Bitmask"                   -> Just RefBitmask
  "Command"                   -> Just RefCommand
  "Constant"                  -> Just RefConstant
  "Enum"                      -> Just RefEnum
  "EnumField"                 -> Just RefEnumField
  "Extension"                 -> Just RefExtension
  "FeatureRequirement"        -> Just RefFeatureRequirement
  "Flag"                      -> Just RefFlag
  "Flags"                     -> Just RefFlags
  "Format"                    -> Just RefFormat
  "FormatComponent"           -> Just RefFormatComponent
  "FormatPlane"               -> Just RefFormatPlane
  "FuncPointer"               -> Just RefFuncPointer
  "FuncPointerParam"          -> Just RefFuncPointerParam
  "Handle"                    -> Just RefHandle
  "Legacy"                    -> Just RefLegacy
  "Member"                    -> Just RefMember
  "Param"                     -> Just RefParam
  "Spirv"                     -> Just RefSpirv
  "SpirvEnables"              -> Just RefSpirvEnables
  "Struct"                    -> Just RefStruct
  "SyncAccess"                -> Just RefSyncAccess
  "SyncEquivalent"            -> Just RefSyncEquivalent
  "SyncPipeline"              -> Just RefSyncPipeline
  "SyncPipelineStage"         -> Just RefSyncPipelineStage
  "SyncStage"                 -> Just RefSyncStage
  "SyncSupport"               -> Just RefSyncSupport
  "Version"                   -> Just RefVersion
  "VideoCodec"                -> Just RefVideoCodec
  "VideoFormat"               -> Just RefVideoFormat
  "VideoProfileMember"        -> Just RefVideoProfileMember
  "VideoProfiles"             -> Just RefVideoProfiles
  "VideoRequiredCapabilities" -> Just RefVideoRequiredCapabilities
  "VulkanObject"              -> Just RefVulkanObject
  _                           -> Nothing

instance ToJSON RefType where
  toJSON = String . refTypeText

instance FromJSON RefType where
  parseJSON = withText "RefType" $ \t ->
    case textRefType t of
      Just r  -> pure r
      Nothing -> fail ("unknown __ref__ tag: " <> T.unpack t)

-- | A cross-reference in the Vulkan JSON dump:
-- @{ "__ref__": <RefType>, "name": <refName> }@.
data Ref = Ref
  { refType :: RefType
  , refName :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Ref where
  parseJSON = withObject "Ref" $ \o ->
    Ref <$> o .: "__ref__" <*> o .: "name"

instance ToJSON Ref where
  toJSON Ref{..} = object
    [ "__ref__" .= refType
    , "name"    .= refName
    ]
