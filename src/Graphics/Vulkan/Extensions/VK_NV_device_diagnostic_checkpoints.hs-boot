{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints  ( CheckpointDataNV
                                                                       , QueueFamilyCheckpointPropertiesNV
                                                                       ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data CheckpointDataNV

instance ToCStruct CheckpointDataNV
instance Show CheckpointDataNV

instance FromCStruct CheckpointDataNV


data QueueFamilyCheckpointPropertiesNV

instance ToCStruct QueueFamilyCheckpointPropertiesNV
instance Show QueueFamilyCheckpointPropertiesNV

instance FromCStruct QueueFamilyCheckpointPropertiesNV

