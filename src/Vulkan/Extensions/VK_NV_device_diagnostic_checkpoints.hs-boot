{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_device_diagnostic_checkpoints"
module Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints  ( CheckpointDataNV
                                                              , QueueFamilyCheckpointPropertiesNV
                                                              ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data CheckpointDataNV

instance ToCStruct CheckpointDataNV
instance Show CheckpointDataNV

instance FromCStruct CheckpointDataNV


data QueueFamilyCheckpointPropertiesNV

instance ToCStruct QueueFamilyCheckpointPropertiesNV
instance Show QueueFamilyCheckpointPropertiesNV

instance FromCStruct QueueFamilyCheckpointPropertiesNV

