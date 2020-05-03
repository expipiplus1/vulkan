{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_texture_gather_bias_lod  (TextureLODGatherFormatPropertiesAMD) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data TextureLODGatherFormatPropertiesAMD

instance ToCStruct TextureLODGatherFormatPropertiesAMD
instance Show TextureLODGatherFormatPropertiesAMD

instance FromCStruct TextureLODGatherFormatPropertiesAMD

