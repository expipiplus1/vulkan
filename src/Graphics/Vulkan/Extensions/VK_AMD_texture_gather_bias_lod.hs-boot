{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod  (TextureLODGatherFormatPropertiesAMD) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data TextureLODGatherFormatPropertiesAMD

instance ToCStruct TextureLODGatherFormatPropertiesAMD
instance Show TextureLODGatherFormatPropertiesAMD

instance FromCStruct TextureLODGatherFormatPropertiesAMD

