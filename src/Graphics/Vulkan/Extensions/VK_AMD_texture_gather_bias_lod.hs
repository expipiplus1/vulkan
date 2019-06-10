{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  TextureLODGatherFormatPropertiesAMD(..)
  , 
#endif
  pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  , pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
  , pattern STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_AMD_texture_gather_bias_lod
  ( pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  , pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkTextureLODGatherFormatPropertiesAMD"
data TextureLODGatherFormatPropertiesAMD = TextureLODGatherFormatPropertiesAMD
  { -- No documentation found for Nested "TextureLODGatherFormatPropertiesAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "TextureLODGatherFormatPropertiesAMD" "supportsTextureGatherLODBiasAMD"
  supportsTextureGatherLODBiasAMD :: Bool
  }
  deriving (Show, Eq)

instance Zero TextureLODGatherFormatPropertiesAMD where
  zero = TextureLODGatherFormatPropertiesAMD Nothing
                                             False

#endif

-- No documentation found for TopLevel "VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME"
pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME = VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION"
pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION :: Integral a => a
pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
