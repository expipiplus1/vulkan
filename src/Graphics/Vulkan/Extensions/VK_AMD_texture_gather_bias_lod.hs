{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod
  ( withCStructTextureLODGatherFormatPropertiesAMD
  , fromCStructTextureLODGatherFormatPropertiesAMD
  , TextureLODGatherFormatPropertiesAMD(..)
  , pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
  , pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_texture_gather_bias_lod
  ( VkTextureLODGatherFormatPropertiesAMD(..)
  , pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_texture_gather_bias_lod
  ( pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  , pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
  )


-- No documentation found for TopLevel "TextureLODGatherFormatPropertiesAMD"
data TextureLODGatherFormatPropertiesAMD = TextureLODGatherFormatPropertiesAMD
  { -- Univalued Member elided
  -- No documentation found for Nested "TextureLODGatherFormatPropertiesAMD" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "TextureLODGatherFormatPropertiesAMD" "supportsTextureGatherLODBiasAMD"
  vkSupportsTextureGatherLODBiasAMD :: Bool
  }
  deriving (Show, Eq)
withCStructTextureLODGatherFormatPropertiesAMD :: TextureLODGatherFormatPropertiesAMD -> (VkTextureLODGatherFormatPropertiesAMD -> IO a) -> IO a
withCStructTextureLODGatherFormatPropertiesAMD from cont = maybeWith withSomeVkStruct (vkPNext (from :: TextureLODGatherFormatPropertiesAMD)) (\pPNext -> cont (VkTextureLODGatherFormatPropertiesAMD VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD pPNext (boolToBool32 (vkSupportsTextureGatherLODBiasAMD (from :: TextureLODGatherFormatPropertiesAMD)))))
fromCStructTextureLODGatherFormatPropertiesAMD :: VkTextureLODGatherFormatPropertiesAMD -> IO TextureLODGatherFormatPropertiesAMD
fromCStructTextureLODGatherFormatPropertiesAMD c = TextureLODGatherFormatPropertiesAMD <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkTextureLODGatherFormatPropertiesAMD)))
                                                                                       <*> pure (bool32ToBool (vkSupportsTextureGatherLODBiasAMD (c :: VkTextureLODGatherFormatPropertiesAMD)))
