{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod
  ( withCStructTextureLODGatherFormatPropertiesAMD
  , fromCStructTextureLODGatherFormatPropertiesAMD
  , TextureLODGatherFormatPropertiesAMD(..)
  , pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  , pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
  , pattern STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_texture_gather_bias_lod
  ( VkTextureLODGatherFormatPropertiesAMD(..)
  , pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  , pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  )



-- | VkTextureLODGatherFormatPropertiesAMD - Structure informing whether or
-- not texture gather bias\/LOD functionality is supported for a given
-- image format and a given physical device.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data TextureLODGatherFormatPropertiesAMD = TextureLODGatherFormatPropertiesAMD
  { -- Univalued member elided
  -- No documentation found for Nested "TextureLODGatherFormatPropertiesAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "TextureLODGatherFormatPropertiesAMD" "supportsTextureGatherLODBiasAMD"
  supportsTextureGatherLODBiasAMD :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkTextureLODGatherFormatPropertiesAMD' and
-- marshal a 'TextureLODGatherFormatPropertiesAMD' into it. The 'VkTextureLODGatherFormatPropertiesAMD' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructTextureLODGatherFormatPropertiesAMD :: TextureLODGatherFormatPropertiesAMD -> (VkTextureLODGatherFormatPropertiesAMD -> IO a) -> IO a
withCStructTextureLODGatherFormatPropertiesAMD marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: TextureLODGatherFormatPropertiesAMD)) (\pPNext -> cont (VkTextureLODGatherFormatPropertiesAMD VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD pPNext (boolToBool32 (supportsTextureGatherLODBiasAMD (marshalled :: TextureLODGatherFormatPropertiesAMD)))))

-- | A function to read a 'VkTextureLODGatherFormatPropertiesAMD' and all additional
-- structures in the pointer chain into a 'TextureLODGatherFormatPropertiesAMD'.
fromCStructTextureLODGatherFormatPropertiesAMD :: VkTextureLODGatherFormatPropertiesAMD -> IO TextureLODGatherFormatPropertiesAMD
fromCStructTextureLODGatherFormatPropertiesAMD c = TextureLODGatherFormatPropertiesAMD <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkTextureLODGatherFormatPropertiesAMD)))
                                                                                       <*> pure (bool32ToBool (vkSupportsTextureGatherLODBiasAMD (c :: VkTextureLODGatherFormatPropertiesAMD)))

instance Zero TextureLODGatherFormatPropertiesAMD where
  zero = TextureLODGatherFormatPropertiesAMD Nothing
                                             False


-- No documentation found for TopLevel "VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME"
pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME = VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION"
pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION :: Integral a => a
pattern AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
