{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_texture_gather_bias_lod
  ( VkTextureLODGatherFormatPropertiesAMD(..)
  , pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  , pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- No documentation found for TopLevel "VkTextureLODGatherFormatPropertiesAMD"
data VkTextureLODGatherFormatPropertiesAMD = VkTextureLODGatherFormatPropertiesAMD
  { -- No documentation found for Nested "VkTextureLODGatherFormatPropertiesAMD" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkTextureLODGatherFormatPropertiesAMD" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkTextureLODGatherFormatPropertiesAMD" "supportsTextureGatherLODBiasAMD"
  vkSupportsTextureGatherLODBiasAMD :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkTextureLODGatherFormatPropertiesAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkTextureLODGatherFormatPropertiesAMD <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkTextureLODGatherFormatPropertiesAMD))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkTextureLODGatherFormatPropertiesAMD))
                *> poke (ptr `plusPtr` 16) (vkSupportsTextureGatherLODBiasAMD (poked :: VkTextureLODGatherFormatPropertiesAMD))

instance Zero VkTextureLODGatherFormatPropertiesAMD where
  zero = VkTextureLODGatherFormatPropertiesAMD VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
                                               zero
                                               zero

-- No documentation found for TopLevel "VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME"
pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME = "VK_AMD_texture_gather_bias_lod"

-- No documentation found for TopLevel "VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION"
pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION :: Integral a => a
pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD"
pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD :: VkStructureType
pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD = VkStructureType 1000041000
