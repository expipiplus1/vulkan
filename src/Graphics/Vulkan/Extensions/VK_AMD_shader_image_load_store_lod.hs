{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_image_load_store_lod
  ( pattern AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME
  , pattern AMD_SHADER_IMAGE_LOAD_STORE_LOD_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_shader_image_load_store_lod
  ( pattern VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME
  , pattern VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME"
pattern AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME = VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_SPEC_VERSION"
pattern AMD_SHADER_IMAGE_LOAD_STORE_LOD_SPEC_VERSION :: Integral a => a
pattern AMD_SHADER_IMAGE_LOAD_STORE_LOD_SPEC_VERSION = VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_SPEC_VERSION
