{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_storage_buffer_storage_class
  ( pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
  , pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_storage_buffer_storage_class
  ( pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
  , pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME"
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME = VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION"
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION :: Integral a => a
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION = VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION
