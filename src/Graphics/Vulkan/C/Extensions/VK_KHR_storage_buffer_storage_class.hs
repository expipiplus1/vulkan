{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_storage_buffer_storage_class
  ( pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
  , pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME"
pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME = "VK_KHR_storage_buffer_storage_class"
-- No documentation found for TopLevel "VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION"
pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION :: Integral a => a
pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION = 1
