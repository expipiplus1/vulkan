{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_storage_buffer_storage_class  ( KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION
                                                                       , pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION
                                                                       , KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                                                                       , pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                                                                       ) where

import Data.String (IsString)

type KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION"
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION = 1


type KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME = "VK_KHR_storage_buffer_storage_class"

-- No documentation found for TopLevel "VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME"
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME = "VK_KHR_storage_buffer_storage_class"

