{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_external_memory_fd"
module Vulkan.Extensions.VK_KHR_external_memory_fd  ( ImportMemoryFdInfoKHR
                                                    , MemoryFdPropertiesKHR
                                                    , MemoryGetFdInfoKHR
                                                    ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ImportMemoryFdInfoKHR

instance ToCStruct ImportMemoryFdInfoKHR
instance Show ImportMemoryFdInfoKHR

instance FromCStruct ImportMemoryFdInfoKHR


data MemoryFdPropertiesKHR

instance ToCStruct MemoryFdPropertiesKHR
instance Show MemoryFdPropertiesKHR

instance FromCStruct MemoryFdPropertiesKHR


data MemoryGetFdInfoKHR

instance ToCStruct MemoryGetFdInfoKHR
instance Show MemoryGetFdInfoKHR

instance FromCStruct MemoryGetFdInfoKHR

