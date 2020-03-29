{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd  ( ImportMemoryFdInfoKHR
                                                             , MemoryFdPropertiesKHR
                                                             , MemoryGetFdInfoKHR
                                                             ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
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

