{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_external_memory_win32  ( ExportMemoryWin32HandleInfoKHR
                                                       , ImportMemoryWin32HandleInfoKHR
                                                       , MemoryGetWin32HandleInfoKHR
                                                       , MemoryWin32HandlePropertiesKHR
                                                       ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ExportMemoryWin32HandleInfoKHR

instance ToCStruct ExportMemoryWin32HandleInfoKHR
instance Show ExportMemoryWin32HandleInfoKHR

instance FromCStruct ExportMemoryWin32HandleInfoKHR


data ImportMemoryWin32HandleInfoKHR

instance ToCStruct ImportMemoryWin32HandleInfoKHR
instance Show ImportMemoryWin32HandleInfoKHR

instance FromCStruct ImportMemoryWin32HandleInfoKHR


data MemoryGetWin32HandleInfoKHR

instance ToCStruct MemoryGetWin32HandleInfoKHR
instance Show MemoryGetWin32HandleInfoKHR

instance FromCStruct MemoryGetWin32HandleInfoKHR


data MemoryWin32HandlePropertiesKHR

instance ToCStruct MemoryWin32HandlePropertiesKHR
instance Show MemoryWin32HandlePropertiesKHR

instance FromCStruct MemoryWin32HandlePropertiesKHR

