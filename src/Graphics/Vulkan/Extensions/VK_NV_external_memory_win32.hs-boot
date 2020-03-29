{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_external_memory_win32  ( ExportMemoryWin32HandleInfoNV
                                                               , ImportMemoryWin32HandleInfoNV
                                                               ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ExportMemoryWin32HandleInfoNV

instance ToCStruct ExportMemoryWin32HandleInfoNV
instance Show ExportMemoryWin32HandleInfoNV

instance FromCStruct ExportMemoryWin32HandleInfoNV


data ImportMemoryWin32HandleInfoNV

instance ToCStruct ImportMemoryWin32HandleInfoNV
instance Show ImportMemoryWin32HandleInfoNV

instance FromCStruct ImportMemoryWin32HandleInfoNV

