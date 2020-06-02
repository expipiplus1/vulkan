{-# language CPP #-}
module Vulkan.Extensions.VK_NV_external_memory_win32  ( ExportMemoryWin32HandleInfoNV
                                                      , ImportMemoryWin32HandleInfoNV
                                                      , HANDLE
                                                      ) where

import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ExportMemoryWin32HandleInfoNV

instance ToCStruct ExportMemoryWin32HandleInfoNV
instance Show ExportMemoryWin32HandleInfoNV

instance FromCStruct ExportMemoryWin32HandleInfoNV


data ImportMemoryWin32HandleInfoNV

instance ToCStruct ImportMemoryWin32HandleInfoNV
instance Show ImportMemoryWin32HandleInfoNV

instance FromCStruct ImportMemoryWin32HandleInfoNV


type HANDLE = Ptr ()

