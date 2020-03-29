{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32  ( ExportFenceWin32HandleInfoKHR
                                                               , FenceGetWin32HandleInfoKHR
                                                               , ImportFenceWin32HandleInfoKHR
                                                               ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ExportFenceWin32HandleInfoKHR

instance ToCStruct ExportFenceWin32HandleInfoKHR
instance Show ExportFenceWin32HandleInfoKHR

instance FromCStruct ExportFenceWin32HandleInfoKHR


data FenceGetWin32HandleInfoKHR

instance ToCStruct FenceGetWin32HandleInfoKHR
instance Show FenceGetWin32HandleInfoKHR

instance FromCStruct FenceGetWin32HandleInfoKHR


data ImportFenceWin32HandleInfoKHR

instance ToCStruct ImportFenceWin32HandleInfoKHR
instance Show ImportFenceWin32HandleInfoKHR

instance FromCStruct ImportFenceWin32HandleInfoKHR

