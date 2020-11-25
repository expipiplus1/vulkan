{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_external_semaphore_win32"
module Vulkan.Extensions.VK_KHR_external_semaphore_win32  ( D3D12FenceSubmitInfoKHR
                                                          , ExportSemaphoreWin32HandleInfoKHR
                                                          , ImportSemaphoreWin32HandleInfoKHR
                                                          , SemaphoreGetWin32HandleInfoKHR
                                                          ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data D3D12FenceSubmitInfoKHR

instance ToCStruct D3D12FenceSubmitInfoKHR
instance Show D3D12FenceSubmitInfoKHR

instance FromCStruct D3D12FenceSubmitInfoKHR


data ExportSemaphoreWin32HandleInfoKHR

instance ToCStruct ExportSemaphoreWin32HandleInfoKHR
instance Show ExportSemaphoreWin32HandleInfoKHR

instance FromCStruct ExportSemaphoreWin32HandleInfoKHR


data ImportSemaphoreWin32HandleInfoKHR

instance ToCStruct ImportSemaphoreWin32HandleInfoKHR
instance Show ImportSemaphoreWin32HandleInfoKHR

instance FromCStruct ImportSemaphoreWin32HandleInfoKHR


data SemaphoreGetWin32HandleInfoKHR

instance ToCStruct SemaphoreGetWin32HandleInfoKHR
instance Show SemaphoreGetWin32HandleInfoKHR

instance FromCStruct SemaphoreGetWin32HandleInfoKHR

