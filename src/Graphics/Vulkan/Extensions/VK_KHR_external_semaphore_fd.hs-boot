{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd  ( ImportSemaphoreFdInfoKHR
                                                                , SemaphoreGetFdInfoKHR
                                                                ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ImportSemaphoreFdInfoKHR

instance ToCStruct ImportSemaphoreFdInfoKHR
instance Show ImportSemaphoreFdInfoKHR

instance FromCStruct ImportSemaphoreFdInfoKHR


data SemaphoreGetFdInfoKHR

instance ToCStruct SemaphoreGetFdInfoKHR
instance Show SemaphoreGetFdInfoKHR

instance FromCStruct SemaphoreGetFdInfoKHR

