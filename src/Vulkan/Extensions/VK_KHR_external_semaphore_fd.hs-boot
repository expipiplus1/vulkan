{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_external_semaphore_fd  ( ImportSemaphoreFdInfoKHR
                                                       , SemaphoreGetFdInfoKHR
                                                       ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ImportSemaphoreFdInfoKHR

instance ToCStruct ImportSemaphoreFdInfoKHR
instance Show ImportSemaphoreFdInfoKHR

instance FromCStruct ImportSemaphoreFdInfoKHR


data SemaphoreGetFdInfoKHR

instance ToCStruct SemaphoreGetFdInfoKHR
instance Show SemaphoreGetFdInfoKHR

instance FromCStruct SemaphoreGetFdInfoKHR

