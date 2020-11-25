{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_external_fence_fd"
module Vulkan.Extensions.VK_KHR_external_fence_fd  ( FenceGetFdInfoKHR
                                                   , ImportFenceFdInfoKHR
                                                   ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data FenceGetFdInfoKHR

instance ToCStruct FenceGetFdInfoKHR
instance Show FenceGetFdInfoKHR

instance FromCStruct FenceGetFdInfoKHR


data ImportFenceFdInfoKHR

instance ToCStruct ImportFenceFdInfoKHR
instance Show ImportFenceFdInfoKHR

instance FromCStruct ImportFenceFdInfoKHR

