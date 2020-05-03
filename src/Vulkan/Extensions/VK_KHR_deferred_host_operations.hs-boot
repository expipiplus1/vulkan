{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_deferred_host_operations  (DeferredOperationInfoKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DeferredOperationInfoKHR

instance ToCStruct DeferredOperationInfoKHR
instance Show DeferredOperationInfoKHR

instance FromCStruct DeferredOperationInfoKHR

