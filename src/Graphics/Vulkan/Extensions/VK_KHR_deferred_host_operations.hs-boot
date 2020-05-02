{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_deferred_host_operations  (DeferredOperationInfoKHR) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DeferredOperationInfoKHR

instance ToCStruct DeferredOperationInfoKHR
instance Show DeferredOperationInfoKHR

instance FromCStruct DeferredOperationInfoKHR

