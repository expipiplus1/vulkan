{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd  ( FenceGetFdInfoKHR
                                                            , ImportFenceFdInfoKHR
                                                            ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data FenceGetFdInfoKHR

instance ToCStruct FenceGetFdInfoKHR
instance Show FenceGetFdInfoKHR

instance FromCStruct FenceGetFdInfoKHR


data ImportFenceFdInfoKHR

instance ToCStruct ImportFenceFdInfoKHR
instance Show ImportFenceFdInfoKHR

instance FromCStruct ImportFenceFdInfoKHR

