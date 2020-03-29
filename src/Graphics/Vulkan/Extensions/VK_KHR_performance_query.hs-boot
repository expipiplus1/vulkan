{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_performance_query  ( AcquireProfilingLockInfoKHR
                                                            , PerformanceCounterDescriptionKHR
                                                            , PerformanceCounterKHR
                                                            , PerformanceQuerySubmitInfoKHR
                                                            , PhysicalDevicePerformanceQueryFeaturesKHR
                                                            , PhysicalDevicePerformanceQueryPropertiesKHR
                                                            , QueryPoolPerformanceCreateInfoKHR
                                                            ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data AcquireProfilingLockInfoKHR

instance ToCStruct AcquireProfilingLockInfoKHR
instance Show AcquireProfilingLockInfoKHR

instance FromCStruct AcquireProfilingLockInfoKHR


data PerformanceCounterDescriptionKHR

instance ToCStruct PerformanceCounterDescriptionKHR
instance Show PerformanceCounterDescriptionKHR

instance FromCStruct PerformanceCounterDescriptionKHR


data PerformanceCounterKHR

instance ToCStruct PerformanceCounterKHR
instance Show PerformanceCounterKHR

instance FromCStruct PerformanceCounterKHR


data PerformanceQuerySubmitInfoKHR

instance ToCStruct PerformanceQuerySubmitInfoKHR
instance Show PerformanceQuerySubmitInfoKHR

instance FromCStruct PerformanceQuerySubmitInfoKHR


data PhysicalDevicePerformanceQueryFeaturesKHR

instance ToCStruct PhysicalDevicePerformanceQueryFeaturesKHR
instance Show PhysicalDevicePerformanceQueryFeaturesKHR

instance FromCStruct PhysicalDevicePerformanceQueryFeaturesKHR


data PhysicalDevicePerformanceQueryPropertiesKHR

instance ToCStruct PhysicalDevicePerformanceQueryPropertiesKHR
instance Show PhysicalDevicePerformanceQueryPropertiesKHR

instance FromCStruct PhysicalDevicePerformanceQueryPropertiesKHR


data QueryPoolPerformanceCreateInfoKHR

instance ToCStruct QueryPoolPerformanceCreateInfoKHR
instance Show QueryPoolPerformanceCreateInfoKHR

instance FromCStruct QueryPoolPerformanceCreateInfoKHR

