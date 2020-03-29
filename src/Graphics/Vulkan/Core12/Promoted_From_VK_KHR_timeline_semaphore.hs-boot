{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore  ( PhysicalDeviceTimelineSemaphoreFeatures
                                                                       , PhysicalDeviceTimelineSemaphoreProperties
                                                                       , SemaphoreSignalInfo
                                                                       , SemaphoreTypeCreateInfo
                                                                       , SemaphoreWaitInfo
                                                                       , TimelineSemaphoreSubmitInfo
                                                                       ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceTimelineSemaphoreFeatures

instance ToCStruct PhysicalDeviceTimelineSemaphoreFeatures
instance Show PhysicalDeviceTimelineSemaphoreFeatures

instance FromCStruct PhysicalDeviceTimelineSemaphoreFeatures


data PhysicalDeviceTimelineSemaphoreProperties

instance ToCStruct PhysicalDeviceTimelineSemaphoreProperties
instance Show PhysicalDeviceTimelineSemaphoreProperties

instance FromCStruct PhysicalDeviceTimelineSemaphoreProperties


data SemaphoreSignalInfo

instance ToCStruct SemaphoreSignalInfo
instance Show SemaphoreSignalInfo

instance FromCStruct SemaphoreSignalInfo


data SemaphoreTypeCreateInfo

instance ToCStruct SemaphoreTypeCreateInfo
instance Show SemaphoreTypeCreateInfo

instance FromCStruct SemaphoreTypeCreateInfo


data SemaphoreWaitInfo

instance ToCStruct SemaphoreWaitInfo
instance Show SemaphoreWaitInfo

instance FromCStruct SemaphoreWaitInfo


data TimelineSemaphoreSubmitInfo

instance ToCStruct TimelineSemaphoreSubmitInfo
instance Show TimelineSemaphoreSubmitInfo

instance FromCStruct TimelineSemaphoreSubmitInfo

