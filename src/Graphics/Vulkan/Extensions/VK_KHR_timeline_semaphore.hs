{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_timeline_semaphore  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR
                                                             , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR
                                                             , pattern STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR
                                                             , pattern STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR
                                                             , pattern STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR
                                                             , pattern STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR
                                                             , pattern SEMAPHORE_TYPE_BINARY_KHR
                                                             , pattern SEMAPHORE_TYPE_TIMELINE_KHR
                                                             , pattern SEMAPHORE_WAIT_ANY_BIT_KHR
                                                             , getSemaphoreCounterValueKHR
                                                             , waitSemaphoresKHR
                                                             , signalSemaphoreKHR
                                                             , SemaphoreWaitFlagsKHR
                                                             , SemaphoreTypeKHR
                                                             , SemaphoreWaitFlagBitsKHR
                                                             , PhysicalDeviceTimelineSemaphoreFeaturesKHR
                                                             , PhysicalDeviceTimelineSemaphorePropertiesKHR
                                                             , SemaphoreTypeCreateInfoKHR
                                                             , TimelineSemaphoreSubmitInfoKHR
                                                             , SemaphoreWaitInfoKHR
                                                             , SemaphoreSignalInfoKHR
                                                             , KHR_TIMELINE_SEMAPHORE_SPEC_VERSION
                                                             , pattern KHR_TIMELINE_SEMAPHORE_SPEC_VERSION
                                                             , KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME
                                                             , pattern KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME
                                                             ) where

import Data.String (IsString)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (getSemaphoreCounterValue)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (signalSemaphore)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (waitSemaphores)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreFeatures)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreProperties)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreSignalInfo)
import Graphics.Vulkan.Core12.Enums.SemaphoreType (SemaphoreType)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreTypeCreateInfo)
import Graphics.Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlagBits)
import Graphics.Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlags)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreWaitInfo)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo)
import Graphics.Vulkan.Core12.Enums.SemaphoreType (SemaphoreType(SEMAPHORE_TYPE_BINARY))
import Graphics.Vulkan.Core12.Enums.SemaphoreType (SemaphoreType(SEMAPHORE_TYPE_TIMELINE))
import Graphics.Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlags)
import Graphics.Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlagBits(SEMAPHORE_WAIT_ANY_BIT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR = STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR"
pattern STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR = STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR"
pattern STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR = STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR"
pattern STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR = STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO


-- No documentation found for TopLevel "VK_SEMAPHORE_TYPE_BINARY_KHR"
pattern SEMAPHORE_TYPE_BINARY_KHR = SEMAPHORE_TYPE_BINARY


-- No documentation found for TopLevel "VK_SEMAPHORE_TYPE_TIMELINE_KHR"
pattern SEMAPHORE_TYPE_TIMELINE_KHR = SEMAPHORE_TYPE_TIMELINE


-- No documentation found for TopLevel "VK_SEMAPHORE_WAIT_ANY_BIT_KHR"
pattern SEMAPHORE_WAIT_ANY_BIT_KHR = SEMAPHORE_WAIT_ANY_BIT


-- No documentation found for TopLevel "vkGetSemaphoreCounterValueKHR"
getSemaphoreCounterValueKHR = getSemaphoreCounterValue


-- No documentation found for TopLevel "vkWaitSemaphoresKHR"
waitSemaphoresKHR = waitSemaphores


-- No documentation found for TopLevel "vkSignalSemaphoreKHR"
signalSemaphoreKHR = signalSemaphore


-- No documentation found for TopLevel "VkSemaphoreWaitFlagsKHR"
type SemaphoreWaitFlagsKHR = SemaphoreWaitFlags


-- No documentation found for TopLevel "VkSemaphoreTypeKHR"
type SemaphoreTypeKHR = SemaphoreType


-- No documentation found for TopLevel "VkSemaphoreWaitFlagBitsKHR"
type SemaphoreWaitFlagBitsKHR = SemaphoreWaitFlagBits


-- No documentation found for TopLevel "VkPhysicalDeviceTimelineSemaphoreFeaturesKHR"
type PhysicalDeviceTimelineSemaphoreFeaturesKHR = PhysicalDeviceTimelineSemaphoreFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceTimelineSemaphorePropertiesKHR"
type PhysicalDeviceTimelineSemaphorePropertiesKHR = PhysicalDeviceTimelineSemaphoreProperties


-- No documentation found for TopLevel "VkSemaphoreTypeCreateInfoKHR"
type SemaphoreTypeCreateInfoKHR = SemaphoreTypeCreateInfo


-- No documentation found for TopLevel "VkTimelineSemaphoreSubmitInfoKHR"
type TimelineSemaphoreSubmitInfoKHR = TimelineSemaphoreSubmitInfo


-- No documentation found for TopLevel "VkSemaphoreWaitInfoKHR"
type SemaphoreWaitInfoKHR = SemaphoreWaitInfo


-- No documentation found for TopLevel "VkSemaphoreSignalInfoKHR"
type SemaphoreSignalInfoKHR = SemaphoreSignalInfo


type KHR_TIMELINE_SEMAPHORE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_TIMELINE_SEMAPHORE_SPEC_VERSION"
pattern KHR_TIMELINE_SEMAPHORE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_TIMELINE_SEMAPHORE_SPEC_VERSION = 2


type KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME = "VK_KHR_timeline_semaphore"

-- No documentation found for TopLevel "VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME"
pattern KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME = "VK_KHR_timeline_semaphore"

