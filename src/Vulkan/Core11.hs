{-# language CPP #-}
-- No documentation found for Chapter "Core11"
module Vulkan.Core11  ( pattern API_VERSION_1_1
                      , module Vulkan.Core11.DeviceInitialization
                      , module Vulkan.Core11.Enums
                      , module Vulkan.Core11.Handles
                      , module Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory
                      , module Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup
                      , module Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage
                      , module Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2
                      , module Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation
                      , module Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template
                      , module Vulkan.Core11.Promoted_From_VK_KHR_device_group
                      , module Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2
                      , module Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation
                      , module Vulkan.Core11.Promoted_From_VK_KHR_external_fence
                      , module Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities
                      , module Vulkan.Core11.Promoted_From_VK_KHR_external_memory
                      , module Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities
                      , module Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore
                      , module Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities
                      , module Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2
                      , module Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
                      , module Vulkan.Core11.Promoted_From_VK_KHR_maintenance1
                      , module Vulkan.Core11.Promoted_From_VK_KHR_maintenance2
                      , module Vulkan.Core11.Promoted_From_VK_KHR_maintenance3
                      , module Vulkan.Core11.Promoted_From_VK_KHR_multiview
                      , module Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion
                      , module Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters
                      , module Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers
                      ) where
import Vulkan.Core11.DeviceInitialization
import Vulkan.Core11.Enums
import Vulkan.Core11.Handles
import Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory
import Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup
import Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage
import Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2
import Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template
import Vulkan.Core11.Promoted_From_VK_KHR_device_group
import Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2
import Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation
import Vulkan.Core11.Promoted_From_VK_KHR_external_fence
import Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities
import Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore
import Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance1
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance2
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance3
import Vulkan.Core11.Promoted_From_VK_KHR_multiview
import Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion
import Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters
import Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers
import Data.Word (Word32)
import Vulkan.Version (pattern MAKE_API_VERSION)
pattern API_VERSION_1_1 :: Word32
pattern API_VERSION_1_1 = MAKE_API_VERSION 1 1 0

