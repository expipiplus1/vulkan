{-# language CPP #-}
module Graphics.Vulkan.Core11  ( pattern API_VERSION_1_1
                               , module Graphics.Vulkan.Core11.DeviceInitialization
                               , module Graphics.Vulkan.Core11.Enums
                               , module Graphics.Vulkan.Core11.Handles
                               , module Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory
                               , module Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_fence
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance1
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters
                               , module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers
                               ) where
import Graphics.Vulkan.Core11.DeviceInitialization
import Graphics.Vulkan.Core11.Enums
import Graphics.Vulkan.Core11.Handles
import Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory
import Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_fence
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance1
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers
import Data.Word (Word32)
import Graphics.Vulkan.Version (pattern MAKE_VERSION)
pattern API_VERSION_1_1 :: Word32
pattern API_VERSION_1_1 = MAKE_VERSION 1 1 0

