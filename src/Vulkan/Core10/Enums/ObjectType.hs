{-# language CPP #-}
module Vulkan.Core10.Enums.ObjectType  (ObjectType( OBJECT_TYPE_UNKNOWN
                                                  , OBJECT_TYPE_INSTANCE
                                                  , OBJECT_TYPE_PHYSICAL_DEVICE
                                                  , OBJECT_TYPE_DEVICE
                                                  , OBJECT_TYPE_QUEUE
                                                  , OBJECT_TYPE_SEMAPHORE
                                                  , OBJECT_TYPE_COMMAND_BUFFER
                                                  , OBJECT_TYPE_FENCE
                                                  , OBJECT_TYPE_DEVICE_MEMORY
                                                  , OBJECT_TYPE_BUFFER
                                                  , OBJECT_TYPE_IMAGE
                                                  , OBJECT_TYPE_EVENT
                                                  , OBJECT_TYPE_QUERY_POOL
                                                  , OBJECT_TYPE_BUFFER_VIEW
                                                  , OBJECT_TYPE_IMAGE_VIEW
                                                  , OBJECT_TYPE_SHADER_MODULE
                                                  , OBJECT_TYPE_PIPELINE_CACHE
                                                  , OBJECT_TYPE_PIPELINE_LAYOUT
                                                  , OBJECT_TYPE_RENDER_PASS
                                                  , OBJECT_TYPE_PIPELINE
                                                  , OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
                                                  , OBJECT_TYPE_SAMPLER
                                                  , OBJECT_TYPE_DESCRIPTOR_POOL
                                                  , OBJECT_TYPE_DESCRIPTOR_SET
                                                  , OBJECT_TYPE_FRAMEBUFFER
                                                  , OBJECT_TYPE_COMMAND_POOL
                                                  , OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT
                                                  , OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV
                                                  , OBJECT_TYPE_DEFERRED_OPERATION_KHR
                                                  , OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL
                                                  , OBJECT_TYPE_ACCELERATION_STRUCTURE_NV
                                                  , OBJECT_TYPE_VALIDATION_CACHE_EXT
                                                  , OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR
                                                  , OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
                                                  , OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT
                                                  , OBJECT_TYPE_DISPLAY_MODE_KHR
                                                  , OBJECT_TYPE_DISPLAY_KHR
                                                  , OBJECT_TYPE_SWAPCHAIN_KHR
                                                  , OBJECT_TYPE_SURFACE_KHR
                                                  , OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
                                                  , OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION
                                                  , ..
                                                  )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkObjectType - Specify an enumeration to track object handle types
--
-- = Description
--
-- \'
--
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'ObjectType'                                  | Vulkan Handle Type                                        |
-- +===============================================+===========================================================+
-- | 'OBJECT_TYPE_UNKNOWN'                         | Unknown\/Undefined Handle                                 |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_INSTANCE'                        | 'Vulkan.Core10.Handles.Instance'                          |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_PHYSICAL_DEVICE'                 | 'Vulkan.Core10.Handles.PhysicalDevice'                    |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DEVICE'                          | 'Vulkan.Core10.Handles.Device'                            |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_QUEUE'                           | 'Vulkan.Core10.Handles.Queue'                             |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_SEMAPHORE'                       | 'Vulkan.Core10.Handles.Semaphore'                         |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_COMMAND_BUFFER'                  | 'Vulkan.Core10.Handles.CommandBuffer'                     |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_FENCE'                           | 'Vulkan.Core10.Handles.Fence'                             |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DEVICE_MEMORY'                   | 'Vulkan.Core10.Handles.DeviceMemory'                      |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_BUFFER'                          | 'Vulkan.Core10.Handles.Buffer'                            |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_IMAGE'                           | 'Vulkan.Core10.Handles.Image'                             |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_EVENT'                           | 'Vulkan.Core10.Handles.Event'                             |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_QUERY_POOL'                      | 'Vulkan.Core10.Handles.QueryPool'                         |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_BUFFER_VIEW'                     | 'Vulkan.Core10.Handles.BufferView'                        |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_IMAGE_VIEW'                      | 'Vulkan.Core10.Handles.ImageView'                         |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_SHADER_MODULE'                   | 'Vulkan.Core10.Handles.ShaderModule'                      |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_PIPELINE_CACHE'                  | 'Vulkan.Core10.Handles.PipelineCache'                     |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_PIPELINE_LAYOUT'                 | 'Vulkan.Core10.Handles.PipelineLayout'                    |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_RENDER_PASS'                     | 'Vulkan.Core10.Handles.RenderPass'                        |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_PIPELINE'                        | 'Vulkan.Core10.Handles.Pipeline'                          |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT'           | 'Vulkan.Core10.Handles.DescriptorSetLayout'               |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_SAMPLER'                         | 'Vulkan.Core10.Handles.Sampler'                           |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DESCRIPTOR_POOL'                 | 'Vulkan.Core10.Handles.DescriptorPool'                    |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DESCRIPTOR_SET'                  | 'Vulkan.Core10.Handles.DescriptorSet'                     |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_FRAMEBUFFER'                     | 'Vulkan.Core10.Handles.Framebuffer'                       |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_COMMAND_POOL'                    | 'Vulkan.Core10.Handles.CommandPool'                       |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION'        | 'Vulkan.Core11.Handles.SamplerYcbcrConversion'            |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE'      | 'Vulkan.Core11.Handles.DescriptorUpdateTemplate'          |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_SURFACE_KHR'                     | 'Vulkan.Extensions.Handles.SurfaceKHR'                    |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_SWAPCHAIN_KHR'                   | 'Vulkan.Extensions.Handles.SwapchainKHR'                  |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DISPLAY_KHR'                     | 'Vulkan.Extensions.Handles.DisplayKHR'                    |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DISPLAY_MODE_KHR'                | 'Vulkan.Extensions.Handles.DisplayModeKHR'                |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT'       | 'Vulkan.Extensions.Handles.DebugReportCallbackEXT'        |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV'     | 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV'      |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT'       | 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT'        |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_VALIDATION_CACHE_EXT'            | 'Vulkan.Extensions.Handles.ValidationCacheEXT'            |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_ACCELERATION_STRUCTURE_NV'       | 'Vulkan.Extensions.Handles.AccelerationStructureNV'       |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR'      | 'Vulkan.Extensions.Handles.AccelerationStructureKHR'      |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL' | 'Vulkan.Extensions.Handles.PerformanceConfigurationINTEL' |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_DEFERRED_OPERATION_KHR'          | 'Vulkan.Extensions.Handles.DeferredOperationKHR'          |
-- +-----------------------------------------------+-----------------------------------------------------------+
-- | 'OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT'           | 'Vulkan.Extensions.Handles.PrivateDataSlotEXT'            |
-- +-----------------------------------------------+-----------------------------------------------------------+
--
-- 'ObjectType' and Vulkan Handle Relationship
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectTagInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_device_memory_report.DeviceMemoryReportCallbackDataEXT',
-- 'Vulkan.Extensions.VK_EXT_private_data.getPrivateDataEXT',
-- 'Vulkan.Extensions.VK_EXT_private_data.setPrivateDataEXT'
newtype ObjectType = ObjectType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_UNKNOWN"
pattern OBJECT_TYPE_UNKNOWN = ObjectType 0
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_INSTANCE"
pattern OBJECT_TYPE_INSTANCE = ObjectType 1
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PHYSICAL_DEVICE"
pattern OBJECT_TYPE_PHYSICAL_DEVICE = ObjectType 2
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEVICE"
pattern OBJECT_TYPE_DEVICE = ObjectType 3
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_QUEUE"
pattern OBJECT_TYPE_QUEUE = ObjectType 4
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SEMAPHORE"
pattern OBJECT_TYPE_SEMAPHORE = ObjectType 5
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_COMMAND_BUFFER"
pattern OBJECT_TYPE_COMMAND_BUFFER = ObjectType 6
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_FENCE"
pattern OBJECT_TYPE_FENCE = ObjectType 7
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEVICE_MEMORY"
pattern OBJECT_TYPE_DEVICE_MEMORY = ObjectType 8
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_BUFFER"
pattern OBJECT_TYPE_BUFFER = ObjectType 9
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_IMAGE"
pattern OBJECT_TYPE_IMAGE = ObjectType 10
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_EVENT"
pattern OBJECT_TYPE_EVENT = ObjectType 11
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_QUERY_POOL"
pattern OBJECT_TYPE_QUERY_POOL = ObjectType 12
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_BUFFER_VIEW"
pattern OBJECT_TYPE_BUFFER_VIEW = ObjectType 13
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_IMAGE_VIEW"
pattern OBJECT_TYPE_IMAGE_VIEW = ObjectType 14
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SHADER_MODULE"
pattern OBJECT_TYPE_SHADER_MODULE = ObjectType 15
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE_CACHE"
pattern OBJECT_TYPE_PIPELINE_CACHE = ObjectType 16
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE_LAYOUT"
pattern OBJECT_TYPE_PIPELINE_LAYOUT = ObjectType 17
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_RENDER_PASS"
pattern OBJECT_TYPE_RENDER_PASS = ObjectType 18
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE"
pattern OBJECT_TYPE_PIPELINE = ObjectType 19
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
pattern OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = ObjectType 20
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SAMPLER"
pattern OBJECT_TYPE_SAMPLER = ObjectType 21
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_POOL"
pattern OBJECT_TYPE_DESCRIPTOR_POOL = ObjectType 22
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_SET"
pattern OBJECT_TYPE_DESCRIPTOR_SET = ObjectType 23
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_FRAMEBUFFER"
pattern OBJECT_TYPE_FRAMEBUFFER = ObjectType 24
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_COMMAND_POOL"
pattern OBJECT_TYPE_COMMAND_POOL = ObjectType 25
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT"
pattern OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT = ObjectType 1000295000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV"
pattern OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV = ObjectType 1000277000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEFERRED_OPERATION_KHR"
pattern OBJECT_TYPE_DEFERRED_OPERATION_KHR = ObjectType 1000268000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL"
pattern OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL = ObjectType 1000210000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV"
pattern OBJECT_TYPE_ACCELERATION_STRUCTURE_NV = ObjectType 1000165000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_VALIDATION_CACHE_EXT"
pattern OBJECT_TYPE_VALIDATION_CACHE_EXT = ObjectType 1000160000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR"
pattern OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR = ObjectType 1000150000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT"
pattern OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT = ObjectType 1000128000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT"
pattern OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT = ObjectType 1000011000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_MODE_KHR"
pattern OBJECT_TYPE_DISPLAY_MODE_KHR = ObjectType 1000002001
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_KHR"
pattern OBJECT_TYPE_DISPLAY_KHR = ObjectType 1000002000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SWAPCHAIN_KHR"
pattern OBJECT_TYPE_SWAPCHAIN_KHR = ObjectType 1000001000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SURFACE_KHR"
pattern OBJECT_TYPE_SURFACE_KHR = ObjectType 1000000000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE"
pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE = ObjectType 1000085000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION"
pattern OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION = ObjectType 1000156000
{-# complete OBJECT_TYPE_UNKNOWN,
             OBJECT_TYPE_INSTANCE,
             OBJECT_TYPE_PHYSICAL_DEVICE,
             OBJECT_TYPE_DEVICE,
             OBJECT_TYPE_QUEUE,
             OBJECT_TYPE_SEMAPHORE,
             OBJECT_TYPE_COMMAND_BUFFER,
             OBJECT_TYPE_FENCE,
             OBJECT_TYPE_DEVICE_MEMORY,
             OBJECT_TYPE_BUFFER,
             OBJECT_TYPE_IMAGE,
             OBJECT_TYPE_EVENT,
             OBJECT_TYPE_QUERY_POOL,
             OBJECT_TYPE_BUFFER_VIEW,
             OBJECT_TYPE_IMAGE_VIEW,
             OBJECT_TYPE_SHADER_MODULE,
             OBJECT_TYPE_PIPELINE_CACHE,
             OBJECT_TYPE_PIPELINE_LAYOUT,
             OBJECT_TYPE_RENDER_PASS,
             OBJECT_TYPE_PIPELINE,
             OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,
             OBJECT_TYPE_SAMPLER,
             OBJECT_TYPE_DESCRIPTOR_POOL,
             OBJECT_TYPE_DESCRIPTOR_SET,
             OBJECT_TYPE_FRAMEBUFFER,
             OBJECT_TYPE_COMMAND_POOL,
             OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT,
             OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV,
             OBJECT_TYPE_DEFERRED_OPERATION_KHR,
             OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL,
             OBJECT_TYPE_ACCELERATION_STRUCTURE_NV,
             OBJECT_TYPE_VALIDATION_CACHE_EXT,
             OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR,
             OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT,
             OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT,
             OBJECT_TYPE_DISPLAY_MODE_KHR,
             OBJECT_TYPE_DISPLAY_KHR,
             OBJECT_TYPE_SWAPCHAIN_KHR,
             OBJECT_TYPE_SURFACE_KHR,
             OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE,
             OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION :: ObjectType #-}

instance Show ObjectType where
  showsPrec p = \case
    OBJECT_TYPE_UNKNOWN -> showString "OBJECT_TYPE_UNKNOWN"
    OBJECT_TYPE_INSTANCE -> showString "OBJECT_TYPE_INSTANCE"
    OBJECT_TYPE_PHYSICAL_DEVICE -> showString "OBJECT_TYPE_PHYSICAL_DEVICE"
    OBJECT_TYPE_DEVICE -> showString "OBJECT_TYPE_DEVICE"
    OBJECT_TYPE_QUEUE -> showString "OBJECT_TYPE_QUEUE"
    OBJECT_TYPE_SEMAPHORE -> showString "OBJECT_TYPE_SEMAPHORE"
    OBJECT_TYPE_COMMAND_BUFFER -> showString "OBJECT_TYPE_COMMAND_BUFFER"
    OBJECT_TYPE_FENCE -> showString "OBJECT_TYPE_FENCE"
    OBJECT_TYPE_DEVICE_MEMORY -> showString "OBJECT_TYPE_DEVICE_MEMORY"
    OBJECT_TYPE_BUFFER -> showString "OBJECT_TYPE_BUFFER"
    OBJECT_TYPE_IMAGE -> showString "OBJECT_TYPE_IMAGE"
    OBJECT_TYPE_EVENT -> showString "OBJECT_TYPE_EVENT"
    OBJECT_TYPE_QUERY_POOL -> showString "OBJECT_TYPE_QUERY_POOL"
    OBJECT_TYPE_BUFFER_VIEW -> showString "OBJECT_TYPE_BUFFER_VIEW"
    OBJECT_TYPE_IMAGE_VIEW -> showString "OBJECT_TYPE_IMAGE_VIEW"
    OBJECT_TYPE_SHADER_MODULE -> showString "OBJECT_TYPE_SHADER_MODULE"
    OBJECT_TYPE_PIPELINE_CACHE -> showString "OBJECT_TYPE_PIPELINE_CACHE"
    OBJECT_TYPE_PIPELINE_LAYOUT -> showString "OBJECT_TYPE_PIPELINE_LAYOUT"
    OBJECT_TYPE_RENDER_PASS -> showString "OBJECT_TYPE_RENDER_PASS"
    OBJECT_TYPE_PIPELINE -> showString "OBJECT_TYPE_PIPELINE"
    OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT -> showString "OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
    OBJECT_TYPE_SAMPLER -> showString "OBJECT_TYPE_SAMPLER"
    OBJECT_TYPE_DESCRIPTOR_POOL -> showString "OBJECT_TYPE_DESCRIPTOR_POOL"
    OBJECT_TYPE_DESCRIPTOR_SET -> showString "OBJECT_TYPE_DESCRIPTOR_SET"
    OBJECT_TYPE_FRAMEBUFFER -> showString "OBJECT_TYPE_FRAMEBUFFER"
    OBJECT_TYPE_COMMAND_POOL -> showString "OBJECT_TYPE_COMMAND_POOL"
    OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT -> showString "OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT"
    OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV -> showString "OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV"
    OBJECT_TYPE_DEFERRED_OPERATION_KHR -> showString "OBJECT_TYPE_DEFERRED_OPERATION_KHR"
    OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL -> showString "OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL"
    OBJECT_TYPE_ACCELERATION_STRUCTURE_NV -> showString "OBJECT_TYPE_ACCELERATION_STRUCTURE_NV"
    OBJECT_TYPE_VALIDATION_CACHE_EXT -> showString "OBJECT_TYPE_VALIDATION_CACHE_EXT"
    OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR -> showString "OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR"
    OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT -> showString "OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT"
    OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT -> showString "OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT"
    OBJECT_TYPE_DISPLAY_MODE_KHR -> showString "OBJECT_TYPE_DISPLAY_MODE_KHR"
    OBJECT_TYPE_DISPLAY_KHR -> showString "OBJECT_TYPE_DISPLAY_KHR"
    OBJECT_TYPE_SWAPCHAIN_KHR -> showString "OBJECT_TYPE_SWAPCHAIN_KHR"
    OBJECT_TYPE_SURFACE_KHR -> showString "OBJECT_TYPE_SURFACE_KHR"
    OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE -> showString "OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE"
    OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION -> showString "OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION"
    ObjectType x -> showParen (p >= 11) (showString "ObjectType " . showsPrec 11 x)

instance Read ObjectType where
  readPrec = parens (choose [("OBJECT_TYPE_UNKNOWN", pure OBJECT_TYPE_UNKNOWN)
                            , ("OBJECT_TYPE_INSTANCE", pure OBJECT_TYPE_INSTANCE)
                            , ("OBJECT_TYPE_PHYSICAL_DEVICE", pure OBJECT_TYPE_PHYSICAL_DEVICE)
                            , ("OBJECT_TYPE_DEVICE", pure OBJECT_TYPE_DEVICE)
                            , ("OBJECT_TYPE_QUEUE", pure OBJECT_TYPE_QUEUE)
                            , ("OBJECT_TYPE_SEMAPHORE", pure OBJECT_TYPE_SEMAPHORE)
                            , ("OBJECT_TYPE_COMMAND_BUFFER", pure OBJECT_TYPE_COMMAND_BUFFER)
                            , ("OBJECT_TYPE_FENCE", pure OBJECT_TYPE_FENCE)
                            , ("OBJECT_TYPE_DEVICE_MEMORY", pure OBJECT_TYPE_DEVICE_MEMORY)
                            , ("OBJECT_TYPE_BUFFER", pure OBJECT_TYPE_BUFFER)
                            , ("OBJECT_TYPE_IMAGE", pure OBJECT_TYPE_IMAGE)
                            , ("OBJECT_TYPE_EVENT", pure OBJECT_TYPE_EVENT)
                            , ("OBJECT_TYPE_QUERY_POOL", pure OBJECT_TYPE_QUERY_POOL)
                            , ("OBJECT_TYPE_BUFFER_VIEW", pure OBJECT_TYPE_BUFFER_VIEW)
                            , ("OBJECT_TYPE_IMAGE_VIEW", pure OBJECT_TYPE_IMAGE_VIEW)
                            , ("OBJECT_TYPE_SHADER_MODULE", pure OBJECT_TYPE_SHADER_MODULE)
                            , ("OBJECT_TYPE_PIPELINE_CACHE", pure OBJECT_TYPE_PIPELINE_CACHE)
                            , ("OBJECT_TYPE_PIPELINE_LAYOUT", pure OBJECT_TYPE_PIPELINE_LAYOUT)
                            , ("OBJECT_TYPE_RENDER_PASS", pure OBJECT_TYPE_RENDER_PASS)
                            , ("OBJECT_TYPE_PIPELINE", pure OBJECT_TYPE_PIPELINE)
                            , ("OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT", pure OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT)
                            , ("OBJECT_TYPE_SAMPLER", pure OBJECT_TYPE_SAMPLER)
                            , ("OBJECT_TYPE_DESCRIPTOR_POOL", pure OBJECT_TYPE_DESCRIPTOR_POOL)
                            , ("OBJECT_TYPE_DESCRIPTOR_SET", pure OBJECT_TYPE_DESCRIPTOR_SET)
                            , ("OBJECT_TYPE_FRAMEBUFFER", pure OBJECT_TYPE_FRAMEBUFFER)
                            , ("OBJECT_TYPE_COMMAND_POOL", pure OBJECT_TYPE_COMMAND_POOL)
                            , ("OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT", pure OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT)
                            , ("OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV", pure OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV)
                            , ("OBJECT_TYPE_DEFERRED_OPERATION_KHR", pure OBJECT_TYPE_DEFERRED_OPERATION_KHR)
                            , ("OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL", pure OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL)
                            , ("OBJECT_TYPE_ACCELERATION_STRUCTURE_NV", pure OBJECT_TYPE_ACCELERATION_STRUCTURE_NV)
                            , ("OBJECT_TYPE_VALIDATION_CACHE_EXT", pure OBJECT_TYPE_VALIDATION_CACHE_EXT)
                            , ("OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR", pure OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR)
                            , ("OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT", pure OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT)
                            , ("OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT", pure OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT)
                            , ("OBJECT_TYPE_DISPLAY_MODE_KHR", pure OBJECT_TYPE_DISPLAY_MODE_KHR)
                            , ("OBJECT_TYPE_DISPLAY_KHR", pure OBJECT_TYPE_DISPLAY_KHR)
                            , ("OBJECT_TYPE_SWAPCHAIN_KHR", pure OBJECT_TYPE_SWAPCHAIN_KHR)
                            , ("OBJECT_TYPE_SURFACE_KHR", pure OBJECT_TYPE_SURFACE_KHR)
                            , ("OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE", pure OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE)
                            , ("OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION", pure OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION)]
                     +++
                     prec 10 (do
                       expectP (Ident "ObjectType")
                       v <- step readPrec
                       pure (ObjectType v)))

