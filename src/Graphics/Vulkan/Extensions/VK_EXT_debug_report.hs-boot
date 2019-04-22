{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( DebugReportCallbackEXT
  , DebugReportFlagBitsEXT
  , DebugReportFlagsEXT
  , DebugReportObjectTypeEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportFlagBitsEXT
  , VkDebugReportObjectTypeEXT
  , VkDebugReportCallbackEXT
  )


-- | VkDebugReportCallbackEXT - Opaque handle to a debug report callback
-- object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkCreateDebugReportCallbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkDestroyDebugReportCallbackEXT'
type DebugReportCallbackEXT = VkDebugReportCallbackEXT

-- | VkDebugReportFlagBitsEXT - Bitmask specifying events which cause a debug
-- report callback
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportFlagsEXT'
type DebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT

-- | VkDebugReportFlagsEXT - Bitmask of VkDebugReportFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportFlagBitsEXT'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackCreateInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportFlagBitsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkDebugReportMessageEXT'
type DebugReportFlagsEXT = DebugReportFlagBitsEXT

-- | VkDebugReportObjectTypeEXT - Specify the type of an object handle
--
-- = Description
--
-- \'
--
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | Vulkan Handle Type        |
-- > | ug_report.VkDebugReportObjectTypeEXT'    |                           |
-- > +==========================================+===========================+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | Unknown\/Undefined Handle |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_UN |                           |
-- > | KNOWN_EXT'                               |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_IN | .DeviceInitialization.VkI |
-- > | STANCE_EXT'                              | nstance'                  |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_PH | .DeviceInitialization.VkP |
-- > | YSICAL_DEVICE_EXT'                       | hysicalDevice'            |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_DE | .DeviceInitialization.VkD |
-- > | VICE_EXT'                                | evice'                    |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_QU | .Queue.VkQueue'           |
-- > | EUE_EXT'                                 |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_SE | .Queue.VkSemaphore'       |
-- > | MAPHORE_EXT'                             |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_CO | .Queue.VkCommandBuffer'   |
-- > | MMAND_BUFFER_EXT'                        |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_FE | .Queue.VkFence'           |
-- > | NCE_EXT'                                 |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_DE | .Memory.VkDeviceMemory'   |
-- > | VICE_MEMORY_EXT'                         |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_BU | .MemoryManagement.VkBuffe |
-- > | FFER_EXT'                                | r'                        |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_IM | .MemoryManagement.VkImage |
-- > | AGE_EXT'                                 | '                         |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_EV | .Event.VkEvent'           |
-- > | ENT_EXT'                                 |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_QU | .Query.VkQueryPool'       |
-- > | ERY_POOL_EXT'                            |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_BU | .BufferView.VkBufferView' |
-- > | FFER_VIEW_EXT'                           |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_IM | .ImageView.VkImageView'   |
-- > | AGE_VIEW_EXT'                            |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_SH | .Shader.VkShaderModule'   |
-- > | ADER_MODULE_EXT'                         |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_PI | .PipelineCache.VkPipeline |
-- > | PELINE_CACHE_EXT'                        | Cache'                    |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_PI | .Pipeline.VkPipelineLayou |
-- > | PELINE_LAYOUT_EXT'                       | t'                        |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_RE | .Pipeline.VkRenderPass'   |
-- > | NDER_PASS_EXT'                           |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_PI | .Pipeline.VkPipeline'     |
-- > | PELINE_EXT'                              |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_DE | .PipelineLayout.VkDescrip |
-- > | SCRIPTOR_SET_LAYOUT_EXT'                 | torSetLayout'             |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_SA | .Sampler.VkSampler'       |
-- > | MPLER_EXT'                               |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_DE | .DescriptorSet.VkDescript |
-- > | SCRIPTOR_POOL_EXT'                       | orPool'                   |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_DE | .DescriptorSet.VkDescript |
-- > | SCRIPTOR_SET_EXT'                        | orSet'                    |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_FR | .Pass.VkFramebuffer'      |
-- > | AMEBUFFER_EXT'                           |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Core10 |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_CO | .CommandPool.VkCommandPoo |
-- > | MMAND_POOL_EXT'                          | l'                        |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Extens |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_DE | ions.VK_EXT_debug_report. |
-- > | BUG_REPORT_CALLBACK_EXT_EXT'             | VkDebugReportCallbackEXT' |
-- > +------------------------------------------+---------------------------+
-- >
-- > VkDebugReportObjectTypeEXT and Vulkan Handle Relationship
--
-- __Note__
--
-- The primary expected use of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VK_ERROR_VALIDATION_FAILED_EXT'
-- is for validation layer testing. It is not expected that an application
-- would see this error code during normal use of the validation layers.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.VkDebugMarkerObjectNameInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.VkDebugMarkerObjectTagInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkDebugReportMessageEXT'
type DebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT
