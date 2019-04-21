{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( withCStructDebugReportCallbackCreateInfoEXT
  , fromCStructDebugReportCallbackCreateInfoEXT
  , DebugReportCallbackCreateInfoEXT(..)
  , DebugReportCallbackEXT
  , DebugReportFlagBitsEXT
  , pattern DEBUG_REPORT_INFORMATION_BIT_EXT
  , pattern DEBUG_REPORT_WARNING_BIT_EXT
  , pattern DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
  , pattern DEBUG_REPORT_ERROR_BIT_EXT
  , pattern DEBUG_REPORT_DEBUG_BIT_EXT
  , DebugReportFlagsEXT
  , DebugReportObjectTypeEXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT
  , pattern DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
  , createDebugReportCallbackEXT
  , debugReportMessageEXT
  , destroyDebugReportCallbackEXT
  , withDebugReportCallbackEXT
  , pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION
  , pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT
  , pattern VK_ERROR_VALIDATION_FAILED_EXT
  , pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Data.ByteString
  ( ByteString
  , useAsCString
  )
import Data.Int
  ( Int32
  )
import Data.Word
  ( Word64
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackCreateInfoEXT(..)
  , VkDebugReportFlagBitsEXT(..)
  , VkDebugReportObjectTypeEXT(..)
  , PFN_vkDebugReportCallbackEXT
  , VkDebugReportCallbackEXT
  , vkCreateDebugReportCallbackEXT
  , vkDebugReportMessageEXT
  , vkDestroyDebugReportCallbackEXT
  , pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT
  , pattern VK_DEBUG_REPORT_ERROR_BIT_EXT
  , pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
  , pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
  , pattern VK_DEBUG_REPORT_WARNING_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT
  , pattern VK_ERROR_VALIDATION_FAILED_EXT
  , pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  , pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT
  )



-- | VkDebugReportCallbackCreateInfoEXT - Structure specifying parameters of
-- a newly created debug report callback
--
-- = Description
--
-- For each
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackEXT'
-- that is created the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackCreateInfoEXT'::@flags@
-- determine when that
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackCreateInfoEXT'::@pfnCallback@
-- is called. When an event happens, the implementation will do a bitwise
-- AND of the event’s
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportFlagBitsEXT'
-- flags to each
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackEXT'
-- object’s flags. For each non-zero result the corresponding callback will
-- be called. The callback will come directly from the component that
-- detected the event, unless some other layer intercepts the calls for its
-- own purposes (filter them in a different way, log to a system error log,
-- etc.).
--
-- An application /may/ receive multiple callbacks if multiple
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackEXT'
-- objects were created. A callback will always be executed in the same
-- thread as the originating Vulkan call.
--
-- A callback may be called from multiple threads simultaneously (if the
-- application is making Vulkan calls from multiple threads).
--
-- Unresolved directive in VkDebugReportCallbackCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDebugReportCallbackCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data DebugReportCallbackCreateInfoEXT = DebugReportCallbackCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DebugReportCallbackCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugReportCallbackCreateInfoEXT" "flags"
  flags :: DebugReportFlagsEXT
  , -- No documentation found for Nested "DebugReportCallbackCreateInfoEXT" "pfnCallback"
  pfnCallback :: PFN_vkDebugReportCallbackEXT
  , -- No documentation found for Nested "DebugReportCallbackCreateInfoEXT" "pUserData"
  userData :: Ptr ()
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDebugReportCallbackCreateInfoEXT' and
-- marshal a 'DebugReportCallbackCreateInfoEXT' into it. The 'VkDebugReportCallbackCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDebugReportCallbackCreateInfoEXT :: DebugReportCallbackCreateInfoEXT -> (VkDebugReportCallbackCreateInfoEXT -> IO a) -> IO a
withCStructDebugReportCallbackCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DebugReportCallbackCreateInfoEXT)) (\pPNext -> cont (VkDebugReportCallbackCreateInfoEXT VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT pPNext (flags (marshalled :: DebugReportCallbackCreateInfoEXT)) (pfnCallback (marshalled :: DebugReportCallbackCreateInfoEXT)) (userData (marshalled :: DebugReportCallbackCreateInfoEXT))))

-- | A function to read a 'VkDebugReportCallbackCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'DebugReportCallbackCreateInfoEXT'.
fromCStructDebugReportCallbackCreateInfoEXT :: VkDebugReportCallbackCreateInfoEXT -> IO DebugReportCallbackCreateInfoEXT
fromCStructDebugReportCallbackCreateInfoEXT c = DebugReportCallbackCreateInfoEXT <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugReportCallbackCreateInfoEXT)))
                                                                                 <*> pure (vkFlags (c :: VkDebugReportCallbackCreateInfoEXT))
                                                                                 <*> pure (vkPfnCallback (c :: VkDebugReportCallbackCreateInfoEXT))
                                                                                 <*> pure (vkPUserData (c :: VkDebugReportCallbackCreateInfoEXT))

instance Zero DebugReportCallbackCreateInfoEXT where
  zero = DebugReportCallbackCreateInfoEXT Nothing
                                          zero
                                          zero
                                          zero


-- | VkDebugReportCallbackEXT - Opaque handle to a debug report callback
-- object
--
-- = See Also
--
-- No cross-references are available
type DebugReportCallbackEXT = VkDebugReportCallbackEXT

-- | VkDebugReportFlagBitsEXT - Bitmask specifying events which cause a debug
-- report callback
--
-- = See Also
--
-- No cross-references are available
type DebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VK_DEBUG_REPORT_INFORMATION_BIT_EXT'
-- specifies an informational message such as resource details that may be
-- handy when debugging an application.
pattern DEBUG_REPORT_INFORMATION_BIT_EXT :: (a ~ DebugReportFlagBitsEXT) => a
pattern DEBUG_REPORT_INFORMATION_BIT_EXT = VK_DEBUG_REPORT_INFORMATION_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VK_DEBUG_REPORT_WARNING_BIT_EXT'
-- specifies use of Vulkan that /may/ expose an app bug. Such cases may not
-- be immediately harmful, such as a fragment shader outputting to a
-- location with no attachment. Other cases /may/ point to behavior that is
-- almost certainly bad when unintended such as using an image whose memory
-- has not been filled. In general if you see a warning but you know that
-- the behavior is intended\/desired, then simply ignore the warning.
pattern DEBUG_REPORT_WARNING_BIT_EXT :: (a ~ DebugReportFlagBitsEXT) => a
pattern DEBUG_REPORT_WARNING_BIT_EXT = VK_DEBUG_REPORT_WARNING_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT'
-- specifies a potentially non-optimal use of Vulkan, e.g. using
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage'
-- when setting
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription'::@loadOp@ to
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR' would have
-- worked.
pattern DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT :: (a ~ DebugReportFlagBitsEXT) => a
pattern DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VK_DEBUG_REPORT_ERROR_BIT_EXT'
-- specifies that the application has violated a valid usage condition of
-- the specification.
pattern DEBUG_REPORT_ERROR_BIT_EXT :: (a ~ DebugReportFlagBitsEXT) => a
pattern DEBUG_REPORT_ERROR_BIT_EXT = VK_DEBUG_REPORT_ERROR_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VK_DEBUG_REPORT_DEBUG_BIT_EXT'
-- specifies diagnostic information from the implementation and layers.
pattern DEBUG_REPORT_DEBUG_BIT_EXT :: (a ~ DebugReportFlagBitsEXT) => a
pattern DEBUG_REPORT_DEBUG_BIT_EXT = VK_DEBUG_REPORT_DEBUG_BIT_EXT

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
-- No cross-references are available
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
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_SU | ions.VK_KHR_surface.VkSur |
-- > | RFACE_KHR_EXT'                           | faceKHR'                  |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Extens |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_SW | ions.VK_KHR_swapchain.VkS |
-- > | APCHAIN_KHR_EXT'                         | wapchainKHR'              |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Extens |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_DE | ions.VK_EXT_debug_report. |
-- > | BUG_REPORT_CALLBACK_EXT_EXT'             | VkDebugReportCallbackEXT' |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Extens |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_DI | ions.VK_KHR_display.VkDis |
-- > | SPLAY_KHR_EXT'                           | playKHR'                  |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Extens |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_DI | ions.VK_KHR_display.VkDis |
-- > | SPLAY_MODE_KHR_EXT'                      | playModeKHR'              |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Extens |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_OB | ions.VK_NVX_device_genera |
-- > | JECT_TABLE_NVX_EXT'                      | ted_commands.VkObjectTabl |
-- > |                                          | eNVX'                     |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Extens |
-- > | ug_report.VK_DEBUG_REPORT_OBJECT_TYPE_IN | ions.VK_NVX_device_genera |
-- > | DIRECT_COMMANDS_LAYOUT_NVX_EXT'          | ted_commands.VkIndirectCo |
-- > |                                          | mmandsLayoutNVX'          |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_KHR_des | 'Graphics.Vulkan.C.Core11 |
-- > | criptor_update_template.VK_DEBUG_REPORT_ | .Promoted_from_VK_KHR_des |
-- > | OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_K | criptor_update_template.V |
-- > | HR_EXT'                                  | kDescriptorUpdateTemplate |
-- > |                                          | '                         |
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
-- No cross-references are available
type DebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT


-- No documentation found for Nested "DebugReportObjectTypeEXT" "DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT :: (a ~ DebugReportObjectTypeEXT) => a
pattern DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT


-- | vkCreateDebugReportCallbackEXT - Create a debug report callback object
--
-- = Parameters
--
-- -   @instance@ the instance the callback will be logged on.
--
-- -   @pCreateInfo@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackCreateInfoEXT'
--     structure which defines the conditions under which this callback
--     will be called.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pCallback@ is a pointer to record the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackEXT'
--     object created.
--
-- = Description
--
-- Unresolved directive in vkCreateDebugReportCallbackEXT.txt -
-- include::{generated}\/validity\/protos\/vkCreateDebugReportCallbackEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
createDebugReportCallbackEXT :: Instance ->  DebugReportCallbackCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (DebugReportCallbackEXT)
createDebugReportCallbackEXT = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pCallback' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructDebugReportCallbackCreateInfoEXT marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateDebugReportCallbackEXT commandTable instance' pCreateInfo' pAllocator pCallback' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pCallback')))))


-- | vkDebugReportMessageEXT - Inject a message into a debug stream
--
-- = Parameters
--
-- -   @instance@ is the debug stream’s
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'.
--
-- -   @flags@ specifies the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportFlagBitsEXT'
--     classification of this event\/message.
--
-- -   @objectType@ is a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportObjectTypeEXT'
--     specifying the type of object being used or created at the time the
--     event was triggered.
--
-- -   @object@ this is the object where the issue was detected. @object@
--     /can/ be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' if
--     there is no object associated with the event.
--
-- -   @location@ is an application defined value.
--
-- -   @messageCode@ is an application defined value.
--
-- -   @pLayerPrefix@ is the abbreviation of the component making this
--     event\/message.
--
-- -   @pMessage@ is a null-terminated string detailing the trigger
--     conditions.
--
-- = Description
--
-- The call will propagate through the layers and generate callback(s) as
-- indicated by the message’s flags. The parameters are passed on to the
-- callback in addition to the @pUserData@ value that was defined at the
-- time the callback was registered.
--
-- == Valid Usage
--
-- -   @object@ /must/ be a Vulkan object or
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @objectType@ is not
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT'
--     and @object@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @object@ /must/
--     be a Vulkan object of the corresponding type associated with
--     @objectType@ as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#debug-report-object-types>.
--
-- Unresolved directive in vkDebugReportMessageEXT.txt -
-- include::{generated}\/validity\/protos\/vkDebugReportMessageEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
debugReportMessageEXT :: Instance ->  DebugReportFlagsEXT ->  DebugReportObjectTypeEXT ->  Word64 ->  CSize ->  Int32 ->  ByteString ->  ByteString ->  IO ()
debugReportMessageEXT = \(Instance instance' commandTable) -> \flags' -> \objectType' -> \object' -> \location' -> \messageCode' -> \layerPrefix' -> \message' -> useAsCString message' (\pMessage' -> useAsCString layerPrefix' (\pLayerPrefix' -> vkDebugReportMessageEXT commandTable instance' flags' objectType' object' location' messageCode' pLayerPrefix' pMessage' *> (pure ())))


-- | vkDestroyDebugReportCallbackEXT - Destroy a debug report callback object
--
-- = Parameters
--
-- -   @instance@ the instance where the callback was created.
--
-- -   @callback@ the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackEXT'
--     object to destroy. @callback@ is an externally synchronized object
--     and /must/ not be used on more than one thread at a time. This means
--     that
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkDestroyDebugReportCallbackEXT'
--     /must/ not be called when a callback is active.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @callback@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @callback@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyDebugReportCallbackEXT.txt -
-- include::{generated}\/validity\/protos\/vkDestroyDebugReportCallbackEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
destroyDebugReportCallbackEXT :: Instance ->  DebugReportCallbackEXT ->  Maybe AllocationCallbacks ->  IO ()
destroyDebugReportCallbackEXT = \(Instance instance' commandTable) -> \callback' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyDebugReportCallbackEXT commandTable instance' callback' pAllocator *> (pure ()))

-- | A safe wrapper for 'createDebugReportCallbackEXT' and 'destroyDebugReportCallbackEXT' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDebugReportCallbackEXT
  :: Instance -> DebugReportCallbackCreateInfoEXT -> Maybe (AllocationCallbacks) -> (DebugReportCallbackEXT -> IO a) -> IO a
withDebugReportCallbackEXT instance' debugReportCallbackCreateInfoEXT allocationCallbacks = bracket
  (createDebugReportCallbackEXT instance' debugReportCallbackCreateInfoEXT allocationCallbacks)
  (\o -> destroyDebugReportCallbackEXT instance' o allocationCallbacks)
