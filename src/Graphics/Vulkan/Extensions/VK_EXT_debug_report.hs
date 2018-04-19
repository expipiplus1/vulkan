{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( VkDebugReportObjectTypeEXT(..)
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
  , VkDebugReportFlagBitsEXT(..)
  , pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT
  , pattern VK_DEBUG_REPORT_WARNING_BIT_EXT
  , pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
  , pattern VK_DEBUG_REPORT_ERROR_BIT_EXT
  , pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT
  , pattern VK_ERROR_VALIDATION_FAILED_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
  , pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  , pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION
  , pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT
  , PFN_vkDebugReportCallbackEXT
  , VkDebugReportCallbackEXT
  , vkCreateDebugReportCallbackEXT
  , vkDestroyDebugReportCallbackEXT
  , vkDebugReportMessageEXT
  , VkDebugReportCallbackCreateInfoEXT(..)
  , VkDebugReportFlagsEXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  )
import Foreign.C.Types
  ( CChar(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkObjectType(..)
  , VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkInstance
  )


-- ** VkDebugReportObjectTypeEXT

-- | VkDebugReportObjectTypeEXT - Specify the type of an object handle
--
-- = Description
-- #_description#
--
-- > +------------------------------------------+---------------------------+
-- > | 'VkDebugReportObjectTypeEXT'             | Vulkan Handle Type        |
-- > +==========================================+===========================+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT | Unknown\/Undefined Handle |
-- > | @                                        |                           |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EX | 'Graphics.Vulkan.Core10.D |
-- > | T@                                       | eviceInitialization.VkIns |
-- > |                                          | tance'                    |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DE | 'Graphics.Vulkan.Core10.D |
-- > | VICE_EXT@                                | eviceInitialization.VkPhy |
-- > |                                          | sicalDevice'              |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT@ | 'Graphics.Vulkan.Core10.D |
-- > |                                          | eviceInitialization.VkDev |
-- > |                                          | ice'                      |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT@  | 'Graphics.Vulkan.Core10.Q |
-- > |                                          | ueue.VkQueue'             |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_E | 'Graphics.Vulkan.Core10.Q |
-- > | XT@                                      | ueue.VkSemaphore'         |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUF | 'Graphics.Vulkan.Core10.Q |
-- > | FER_EXT@                                 | ueue.VkCommandBuffer'     |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT@  | 'Graphics.Vulkan.Core10.Q |
-- > |                                          | ueue.VkFence'             |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMO | 'Graphics.Vulkan.Core10.M |
-- > | RY_EXT@                                  | emory.VkDeviceMemory'     |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT@ | 'Graphics.Vulkan.Core10.M |
-- > |                                          | emoryManagement.VkBuffer' |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT@  | 'Graphics.Vulkan.Core10.M |
-- > |                                          | emoryManagement.VkImage'  |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT@  | 'Graphics.Vulkan.Core10.E |
-- > |                                          | vent.VkEvent'             |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_ | 'Graphics.Vulkan.Core10.Q |
-- > | EXT@                                     | uery.VkQueryPool'         |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW | 'Graphics.Vulkan.Core10.B |
-- > | _EXT@                                    | ufferView.VkBufferView'   |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_ | 'Graphics.Vulkan.Core10.I |
-- > | EXT@                                     | mageView.VkImageView'     |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODU | 'Graphics.Vulkan.Core10.S |
-- > | LE_EXT@                                  | hader.VkShaderModule'     |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CA | 'Graphics.Vulkan.Core10.P |
-- > | CHE_EXT@                                 | ipelineCache.VkPipelineCa |
-- > |                                          | che'                      |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LA | 'Graphics.Vulkan.Core10.P |
-- > | YOUT_EXT@                                | ipeline.VkPipelineLayout' |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS | 'Graphics.Vulkan.Core10.P |
-- > | _EXT@                                    | ipeline.VkRenderPass'     |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EX | 'Graphics.Vulkan.Core10.P |
-- > | T@                                       | ipeline.VkPipeline'       |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_ | 'Graphics.Vulkan.Core10.P |
-- > | SET_LAYOUT_EXT@                          | ipelineLayout.VkDescripto |
-- > |                                          | rSetLayout'               |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT | 'Graphics.Vulkan.Core10.S |
-- > | @                                        | ampler.VkSampler'         |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_ | 'Graphics.Vulkan.Core10.D |
-- > | POOL_EXT@                                | escriptorSet.VkDescriptor |
-- > |                                          | Pool'                     |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_ | 'Graphics.Vulkan.Core10.D |
-- > | SET_EXT@                                 | escriptorSet.VkDescriptor |
-- > |                                          | Set'                      |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER | 'Graphics.Vulkan.Core10.P |
-- > | _EXT@                                    | ass.VkFramebuffer'        |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POO | 'Graphics.Vulkan.Core10.C |
-- > | L_EXT@                                   | ommandPool.VkCommandPool' |
-- > +------------------------------------------+---------------------------+
-- > | @VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPOR | 'VkDebugReportCallbackEXT |
-- > | T_CALLBACK_EXT_EXT@                      | '                         |
-- > +------------------------------------------+---------------------------+
-- >
-- > VkDebugReportObjectTypeEXT and Vulkan Handle Relationship
--
-- __Note__
--
-- The primary expected use of @VK_ERROR_VALIDATION_FAILED_EXT@ is for
-- validation layer testing. It is not expected that an application would
-- see this error code during normal use of the validation layers.
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.VkDebugMarkerObjectNameInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.VkDebugMarkerObjectTagInfoEXT',
-- 'vkDebugReportMessageEXT'
newtype VkDebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkDebugReportObjectTypeEXT where
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDebugReportObjectTypeEXT 1000156000) = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT"
  showsPrec _ (VkDebugReportObjectTypeEXT 1000085000) = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT"
  showsPrec p (VkDebugReportObjectTypeEXT x) = showParen (p >= 11) (showString "VkDebugReportObjectTypeEXT " . showsPrec 11 x)

instance Read VkDebugReportObjectTypeEXT where
  readPrec = parens ( choose [ ("VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT",                      pure VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT",                     pure VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT",              pure VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT",                       pure VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT",                        pure VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT",                    pure VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT",               pure VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT",                        pure VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT",                pure VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT",                       pure VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT",                        pure VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT",                        pure VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT",                   pure VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT",                  pure VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT",                   pure VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT",                pure VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT",               pure VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT",              pure VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT",                  pure VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT",                     pure VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT",        pure VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT",                      pure VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT",              pure VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT",               pure VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT",                  pure VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT",                 pure VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT",                  pure VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT",                pure VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT",    pure VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT",                  pure VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT",             pure VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT",             pure VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT",         pure VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT",   pure (VkDebugReportObjectTypeEXT 1000156000))
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT", pure (VkDebugReportObjectTypeEXT 1000085000))
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT",   pure (VkDebugReportObjectTypeEXT 1000156000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugReportObjectTypeEXT")
                        v <- step readPrec
                        pure (VkDebugReportObjectTypeEXT v)
                        )
                    )

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT = VkDebugReportObjectTypeEXT 0

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT = VkDebugReportObjectTypeEXT 1

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT = VkDebugReportObjectTypeEXT 2

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT = VkDebugReportObjectTypeEXT 3

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT = VkDebugReportObjectTypeEXT 4

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT = VkDebugReportObjectTypeEXT 5

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT = VkDebugReportObjectTypeEXT 6

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT = VkDebugReportObjectTypeEXT 7

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT = VkDebugReportObjectTypeEXT 8

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT = VkDebugReportObjectTypeEXT 9

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT = VkDebugReportObjectTypeEXT 10

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT = VkDebugReportObjectTypeEXT 11

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT = VkDebugReportObjectTypeEXT 12

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT = VkDebugReportObjectTypeEXT 13

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT = VkDebugReportObjectTypeEXT 14

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT = VkDebugReportObjectTypeEXT 15

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT = VkDebugReportObjectTypeEXT 16

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT = VkDebugReportObjectTypeEXT 17

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT = VkDebugReportObjectTypeEXT 18

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT = VkDebugReportObjectTypeEXT 19

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = VkDebugReportObjectTypeEXT 20

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT = VkDebugReportObjectTypeEXT 21

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT = VkDebugReportObjectTypeEXT 22

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT = VkDebugReportObjectTypeEXT 23

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT = VkDebugReportObjectTypeEXT 24

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT = VkDebugReportObjectTypeEXT 25

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT = VkDebugReportObjectTypeEXT 26

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT = VkDebugReportObjectTypeEXT 27

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT = VkDebugReportObjectTypeEXT 28

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT = VkDebugReportObjectTypeEXT 29

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT = VkDebugReportObjectTypeEXT 30

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT = VkDebugReportObjectTypeEXT 31

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT = VkDebugReportObjectTypeEXT 32

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT = VkDebugReportObjectTypeEXT 33
-- ** VkDebugReportFlagBitsEXT

-- | VkDebugReportFlagBitsEXT - Bitmask specifying events which cause a debug
-- report callback
--
-- = See Also
-- #_see_also#
--
-- 'VkDebugReportFlagsEXT'
newtype VkDebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDebugReportFlagBitsEXT where
  showsPrec _ VK_DEBUG_REPORT_INFORMATION_BIT_EXT = showString "VK_DEBUG_REPORT_INFORMATION_BIT_EXT"
  showsPrec _ VK_DEBUG_REPORT_WARNING_BIT_EXT = showString "VK_DEBUG_REPORT_WARNING_BIT_EXT"
  showsPrec _ VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = showString "VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT"
  showsPrec _ VK_DEBUG_REPORT_ERROR_BIT_EXT = showString "VK_DEBUG_REPORT_ERROR_BIT_EXT"
  showsPrec _ VK_DEBUG_REPORT_DEBUG_BIT_EXT = showString "VK_DEBUG_REPORT_DEBUG_BIT_EXT"
  showsPrec p (VkDebugReportFlagBitsEXT x) = showParen (p >= 11) (showString "VkDebugReportFlagBitsEXT " . showsPrec 11 x)

instance Read VkDebugReportFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_DEBUG_REPORT_INFORMATION_BIT_EXT",         pure VK_DEBUG_REPORT_INFORMATION_BIT_EXT)
                             , ("VK_DEBUG_REPORT_WARNING_BIT_EXT",             pure VK_DEBUG_REPORT_WARNING_BIT_EXT)
                             , ("VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT", pure VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT)
                             , ("VK_DEBUG_REPORT_ERROR_BIT_EXT",               pure VK_DEBUG_REPORT_ERROR_BIT_EXT)
                             , ("VK_DEBUG_REPORT_DEBUG_BIT_EXT",               pure VK_DEBUG_REPORT_DEBUG_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugReportFlagBitsEXT")
                        v <- step readPrec
                        pure (VkDebugReportFlagBitsEXT v)
                        )
                    )

-- | @VK_DEBUG_REPORT_INFORMATION_BIT_EXT@ specifies an informational message
-- such as resource details that may be handy when debugging an
-- application.
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000001

-- | @VK_DEBUG_REPORT_WARNING_BIT_EXT@ specifies use of Vulkan that /may/
-- expose an app bug. Such cases may not be immediately harmful, such as a
-- fragment shader outputting to a location with no attachment. Other cases
-- /may/ point to behavior that is almost certainly bad when unintended
-- such as using an image whose memory has not been filled. In general if
-- you see a warning but you know that the behavior is intended\/desired,
-- then simply ignore the warning.
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000002

-- | @VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT@ specifies a potentially
-- non-optimal use of Vulkan, e.g. using
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearColorImage' when
-- setting 'Graphics.Vulkan.Core10.Pass.VkAttachmentDescription'::@loadOp@
-- to @VK_ATTACHMENT_LOAD_OP_CLEAR@ would have worked.
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000004

-- | @VK_DEBUG_REPORT_ERROR_BIT_EXT@ specifies that an error that may cause
-- undefined results, including an application crash.
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000008

-- | @VK_DEBUG_REPORT_DEBUG_BIT_EXT@ specifies diagnostic information from
-- the implementation and layers.
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000010
-- No documentation found for Nested "VkResult" "VK_ERROR_VALIDATION_FAILED_EXT"
pattern VK_ERROR_VALIDATION_FAILED_EXT :: VkResult
pattern VK_ERROR_VALIDATION_FAILED_EXT = VkResult (-1000011001)
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT = VkStructureType 1000011000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT"
pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT :: VkObjectType
pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT = VkObjectType 1000011000
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT = VkDebugReportObjectTypeEXT 1000156000
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT = VkDebugReportObjectTypeEXT 1000085000
-- No documentation found for TopLevel "VK_EXT_DEBUG_REPORT_SPEC_VERSION"
pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION = 9
-- No documentation found for TopLevel "VK_EXT_DEBUG_REPORT_EXTENSION_NAME"
pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT = VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
-- | PFN_vkDebugReportCallbackEXT - Application-defined debug report callback
-- function
--
-- = Parameters
-- #_parameters#
--
-- -   @flags@ specifies the 'VkDebugReportFlagBitsEXT' that triggered this
--     callback.
--
-- -   @objectType@ is a 'VkDebugReportObjectTypeEXT' value specifying the
--     type of object being used or created at the time the event was
--     triggered.
--
-- -   @object@ is the object where the issue was detected. If @objectType@
--     is @VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT@, @object@ is undefined.
--
-- -   @location@ is a component (layer, driver, loader) defined value that
--     specifies the /location/ of the trigger. This is an /optional/
--     value.
--
-- -   @messageCode@ is a layer-defined value indicating what test
--     triggered this callback.
--
-- -   @pLayerPrefix@ is a null-terminated string that is an abbreviation
--     of the name of the component making the callback. @pLayerPrefix@ is
--     only valid for the duration of the callback.
--
-- -   @pMessage@ is a null-terminated string detailing the trigger
--     conditions. @pMessage@ is only valid for the duration of the
--     callback.
--
-- -   @pUserData@ is the user data given when the
--     'VkDebugReportCallbackEXT' was created.
--
-- = Description
-- #_description#
--
-- The callback /must/ not call @vkDestroyDebugReportCallbackEXT@.
--
-- The callback returns a @VkBool32@, which is interpreted in a
-- layer-specified manner. The application /should/ always return
-- @VK_FALSE@. The @VK_TRUE@ value is reserved for use in layer
-- development.
--
-- @object@ /must/ be a Vulkan object or @VK_NULL_HANDLE@. If @objectType@
-- is not @VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT@ and @object@ is not
-- @VK_NULL_HANDLE@, @object@ /must/ be a Vulkan object of the
-- corresponding type associated with @objectType@ as defined in
-- <{html_spec_relative}#debug-report-object-types {html_spec_relative}#debug-report-object-types>.
--
-- = See Also
-- #_see_also#
--
-- 'VkDebugReportCallbackCreateInfoEXT'
type PFN_vkDebugReportCallbackEXT = Ptr (("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> ("pUserData" ::: Ptr ()) -> IO VkBool32)
-- | Dummy data to tag the 'Ptr' with
data VkDebugReportCallbackEXT_T
-- | VkDebugReportCallbackEXT - Opaque handle to a debug report callback
-- object
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'vkCreateDebugReportCallbackEXT', 'vkDestroyDebugReportCallbackEXT'
type VkDebugReportCallbackEXT = Ptr VkDebugReportCallbackEXT_T
-- | vkCreateDebugReportCallbackEXT - Create a debug report callback object
--
-- = Parameters
-- #_parameters#
--
-- -   @instance@ the instance the callback will be logged on.
--
-- -   @pCreateInfo@ points to a 'VkDebugReportCallbackCreateInfoEXT'
--     structure which defines the conditions under which this callback
--     will be called.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pCallback@ is a pointer to record the @VkDebugReportCallbackEXT@
--     object created.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkDebugReportCallbackCreateInfoEXT@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pCallback@ /must/ be a valid pointer to a
--     @VkDebugReportCallbackEXT@ handle
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDebugReportCallbackCreateInfoEXT', 'VkDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance'
foreign import ccall "vkCreateDebugReportCallbackEXT" vkCreateDebugReportCallbackEXT :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult
-- | vkDestroyDebugReportCallbackEXT - Destroy a debug report callback object
--
-- = Parameters
-- #_parameters#
--
-- -   @instance@ the instance where the callback was created.
--
-- -   @callback@ the @VkDebugReportCallbackEXT@ object to destroy.
--     @callback@ is an externally synchronized object and /must/ not be
--     used on more than one thread at a time. This means that
--     @vkDestroyDebugReportCallbackEXT@ /must/ not be called when a
--     callback is active.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   If @VkAllocationCallbacks@ were provided when @callback@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @callback@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @callback@ /must/ be a valid @VkDebugReportCallbackEXT@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @callback@ /must/ have been created, allocated, or retrieved from
--     @instance@
--
-- == Host Synchronization
--
-- -   Host access to @callback@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance'
foreign import ccall "vkDestroyDebugReportCallbackEXT" vkDestroyDebugReportCallbackEXT :: ("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkDebugReportMessageEXT - Inject a message into a debug stream
--
-- = Parameters
-- #_parameters#
--
-- -   @instance@ is the debug stream’s @VkInstance@.
--
-- -   @flags@ specifies the 'VkDebugReportFlagBitsEXT' classification of
--     this event\/message.
--
-- -   @objectType@ is a 'VkDebugReportObjectTypeEXT' specifying the type
--     of object being used or created at the time the event was triggered.
--
-- -   @object@ this is the object where the issue was detected. @object@
--     /can/ be @VK_NULL_HANDLE@ if there is no object associated with the
--     event.
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
-- #_description#
--
-- The call will propagate through the layers and generate callback(s) as
-- indicated by the message’s flags. The parameters are passed on to the
-- callback in addition to the @pUserData@ value that was defined at the
-- time the callback was registered.
--
-- == Valid Usage
--
-- -   @object@ /must/ be a Vulkan object or @VK_NULL_HANDLE@
--
-- -   If @objectType@ is not @VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT@ and
--     @object@ is not @VK_NULL_HANDLE@, @object@ /must/ be a Vulkan object
--     of the corresponding type associated with @objectType@ as defined in
--     <{html_spec_relative}#debug-report-object-types {html_spec_relative}#debug-report-object-types>.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @flags@ /must/ be a valid combination of 'VkDebugReportFlagBitsEXT'
--     values
--
-- -   @flags@ /must/ not be @0@
--
-- -   @objectType@ /must/ be a valid 'VkDebugReportObjectTypeEXT' value
--
-- -   @pLayerPrefix@ /must/ be a null-terminated UTF-8 string
--
-- -   @pMessage@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
-- #_see_also#
--
-- 'VkDebugReportFlagsEXT', 'VkDebugReportObjectTypeEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance'
foreign import ccall "vkDebugReportMessageEXT" vkDebugReportMessageEXT :: ("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()
-- | VkDebugReportCallbackCreateInfoEXT - Structure specifying parameters of
-- a newly created debug report callback
--
-- = Description
-- #_description#
--
-- For each @VkDebugReportCallbackEXT@ that is created the
-- @VkDebugReportCallbackCreateInfoEXT@::@flags@ determine when that
-- @VkDebugReportCallbackCreateInfoEXT@::@pfnCallback@ is called. When an
-- event happens, the implementation will do a bitwise AND of the event’s
-- 'VkDebugReportFlagBitsEXT' flags to each @VkDebugReportCallbackEXT@
-- object’s flags. For each non-zero result the corresponding callback will
-- be called. The callback will come directly from the component that
-- detected the event, unless some other layer intercepts the calls for its
-- own purposes (filter them in a different way, log to a system error log,
-- etc.).
--
-- An application /may/ receive multiple callbacks if multiple
-- @VkDebugReportCallbackEXT@ objects were created. A callback will always
-- be executed in the same thread as the originating Vulkan call.
--
-- A callback may be called from multiple threads simultaneously (if the
-- application is making Vulkan calls from multiple threads).
--
-- == Valid Usage
--
-- -   @pfnCallback@ /must/ be a valid 'PFN_vkDebugReportCallbackEXT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT@
--
-- -   @flags@ /must/ be a valid combination of 'VkDebugReportFlagBitsEXT'
--     values
--
-- = See Also
-- #_see_also#
--
-- 'PFN_vkDebugReportCallbackEXT', 'VkDebugReportFlagsEXT',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateDebugReportCallbackEXT'
data VkDebugReportCallbackCreateInfoEXT = VkDebugReportCallbackCreateInfoEXT
  { -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "vkFlags"
  vkFlags :: VkDebugReportFlagsEXT
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "vkPfnCallback"
  vkPfnCallback :: PFN_vkDebugReportCallbackEXT
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "vkPUserData"
  vkPUserData :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkDebugReportCallbackCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDebugReportCallbackCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPfnCallback (poked :: VkDebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPUserData (poked :: VkDebugReportCallbackCreateInfoEXT))
-- | VkDebugReportFlagsEXT - Bitmask of VkDebugReportFlagBitsEXT
--
-- = Description
-- #_description#
--
-- @VkDebugReportFlagsEXT@ is a bitmask type for setting a mask of zero or
-- more 'VkDebugReportFlagBitsEXT'.
--
-- = See Also
-- #_see_also#
--
-- 'VkDebugReportCallbackCreateInfoEXT', 'VkDebugReportFlagBitsEXT',
-- 'vkDebugReportMessageEXT'
type VkDebugReportFlagsEXT = VkDebugReportFlagBitsEXT
