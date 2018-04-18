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

-- | 
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

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT = VkDebugReportObjectTypeEXT 0

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT = VkDebugReportObjectTypeEXT 1

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT = VkDebugReportObjectTypeEXT 2

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT = VkDebugReportObjectTypeEXT 3

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT = VkDebugReportObjectTypeEXT 4

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT = VkDebugReportObjectTypeEXT 5

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT = VkDebugReportObjectTypeEXT 6

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT = VkDebugReportObjectTypeEXT 7

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT = VkDebugReportObjectTypeEXT 8

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT = VkDebugReportObjectTypeEXT 9

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT = VkDebugReportObjectTypeEXT 10

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT = VkDebugReportObjectTypeEXT 11

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT = VkDebugReportObjectTypeEXT 12

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT = VkDebugReportObjectTypeEXT 13

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT = VkDebugReportObjectTypeEXT 14

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT = VkDebugReportObjectTypeEXT 15

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT = VkDebugReportObjectTypeEXT 16

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT = VkDebugReportObjectTypeEXT 17

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT = VkDebugReportObjectTypeEXT 18

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT = VkDebugReportObjectTypeEXT 19

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = VkDebugReportObjectTypeEXT 20

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT = VkDebugReportObjectTypeEXT 21

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT = VkDebugReportObjectTypeEXT 22

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT = VkDebugReportObjectTypeEXT 23

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT = VkDebugReportObjectTypeEXT 24

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT = VkDebugReportObjectTypeEXT 25

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT = VkDebugReportObjectTypeEXT 26

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT = VkDebugReportObjectTypeEXT 27

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT = VkDebugReportObjectTypeEXT 28

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT = VkDebugReportObjectTypeEXT 29

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT = VkDebugReportObjectTypeEXT 30

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT = VkDebugReportObjectTypeEXT 31

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT = VkDebugReportObjectTypeEXT 32

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT = VkDebugReportObjectTypeEXT 33
-- ** VkDebugReportFlagBitsEXT

-- | 
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

-- | 
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000001

-- | 
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000002

-- | 
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000004

-- | 
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000008

-- | 
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000010
-- | Nothing
pattern VK_ERROR_VALIDATION_FAILED_EXT :: VkResult
pattern VK_ERROR_VALIDATION_FAILED_EXT = VkResult (-1000011001)
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT = VkStructureType 1000011000
-- | Just "VkDebugReportCallbackEXT"
pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT :: VkObjectType
pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT = VkObjectType 1000011000
-- | Nothing
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT = VkDebugReportObjectTypeEXT 1000156000
-- | Nothing
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT = VkDebugReportObjectTypeEXT 1000085000
pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION = 9
pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT = VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
-- |
type PFN_vkDebugReportCallbackEXT = Ptr (("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> ("pUserData" ::: Ptr ()) -> IO VkBool32)
-- |
data VkDebugReportCallbackEXT_T
type VkDebugReportCallbackEXT = Ptr VkDebugReportCallbackEXT_T
-- | 
foreign import ccall "vkCreateDebugReportCallbackEXT" vkCreateDebugReportCallbackEXT :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult
-- | 
foreign import ccall "vkDestroyDebugReportCallbackEXT" vkDestroyDebugReportCallbackEXT :: ("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkDebugReportMessageEXT" vkDebugReportMessageEXT :: ("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()
-- | TODO: Struct comments
data VkDebugReportCallbackCreateInfoEXT = VkDebugReportCallbackCreateInfoEXT
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkDebugReportFlagsEXT
  , vkPfnCallback :: PFN_vkDebugReportCallbackEXT
  , vkPUserData :: Ptr ()
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
type VkDebugReportFlagsEXT = VkDebugReportFlagBitsEXT
