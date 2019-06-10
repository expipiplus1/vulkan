{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( PFN_vkDebugReportCallbackEXT
  , VkDebugReportCallbackCreateInfoEXT(..)
  , VkDebugReportCallbackEXT
  , VkDebugReportFlagBitsEXT(..)
  , pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT
  , pattern VK_DEBUG_REPORT_WARNING_BIT_EXT
  , pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
  , pattern VK_DEBUG_REPORT_ERROR_BIT_EXT
  , pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT
  , VkDebugReportFlagsEXT
  , VkDebugReportObjectTypeEXT(..)
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
  , FN_vkCreateDebugReportCallbackEXT
  , PFN_vkCreateDebugReportCallbackEXT
  , vkCreateDebugReportCallbackEXT
  , FN_vkDebugReportMessageEXT
  , PFN_vkDebugReportMessageEXT
  , vkDebugReportMessageEXT
  , FN_vkDestroyDebugReportCallbackEXT
  , PFN_vkDestroyDebugReportCallbackEXT
  , vkDestroyDebugReportCallbackEXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT
  , pattern VK_ERROR_VALIDATION_FAILED_EXT
  , pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  , pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT
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
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkInstance
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "PFN_vkDebugReportCallbackEXT"
type PFN_vkDebugReportCallbackEXT = Ptr (("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> ("pUserData" ::: Ptr ()) -> IO VkBool32)

-- No documentation found for TopLevel "VkDebugReportCallbackCreateInfoEXT"
data VkDebugReportCallbackCreateInfoEXT = VkDebugReportCallbackCreateInfoEXT
  { -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "flags"
  vkFlags :: VkDebugReportFlagsEXT
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "pfnCallback"
  vkPfnCallback :: PFN_vkDebugReportCallbackEXT
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "pUserData"
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

instance Zero VkDebugReportCallbackCreateInfoEXT where
  zero = VkDebugReportCallbackCreateInfoEXT VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
                                            zero
                                            zero
                                            zero
                                            zero

-- | Dummy data to tag the 'Ptr' with
data VkDebugReportCallbackEXT_T
-- No documentation found for TopLevel "VkDebugReportCallbackEXT"
type VkDebugReportCallbackEXT = Ptr VkDebugReportCallbackEXT_T

-- ** VkDebugReportFlagBitsEXT

-- No documentation found for TopLevel "VkDebugReportFlagBitsEXT"
newtype VkDebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_INFORMATION_BIT_EXT"
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_WARNING_BIT_EXT"
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000002

-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT"
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000004

-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_ERROR_BIT_EXT"
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000008

-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_DEBUG_BIT_EXT"
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT :: VkDebugReportFlagBitsEXT
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = VkDebugReportFlagBitsEXT 0x00000010

-- No documentation found for TopLevel "VkDebugReportFlagsEXT"
type VkDebugReportFlagsEXT = VkDebugReportFlagBitsEXT

-- ** VkDebugReportObjectTypeEXT

-- No documentation found for TopLevel "VkDebugReportObjectTypeEXT"
newtype VkDebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT Int32
  deriving (Eq, Ord, Storable, Zero)

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
  showsPrec _ (VkDebugReportObjectTypeEXT 1000165000) = showString "VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT"
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
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT",  pure (VkDebugReportObjectTypeEXT 1000165000))
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

-- No documentation found for TopLevel "vkCreateDebugReportCallbackEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDebugReportCallbackEXT" vkCreateDebugReportCallbackEXT :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult
#else
vkCreateDebugReportCallbackEXT :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult
vkCreateDebugReportCallbackEXT deviceCmds = mkVkCreateDebugReportCallbackEXT (pVkCreateDebugReportCallbackEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDebugReportCallbackEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult)
#endif

type FN_vkCreateDebugReportCallbackEXT = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult
type PFN_vkCreateDebugReportCallbackEXT = FunPtr FN_vkCreateDebugReportCallbackEXT

-- No documentation found for TopLevel "vkDebugReportMessageEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDebugReportMessageEXT" vkDebugReportMessageEXT :: ("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()
#else
vkDebugReportMessageEXT :: InstanceCmds -> ("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()
vkDebugReportMessageEXT deviceCmds = mkVkDebugReportMessageEXT (pVkDebugReportMessageEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugReportMessageEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()) -> (("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ())
#endif

type FN_vkDebugReportMessageEXT = ("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()
type PFN_vkDebugReportMessageEXT = FunPtr FN_vkDebugReportMessageEXT

-- No documentation found for TopLevel "vkDestroyDebugReportCallbackEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDebugReportCallbackEXT" vkDestroyDebugReportCallbackEXT :: ("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyDebugReportCallbackEXT :: InstanceCmds -> ("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyDebugReportCallbackEXT deviceCmds = mkVkDestroyDebugReportCallbackEXT (pVkDestroyDebugReportCallbackEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDebugReportCallbackEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyDebugReportCallbackEXT = ("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDebugReportCallbackEXT = FunPtr FN_vkDestroyDebugReportCallbackEXT

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT = VkDebugReportObjectTypeEXT 1000085000

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT = VkDebugReportObjectTypeEXT 1000156000

-- No documentation found for Nested "VkResult" "VK_ERROR_VALIDATION_FAILED_EXT"
pattern VK_ERROR_VALIDATION_FAILED_EXT :: VkResult
pattern VK_ERROR_VALIDATION_FAILED_EXT = VkResult (-1000011001)

-- No documentation found for TopLevel "VK_EXT_DEBUG_REPORT_EXTENSION_NAME"
pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"

-- No documentation found for TopLevel "VK_EXT_DEBUG_REPORT_SPEC_VERSION"
pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION = 9

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT"
pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT :: VkObjectType
pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT = VkObjectType 1000011000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT = VkStructureType 1000011000

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT = VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
