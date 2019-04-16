{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( withCStructDebugReportCallbackCreateInfoEXT
  , fromCStructDebugReportCallbackCreateInfoEXT
  , DebugReportCallbackCreateInfoEXT(..)
  , DebugReportCallbackEXT
  , DebugReportFlagBitsEXT
  , DebugReportFlagsEXT
  , DebugReportObjectTypeEXT
  , createDebugReportCallbackEXT
  , debugReportMessageEXT
  , destroyDebugReportCallbackEXT
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
  ( throwIO
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
import qualified Graphics.Vulkan.C.Dynamic
  ( createDebugReportCallbackEXT
  , debugReportMessageEXT
  , destroyDebugReportCallbackEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackCreateInfoEXT(..)
  , VkDebugReportFlagBitsEXT(..)
  , VkDebugReportObjectTypeEXT(..)
  , PFN_vkDebugReportCallbackEXT
  , VkDebugReportCallbackEXT
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


-- No documentation found for TopLevel "DebugReportCallbackCreateInfoEXT"
data DebugReportCallbackCreateInfoEXT = DebugReportCallbackCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DebugReportCallbackCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugReportCallbackCreateInfoEXT" "flags"
  vkFlags :: DebugReportFlagsEXT
  , -- No documentation found for Nested "DebugReportCallbackCreateInfoEXT" "pfnCallback"
  vkPfnCallback :: PFN_vkDebugReportCallbackEXT
  , -- No documentation found for Nested "DebugReportCallbackCreateInfoEXT" "pUserData"
  vkPUserData :: Ptr ()
  }
  deriving (Show, Eq)
withCStructDebugReportCallbackCreateInfoEXT :: DebugReportCallbackCreateInfoEXT -> (VkDebugReportCallbackCreateInfoEXT -> IO a) -> IO a
withCStructDebugReportCallbackCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: DebugReportCallbackCreateInfoEXT)) (\pPNext -> cont (VkDebugReportCallbackCreateInfoEXT VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT pPNext (vkFlags (from :: DebugReportCallbackCreateInfoEXT)) (vkPfnCallback (from :: DebugReportCallbackCreateInfoEXT)) (vkPUserData (from :: DebugReportCallbackCreateInfoEXT))))
fromCStructDebugReportCallbackCreateInfoEXT :: VkDebugReportCallbackCreateInfoEXT -> IO DebugReportCallbackCreateInfoEXT
fromCStructDebugReportCallbackCreateInfoEXT c = DebugReportCallbackCreateInfoEXT <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugReportCallbackCreateInfoEXT)))
                                                                                 <*> pure (vkFlags (c :: VkDebugReportCallbackCreateInfoEXT))
                                                                                 <*> pure (vkPfnCallback (c :: VkDebugReportCallbackCreateInfoEXT))
                                                                                 <*> pure (vkPUserData (c :: VkDebugReportCallbackCreateInfoEXT))
-- No documentation found for TopLevel "DebugReportCallbackEXT"
type DebugReportCallbackEXT = VkDebugReportCallbackEXT
-- No documentation found for TopLevel "DebugReportFlagBitsEXT"
type DebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT
-- No documentation found for TopLevel "DebugReportFlagsEXT"
type DebugReportFlagsEXT = DebugReportFlagBitsEXT
-- No documentation found for TopLevel "DebugReportObjectTypeEXT"
type DebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT

-- | Wrapper for vkCreateDebugReportCallbackEXT
createDebugReportCallbackEXT :: Instance ->  DebugReportCallbackCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (DebugReportCallbackEXT)
createDebugReportCallbackEXT = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pCallback -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDebugReportCallbackCreateInfoEXT a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createDebugReportCallbackEXT commandTable instance' pCreateInfo pAllocator pCallback >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pCallback)))))

-- | Wrapper for vkDebugReportMessageEXT
debugReportMessageEXT :: Instance ->  DebugReportFlagsEXT ->  DebugReportObjectTypeEXT ->  Word64 ->  CSize ->  Int32 ->  ByteString ->  ByteString ->  IO ()
debugReportMessageEXT = \(Instance instance' commandTable) -> \flags -> \objectType -> \object -> \location -> \messageCode -> \layerPrefix -> \message -> useAsCString message (\pMessage -> useAsCString layerPrefix (\pLayerPrefix -> Graphics.Vulkan.C.Dynamic.debugReportMessageEXT commandTable instance' flags objectType object location messageCode pLayerPrefix pMessage *> (pure ())))

-- | Wrapper for vkDestroyDebugReportCallbackEXT
destroyDebugReportCallbackEXT :: Instance ->  DebugReportCallbackEXT ->  Maybe AllocationCallbacks ->  IO ()
destroyDebugReportCallbackEXT = \(Instance instance' commandTable) -> \callback -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyDebugReportCallbackEXT commandTable instance' callback pAllocator *> (pure ()))
