{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( PFN_vkDebugReportCallbackEXT
  , VkDebugReportCallbackCreateInfoEXT
  , VkDebugReportCallbackEXT
  , VkDebugReportFlagBitsEXT
  , VkDebugReportFlagsEXT
  , VkDebugReportObjectTypeEXT
  , FN_vkCreateDebugReportCallbackEXT
  , PFN_vkCreateDebugReportCallbackEXT
  , FN_vkDebugReportMessageEXT
  , PFN_vkDebugReportMessageEXT
  , FN_vkDestroyDebugReportCallbackEXT
  , PFN_vkDestroyDebugReportCallbackEXT
  ) where

import Data.Int
  ( Int32
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
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkBool32
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkInstance
  )


-- | PFN_vkDebugReportCallbackEXT - Application-defined debug report callback
-- function
--
-- = Parameters
--
-- -   @flags@ specifies the 'VkDebugReportFlagBitsEXT' that triggered this
--     callback.
--
-- -   @objectType@ is a 'VkDebugReportObjectTypeEXT' value specifying the
--     type of object being used or created at the time the event was
--     triggered.
--
-- -   @object@ is the object where the issue was detected. If @objectType@
--     is 'VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT', @object@ is undefined.
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
--
-- The callback /must/ not call 'vkDestroyDebugReportCallbackEXT'.
--
-- The callback returns a 'Graphics.Vulkan.C.Core10.Core.VkBool32', which
-- is interpreted in a layer-specified manner. The application /should/
-- always return 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'. The
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' value is reserved for use in
-- layer development.
--
-- @object@ /must/ be a Vulkan object or
-- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'. If @objectType@ is
-- not 'VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT' and @object@ is not
-- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @object@ /must/ be
-- a Vulkan object of the corresponding type associated with @objectType@
-- as defined in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#debug-report-object-types>.
--
-- = See Also
--
-- 'VkDebugReportCallbackCreateInfoEXT'
type PFN_vkDebugReportCallbackEXT = Ptr (("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> ("pUserData" ::: Ptr ()) -> IO VkBool32)

data VkDebugReportCallbackCreateInfoEXT

-- | Dummy data to tag the 'Ptr' with
data VkDebugReportCallbackEXT_T
-- | VkDebugReportCallbackEXT - Opaque handle to a debug report callback
-- object
--
-- = See Also
--
-- 'vkCreateDebugReportCallbackEXT', 'vkDestroyDebugReportCallbackEXT'
type VkDebugReportCallbackEXT = Ptr VkDebugReportCallbackEXT_T

data VkDebugReportFlagBitsEXT

-- | VkDebugReportFlagsEXT - Bitmask of VkDebugReportFlagBitsEXT
--
-- = Description
--
-- 'VkDebugReportFlagsEXT' is a bitmask type for setting a mask of zero or
-- more 'VkDebugReportFlagBitsEXT'.
--
-- = See Also
--
-- 'VkDebugReportCallbackCreateInfoEXT', 'VkDebugReportFlagBitsEXT',
-- 'vkDebugReportMessageEXT'
type VkDebugReportFlagsEXT = VkDebugReportFlagBitsEXT

data VkDebugReportObjectTypeEXT

type FN_vkCreateDebugReportCallbackEXT = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult
type PFN_vkCreateDebugReportCallbackEXT = FunPtr FN_vkCreateDebugReportCallbackEXT

type FN_vkDebugReportMessageEXT = ("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()
type PFN_vkDebugReportMessageEXT = FunPtr FN_vkDebugReportMessageEXT

type FN_vkDestroyDebugReportCallbackEXT = ("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDebugReportCallbackEXT = FunPtr FN_vkDestroyDebugReportCallbackEXT
