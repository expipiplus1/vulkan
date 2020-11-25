{-# language CPP #-}
-- No documentation found for Chapter "Result"
module Vulkan.Core10.Enums.Result  (Result( SUCCESS
                                          , NOT_READY
                                          , TIMEOUT
                                          , EVENT_SET
                                          , EVENT_RESET
                                          , INCOMPLETE
                                          , ERROR_OUT_OF_HOST_MEMORY
                                          , ERROR_OUT_OF_DEVICE_MEMORY
                                          , ERROR_INITIALIZATION_FAILED
                                          , ERROR_DEVICE_LOST
                                          , ERROR_MEMORY_MAP_FAILED
                                          , ERROR_LAYER_NOT_PRESENT
                                          , ERROR_EXTENSION_NOT_PRESENT
                                          , ERROR_FEATURE_NOT_PRESENT
                                          , ERROR_INCOMPATIBLE_DRIVER
                                          , ERROR_TOO_MANY_OBJECTS
                                          , ERROR_FORMAT_NOT_SUPPORTED
                                          , ERROR_FRAGMENTED_POOL
                                          , ERROR_UNKNOWN
                                          , PIPELINE_COMPILE_REQUIRED_EXT
                                          , OPERATION_NOT_DEFERRED_KHR
                                          , OPERATION_DEFERRED_KHR
                                          , THREAD_DONE_KHR
                                          , THREAD_IDLE_KHR
                                          , ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
                                          , ERROR_NOT_PERMITTED_EXT
                                          , ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
                                          , ERROR_INVALID_SHADER_NV
                                          , ERROR_VALIDATION_FAILED_EXT
                                          , ERROR_INCOMPATIBLE_DISPLAY_KHR
                                          , ERROR_OUT_OF_DATE_KHR
                                          , SUBOPTIMAL_KHR
                                          , ERROR_NATIVE_WINDOW_IN_USE_KHR
                                          , ERROR_SURFACE_LOST_KHR
                                          , ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS
                                          , ERROR_FRAGMENTATION
                                          , ERROR_INVALID_EXTERNAL_HANDLE
                                          , ERROR_OUT_OF_POOL_MEMORY
                                          , ..
                                          )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkResult"
newtype Result = Result Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkResult" "VK_SUCCESS"
pattern SUCCESS                              = Result 0
-- No documentation found for Nested "VkResult" "VK_NOT_READY"
pattern NOT_READY                            = Result 1
-- No documentation found for Nested "VkResult" "VK_TIMEOUT"
pattern TIMEOUT                              = Result 2
-- No documentation found for Nested "VkResult" "VK_EVENT_SET"
pattern EVENT_SET                            = Result 3
-- No documentation found for Nested "VkResult" "VK_EVENT_RESET"
pattern EVENT_RESET                          = Result 4
-- No documentation found for Nested "VkResult" "VK_INCOMPLETE"
pattern INCOMPLETE                           = Result 5
-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_HOST_MEMORY"
pattern ERROR_OUT_OF_HOST_MEMORY             = Result (-1)
-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_DEVICE_MEMORY"
pattern ERROR_OUT_OF_DEVICE_MEMORY           = Result (-2)
-- No documentation found for Nested "VkResult" "VK_ERROR_INITIALIZATION_FAILED"
pattern ERROR_INITIALIZATION_FAILED          = Result (-3)
-- No documentation found for Nested "VkResult" "VK_ERROR_DEVICE_LOST"
pattern ERROR_DEVICE_LOST                    = Result (-4)
-- No documentation found for Nested "VkResult" "VK_ERROR_MEMORY_MAP_FAILED"
pattern ERROR_MEMORY_MAP_FAILED              = Result (-5)
-- No documentation found for Nested "VkResult" "VK_ERROR_LAYER_NOT_PRESENT"
pattern ERROR_LAYER_NOT_PRESENT              = Result (-6)
-- No documentation found for Nested "VkResult" "VK_ERROR_EXTENSION_NOT_PRESENT"
pattern ERROR_EXTENSION_NOT_PRESENT          = Result (-7)
-- No documentation found for Nested "VkResult" "VK_ERROR_FEATURE_NOT_PRESENT"
pattern ERROR_FEATURE_NOT_PRESENT            = Result (-8)
-- No documentation found for Nested "VkResult" "VK_ERROR_INCOMPATIBLE_DRIVER"
pattern ERROR_INCOMPATIBLE_DRIVER            = Result (-9)
-- No documentation found for Nested "VkResult" "VK_ERROR_TOO_MANY_OBJECTS"
pattern ERROR_TOO_MANY_OBJECTS               = Result (-10)
-- No documentation found for Nested "VkResult" "VK_ERROR_FORMAT_NOT_SUPPORTED"
pattern ERROR_FORMAT_NOT_SUPPORTED           = Result (-11)
-- No documentation found for Nested "VkResult" "VK_ERROR_FRAGMENTED_POOL"
pattern ERROR_FRAGMENTED_POOL                = Result (-12)
-- No documentation found for Nested "VkResult" "VK_ERROR_UNKNOWN"
pattern ERROR_UNKNOWN                        = Result (-13)
-- No documentation found for Nested "VkResult" "VK_PIPELINE_COMPILE_REQUIRED_EXT"
pattern PIPELINE_COMPILE_REQUIRED_EXT        = Result 1000297000
-- No documentation found for Nested "VkResult" "VK_OPERATION_NOT_DEFERRED_KHR"
pattern OPERATION_NOT_DEFERRED_KHR           = Result 1000268003
-- No documentation found for Nested "VkResult" "VK_OPERATION_DEFERRED_KHR"
pattern OPERATION_DEFERRED_KHR               = Result 1000268002
-- No documentation found for Nested "VkResult" "VK_THREAD_DONE_KHR"
pattern THREAD_DONE_KHR                      = Result 1000268001
-- No documentation found for Nested "VkResult" "VK_THREAD_IDLE_KHR"
pattern THREAD_IDLE_KHR                      = Result 1000268000
-- No documentation found for Nested "VkResult" "VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT"
pattern ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT = Result (-1000255000)
-- No documentation found for Nested "VkResult" "VK_ERROR_NOT_PERMITTED_EXT"
pattern ERROR_NOT_PERMITTED_EXT              = Result (-1000174001)
-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT"
pattern ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT = Result (-1000158000)
-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_SHADER_NV"
pattern ERROR_INVALID_SHADER_NV              = Result (-1000012000)
-- No documentation found for Nested "VkResult" "VK_ERROR_VALIDATION_FAILED_EXT"
pattern ERROR_VALIDATION_FAILED_EXT          = Result (-1000011001)
-- No documentation found for Nested "VkResult" "VK_ERROR_INCOMPATIBLE_DISPLAY_KHR"
pattern ERROR_INCOMPATIBLE_DISPLAY_KHR       = Result (-1000003001)
-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_DATE_KHR"
pattern ERROR_OUT_OF_DATE_KHR                = Result (-1000001004)
-- No documentation found for Nested "VkResult" "VK_SUBOPTIMAL_KHR"
pattern SUBOPTIMAL_KHR                       = Result 1000001003
-- No documentation found for Nested "VkResult" "VK_ERROR_NATIVE_WINDOW_IN_USE_KHR"
pattern ERROR_NATIVE_WINDOW_IN_USE_KHR       = Result (-1000000001)
-- No documentation found for Nested "VkResult" "VK_ERROR_SURFACE_LOST_KHR"
pattern ERROR_SURFACE_LOST_KHR               = Result (-1000000000)
-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS"
pattern ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS = Result (-1000257000)
-- No documentation found for Nested "VkResult" "VK_ERROR_FRAGMENTATION"
pattern ERROR_FRAGMENTATION                  = Result (-1000161000)
-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_EXTERNAL_HANDLE"
pattern ERROR_INVALID_EXTERNAL_HANDLE        = Result (-1000072003)
-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_POOL_MEMORY"
pattern ERROR_OUT_OF_POOL_MEMORY             = Result (-1000069000)
{-# complete SUCCESS,
             NOT_READY,
             TIMEOUT,
             EVENT_SET,
             EVENT_RESET,
             INCOMPLETE,
             ERROR_OUT_OF_HOST_MEMORY,
             ERROR_OUT_OF_DEVICE_MEMORY,
             ERROR_INITIALIZATION_FAILED,
             ERROR_DEVICE_LOST,
             ERROR_MEMORY_MAP_FAILED,
             ERROR_LAYER_NOT_PRESENT,
             ERROR_EXTENSION_NOT_PRESENT,
             ERROR_FEATURE_NOT_PRESENT,
             ERROR_INCOMPATIBLE_DRIVER,
             ERROR_TOO_MANY_OBJECTS,
             ERROR_FORMAT_NOT_SUPPORTED,
             ERROR_FRAGMENTED_POOL,
             ERROR_UNKNOWN,
             PIPELINE_COMPILE_REQUIRED_EXT,
             OPERATION_NOT_DEFERRED_KHR,
             OPERATION_DEFERRED_KHR,
             THREAD_DONE_KHR,
             THREAD_IDLE_KHR,
             ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT,
             ERROR_NOT_PERMITTED_EXT,
             ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT,
             ERROR_INVALID_SHADER_NV,
             ERROR_VALIDATION_FAILED_EXT,
             ERROR_INCOMPATIBLE_DISPLAY_KHR,
             ERROR_OUT_OF_DATE_KHR,
             SUBOPTIMAL_KHR,
             ERROR_NATIVE_WINDOW_IN_USE_KHR,
             ERROR_SURFACE_LOST_KHR,
             ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS,
             ERROR_FRAGMENTATION,
             ERROR_INVALID_EXTERNAL_HANDLE,
             ERROR_OUT_OF_POOL_MEMORY :: Result #-}

conNameResult :: String
conNameResult = "Result"

enumPrefixResult :: String
enumPrefixResult = ""

showTableResult :: [(Result, String)]
showTableResult =
  [ (SUCCESS                             , "SUCCESS")
  , (NOT_READY                           , "NOT_READY")
  , (TIMEOUT                             , "TIMEOUT")
  , (EVENT_SET                           , "EVENT_SET")
  , (EVENT_RESET                         , "EVENT_RESET")
  , (INCOMPLETE                          , "INCOMPLETE")
  , (ERROR_OUT_OF_HOST_MEMORY            , "ERROR_OUT_OF_HOST_MEMORY")
  , (ERROR_OUT_OF_DEVICE_MEMORY          , "ERROR_OUT_OF_DEVICE_MEMORY")
  , (ERROR_INITIALIZATION_FAILED         , "ERROR_INITIALIZATION_FAILED")
  , (ERROR_DEVICE_LOST                   , "ERROR_DEVICE_LOST")
  , (ERROR_MEMORY_MAP_FAILED             , "ERROR_MEMORY_MAP_FAILED")
  , (ERROR_LAYER_NOT_PRESENT             , "ERROR_LAYER_NOT_PRESENT")
  , (ERROR_EXTENSION_NOT_PRESENT         , "ERROR_EXTENSION_NOT_PRESENT")
  , (ERROR_FEATURE_NOT_PRESENT           , "ERROR_FEATURE_NOT_PRESENT")
  , (ERROR_INCOMPATIBLE_DRIVER           , "ERROR_INCOMPATIBLE_DRIVER")
  , (ERROR_TOO_MANY_OBJECTS              , "ERROR_TOO_MANY_OBJECTS")
  , (ERROR_FORMAT_NOT_SUPPORTED          , "ERROR_FORMAT_NOT_SUPPORTED")
  , (ERROR_FRAGMENTED_POOL               , "ERROR_FRAGMENTED_POOL")
  , (ERROR_UNKNOWN                       , "ERROR_UNKNOWN")
  , (PIPELINE_COMPILE_REQUIRED_EXT       , "PIPELINE_COMPILE_REQUIRED_EXT")
  , (OPERATION_NOT_DEFERRED_KHR          , "OPERATION_NOT_DEFERRED_KHR")
  , (OPERATION_DEFERRED_KHR              , "OPERATION_DEFERRED_KHR")
  , (THREAD_DONE_KHR                     , "THREAD_DONE_KHR")
  , (THREAD_IDLE_KHR                     , "THREAD_IDLE_KHR")
  , (ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT, "ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT")
  , (ERROR_NOT_PERMITTED_EXT             , "ERROR_NOT_PERMITTED_EXT")
  , (ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT, "ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT")
  , (ERROR_INVALID_SHADER_NV             , "ERROR_INVALID_SHADER_NV")
  , (ERROR_VALIDATION_FAILED_EXT         , "ERROR_VALIDATION_FAILED_EXT")
  , (ERROR_INCOMPATIBLE_DISPLAY_KHR      , "ERROR_INCOMPATIBLE_DISPLAY_KHR")
  , (ERROR_OUT_OF_DATE_KHR               , "ERROR_OUT_OF_DATE_KHR")
  , (SUBOPTIMAL_KHR                      , "SUBOPTIMAL_KHR")
  , (ERROR_NATIVE_WINDOW_IN_USE_KHR      , "ERROR_NATIVE_WINDOW_IN_USE_KHR")
  , (ERROR_SURFACE_LOST_KHR              , "ERROR_SURFACE_LOST_KHR")
  , (ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS, "ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS")
  , (ERROR_FRAGMENTATION                 , "ERROR_FRAGMENTATION")
  , (ERROR_INVALID_EXTERNAL_HANDLE       , "ERROR_INVALID_EXTERNAL_HANDLE")
  , (ERROR_OUT_OF_POOL_MEMORY            , "ERROR_OUT_OF_POOL_MEMORY")
  ]


instance Show Result where
showsPrec = enumShowsPrec enumPrefixResult showTableResult conNameResult (\(Result x) -> x) (showsPrec 11)


instance Read Result where
  readPrec = enumReadPrec enumPrefixResult showTableResult conNameResult Result

