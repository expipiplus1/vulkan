{-# language CPP #-}
-- | = Name
--
-- XR_EXT_debug_utils - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_debug_utils  XR_EXT_debug_utils>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 20
--
-- = Revision
--
-- 3
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'PFN_xrDebugUtilsMessengerCallbackEXT', 'DebugUtilsLabelEXT',
-- 'DebugUtilsMessengerCallbackDataEXT',
-- 'DebugUtilsMessengerCreateInfoEXT', 'DebugUtilsObjectNameInfoEXT',
-- 'createDebugUtilsMessengerEXT', 'destroyDebugUtilsMessengerEXT',
-- 'sessionBeginDebugUtilsLabelRegionEXT',
-- 'sessionEndDebugUtilsLabelRegionEXT', 'sessionInsertDebugUtilsLabelEXT',
-- 'setDebugUtilsObjectNameEXT', 'submitDebugUtilsMessageEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_debug_utils OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_debug_utils  ( DebugUtilsLabelEXT
                                             , DebugUtilsMessengerCallbackDataEXT
                                             , DebugUtilsMessengerCreateInfoEXT
                                             , DebugUtilsObjectNameInfoEXT
                                             , DebugUtilsMessageSeverityFlagsEXT
                                             , DebugUtilsMessageSeverityFlagBitsEXT
                                             , DebugUtilsMessageTypeFlagsEXT
                                             , DebugUtilsMessageTypeFlagBitsEXT
                                             ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data DebugUtilsLabelEXT

instance ToCStruct DebugUtilsLabelEXT
instance Show DebugUtilsLabelEXT

instance FromCStruct DebugUtilsLabelEXT


data DebugUtilsMessengerCallbackDataEXT

instance ToCStruct DebugUtilsMessengerCallbackDataEXT
instance Show DebugUtilsMessengerCallbackDataEXT

instance FromCStruct DebugUtilsMessengerCallbackDataEXT


data DebugUtilsMessengerCreateInfoEXT

instance ToCStruct DebugUtilsMessengerCreateInfoEXT
instance Show DebugUtilsMessengerCreateInfoEXT

instance FromCStruct DebugUtilsMessengerCreateInfoEXT


data DebugUtilsObjectNameInfoEXT

instance ToCStruct DebugUtilsObjectNameInfoEXT
instance Show DebugUtilsObjectNameInfoEXT

instance FromCStruct DebugUtilsObjectNameInfoEXT


type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

data DebugUtilsMessageSeverityFlagBitsEXT


type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT

data DebugUtilsMessageTypeFlagBitsEXT

