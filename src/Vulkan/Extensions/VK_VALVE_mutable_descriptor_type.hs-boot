{-# language CPP #-}
-- | = Name
--
-- VK_VALVE_mutable_descriptor_type - device extension
--
-- -   Joshua Ashton
--     <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_VALVE_mutable_descriptor_type:%20&body=@Joshua-Ashton%20 >
--
-- = See Also
--
-- 'MutableDescriptorTypeCreateInfoVALVE',
-- 'MutableDescriptorTypeListVALVE',
-- 'PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_mutable_descriptor_type Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_mutable_descriptor_type  ( MutableDescriptorTypeCreateInfoVALVE
                                                           , MutableDescriptorTypeListVALVE
                                                           , PhysicalDeviceMutableDescriptorTypeFeaturesVALVE
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MutableDescriptorTypeCreateInfoVALVE

instance ToCStruct MutableDescriptorTypeCreateInfoVALVE
instance Show MutableDescriptorTypeCreateInfoVALVE

instance FromCStruct MutableDescriptorTypeCreateInfoVALVE


data MutableDescriptorTypeListVALVE

instance ToCStruct MutableDescriptorTypeListVALVE
instance Show MutableDescriptorTypeListVALVE

instance FromCStruct MutableDescriptorTypeListVALVE


data PhysicalDeviceMutableDescriptorTypeFeaturesVALVE

instance ToCStruct PhysicalDeviceMutableDescriptorTypeFeaturesVALVE
instance Show PhysicalDeviceMutableDescriptorTypeFeaturesVALVE

instance FromCStruct PhysicalDeviceMutableDescriptorTypeFeaturesVALVE

