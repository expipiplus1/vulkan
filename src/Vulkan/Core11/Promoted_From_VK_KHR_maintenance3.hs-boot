{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance3"
module Vulkan.Core11.Promoted_From_VK_KHR_maintenance3  ( DescriptorSetLayoutSupport
                                                        , PhysicalDeviceMaintenance3Properties
                                                        ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role DescriptorSetLayoutSupport nominal
data DescriptorSetLayoutSupport (es :: [Type])

instance (Extendss DescriptorSetLayoutSupport es, PokeChain es) => ToCStruct (DescriptorSetLayoutSupport es)
instance Show (Chain es) => Show (DescriptorSetLayoutSupport es)

instance (Extendss DescriptorSetLayoutSupport es, PeekChain es) => FromCStruct (DescriptorSetLayoutSupport es)


data PhysicalDeviceMaintenance3Properties

instance ToCStruct PhysicalDeviceMaintenance3Properties
instance Show PhysicalDeviceMaintenance3Properties

instance FromCStruct PhysicalDeviceMaintenance3Properties

