{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3  ( DescriptorSetLayoutSupport
                                                                 , PhysicalDeviceMaintenance3Properties
                                                                 ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role DescriptorSetLayoutSupport nominal
data DescriptorSetLayoutSupport (es :: [Type])

instance PokeChain es => ToCStruct (DescriptorSetLayoutSupport es)
instance Show (Chain es) => Show (DescriptorSetLayoutSupport es)

instance PeekChain es => FromCStruct (DescriptorSetLayoutSupport es)


data PhysicalDeviceMaintenance3Properties

instance ToCStruct PhysicalDeviceMaintenance3Properties
instance Show PhysicalDeviceMaintenance3Properties

instance FromCStruct PhysicalDeviceMaintenance3Properties

