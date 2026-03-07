{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'"
module Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'  ( BindDescriptorSetsInfo
                                                                                , BindMemoryStatus
                                                                                , PhysicalDeviceMaintenance6Features
                                                                                , PhysicalDeviceMaintenance6Properties
                                                                                , PushConstantsInfo
                                                                                , PushDescriptorSetInfo
                                                                                , PushDescriptorSetWithTemplateInfo
                                                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role BindDescriptorSetsInfo nominal
data BindDescriptorSetsInfo (es :: [Type])

instance ( Extendss BindDescriptorSetsInfo es
         , PokeChain es ) => ToCStruct (BindDescriptorSetsInfo es)
instance Show (Chain es) => Show (BindDescriptorSetsInfo es)

instance ( Extendss BindDescriptorSetsInfo es
         , PeekChain es ) => FromCStruct (BindDescriptorSetsInfo es)


data BindMemoryStatus

instance ToCStruct BindMemoryStatus
instance Show BindMemoryStatus

instance FromCStruct BindMemoryStatus


data PhysicalDeviceMaintenance6Features

instance ToCStruct PhysicalDeviceMaintenance6Features
instance Show PhysicalDeviceMaintenance6Features

instance FromCStruct PhysicalDeviceMaintenance6Features


data PhysicalDeviceMaintenance6Properties

instance ToCStruct PhysicalDeviceMaintenance6Properties
instance Show PhysicalDeviceMaintenance6Properties

instance FromCStruct PhysicalDeviceMaintenance6Properties


type role PushConstantsInfo nominal
data PushConstantsInfo (es :: [Type])

instance ( Extendss PushConstantsInfo es
         , PokeChain es ) => ToCStruct (PushConstantsInfo es)
instance Show (Chain es) => Show (PushConstantsInfo es)

instance ( Extendss PushConstantsInfo es
         , PeekChain es ) => FromCStruct (PushConstantsInfo es)


type role PushDescriptorSetInfo nominal
data PushDescriptorSetInfo (es :: [Type])

instance ( Extendss PushDescriptorSetInfo es
         , PokeChain es ) => ToCStruct (PushDescriptorSetInfo es)
instance Show (Chain es) => Show (PushDescriptorSetInfo es)

instance ( Extendss PushDescriptorSetInfo es
         , PeekChain es ) => FromCStruct (PushDescriptorSetInfo es)


type role PushDescriptorSetWithTemplateInfo nominal
data PushDescriptorSetWithTemplateInfo (es :: [Type])

instance ( Extendss PushDescriptorSetWithTemplateInfo es
         , PokeChain es ) => ToCStruct (PushDescriptorSetWithTemplateInfo es)
instance Show (Chain es) => Show (PushDescriptorSetWithTemplateInfo es)

instance ( Extendss PushDescriptorSetWithTemplateInfo es
         , PeekChain es ) => FromCStruct (PushDescriptorSetWithTemplateInfo es)

