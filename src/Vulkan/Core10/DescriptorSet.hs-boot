{-# language CPP #-}
-- No documentation found for Chapter "DescriptorSet"
module Vulkan.Core10.DescriptorSet  ( CopyDescriptorSet
                                    , DescriptorBufferInfo
                                    , DescriptorImageInfo
                                    , DescriptorPoolCreateInfo
                                    , DescriptorPoolSize
                                    , DescriptorSetAllocateInfo
                                    , DescriptorSetLayoutBinding
                                    , DescriptorSetLayoutCreateInfo
                                    , WriteDescriptorSet
                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data CopyDescriptorSet

instance ToCStruct CopyDescriptorSet
instance Show CopyDescriptorSet

instance FromCStruct CopyDescriptorSet


data DescriptorBufferInfo

instance ToCStruct DescriptorBufferInfo
instance Show DescriptorBufferInfo

instance FromCStruct DescriptorBufferInfo


data DescriptorImageInfo

instance ToCStruct DescriptorImageInfo
instance Show DescriptorImageInfo

instance FromCStruct DescriptorImageInfo


type role DescriptorPoolCreateInfo nominal
data DescriptorPoolCreateInfo (es :: [Type])

instance ( Extendss DescriptorPoolCreateInfo es
         , PokeChain es ) => ToCStruct (DescriptorPoolCreateInfo es)
instance Show (Chain es) => Show (DescriptorPoolCreateInfo es)

instance ( Extendss DescriptorPoolCreateInfo es
         , PeekChain es ) => FromCStruct (DescriptorPoolCreateInfo es)


data DescriptorPoolSize

instance ToCStruct DescriptorPoolSize
instance Show DescriptorPoolSize

instance FromCStruct DescriptorPoolSize


type role DescriptorSetAllocateInfo nominal
data DescriptorSetAllocateInfo (es :: [Type])

instance ( Extendss DescriptorSetAllocateInfo es
         , PokeChain es ) => ToCStruct (DescriptorSetAllocateInfo es)
instance Show (Chain es) => Show (DescriptorSetAllocateInfo es)

instance ( Extendss DescriptorSetAllocateInfo es
         , PeekChain es ) => FromCStruct (DescriptorSetAllocateInfo es)


data DescriptorSetLayoutBinding

instance ToCStruct DescriptorSetLayoutBinding
instance Show DescriptorSetLayoutBinding

instance FromCStruct DescriptorSetLayoutBinding


type role DescriptorSetLayoutCreateInfo nominal
data DescriptorSetLayoutCreateInfo (es :: [Type])

instance ( Extendss DescriptorSetLayoutCreateInfo es
         , PokeChain es ) => ToCStruct (DescriptorSetLayoutCreateInfo es)
instance Show (Chain es) => Show (DescriptorSetLayoutCreateInfo es)

instance ( Extendss DescriptorSetLayoutCreateInfo es
         , PeekChain es ) => FromCStruct (DescriptorSetLayoutCreateInfo es)


type role WriteDescriptorSet nominal
data WriteDescriptorSet (es :: [Type])

instance ( Extendss WriteDescriptorSet es
         , PokeChain es ) => ToCStruct (WriteDescriptorSet es)
instance Show (Chain es) => Show (WriteDescriptorSet es)

instance ( Extendss WriteDescriptorSet es
         , PeekChain es ) => FromCStruct (WriteDescriptorSet es)

