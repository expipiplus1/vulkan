{-# language CPP #-}
module Graphics.Vulkan.Core10.DescriptorSet  ( CopyDescriptorSet
                                             , DescriptorBufferInfo
                                             , DescriptorImageInfo
                                             , DescriptorPoolCreateInfo
                                             , DescriptorPoolSize
                                             , DescriptorSetAllocateInfo
                                             , DescriptorSetLayoutBinding
                                             , DescriptorSetLayoutCreateInfo
                                             , WriteDescriptorSet
                                             ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
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

instance PokeChain es => ToCStruct (DescriptorPoolCreateInfo es)
instance Show (Chain es) => Show (DescriptorPoolCreateInfo es)

instance PeekChain es => FromCStruct (DescriptorPoolCreateInfo es)


data DescriptorPoolSize

instance ToCStruct DescriptorPoolSize
instance Show DescriptorPoolSize

instance FromCStruct DescriptorPoolSize


type role DescriptorSetAllocateInfo nominal
data DescriptorSetAllocateInfo (es :: [Type])

instance PokeChain es => ToCStruct (DescriptorSetAllocateInfo es)
instance Show (Chain es) => Show (DescriptorSetAllocateInfo es)

instance PeekChain es => FromCStruct (DescriptorSetAllocateInfo es)


data DescriptorSetLayoutBinding

instance ToCStruct DescriptorSetLayoutBinding
instance Show DescriptorSetLayoutBinding

instance FromCStruct DescriptorSetLayoutBinding


type role DescriptorSetLayoutCreateInfo nominal
data DescriptorSetLayoutCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (DescriptorSetLayoutCreateInfo es)
instance Show (Chain es) => Show (DescriptorSetLayoutCreateInfo es)

instance PeekChain es => FromCStruct (DescriptorSetLayoutCreateInfo es)


type role WriteDescriptorSet nominal
data WriteDescriptorSet (es :: [Type])

instance PokeChain es => ToCStruct (WriteDescriptorSet es)
instance Show (Chain es) => Show (WriteDescriptorSet es)

instance PeekChain es => FromCStruct (WriteDescriptorSet es)

