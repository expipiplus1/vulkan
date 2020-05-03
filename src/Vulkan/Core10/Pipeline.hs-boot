{-# language CPP #-}
module Vulkan.Core10.Pipeline  ( ComputePipelineCreateInfo
                               , GraphicsPipelineCreateInfo
                               , PipelineColorBlendAttachmentState
                               , PipelineColorBlendStateCreateInfo
                               , PipelineDepthStencilStateCreateInfo
                               , PipelineDynamicStateCreateInfo
                               , PipelineInputAssemblyStateCreateInfo
                               , PipelineMultisampleStateCreateInfo
                               , PipelineRasterizationStateCreateInfo
                               , PipelineShaderStageCreateInfo
                               , PipelineTessellationStateCreateInfo
                               , PipelineVertexInputStateCreateInfo
                               , PipelineViewportStateCreateInfo
                               , SpecializationInfo
                               , SpecializationMapEntry
                               , StencilOpState
                               , VertexInputAttributeDescription
                               , VertexInputBindingDescription
                               ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role ComputePipelineCreateInfo nominal
data ComputePipelineCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (ComputePipelineCreateInfo es)
instance Show (Chain es) => Show (ComputePipelineCreateInfo es)

instance PeekChain es => FromCStruct (ComputePipelineCreateInfo es)


type role GraphicsPipelineCreateInfo nominal
data GraphicsPipelineCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (GraphicsPipelineCreateInfo es)
instance Show (Chain es) => Show (GraphicsPipelineCreateInfo es)

instance PeekChain es => FromCStruct (GraphicsPipelineCreateInfo es)


data PipelineColorBlendAttachmentState

instance ToCStruct PipelineColorBlendAttachmentState
instance Show PipelineColorBlendAttachmentState

instance FromCStruct PipelineColorBlendAttachmentState


type role PipelineColorBlendStateCreateInfo nominal
data PipelineColorBlendStateCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (PipelineColorBlendStateCreateInfo es)
instance Show (Chain es) => Show (PipelineColorBlendStateCreateInfo es)

instance PeekChain es => FromCStruct (PipelineColorBlendStateCreateInfo es)


data PipelineDepthStencilStateCreateInfo

instance ToCStruct PipelineDepthStencilStateCreateInfo
instance Show PipelineDepthStencilStateCreateInfo

instance FromCStruct PipelineDepthStencilStateCreateInfo


data PipelineDynamicStateCreateInfo

instance ToCStruct PipelineDynamicStateCreateInfo
instance Show PipelineDynamicStateCreateInfo

instance FromCStruct PipelineDynamicStateCreateInfo


data PipelineInputAssemblyStateCreateInfo

instance ToCStruct PipelineInputAssemblyStateCreateInfo
instance Show PipelineInputAssemblyStateCreateInfo

instance FromCStruct PipelineInputAssemblyStateCreateInfo


type role PipelineMultisampleStateCreateInfo nominal
data PipelineMultisampleStateCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (PipelineMultisampleStateCreateInfo es)
instance Show (Chain es) => Show (PipelineMultisampleStateCreateInfo es)

instance PeekChain es => FromCStruct (PipelineMultisampleStateCreateInfo es)


type role PipelineRasterizationStateCreateInfo nominal
data PipelineRasterizationStateCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (PipelineRasterizationStateCreateInfo es)
instance Show (Chain es) => Show (PipelineRasterizationStateCreateInfo es)

instance PeekChain es => FromCStruct (PipelineRasterizationStateCreateInfo es)


type role PipelineShaderStageCreateInfo nominal
data PipelineShaderStageCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (PipelineShaderStageCreateInfo es)
instance Show (Chain es) => Show (PipelineShaderStageCreateInfo es)

instance PeekChain es => FromCStruct (PipelineShaderStageCreateInfo es)


type role PipelineTessellationStateCreateInfo nominal
data PipelineTessellationStateCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (PipelineTessellationStateCreateInfo es)
instance Show (Chain es) => Show (PipelineTessellationStateCreateInfo es)

instance PeekChain es => FromCStruct (PipelineTessellationStateCreateInfo es)


type role PipelineVertexInputStateCreateInfo nominal
data PipelineVertexInputStateCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (PipelineVertexInputStateCreateInfo es)
instance Show (Chain es) => Show (PipelineVertexInputStateCreateInfo es)

instance PeekChain es => FromCStruct (PipelineVertexInputStateCreateInfo es)


type role PipelineViewportStateCreateInfo nominal
data PipelineViewportStateCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (PipelineViewportStateCreateInfo es)
instance Show (Chain es) => Show (PipelineViewportStateCreateInfo es)

instance PeekChain es => FromCStruct (PipelineViewportStateCreateInfo es)


data SpecializationInfo

instance ToCStruct SpecializationInfo
instance Show SpecializationInfo

instance FromCStruct SpecializationInfo


data SpecializationMapEntry

instance ToCStruct SpecializationMapEntry
instance Show SpecializationMapEntry

instance FromCStruct SpecializationMapEntry


data StencilOpState

instance ToCStruct StencilOpState
instance Show StencilOpState

instance FromCStruct StencilOpState


data VertexInputAttributeDescription

instance ToCStruct VertexInputAttributeDescription
instance Show VertexInputAttributeDescription

instance FromCStruct VertexInputAttributeDescription


data VertexInputBindingDescription

instance ToCStruct VertexInputBindingDescription
instance Show VertexInputBindingDescription

instance FromCStruct VertexInputBindingDescription

