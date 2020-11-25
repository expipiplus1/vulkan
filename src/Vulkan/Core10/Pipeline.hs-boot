{-# language CPP #-}
-- No documentation found for Chapter "Pipeline"
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
                               , Viewport
                               ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role ComputePipelineCreateInfo nominal
data ComputePipelineCreateInfo (es :: [Type])

instance (Extendss ComputePipelineCreateInfo es, PokeChain es) => ToCStruct (ComputePipelineCreateInfo es)
instance Show (Chain es) => Show (ComputePipelineCreateInfo es)

instance (Extendss ComputePipelineCreateInfo es, PeekChain es) => FromCStruct (ComputePipelineCreateInfo es)


type role GraphicsPipelineCreateInfo nominal
data GraphicsPipelineCreateInfo (es :: [Type])

instance (Extendss GraphicsPipelineCreateInfo es, PokeChain es) => ToCStruct (GraphicsPipelineCreateInfo es)
instance Show (Chain es) => Show (GraphicsPipelineCreateInfo es)

instance (Extendss GraphicsPipelineCreateInfo es, PeekChain es) => FromCStruct (GraphicsPipelineCreateInfo es)


data PipelineColorBlendAttachmentState

instance ToCStruct PipelineColorBlendAttachmentState
instance Show PipelineColorBlendAttachmentState

instance FromCStruct PipelineColorBlendAttachmentState


type role PipelineColorBlendStateCreateInfo nominal
data PipelineColorBlendStateCreateInfo (es :: [Type])

instance (Extendss PipelineColorBlendStateCreateInfo es, PokeChain es) => ToCStruct (PipelineColorBlendStateCreateInfo es)
instance Show (Chain es) => Show (PipelineColorBlendStateCreateInfo es)

instance (Extendss PipelineColorBlendStateCreateInfo es, PeekChain es) => FromCStruct (PipelineColorBlendStateCreateInfo es)


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

instance (Extendss PipelineMultisampleStateCreateInfo es, PokeChain es) => ToCStruct (PipelineMultisampleStateCreateInfo es)
instance Show (Chain es) => Show (PipelineMultisampleStateCreateInfo es)

instance (Extendss PipelineMultisampleStateCreateInfo es, PeekChain es) => FromCStruct (PipelineMultisampleStateCreateInfo es)


type role PipelineRasterizationStateCreateInfo nominal
data PipelineRasterizationStateCreateInfo (es :: [Type])

instance (Extendss PipelineRasterizationStateCreateInfo es, PokeChain es) => ToCStruct (PipelineRasterizationStateCreateInfo es)
instance Show (Chain es) => Show (PipelineRasterizationStateCreateInfo es)

instance (Extendss PipelineRasterizationStateCreateInfo es, PeekChain es) => FromCStruct (PipelineRasterizationStateCreateInfo es)


type role PipelineShaderStageCreateInfo nominal
data PipelineShaderStageCreateInfo (es :: [Type])

instance (Extendss PipelineShaderStageCreateInfo es, PokeChain es) => ToCStruct (PipelineShaderStageCreateInfo es)
instance Show (Chain es) => Show (PipelineShaderStageCreateInfo es)

instance (Extendss PipelineShaderStageCreateInfo es, PeekChain es) => FromCStruct (PipelineShaderStageCreateInfo es)


type role PipelineTessellationStateCreateInfo nominal
data PipelineTessellationStateCreateInfo (es :: [Type])

instance (Extendss PipelineTessellationStateCreateInfo es, PokeChain es) => ToCStruct (PipelineTessellationStateCreateInfo es)
instance Show (Chain es) => Show (PipelineTessellationStateCreateInfo es)

instance (Extendss PipelineTessellationStateCreateInfo es, PeekChain es) => FromCStruct (PipelineTessellationStateCreateInfo es)


type role PipelineVertexInputStateCreateInfo nominal
data PipelineVertexInputStateCreateInfo (es :: [Type])

instance (Extendss PipelineVertexInputStateCreateInfo es, PokeChain es) => ToCStruct (PipelineVertexInputStateCreateInfo es)
instance Show (Chain es) => Show (PipelineVertexInputStateCreateInfo es)

instance (Extendss PipelineVertexInputStateCreateInfo es, PeekChain es) => FromCStruct (PipelineVertexInputStateCreateInfo es)


type role PipelineViewportStateCreateInfo nominal
data PipelineViewportStateCreateInfo (es :: [Type])

instance (Extendss PipelineViewportStateCreateInfo es, PokeChain es) => ToCStruct (PipelineViewportStateCreateInfo es)
instance Show (Chain es) => Show (PipelineViewportStateCreateInfo es)

instance (Extendss PipelineViewportStateCreateInfo es, PeekChain es) => FromCStruct (PipelineViewportStateCreateInfo es)


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


data Viewport

instance ToCStruct Viewport
instance Show Viewport

instance FromCStruct Viewport

