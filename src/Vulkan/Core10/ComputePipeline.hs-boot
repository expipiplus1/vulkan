{-# language CPP #-}
-- No documentation found for Chapter "ComputePipeline"
module Vulkan.Core10.ComputePipeline  ( ComputePipelineCreateInfo
                                      , PipelineShaderStageCreateInfo
                                      , SpecializationInfo
                                      , SpecializationMapEntry
                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role ComputePipelineCreateInfo nominal
data ComputePipelineCreateInfo (es :: [Type])

instance ( Extendss ComputePipelineCreateInfo es
         , PokeChain es ) => ToCStruct (ComputePipelineCreateInfo es)
instance Show (Chain es) => Show (ComputePipelineCreateInfo es)

instance ( Extendss ComputePipelineCreateInfo es
         , PeekChain es ) => FromCStruct (ComputePipelineCreateInfo es)


type role PipelineShaderStageCreateInfo nominal
data PipelineShaderStageCreateInfo (es :: [Type])

instance ( Extendss PipelineShaderStageCreateInfo es
         , PokeChain es ) => ToCStruct (PipelineShaderStageCreateInfo es)
instance Show (Chain es) => Show (PipelineShaderStageCreateInfo es)

instance ( Extendss PipelineShaderStageCreateInfo es
         , PeekChain es ) => FromCStruct (PipelineShaderStageCreateInfo es)


data SpecializationInfo

instance ToCStruct SpecializationInfo
instance Show SpecializationInfo

instance FromCStruct SpecializationInfo


data SpecializationMapEntry

instance ToCStruct SpecializationMapEntry
instance Show SpecializationMapEntry

instance FromCStruct SpecializationMapEntry

