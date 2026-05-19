{-# language CPP #-}
-- No documentation found for Chapter "OtherTypes"
module Vulkan.Core10.OtherTypes  ( DispatchIndirectCommand
                                 , DrawIndexedIndirectCommand
                                 , DrawIndirectCommand
                                 , PipelineCacheHeaderVersionOne
                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DispatchIndirectCommand

instance ToCStruct DispatchIndirectCommand
instance Show DispatchIndirectCommand

instance FromCStruct DispatchIndirectCommand


data DrawIndexedIndirectCommand

instance ToCStruct DrawIndexedIndirectCommand
instance Show DrawIndexedIndirectCommand

instance FromCStruct DrawIndexedIndirectCommand


data DrawIndirectCommand

instance ToCStruct DrawIndirectCommand
instance Show DrawIndirectCommand

instance FromCStruct DrawIndirectCommand


data PipelineCacheHeaderVersionOne

instance ToCStruct PipelineCacheHeaderVersionOne
instance Show PipelineCacheHeaderVersionOne

instance FromCStruct PipelineCacheHeaderVersionOne

