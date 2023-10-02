{-# language CPP #-}
-- No documentation found for Chapter "StaticMemoryFunctionality"
module Vulkan.Core10.StaticMemoryFunctionality  ( CommandPoolMemoryConsumption
                                                , CommandPoolMemoryReservationCreateInfo
                                                , DeviceObjectReservationCreateInfo
                                                , PipelinePoolSize
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data CommandPoolMemoryConsumption

instance ToCStruct CommandPoolMemoryConsumption
instance Show CommandPoolMemoryConsumption

instance FromCStruct CommandPoolMemoryConsumption


data CommandPoolMemoryReservationCreateInfo

instance ToCStruct CommandPoolMemoryReservationCreateInfo
instance Show CommandPoolMemoryReservationCreateInfo

instance FromCStruct CommandPoolMemoryReservationCreateInfo


type role DeviceObjectReservationCreateInfo nominal
data DeviceObjectReservationCreateInfo (es :: [Type])

instance ( Extendss DeviceObjectReservationCreateInfo es
         , PokeChain es ) => ToCStruct (DeviceObjectReservationCreateInfo es)
instance Show (Chain es) => Show (DeviceObjectReservationCreateInfo es)

instance ( Extendss DeviceObjectReservationCreateInfo es
         , PeekChain es ) => FromCStruct (DeviceObjectReservationCreateInfo es)


data PipelinePoolSize

instance ToCStruct PipelinePoolSize
instance Show PipelinePoolSize

instance FromCStruct PipelinePoolSize

