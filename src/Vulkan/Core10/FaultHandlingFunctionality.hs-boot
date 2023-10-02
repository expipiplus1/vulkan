{-# language CPP #-}
-- No documentation found for Chapter "FaultHandlingFunctionality"
module Vulkan.Core10.FaultHandlingFunctionality  ( FaultCallbackInfo
                                                 , FaultData
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data FaultCallbackInfo

instance ToCStruct FaultCallbackInfo
instance Show FaultCallbackInfo

instance FromCStruct FaultCallbackInfo


data FaultData

instance ToCStruct FaultData
instance Show FaultData

instance FromCStruct FaultData

