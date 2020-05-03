{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_validation_flags  (ValidationFlagsEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ValidationFlagsEXT

instance ToCStruct ValidationFlagsEXT
instance Show ValidationFlagsEXT

instance FromCStruct ValidationFlagsEXT

