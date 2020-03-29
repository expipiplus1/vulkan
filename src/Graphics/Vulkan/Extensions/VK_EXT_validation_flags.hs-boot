{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_validation_flags  (ValidationFlagsEXT) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ValidationFlagsEXT

instance ToCStruct ValidationFlagsEXT
instance Show ValidationFlagsEXT

instance FromCStruct ValidationFlagsEXT

