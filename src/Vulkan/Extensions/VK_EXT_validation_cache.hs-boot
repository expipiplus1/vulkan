{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_validation_cache  ( ShaderModuleValidationCacheCreateInfoEXT
                                                  , ValidationCacheCreateInfoEXT
                                                  ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ShaderModuleValidationCacheCreateInfoEXT

instance ToCStruct ShaderModuleValidationCacheCreateInfoEXT
instance Show ShaderModuleValidationCacheCreateInfoEXT

instance FromCStruct ShaderModuleValidationCacheCreateInfoEXT


data ValidationCacheCreateInfoEXT

instance ToCStruct ValidationCacheCreateInfoEXT
instance Show ValidationCacheCreateInfoEXT

instance FromCStruct ValidationCacheCreateInfoEXT

