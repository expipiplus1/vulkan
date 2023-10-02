{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_application_parameters"
module Vulkan.Extensions.VK_EXT_application_parameters  (ApplicationParametersEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ApplicationParametersEXT

instance ToCStruct ApplicationParametersEXT
instance Show ApplicationParametersEXT

instance FromCStruct ApplicationParametersEXT

