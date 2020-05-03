{-# language CPP #-}
module Vulkan.Extensions.VK_NV_compute_shader_derivatives  (PhysicalDeviceComputeShaderDerivativesFeaturesNV) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceComputeShaderDerivativesFeaturesNV

instance ToCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV
instance Show PhysicalDeviceComputeShaderDerivativesFeaturesNV

instance FromCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV

