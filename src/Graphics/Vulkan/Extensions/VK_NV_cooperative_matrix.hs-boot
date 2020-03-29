{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix  ( CooperativeMatrixPropertiesNV
                                                            , PhysicalDeviceCooperativeMatrixFeaturesNV
                                                            , PhysicalDeviceCooperativeMatrixPropertiesNV
                                                            ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data CooperativeMatrixPropertiesNV

instance ToCStruct CooperativeMatrixPropertiesNV
instance Show CooperativeMatrixPropertiesNV

instance FromCStruct CooperativeMatrixPropertiesNV


data PhysicalDeviceCooperativeMatrixFeaturesNV

instance ToCStruct PhysicalDeviceCooperativeMatrixFeaturesNV
instance Show PhysicalDeviceCooperativeMatrixFeaturesNV

instance FromCStruct PhysicalDeviceCooperativeMatrixFeaturesNV


data PhysicalDeviceCooperativeMatrixPropertiesNV

instance ToCStruct PhysicalDeviceCooperativeMatrixPropertiesNV
instance Show PhysicalDeviceCooperativeMatrixPropertiesNV

instance FromCStruct PhysicalDeviceCooperativeMatrixPropertiesNV

