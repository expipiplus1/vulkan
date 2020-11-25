{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_mesh_shader"
module Vulkan.Extensions.VK_NV_mesh_shader  ( DrawMeshTasksIndirectCommandNV
                                            , PhysicalDeviceMeshShaderFeaturesNV
                                            , PhysicalDeviceMeshShaderPropertiesNV
                                            ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DrawMeshTasksIndirectCommandNV

instance ToCStruct DrawMeshTasksIndirectCommandNV
instance Show DrawMeshTasksIndirectCommandNV

instance FromCStruct DrawMeshTasksIndirectCommandNV


data PhysicalDeviceMeshShaderFeaturesNV

instance ToCStruct PhysicalDeviceMeshShaderFeaturesNV
instance Show PhysicalDeviceMeshShaderFeaturesNV

instance FromCStruct PhysicalDeviceMeshShaderFeaturesNV


data PhysicalDeviceMeshShaderPropertiesNV

instance ToCStruct PhysicalDeviceMeshShaderPropertiesNV
instance Show PhysicalDeviceMeshShaderPropertiesNV

instance FromCStruct PhysicalDeviceMeshShaderPropertiesNV

