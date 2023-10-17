{-# language CPP #-}
-- | = Name
--
-- VK_KHR_ray_tracing_position_fetch - device extension
--
-- == VK_KHR_ray_tracing_position_fetch
--
-- [__Name String__]
--     @VK_KHR_ray_tracing_position_fetch@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     482
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
--
-- [__Contact__]
--
--     -   Eric Werness
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_ray_tracing_position_fetch.adoc VK_KHR_ray_tracing_position_fetch>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-02-17
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_ray_tracing_position_fetch.html SPV_KHR_ray_tracing_position_fetch>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_ray_tracing_position_fetch.txt GLSL_EXT_ray_tracing_position_fetch>
--
--     -   Interacts with @VK_KHR_ray_tracing_pipeline@
--
--     -   Interacts with @VK_KHR_ray_query@
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Stu Smith, AMD
--
--     -   Yuriy O’Donnell, Epic Games
--
--     -   Ralph Potter, Samsung
--
--     -   Joshua Barczak, Intel
--
--     -   Lionel Landwerlin, Intel
--
--     -   Andrew Garrard, Imagination Technologies
--
--     -   Alex Bourd, Qualcomm
--
--     -   Yunpeng Zhu, Huawei Technologies
--
--     -   Marius Bjorge, Arm
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- @VK_KHR_ray_tracing_position_fetch@ adds the ability to fetch the vertex
-- positions in the shader from a hit triangle as stored in the
-- acceleration structure.
--
-- An application adds
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_DATA_ACCESS_KHR'
-- to the acceleration structure at build time. Then, if the hit is a
-- triangle geometry, the shader (any-hit or closest hit for ray pipelines
-- or using ray query) /can/ fetch the three, three-component vertex
-- positions in object space, of the triangle which was hit.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingPositionFetchFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_RAY_TRACING_POSITION_FETCH_EXTENSION_NAME'
--
-- -   'KHR_RAY_TRACING_POSITION_FETCH_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_DATA_ACCESS_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_POSITION_FETCH_FEATURES_KHR'
--
-- == New Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-hittrianglevertexpositions HitTriangleVertexPositionsKHR>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-RayTracingPositionFetchKHR RayTracingPositionFetchKHR>
--
-- == Issues
--
-- None Yet!
--
-- == Version History
--
-- -   Revision 1, 2023-02-17 (Eric Werness)
--
--     -   internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceRayTracingPositionFetchFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_ray_tracing_position_fetch Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_ray_tracing_position_fetch  ( PhysicalDeviceRayTracingPositionFetchFeaturesKHR(..)
                                                            , KHR_RAY_TRACING_POSITION_FETCH_SPEC_VERSION
                                                            , pattern KHR_RAY_TRACING_POSITION_FETCH_SPEC_VERSION
                                                            , KHR_RAY_TRACING_POSITION_FETCH_EXTENSION_NAME
                                                            , pattern KHR_RAY_TRACING_POSITION_FETCH_EXTENSION_NAME
                                                            , BuildAccelerationStructureFlagBitsKHR(..)
                                                            , BuildAccelerationStructureFlagsKHR
                                                            ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_POSITION_FETCH_FEATURES_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
-- | VkPhysicalDeviceRayTracingPositionFetchFeaturesKHR - Structure
-- describing support for fetching vertex positions of hit triangles
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingPositionFetchFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRayTracingPositionFetchFeaturesKHR' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_position_fetch VK_KHR_ray_tracing_position_fetch>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingPositionFetchFeaturesKHR = PhysicalDeviceRayTracingPositionFetchFeaturesKHR
  { -- | #features-rayTracingPositionFetch# @rayTracingPositionFetch@ indicates
    -- that the implementation supports fetching the object space vertex
    -- positions of a hit triangle.
    rayTracingPositionFetch :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingPositionFetchFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceRayTracingPositionFetchFeaturesKHR

instance ToCStruct PhysicalDeviceRayTracingPositionFetchFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingPositionFetchFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_POSITION_FETCH_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayTracingPositionFetch))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_POSITION_FETCH_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingPositionFetchFeaturesKHR where
  peekCStruct p = do
    rayTracingPositionFetch <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingPositionFetchFeaturesKHR
             (bool32ToBool rayTracingPositionFetch)

instance Storable PhysicalDeviceRayTracingPositionFetchFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingPositionFetchFeaturesKHR where
  zero = PhysicalDeviceRayTracingPositionFetchFeaturesKHR
           zero


type KHR_RAY_TRACING_POSITION_FETCH_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_POSITION_FETCH_SPEC_VERSION"
pattern KHR_RAY_TRACING_POSITION_FETCH_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_RAY_TRACING_POSITION_FETCH_SPEC_VERSION = 1


type KHR_RAY_TRACING_POSITION_FETCH_EXTENSION_NAME = "VK_KHR_ray_tracing_position_fetch"

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_POSITION_FETCH_EXTENSION_NAME"
pattern KHR_RAY_TRACING_POSITION_FETCH_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_RAY_TRACING_POSITION_FETCH_EXTENSION_NAME = "VK_KHR_ray_tracing_position_fetch"

