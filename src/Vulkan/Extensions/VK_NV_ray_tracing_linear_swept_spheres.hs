{-# language CPP #-}
-- | = Name
--
-- VK_NV_ray_tracing_linear_swept_spheres - device extension
--
-- = VK_NV_ray_tracing_linear_swept_spheres
--
-- [__Name String__]
--     @VK_NV_ray_tracing_linear_swept_spheres@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     430
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_linear_swept_spheres.html SPV_NV_linear_swept_spheres>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_ray_tracing_linear_swept_spheres] @vkushwaha%0A*Here describe the issue or question you have about the VK_NV_ray_tracing_linear_swept_spheres extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_ray_tracing_linear_swept_spheres.adoc VK_NV_ray_tracing_linear_swept_spheres>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-03
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_linear_swept_spheres.html SPV_NV_linear_swept_spheres>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_linear_swept_spheres.txt GL_NV_linear_swept_spheres>
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
--     -   Nathan Morrical, NVIDIA
--
-- == Description
--
-- This extension adds two new primitives for ray tracing: a sphere
-- primitive and a linear swept sphere (LSS) primitive. The purpose of the
-- LSS primitive is to enable rendering of high quality hair and fur using
-- a compact primitive representation encoded in the acceleration
-- structure. Sphere primitives are defined by a position and a radius and
-- are a subset of LSS, but are useful in their own right, for example for
-- particle systems.
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_NV_linear_swept_spheres@
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryKHR':
--
--     -   'AccelerationStructureGeometryLinearSweptSpheresDataNV'
--
--     -   'AccelerationStructureGeometrySpheresDataNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV'
--
-- == New Enums
--
-- -   'RayTracingLssIndexingModeNV'
--
-- -   'RayTracingLssPrimitiveEndCapsModeNV'
--
-- == New Enum Constants
--
-- -   'NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_EXTENSION_NAME'
--
-- -   'NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_RADIUS_BUFFER_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryTypeKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_LINEAR_SWEPT_SPHERES_NV'
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_SPHERES_NV'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_RAY_TRACING_ALLOW_SPHERES_AND_LINEAR_SWEPT_SPHERES_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_LINEAR_SWEPT_SPHERES_DATA_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_SPHERES_DATA_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_LINEAR_SWEPT_SPHERES_FEATURES_NV'
--
-- == New or Modified Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitissphere HitIsSphereNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitislss HitIsLSSNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitsphereposition HitSpherePositionNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitsphereradius HitSphereRadiusNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitlsspositions HitLSSPositionsNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitlssradii HitLSSRadiiNV>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-RayTracingSpheresGeometryNV RayTracingSpheresGeometryNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-RayTracingLinearSweptSpheresGeometryNV RayTracingLinearSweptSpheresGeometryNV>
--
-- == Version History
--
-- -   Revision 1, 2025-01-03 (Vikram Kushwaha)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_ray_tracing_linear_swept_spheres Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_ray_tracing_linear_swept_spheres  ( AccelerationStructureGeometryLinearSweptSpheresDataNV(..)
                                                                 , AccelerationStructureGeometrySpheresDataNV(..)
                                                                 , PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV(..)
                                                                 , RayTracingLssIndexingModeNV( RAY_TRACING_LSS_INDEXING_MODE_LIST_NV
                                                                                              , RAY_TRACING_LSS_INDEXING_MODE_SUCCESSIVE_NV
                                                                                              , ..
                                                                                              )
                                                                 , RayTracingLssPrimitiveEndCapsModeNV( RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_NONE_NV
                                                                                                      , RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_CHAINED_NV
                                                                                                      , ..
                                                                                                      )
                                                                 , NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_SPEC_VERSION
                                                                 , pattern NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_SPEC_VERSION
                                                                 , NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_EXTENSION_NAME
                                                                 , pattern NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_EXTENSION_NAME
                                                                 , DeviceOrHostAddressConstKHR(..)
                                                                 , GeometryTypeKHR(..)
                                                                 ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_LINEAR_SWEPT_SPHERES_DATA_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_SPHERES_DATA_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_LINEAR_SWEPT_SPHERES_FEATURES_NV))
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR(..))
-- | VkAccelerationStructureGeometryLinearSweptSpheresDataNV - Structure
-- specifying a LSS geometry in a bottom-level acceleration structure
--
-- = Description
--
-- If an index buffer is not specified in @indexData@, LSS primitives are
-- rendered individually using subsequent pairs of vertices similar to
-- 'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST'.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-None-10419#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-linearSweptSpheres linearSweptSpheres>
--     feature /must/ be enabled
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-vertexStride-10421#
--     @vertexStride@ /must/ be a multiple of:
--
--     -   the
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats size of the format>
--         specified in @vertexFormat@ if that format is a
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-packed packed format>
--
--     -   the
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats component size>
--         specified in @vertexFormat@ if that format is not a
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-packed packed format>
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-vertexStride-10422#
--     @vertexStride@ and @radiusStride@ /must/ be less than or equal to
--     232-1
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-vertexFormat-10423#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-buffer-view-format-features format features>
--     of @vertexFormat@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR'
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-radiusFormat-10424#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-buffer-view-format-features format features>
--     of @radiusFormat@ /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_RADIUS_BUFFER_BIT_NV'
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-radiusData-10426#
--     All values referenced in @radiusData@ /must/ be greater than or
--     equal to @0@
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-indexingMode-10427#
--     If @indexingMode@ is 'RAY_TRACING_LSS_INDEXING_MODE_SUCCESSIVE_NV',
--     @indexData@ /must/ not be @NULL@
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-indexData-10428#
--     @indexType@ /must/ be
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16',
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32', or
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_LINEAR_SWEPT_SPHERES_DATA_NV'
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-vertexFormat-parameter#
--     @vertexFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-vertexData-parameter#
--     @vertexData@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--     union
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-radiusFormat-parameter#
--     @radiusFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-radiusData-parameter#
--     @radiusData@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--     union
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-indexType-parameter#
--     @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-indexData-parameter#
--     @indexData@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--     union
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-indexingMode-parameter#
--     @indexingMode@ /must/ be a valid 'RayTracingLssIndexingModeNV' value
--
-- -   #VUID-VkAccelerationStructureGeometryLinearSweptSpheresDataNV-endCapsMode-parameter#
--     @endCapsMode@ /must/ be a valid
--     'RayTracingLssPrimitiveEndCapsModeNV' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_linear_swept_spheres VK_NV_ray_tracing_linear_swept_spheres>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'RayTracingLssIndexingModeNV', 'RayTracingLssPrimitiveEndCapsModeNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryLinearSweptSpheresDataNV = AccelerationStructureGeometryLinearSweptSpheresDataNV
  { -- | @vertexFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of each LSS
    -- vertex element.
    vertexFormat :: Format
  , -- | @vertexData@ is a device or host address of memory containing vertex
    -- data for this geometry.
    vertexData :: DeviceOrHostAddressConstKHR
  , -- | @vertexStride@ is the stride in bytes between each vertex element.
    vertexStride :: DeviceSize
  , -- | @radiusFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of each LSS
    -- radius.
    radiusFormat :: Format
  , -- | @radiusData@ is a device or host address of memory containing LSS radius
    -- data value.
    radiusData :: DeviceOrHostAddressConstKHR
  , -- | @radiusStride@ is the stride in bytes between each radius value.
    radiusStride :: DeviceSize
  , -- | @indexType@ is the 'Vulkan.Core10.Enums.IndexType.IndexType' of each
    -- index element.
    indexType :: IndexType
  , -- | @indexData@ is a device or host address of memory containing index data
    -- for vertex and radius buffers for this geometry. When @indexType@ is
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR' it /must/ be @NULL@.
    indexData :: DeviceOrHostAddressConstKHR
  , -- | @indexStride@ is the stride in bytes between each index element.
    indexStride :: DeviceSize
  , -- | @indexingMode@ is a 'RayTracingLssIndexingModeNV' value specifying the
    -- mode of indexing.
    indexingMode :: RayTracingLssIndexingModeNV
  , -- | @endCapsMode@ is a 'RayTracingLssPrimitiveEndCapsModeNV' value
    -- specifying the endcaps mode for LSS primitives.
    endCapsMode :: RayTracingLssPrimitiveEndCapsModeNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryLinearSweptSpheresDataNV)
#endif
deriving instance Show AccelerationStructureGeometryLinearSweptSpheresDataNV

instance ToCStruct AccelerationStructureGeometryLinearSweptSpheresDataNV where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryLinearSweptSpheresDataNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_LINEAR_SWEPT_SPHERES_DATA_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (vertexFormat)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (vertexData) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (vertexStride)
    lift $ poke ((p `plusPtr` 40 :: Ptr Format)) (radiusFormat)
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr DeviceOrHostAddressConstKHR)) (radiusData) . ($ ())
    lift $ poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (radiusStride)
    lift $ poke ((p `plusPtr` 64 :: Ptr IndexType)) (indexType)
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressConstKHR)) (indexData) . ($ ())
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (indexStride)
    lift $ poke ((p `plusPtr` 88 :: Ptr RayTracingLssIndexingModeNV)) (indexingMode)
    lift $ poke ((p `plusPtr` 92 :: Ptr RayTracingLssPrimitiveEndCapsModeNV)) (endCapsMode)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_LINEAR_SWEPT_SPHERES_DATA_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Format)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr IndexType)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 88 :: Ptr RayTracingLssIndexingModeNV)) (zero)
    lift $ poke ((p `plusPtr` 92 :: Ptr RayTracingLssPrimitiveEndCapsModeNV)) (zero)
    lift $ f

instance Zero AccelerationStructureGeometryLinearSweptSpheresDataNV where
  zero = AccelerationStructureGeometryLinearSweptSpheresDataNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureGeometrySpheresDataNV - Structure specifying a
-- sphere geometry in a bottom-level acceleration structure
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-None-10429# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-spheres spheres>
--     feature /must/ be enabled
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-vertexStride-10431#
--     @vertexStride@ /must/ be a multiple of:
--
--     -   the
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats size of the format>
--         specified in @vertexFormat@ if that format is a
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-packed packed format>
--
--     -   the smallest
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats component size>
--         specified in @vertexFormat@ if that format is not a
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-packed packed format>
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-vertexStride-10432#
--     @vertexStride@ and @radiusStride@ /must/ be less than or equal to
--     232-1
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-vertexFormat-10434#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-buffer-view-format-features format features>
--     of @vertexFormat@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR'
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-radiusFormat-10435#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-buffer-view-format-features format features>
--     of @radiusFormat@ /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_RADIUS_BUFFER_BIT_NV'
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-radiusData-10436#
--     All values referenced in @radiusData@ /must/ be greater than or
--     equal to @0@
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-indexData-10437#
--     @indexType@ /must/ be
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16',
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32',
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_SPHERES_DATA_NV'
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-vertexFormat-parameter#
--     @vertexFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-vertexData-parameter#
--     @vertexData@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--     union
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-radiusFormat-parameter#
--     @radiusFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-radiusData-parameter#
--     @radiusData@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--     union
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-indexType-parameter#
--     @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-VkAccelerationStructureGeometrySpheresDataNV-indexData-parameter#
--     @indexData@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--     union
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_linear_swept_spheres VK_NV_ray_tracing_linear_swept_spheres>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometrySpheresDataNV = AccelerationStructureGeometrySpheresDataNV
  { -- | @vertexFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of each
    -- sphere’s vertex element.
    vertexFormat :: Format
  , -- | @vertexData@ is a device or host address of memory containing vertex
    -- data in form of pairs of centers of spheres that define all sphere
    -- geometry.
    vertexData :: DeviceOrHostAddressConstKHR
  , -- | @vertexStride@ is the stride in bytes between each vertex element.
    vertexStride :: DeviceSize
  , -- | @radiusFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of each
    -- sphere’s radius.
    radiusFormat :: Format
  , -- | @radiusData@ is a device or host address of memory containing sphere’s
    -- radius data value.
    radiusData :: DeviceOrHostAddressConstKHR
  , -- | @radiusStride@ is the stride in bytes between each radius value.
    radiusStride :: DeviceSize
  , -- | @indexType@ is the 'Vulkan.Core10.Enums.IndexType.IndexType' of each
    -- index element.
    indexType :: IndexType
  , -- | @indexData@ is a device or host address of memory containing index data
    -- for vertex and radius buffers for this geometry. When @indexType@ is
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR' it /must/ be @NULL@.
    indexData :: DeviceOrHostAddressConstKHR
  , -- | @indexStride@ is the stride in bytes between each index element.
    indexStride :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometrySpheresDataNV)
#endif
deriving instance Show AccelerationStructureGeometrySpheresDataNV

instance ToCStruct AccelerationStructureGeometrySpheresDataNV where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometrySpheresDataNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_SPHERES_DATA_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (vertexFormat)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (vertexData) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (vertexStride)
    lift $ poke ((p `plusPtr` 40 :: Ptr Format)) (radiusFormat)
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr DeviceOrHostAddressConstKHR)) (radiusData) . ($ ())
    lift $ poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (radiusStride)
    lift $ poke ((p `plusPtr` 64 :: Ptr IndexType)) (indexType)
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressConstKHR)) (indexData) . ($ ())
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (indexStride)
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_SPHERES_DATA_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Format)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr IndexType)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (zero)
    lift $ f

instance Zero AccelerationStructureGeometrySpheresDataNV where
  zero = AccelerationStructureGeometrySpheresDataNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV - Structure
-- describing the ray tracing linear swept spheres features that can be
-- supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_linear_swept_spheres VK_NV_ray_tracing_linear_swept_spheres>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV = PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV
  { -- | #features-spheres# @spheres@ indicates whether the implementation
    -- supports sphere primitives in ray tracing.
    spheres :: Bool
  , -- | #features-linearSweptSpheres# @linearSweptSpheres@ indicates whether the
    -- implementation supports linear swept sphere primitives in ray tracing.
    linearSweptSpheres :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV)
#endif
deriving instance Show PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV

instance ToCStruct PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_LINEAR_SWEPT_SPHERES_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (spheres))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (linearSweptSpheres))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_LINEAR_SWEPT_SPHERES_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV where
  peekCStruct p = do
    spheres <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    linearSweptSpheres <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV
             (bool32ToBool spheres) (bool32ToBool linearSweptSpheres)

instance Storable PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV where
  zero = PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV
           zero
           zero


-- | VkRayTracingLssIndexingModeNV - LSS indexing mode
--
-- = Description
--
-- -   'RAY_TRACING_LSS_INDEXING_MODE_LIST_NV' specifies that a list of
--     indices is provided where each consecutive pair of indices define a
--     LSS primitive.
--
-- -   'RAY_TRACING_LSS_INDEXING_MODE_SUCCESSIVE_NV' specifies a successive
--     implicit indexing format, in which each LSS primitive is defined by
--     two successive positions and radii, (k, k + 1), where k is a single
--     index provided in the index buffer. In this indexing scheme, there
--     is a 1:1 mapping between the index buffer and primitive index within
--     the geometry.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_linear_swept_spheres VK_NV_ray_tracing_linear_swept_spheres>,
-- 'AccelerationStructureGeometryLinearSweptSpheresDataNV'
newtype RayTracingLssIndexingModeNV = RayTracingLssIndexingModeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkRayTracingLssIndexingModeNV" "VK_RAY_TRACING_LSS_INDEXING_MODE_LIST_NV"
pattern RAY_TRACING_LSS_INDEXING_MODE_LIST_NV = RayTracingLssIndexingModeNV 0

-- No documentation found for Nested "VkRayTracingLssIndexingModeNV" "VK_RAY_TRACING_LSS_INDEXING_MODE_SUCCESSIVE_NV"
pattern RAY_TRACING_LSS_INDEXING_MODE_SUCCESSIVE_NV = RayTracingLssIndexingModeNV 1

{-# COMPLETE
  RAY_TRACING_LSS_INDEXING_MODE_LIST_NV
  , RAY_TRACING_LSS_INDEXING_MODE_SUCCESSIVE_NV ::
    RayTracingLssIndexingModeNV
  #-}

conNameRayTracingLssIndexingModeNV :: String
conNameRayTracingLssIndexingModeNV = "RayTracingLssIndexingModeNV"

enumPrefixRayTracingLssIndexingModeNV :: String
enumPrefixRayTracingLssIndexingModeNV = "RAY_TRACING_LSS_INDEXING_MODE_"

showTableRayTracingLssIndexingModeNV :: [(RayTracingLssIndexingModeNV, String)]
showTableRayTracingLssIndexingModeNV =
  [
    ( RAY_TRACING_LSS_INDEXING_MODE_LIST_NV
    , "LIST_NV"
    )
  ,
    ( RAY_TRACING_LSS_INDEXING_MODE_SUCCESSIVE_NV
    , "SUCCESSIVE_NV"
    )
  ]

instance Show RayTracingLssIndexingModeNV where
  showsPrec =
    enumShowsPrec
      enumPrefixRayTracingLssIndexingModeNV
      showTableRayTracingLssIndexingModeNV
      conNameRayTracingLssIndexingModeNV
      (\(RayTracingLssIndexingModeNV x) -> x)
      (showsPrec 11)

instance Read RayTracingLssIndexingModeNV where
  readPrec =
    enumReadPrec
      enumPrefixRayTracingLssIndexingModeNV
      showTableRayTracingLssIndexingModeNV
      conNameRayTracingLssIndexingModeNV
      RayTracingLssIndexingModeNV

-- | VkRayTracingLssPrimitiveEndCapsModeNV - LSS endcaps mode
--
-- = Description
--
-- -   'RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_NONE_NV' disables all
--     endcaps and the chain boundaries have no influence.
--
-- -   'RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_CHAINED_NV' specifies that
--     when 'RAY_TRACING_LSS_INDEXING_MODE_SUCCESSIVE_NV' is used as
--     indexing mode for the LSS primitive, the first primitive in each
--     chain will have both endcaps enabled, and every following primitive
--     in the chain only has endcaps at the trailing position enabled.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_linear_swept_spheres VK_NV_ray_tracing_linear_swept_spheres>,
-- 'AccelerationStructureGeometryLinearSweptSpheresDataNV'
newtype RayTracingLssPrimitiveEndCapsModeNV = RayTracingLssPrimitiveEndCapsModeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkRayTracingLssPrimitiveEndCapsModeNV" "VK_RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_NONE_NV"
pattern RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_NONE_NV = RayTracingLssPrimitiveEndCapsModeNV 0

-- No documentation found for Nested "VkRayTracingLssPrimitiveEndCapsModeNV" "VK_RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_CHAINED_NV"
pattern RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_CHAINED_NV = RayTracingLssPrimitiveEndCapsModeNV 1

{-# COMPLETE
  RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_NONE_NV
  , RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_CHAINED_NV ::
    RayTracingLssPrimitiveEndCapsModeNV
  #-}

conNameRayTracingLssPrimitiveEndCapsModeNV :: String
conNameRayTracingLssPrimitiveEndCapsModeNV = "RayTracingLssPrimitiveEndCapsModeNV"

enumPrefixRayTracingLssPrimitiveEndCapsModeNV :: String
enumPrefixRayTracingLssPrimitiveEndCapsModeNV = "RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_"

showTableRayTracingLssPrimitiveEndCapsModeNV :: [(RayTracingLssPrimitiveEndCapsModeNV, String)]
showTableRayTracingLssPrimitiveEndCapsModeNV =
  [
    ( RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_NONE_NV
    , "NONE_NV"
    )
  ,
    ( RAY_TRACING_LSS_PRIMITIVE_END_CAPS_MODE_CHAINED_NV
    , "CHAINED_NV"
    )
  ]

instance Show RayTracingLssPrimitiveEndCapsModeNV where
  showsPrec =
    enumShowsPrec
      enumPrefixRayTracingLssPrimitiveEndCapsModeNV
      showTableRayTracingLssPrimitiveEndCapsModeNV
      conNameRayTracingLssPrimitiveEndCapsModeNV
      (\(RayTracingLssPrimitiveEndCapsModeNV x) -> x)
      (showsPrec 11)

instance Read RayTracingLssPrimitiveEndCapsModeNV where
  readPrec =
    enumReadPrec
      enumPrefixRayTracingLssPrimitiveEndCapsModeNV
      showTableRayTracingLssPrimitiveEndCapsModeNV
      conNameRayTracingLssPrimitiveEndCapsModeNV
      RayTracingLssPrimitiveEndCapsModeNV

type NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_SPEC_VERSION"
pattern NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_SPEC_VERSION :: forall a . Integral a => a
pattern NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_SPEC_VERSION = 1


type NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_EXTENSION_NAME = "VK_NV_ray_tracing_linear_swept_spheres"

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_EXTENSION_NAME"
pattern NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_EXTENSION_NAME = "VK_NV_ray_tracing_linear_swept_spheres"

