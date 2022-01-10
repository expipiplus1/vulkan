{-# language CPP #-}
-- | = Name
--
-- VK_KHR_ray_query - device extension
--
-- == VK_KHR_ray_query
--
-- [__Name String__]
--     @VK_KHR_ray_query@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     349
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
--     -   Requires @VK_KHR_spirv_1_4@
--
--     -   Requires @VK_KHR_acceleration_structure@
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_ray_query] @dgkoch%0A<<Here describe the issue or question you have about the VK_KHR_ray_query extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-11-12
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_ray_query.html SPV_KHR_ray_query>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_ray_query.txt GLSL_EXT_ray_query>
--
-- [__Contributors__]
--
--     -   Matthäus Chajdas, AMD
--
--     -   Greg Grebe, AMD
--
--     -   Nicolai Hähnle, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Dave Oldcorn, AMD
--
--     -   Skyler Saleh, AMD
--
--     -   Mathieu Robart, Arm
--
--     -   Marius Bjorge, Arm
--
--     -   Tom Olson, Arm
--
--     -   Sebastian Tafuri, EA
--
--     -   Henrik Rydgard, Embark
--
--     -   Juan Cañada, Epic Games
--
--     -   Patrick Kelly, Epic Games
--
--     -   Yuriy O’Donnell, Epic Games
--
--     -   Michael Doggett, Facebook\/Oculus
--
--     -   Andrew Garrard, Imagination
--
--     -   Don Scorgie, Imagination
--
--     -   Dae Kim, Imagination
--
--     -   Joshua Barczak, Intel
--
--     -   Slawek Grajewski, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Pascal Gautron, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
--     -   Robert Stepinski, NVIDIA
--
--     -   Martin Stich, NVIDIA
--
--     -   Nuno Subtil, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Jon Leech, Khronos
--
--     -   Jeroen van Schijndel, OTOY
--
--     -   Juul Joosten, OTOY
--
--     -   Alex Bourd, Qualcomm
--
--     -   Roman Larionov, Qualcomm
--
--     -   David McAllister, Qualcomm
--
--     -   Spencer Fricke, Samsung
--
--     -   Lewis Gordon, Samsung
--
--     -   Ralph Potter, Samsung
--
--     -   Jasper Bekkers, Traverse Research
--
--     -   Jesse Barker, Unity
--
--     -   Baldur Karlsson, Valve
--
-- == Description
--
-- Rasterization has been the dominant method to produce interactive
-- graphics, but increasing performance of graphics hardware has made ray
-- tracing a viable option for interactive rendering. Being able to
-- integrate ray tracing with traditional rasterization makes it easier for
-- applications to incrementally add ray traced effects to existing
-- applications or to do hybrid approaches with rasterization for primary
-- visibility and ray tracing for secondary queries.
--
-- Ray queries are available to all shader types, including graphics,
-- compute and ray tracing pipelines. Ray queries are not able to launch
-- additional shaders, instead returning traversal results to the calling
-- shader.
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_KHR_ray_query@
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayQueryFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_RAY_QUERY_EXTENSION_NAME'
--
-- -   'KHR_RAY_QUERY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayQueryKHR RayQueryKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayTraversalPrimitiveCullingKHR RayTraversalPrimitiveCullingKHR>
--
-- == Sample Code
--
-- Example of ray query in a GLSL shader
--
-- > rayQueryEXT rq;
-- >
-- > rayQueryInitializeEXT(rq, accStruct, gl_RayFlagsNoneEXT, 0, origin, tMin, direction, tMax);
-- >
-- > while(rayQueryProceedEXT(rq)) {
-- >     if (rayQueryGetIntersectionTypeEXT(rq, false) == gl_RayQueryCandidateIntersectionTriangleEXT) {
-- >         //...
-- >         rayQueryConfirmIntersectionEXT(rq);
-- >     }
-- > }
-- >
-- > if (rayQueryGetIntersectionTypeEXT(rq, true) == gl_RayQueryCommittedIntersectionNoneEXT) {
-- >     //...
-- > }
--
-- == Issues
--
-- (1) What are the changes between the public provisional
-- (VK_KHR_ray_tracing v8) release and the final
-- (VK_KHR_acceleration_structure v11 \/ VK_KHR_ray_query v1) release?
--
-- -   refactor VK_KHR_ray_tracing into 3 extensions, enabling
--     implementation flexibility and decoupling ray query support from ray
--     pipelines:
--
--     -   @VK_KHR_acceleration_structure@ (for acceleration structure
--         operations)
--
--     -   @VK_KHR_ray_tracing_pipeline@ (for ray tracing pipeline and
--         shader stages)
--
--     -   @VK_KHR_ray_query@ (for ray queries in existing shader stages)
--
-- -   Update SPIRV capabilities to use @RayQueryKHR@
--
-- -   extension is no longer provisional
--
-- == Version History
--
-- -   Revision 1, 2020-11-12 (Mathieu Robart, Daniel Koch, Andrew Garrard)
--
--     -   Decomposition of the specification, from VK_KHR_ray_tracing to
--         VK_KHR_ray_query (#1918,!3912)
--
--     -   update to use @RayQueryKHR@ SPIR-V capability
--
--     -   add numerical limits for ray parameters (#2235,!3960)
--
--     -   relax formula for ray intersection candidate determination
--         (#2322,!4080)
--
--     -   restrict traces to TLAS (#2239,!4141)
--
--     -   require @HitT@ to be in ray interval for
--         @OpRayQueryGenerateIntersectionKHR@ (#2359,!4146)
--
--     -   add ray query shader stages for AS read bit (#2407,!4203)
--
-- == See Also
--
-- 'PhysicalDeviceRayQueryFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_query Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_ray_query  ( PhysicalDeviceRayQueryFeaturesKHR(..)
                                           , KHR_RAY_QUERY_SPEC_VERSION
                                           , pattern KHR_RAY_QUERY_SPEC_VERSION
                                           , KHR_RAY_QUERY_EXTENSION_NAME
                                           , pattern KHR_RAY_QUERY_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR))
-- | VkPhysicalDeviceRayQueryFeaturesKHR - Structure describing the ray query
-- features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceRayQueryFeaturesKHR' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRayQueryFeaturesKHR' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively
-- enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_query VK_KHR_ray_query>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayQueryFeaturesKHR = PhysicalDeviceRayQueryFeaturesKHR
  { -- | #features-rayQuery# @rayQuery@ indicates whether the implementation
    -- supports ray query (@OpRayQueryProceedKHR@) functionality.
    rayQuery :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayQueryFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceRayQueryFeaturesKHR

instance ToCStruct PhysicalDeviceRayQueryFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayQueryFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayQuery))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayQueryFeaturesKHR where
  peekCStruct p = do
    rayQuery <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRayQueryFeaturesKHR
             (bool32ToBool rayQuery)

instance Storable PhysicalDeviceRayQueryFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayQueryFeaturesKHR where
  zero = PhysicalDeviceRayQueryFeaturesKHR
           zero


type KHR_RAY_QUERY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_RAY_QUERY_SPEC_VERSION"
pattern KHR_RAY_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_RAY_QUERY_SPEC_VERSION = 1


type KHR_RAY_QUERY_EXTENSION_NAME = "VK_KHR_ray_query"

-- No documentation found for TopLevel "VK_KHR_RAY_QUERY_EXTENSION_NAME"
pattern KHR_RAY_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_RAY_QUERY_EXTENSION_NAME = "VK_KHR_ray_query"

