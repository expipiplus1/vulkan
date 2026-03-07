{-# language CPP #-}
-- | = Name
--
-- VK_NV_ray_tracing_validation - device extension
--
-- = VK_NV_ray_tracing_validation
--
-- [__Name String__]
--     @VK_NV_ray_tracing_validation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     569
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_ray_tracing_validation] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_NV_ray_tracing_validation extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_ray_tracing_validation.adoc VK_NV_ray_tracing_validation>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-03-04
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension adds support for performing ray tracing validation at an
-- implementation level.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingValidationFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_RAY_TRACING_VALIDATION_EXTENSION_NAME'
--
-- -   'NV_RAY_TRACING_VALIDATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_VALIDATION_FEATURES_NV'
--
-- == Version History
--
-- -   Revision 1, 2024-03-04 (Vikram Kushwaha)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_ray_tracing_validation Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_ray_tracing_validation  ( PhysicalDeviceRayTracingValidationFeaturesNV(..)
                                                       , NV_RAY_TRACING_VALIDATION_SPEC_VERSION
                                                       , pattern NV_RAY_TRACING_VALIDATION_SPEC_VERSION
                                                       , NV_RAY_TRACING_VALIDATION_EXTENSION_NAME
                                                       , pattern NV_RAY_TRACING_VALIDATION_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_VALIDATION_FEATURES_NV))
-- | VkPhysicalDeviceRayTracingValidationFeaturesNV - Structure describing
-- the ray tracing validation features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingValidationFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceRayTracingValidationFeaturesNV', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_validation VK_NV_ray_tracing_validation>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingValidationFeaturesNV = PhysicalDeviceRayTracingValidationFeaturesNV
  { -- | #features-rayTracingValidation# @rayTracingValidation@ indicates whether
    -- the implementation supports the ray tracing validation feature.
    rayTracingValidation :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingValidationFeaturesNV)
#endif
deriving instance Show PhysicalDeviceRayTracingValidationFeaturesNV

instance ToCStruct PhysicalDeviceRayTracingValidationFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingValidationFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_VALIDATION_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayTracingValidation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_VALIDATION_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingValidationFeaturesNV where
  peekCStruct p = do
    rayTracingValidation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingValidationFeaturesNV
             (bool32ToBool rayTracingValidation)

instance Storable PhysicalDeviceRayTracingValidationFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingValidationFeaturesNV where
  zero = PhysicalDeviceRayTracingValidationFeaturesNV
           zero


type NV_RAY_TRACING_VALIDATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_VALIDATION_SPEC_VERSION"
pattern NV_RAY_TRACING_VALIDATION_SPEC_VERSION :: forall a . Integral a => a
pattern NV_RAY_TRACING_VALIDATION_SPEC_VERSION = 1


type NV_RAY_TRACING_VALIDATION_EXTENSION_NAME = "VK_NV_ray_tracing_validation"

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_VALIDATION_EXTENSION_NAME"
pattern NV_RAY_TRACING_VALIDATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_RAY_TRACING_VALIDATION_EXTENSION_NAME = "VK_NV_ray_tracing_validation"

