{-# language CPP #-}
-- | = Name
--
-- VK_NV_ray_tracing_invocation_reorder - device extension
--
-- == VK_NV_ray_tracing_invocation_reorder
--
-- [__Name String__]
--     @VK_NV_ray_tracing_invocation_reorder@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     491
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_ray_tracing_pipeline@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_ray_tracing_invocation_reorder] @ewerness-nv%0A*Here describe the issue or question you have about the VK_NV_ray_tracing_invocation_reorder extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-11-02
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_invocation_reorder.html SPV_NV_shader_invocation_reorder>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_shader_invocation_reorder.txt GL_NV_shader_invocation_reorder>
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
-- == Description
--
-- The ray tracing pipeline API provides some ability to reorder for
-- locality, but it is useful to have more control over how the reordering
-- happens and what information is included in the reordering. The shader
-- API provides a hit object to contain result information from the hit
-- which can be used as part of the explicit sorting plus options that
-- contain an integer for hint bits to use to add more locality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingInvocationReorderFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRayTracingInvocationReorderPropertiesNV'
--
-- == New Enums
--
-- -   'RayTracingInvocationReorderModeNV'
--
-- == New Enum Constants
--
-- -   'NV_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME'
--
-- -   'NV_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2020-09-12 (Eric Werness, Ashwin Lele)
--
--     -   Initial external release
--
-- == See Also
--
-- 'PhysicalDeviceRayTracingInvocationReorderFeaturesNV',
-- 'PhysicalDeviceRayTracingInvocationReorderPropertiesNV',
-- 'RayTracingInvocationReorderModeNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_ray_tracing_invocation_reorder Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_ray_tracing_invocation_reorder  ( PhysicalDeviceRayTracingInvocationReorderFeaturesNV(..)
                                                               , PhysicalDeviceRayTracingInvocationReorderPropertiesNV(..)
                                                               , RayTracingInvocationReorderModeNV( RAY_TRACING_INVOCATION_REORDER_MODE_NONE_NV
                                                                                                  , RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_NV
                                                                                                  , ..
                                                                                                  )
                                                               , NV_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION
                                                               , pattern NV_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION
                                                               , NV_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME
                                                               , pattern NV_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME
                                                               ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_NV))
-- | VkPhysicalDeviceRayTracingInvocationReorderFeaturesNV - Structure
-- describing feature to control ray tracing invocation reordering
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingInvocationReorderFeaturesNV' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRayTracingInvocationReorderFeaturesNV' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_invocation_reorder VK_NV_ray_tracing_invocation_reorder>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingInvocationReorderFeaturesNV = PhysicalDeviceRayTracingInvocationReorderFeaturesNV
  { -- | #features-rayTracingInvocationReorder# @rayTracingInvocationReorder@
    -- indicates that the implementation supports
    -- @SPV_NV_shader_invocation_reorder@.
    rayTracingInvocationReorder :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingInvocationReorderFeaturesNV)
#endif
deriving instance Show PhysicalDeviceRayTracingInvocationReorderFeaturesNV

instance ToCStruct PhysicalDeviceRayTracingInvocationReorderFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingInvocationReorderFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayTracingInvocationReorder))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingInvocationReorderFeaturesNV where
  peekCStruct p = do
    rayTracingInvocationReorder <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingInvocationReorderFeaturesNV
             (bool32ToBool rayTracingInvocationReorder)

instance Storable PhysicalDeviceRayTracingInvocationReorderFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingInvocationReorderFeaturesNV where
  zero = PhysicalDeviceRayTracingInvocationReorderFeaturesNV
           zero


-- | VkPhysicalDeviceRayTracingInvocationReorderPropertiesNV - Structure
-- describing shader module identifier properties of an implementation
--
-- = Members
--
-- The members of the
-- 'PhysicalDeviceRayTracingInvocationReorderPropertiesNV' structure
-- describe the following:
--
-- = Description
--
-- Note
--
-- Because the extension changes how hits are managed there is a
-- compatibility reason to expose the extension even when an implementation
-- does not have sorting active.
--
-- If the 'PhysicalDeviceRayTracingInvocationReorderPropertiesNV' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_invocation_reorder VK_NV_ray_tracing_invocation_reorder>,
-- 'RayTracingInvocationReorderModeNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingInvocationReorderPropertiesNV = PhysicalDeviceRayTracingInvocationReorderPropertiesNV
  { -- | @rayTracingInvocationReorderReorderingHint@ is a hint indicating if the
    -- implementation will actually reorder at the reorder calls.
    rayTracingInvocationReorderReorderingHint :: RayTracingInvocationReorderModeNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingInvocationReorderPropertiesNV)
#endif
deriving instance Show PhysicalDeviceRayTracingInvocationReorderPropertiesNV

instance ToCStruct PhysicalDeviceRayTracingInvocationReorderPropertiesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingInvocationReorderPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingInvocationReorderModeNV)) (rayTracingInvocationReorderReorderingHint)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingInvocationReorderModeNV)) (zero)
    f

instance FromCStruct PhysicalDeviceRayTracingInvocationReorderPropertiesNV where
  peekCStruct p = do
    rayTracingInvocationReorderReorderingHint <- peek @RayTracingInvocationReorderModeNV ((p `plusPtr` 16 :: Ptr RayTracingInvocationReorderModeNV))
    pure $ PhysicalDeviceRayTracingInvocationReorderPropertiesNV
             rayTracingInvocationReorderReorderingHint

instance Storable PhysicalDeviceRayTracingInvocationReorderPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingInvocationReorderPropertiesNV where
  zero = PhysicalDeviceRayTracingInvocationReorderPropertiesNV
           zero


-- | VkRayTracingInvocationReorderModeNV - Enum providing a hint on how the
-- application /may/ reorder
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_invocation_reorder VK_NV_ray_tracing_invocation_reorder>,
-- 'PhysicalDeviceRayTracingInvocationReorderPropertiesNV'
newtype RayTracingInvocationReorderModeNV = RayTracingInvocationReorderModeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'RAY_TRACING_INVOCATION_REORDER_MODE_NONE_NV' indicates that the
-- implementation is likely to not reorder at reorder calls.
pattern RAY_TRACING_INVOCATION_REORDER_MODE_NONE_NV = RayTracingInvocationReorderModeNV 0

-- | 'RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_NV' indicates that the
-- implementation is likely to reorder at reorder calls.
pattern RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_NV = RayTracingInvocationReorderModeNV 1

{-# COMPLETE
  RAY_TRACING_INVOCATION_REORDER_MODE_NONE_NV
  , RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_NV ::
    RayTracingInvocationReorderModeNV
  #-}

conNameRayTracingInvocationReorderModeNV :: String
conNameRayTracingInvocationReorderModeNV = "RayTracingInvocationReorderModeNV"

enumPrefixRayTracingInvocationReorderModeNV :: String
enumPrefixRayTracingInvocationReorderModeNV = "RAY_TRACING_INVOCATION_REORDER_MODE_"

showTableRayTracingInvocationReorderModeNV :: [(RayTracingInvocationReorderModeNV, String)]
showTableRayTracingInvocationReorderModeNV =
  [
    ( RAY_TRACING_INVOCATION_REORDER_MODE_NONE_NV
    , "NONE_NV"
    )
  ,
    ( RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_NV
    , "REORDER_NV"
    )
  ]

instance Show RayTracingInvocationReorderModeNV where
  showsPrec =
    enumShowsPrec
      enumPrefixRayTracingInvocationReorderModeNV
      showTableRayTracingInvocationReorderModeNV
      conNameRayTracingInvocationReorderModeNV
      (\(RayTracingInvocationReorderModeNV x) -> x)
      (showsPrec 11)

instance Read RayTracingInvocationReorderModeNV where
  readPrec =
    enumReadPrec
      enumPrefixRayTracingInvocationReorderModeNV
      showTableRayTracingInvocationReorderModeNV
      conNameRayTracingInvocationReorderModeNV
      RayTracingInvocationReorderModeNV

type NV_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION"
pattern NV_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION :: forall a . Integral a => a
pattern NV_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION = 1


type NV_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME = "VK_NV_ray_tracing_invocation_reorder"

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME"
pattern NV_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME = "VK_NV_ray_tracing_invocation_reorder"

