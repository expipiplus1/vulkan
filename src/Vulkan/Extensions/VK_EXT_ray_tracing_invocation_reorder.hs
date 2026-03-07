{-# language CPP #-}
-- | = Name
--
-- VK_EXT_ray_tracing_invocation_reorder - device extension
--
-- = VK_EXT_ray_tracing_invocation_reorder
--
-- [__Name String__]
--     @VK_EXT_ray_tracing_invocation_reorder@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     582
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_invocation_reorder.html SPV_EXT_shader_invocation_reorder>
--
-- [__Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_ray_tracing_invocation_reorder] @ewerness-nv%0A*Here describe the issue or question you have about the VK_EXT_ray_tracing_invocation_reorder extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_ray_tracing_invocation_reorder.adoc VK_EXT_ray_tracing_invocation_reorder>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-11-12
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_shader_invocation_reorder.txt GL_EXT_shader_invocation_reorder>
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Stu Smith, AMD
--
--     -   Aaron Hagan, AMD
--
--     -   Tyler Nowicki, AMD
--
--     -   Sebastian Neubauer, AMD
--
--     -   Radoslaw Drabinski, Intel
--
--     -   Sven Woop, Intel
--
--     -   Aleksandra Krstic, QUALCOMM
--
--     -   Andrew Garrard, Imagination Technologies
--
--     -   Mathieu Robart, Arm Limited
--
--     -   Tom Olson, Khronos
--
--     -   Ralph Potter, Samsung Electronics
--
--     -   Antonio Caggiano, LunarG
--
-- == Description
--
-- The ray tracing pipeline API provides some ability to reorder for
-- locality, but it is useful to have more control over how the reordering
-- happens and what information is included in the reordering. The shader
-- API provides a hit object to contain result information from the hit
-- which can be used as part of the explicit sorting plus options that
-- contain an integer for hint bits to use to add more coherency.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingInvocationReorderFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRayTracingInvocationReorderPropertiesEXT'
--
-- == New Enums
--
-- -   'RayTracingInvocationReorderModeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME'
--
-- -   'EXT_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2025-11-12 (Eric Werness)
--
--     -   Internal development - forked from NV
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_ray_tracing_invocation_reorder Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_ray_tracing_invocation_reorder  ( PhysicalDeviceRayTracingInvocationReorderFeaturesEXT(..)
                                                                , PhysicalDeviceRayTracingInvocationReorderPropertiesEXT(..)
                                                                , RayTracingInvocationReorderModeEXT( RAY_TRACING_INVOCATION_REORDER_MODE_NONE_EXT
                                                                                                    , RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_EXT
                                                                                                    , ..
                                                                                                    )
                                                                , EXT_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION
                                                                , pattern EXT_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION
                                                                , EXT_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME
                                                                , pattern EXT_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_EXT))
-- | VkPhysicalDeviceRayTracingInvocationReorderFeaturesEXT - Structure
-- describing feature to control ray tracing invocation reordering
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingInvocationReorderFeaturesEXT' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceRayTracingInvocationReorderFeaturesEXT', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_ray_tracing_invocation_reorder VK_EXT_ray_tracing_invocation_reorder>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingInvocationReorderFeaturesEXT = PhysicalDeviceRayTracingInvocationReorderFeaturesEXT
  { -- | #features-rayTracingInvocationReorder# @rayTracingInvocationReorder@
    -- indicates that the implementation supports
    -- @SPV_EXT_shader_invocation_reorder@.
    rayTracingInvocationReorder :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingInvocationReorderFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceRayTracingInvocationReorderFeaturesEXT

instance ToCStruct PhysicalDeviceRayTracingInvocationReorderFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingInvocationReorderFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayTracingInvocationReorder))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingInvocationReorderFeaturesEXT where
  peekCStruct p = do
    rayTracingInvocationReorder <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingInvocationReorderFeaturesEXT
             (bool32ToBool rayTracingInvocationReorder)

instance Storable PhysicalDeviceRayTracingInvocationReorderFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingInvocationReorderFeaturesEXT where
  zero = PhysicalDeviceRayTracingInvocationReorderFeaturesEXT
           zero


-- | VkPhysicalDeviceRayTracingInvocationReorderPropertiesEXT - Structure
-- describing shader module identifier properties of an implementation
--
-- = Description
--
-- If @rayTracingInvocationReorderReorderingHint@ is
-- 'RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_EXT' there /must/ exist
-- conditions under which the ordered set of invocations before a reorder
-- instruction is different than the ordered set of invocations after the
-- reorder instruction. The ordering of a set of invocations is determined
-- by the @SubgroupId@ of an invocation’s subgroup and the
-- @SubGroupInvocationId@ of an invocation within that subgroup.
--
-- The reorder instructions are:
--
-- Because the extension changes how hits are managed there is a
-- compatibility reason to expose the extension even when an implementation
-- does not have sorting active.
--
-- If the 'PhysicalDeviceRayTracingInvocationReorderPropertiesEXT'
-- structure is included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_ray_tracing_invocation_reorder VK_EXT_ray_tracing_invocation_reorder>,
-- 'RayTracingInvocationReorderModeEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingInvocationReorderPropertiesEXT = PhysicalDeviceRayTracingInvocationReorderPropertiesEXT
  { -- | @rayTracingInvocationReorderReorderingHint@ is a hint indicating if the
    -- implementation /may/ reorder at the reorder calls.
    rayTracingInvocationReorderReorderingHint :: RayTracingInvocationReorderModeEXT
  , -- | @maxShaderBindingTableRecordIndex@ is the maximum shader binding table
    -- record index allowed to be passed in to
    -- @OpHitObjectSetShaderBindingTableRecordIndexEXT@
    maxShaderBindingTableRecordIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingInvocationReorderPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceRayTracingInvocationReorderPropertiesEXT

instance ToCStruct PhysicalDeviceRayTracingInvocationReorderPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingInvocationReorderPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingInvocationReorderModeEXT)) (rayTracingInvocationReorderReorderingHint)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxShaderBindingTableRecordIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingInvocationReorderModeEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceRayTracingInvocationReorderPropertiesEXT where
  peekCStruct p = do
    rayTracingInvocationReorderReorderingHint <- peek @RayTracingInvocationReorderModeEXT ((p `plusPtr` 16 :: Ptr RayTracingInvocationReorderModeEXT))
    maxShaderBindingTableRecordIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PhysicalDeviceRayTracingInvocationReorderPropertiesEXT
             rayTracingInvocationReorderReorderingHint
             maxShaderBindingTableRecordIndex

instance Storable PhysicalDeviceRayTracingInvocationReorderPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingInvocationReorderPropertiesEXT where
  zero = PhysicalDeviceRayTracingInvocationReorderPropertiesEXT
           zero
           zero


-- | VkRayTracingInvocationReorderModeEXT - Enum providing a hint on how the
-- application /may/ reorder
--
-- = Description
--
-- -   'RAY_TRACING_INVOCATION_REORDER_MODE_NONE_EXT' specifies that the
--     implementation does not reorder at reorder calls.
--
-- -   'RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_EXT' specifies that the
--     implementation /may/ reorder at reorder calls.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_ray_tracing_invocation_reorder VK_EXT_ray_tracing_invocation_reorder>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_invocation_reorder VK_NV_ray_tracing_invocation_reorder>,
-- 'PhysicalDeviceRayTracingInvocationReorderPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_invocation_reorder.PhysicalDeviceRayTracingInvocationReorderPropertiesNV'
newtype RayTracingInvocationReorderModeEXT = RayTracingInvocationReorderModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkRayTracingInvocationReorderModeEXT" "VK_RAY_TRACING_INVOCATION_REORDER_MODE_NONE_EXT"
pattern RAY_TRACING_INVOCATION_REORDER_MODE_NONE_EXT = RayTracingInvocationReorderModeEXT 0

-- No documentation found for Nested "VkRayTracingInvocationReorderModeEXT" "VK_RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_EXT"
pattern RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_EXT = RayTracingInvocationReorderModeEXT 1

{-# COMPLETE
  RAY_TRACING_INVOCATION_REORDER_MODE_NONE_EXT
  , RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_EXT ::
    RayTracingInvocationReorderModeEXT
  #-}

conNameRayTracingInvocationReorderModeEXT :: String
conNameRayTracingInvocationReorderModeEXT = "RayTracingInvocationReorderModeEXT"

enumPrefixRayTracingInvocationReorderModeEXT :: String
enumPrefixRayTracingInvocationReorderModeEXT = "RAY_TRACING_INVOCATION_REORDER_MODE_"

showTableRayTracingInvocationReorderModeEXT :: [(RayTracingInvocationReorderModeEXT, String)]
showTableRayTracingInvocationReorderModeEXT =
  [
    ( RAY_TRACING_INVOCATION_REORDER_MODE_NONE_EXT
    , "NONE_EXT"
    )
  ,
    ( RAY_TRACING_INVOCATION_REORDER_MODE_REORDER_EXT
    , "REORDER_EXT"
    )
  ]

instance Show RayTracingInvocationReorderModeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixRayTracingInvocationReorderModeEXT
      showTableRayTracingInvocationReorderModeEXT
      conNameRayTracingInvocationReorderModeEXT
      (\(RayTracingInvocationReorderModeEXT x) -> x)
      (showsPrec 11)

instance Read RayTracingInvocationReorderModeEXT where
  readPrec =
    enumReadPrec
      enumPrefixRayTracingInvocationReorderModeEXT
      showTableRayTracingInvocationReorderModeEXT
      conNameRayTracingInvocationReorderModeEXT
      RayTracingInvocationReorderModeEXT

type EXT_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION"
pattern EXT_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION = 1


type EXT_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME = "VK_EXT_ray_tracing_invocation_reorder"

-- No documentation found for TopLevel "VK_EXT_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME"
pattern EXT_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME = "VK_EXT_ray_tracing_invocation_reorder"

