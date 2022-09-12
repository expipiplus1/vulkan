{-# language CPP #-}
-- | = Name
--
-- VK_EXT_non_seamless_cube_map - device extension
--
-- == VK_EXT_non_seamless_cube_map
--
-- [__Name String__]
--     @VK_EXT_non_seamless_cube_map@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     423
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Special Uses__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Georg Lehmann
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_non_seamless_cube_map] @DadSchoorse%0A*Here describe the issue or question you have about the VK_EXT_non_seamless_cube_map extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_non_seamless_cube_map.adoc VK_EXT_non_seamless_cube_map>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-04
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Georg Lehmann
--
-- == Description
--
-- This extension provides functionality to disable
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-cubemapedge cube map edge handling>
-- on a per sampler level which matches the behavior of other graphics
-- APIs.
--
-- This extension may be useful for building translation layers for those
-- APIs or for porting applications that rely on this cube map behavior.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceNonSeamlessCubeMapFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_NON_SEAMLESS_CUBE_MAP_EXTENSION_NAME'
--
-- -   'EXT_NON_SEAMLESS_CUBE_MAP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_NON_SEAMLESS_CUBE_MAP_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_NON_SEAMLESS_CUBE_MAP_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-09-04 (Georg Lehmann)
--
--     -   First Version
--
-- == See Also
--
-- 'PhysicalDeviceNonSeamlessCubeMapFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_non_seamless_cube_map Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_non_seamless_cube_map  ( PhysicalDeviceNonSeamlessCubeMapFeaturesEXT(..)
                                                       , EXT_NON_SEAMLESS_CUBE_MAP_SPEC_VERSION
                                                       , pattern EXT_NON_SEAMLESS_CUBE_MAP_SPEC_VERSION
                                                       , EXT_NON_SEAMLESS_CUBE_MAP_EXTENSION_NAME
                                                       , pattern EXT_NON_SEAMLESS_CUBE_MAP_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_NON_SEAMLESS_CUBE_MAP_FEATURES_EXT))
-- | VkPhysicalDeviceNonSeamlessCubeMapFeaturesEXT - Structure describing
-- features to disable seamless cube maps
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceNonSeamlessCubeMapFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceNonSeamlessCubeMapFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_non_seamless_cube_map VK_EXT_non_seamless_cube_map>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceNonSeamlessCubeMapFeaturesEXT = PhysicalDeviceNonSeamlessCubeMapFeaturesEXT
  { -- | #features-nonSeamlessCubeMap# @nonSeamlessCubeMap@ indicates that the
    -- implementation supports
    -- 'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_NON_SEAMLESS_CUBE_MAP_BIT_EXT'.
    nonSeamlessCubeMap :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceNonSeamlessCubeMapFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceNonSeamlessCubeMapFeaturesEXT

instance ToCStruct PhysicalDeviceNonSeamlessCubeMapFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceNonSeamlessCubeMapFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_NON_SEAMLESS_CUBE_MAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (nonSeamlessCubeMap))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_NON_SEAMLESS_CUBE_MAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceNonSeamlessCubeMapFeaturesEXT where
  peekCStruct p = do
    nonSeamlessCubeMap <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceNonSeamlessCubeMapFeaturesEXT
             (bool32ToBool nonSeamlessCubeMap)

instance Storable PhysicalDeviceNonSeamlessCubeMapFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceNonSeamlessCubeMapFeaturesEXT where
  zero = PhysicalDeviceNonSeamlessCubeMapFeaturesEXT
           zero


type EXT_NON_SEAMLESS_CUBE_MAP_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_NON_SEAMLESS_CUBE_MAP_SPEC_VERSION"
pattern EXT_NON_SEAMLESS_CUBE_MAP_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_NON_SEAMLESS_CUBE_MAP_SPEC_VERSION = 1


type EXT_NON_SEAMLESS_CUBE_MAP_EXTENSION_NAME = "VK_EXT_non_seamless_cube_map"

-- No documentation found for TopLevel "VK_EXT_NON_SEAMLESS_CUBE_MAP_EXTENSION_NAME"
pattern EXT_NON_SEAMLESS_CUBE_MAP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_NON_SEAMLESS_CUBE_MAP_EXTENSION_NAME = "VK_EXT_non_seamless_cube_map"

