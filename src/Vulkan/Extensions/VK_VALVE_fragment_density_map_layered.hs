{-# language CPP #-}
-- | = Name
--
-- VK_VALVE_fragment_density_map_layered - device extension
--
-- = VK_VALVE_fragment_density_map_layered
--
-- [__Name String__]
--     @VK_VALVE_fragment_density_map_layered@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     612
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4 Vulkan Version 1.4>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
--
-- [__Contact__]
--
--     -   Connor Abbott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_fragment_density_map_layered] @cwabbott0%0A*Here describe the issue or question you have about the VK_VALVE_fragment_density_map_layered extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-06
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with Vulkan 1.1.
--
--     -   Interacts with Vulkan 1.3.
--
--     -   Interacts with Vulkan 1.4.
--
--     -   Interacts with @VK_EXT_fragment_density_map@.
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Connor Abbott, VALVE
--
--     -   Mike Blumenkrantz, VALVE
--
-- == Description
--
-- This extension enables the use of layered fragment density maps.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineFragmentDensityMapLayeredCreateInfoVALVE'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE'
--
-- == New Enum Constants
--
-- -   'VALVE_FRAGMENT_DENSITY_MAP_LAYERED_EXTENSION_NAME'
--
-- -   'VALVE_FRAGMENT_DENSITY_MAP_LAYERED_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE'
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE'
--
-- -   Extending
--     'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RenderPassCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RENDER_PASS_CREATE_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_FEATURES_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_PROPERTIES_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_FRAGMENT_DENSITY_MAP_LAYERED_CREATE_INFO_VALVE'
--
-- == Version History
--
-- -   Revision 1, 2025-06-06 (Mike Blumenkrantz)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_VALVE_fragment_density_map_layered Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_fragment_density_map_layered  ( PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE(..)
                                                                , PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE(..)
                                                                , PipelineFragmentDensityMapLayeredCreateInfoVALVE(..)
                                                                , VALVE_FRAGMENT_DENSITY_MAP_LAYERED_SPEC_VERSION
                                                                , pattern VALVE_FRAGMENT_DENSITY_MAP_LAYERED_SPEC_VERSION
                                                                , VALVE_FRAGMENT_DENSITY_MAP_LAYERED_EXTENSION_NAME
                                                                , pattern VALVE_FRAGMENT_DENSITY_MAP_LAYERED_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_FEATURES_VALVE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_PROPERTIES_VALVE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_FRAGMENT_DENSITY_MAP_LAYERED_CREATE_INFO_VALVE))
-- | VkPhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE - Structure
-- describing fragment density map layered properties that can be supported
-- by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_fragment_density_map_layered VK_VALVE_fragment_density_map_layered>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE = PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE
  { -- | #limits-maxFragmentDensityMapLayers# @maxFragmentDensityMapLayers@ is
    -- the maximum number of layers to use with a layered fragment density map.
    maxFragmentDensityMapLayers :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE

instance ToCStruct PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_PROPERTIES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxFragmentDensityMapLayers)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_PROPERTIES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE where
  peekCStruct p = do
    maxFragmentDensityMapLayers <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE
             maxFragmentDensityMapLayers

instance Storable PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE where
  zero = PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE
           zero


-- | VkPhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE - Structure
-- describing additional layered fragment density map features that can be
-- supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_fragment_density_map_layered VK_VALVE_fragment_density_map_layered>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE = PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE
  { -- | #features-fragmentDensityMapLayered# @fragmentDensityMapLayered@
    -- specifies whether the implementation supports layered fragment density
    -- maps.
    fragmentDensityMapLayered :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE

instance ToCStruct PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_FEATURES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMapLayered))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_FEATURES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE where
  peekCStruct p = do
    fragmentDensityMapLayered <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE
             (bool32ToBool fragmentDensityMapLayered)

instance Storable PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE where
  zero = PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE
           zero


-- | VkPipelineFragmentDensityMapLayeredCreateInfoVALVE - Structure
-- specifying layered fragment density map info
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_fragment_density_map_layered VK_VALVE_fragment_density_map_layered>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineFragmentDensityMapLayeredCreateInfoVALVE = PipelineFragmentDensityMapLayeredCreateInfoVALVE
  { -- | @maxFragmentDensityMapLayers@ is the maximum number of layers which can
    -- be used with a fragment density map.
    --
    -- #VUID-VkPipelineFragmentDensityMapLayeredCreateInfoVALVE-maxFragmentDensityMapLayers-10825#
    -- @maxFragmentDensityMapLayers@ /must/ be less than or equal to
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxFragmentDensityMapLayers maxFragmentDensityMapLayers>
    maxFragmentDensityMapLayers :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineFragmentDensityMapLayeredCreateInfoVALVE)
#endif
deriving instance Show PipelineFragmentDensityMapLayeredCreateInfoVALVE

instance ToCStruct PipelineFragmentDensityMapLayeredCreateInfoVALVE where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineFragmentDensityMapLayeredCreateInfoVALVE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_DENSITY_MAP_LAYERED_CREATE_INFO_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxFragmentDensityMapLayers)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_DENSITY_MAP_LAYERED_CREATE_INFO_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineFragmentDensityMapLayeredCreateInfoVALVE where
  peekCStruct p = do
    maxFragmentDensityMapLayers <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PipelineFragmentDensityMapLayeredCreateInfoVALVE
             maxFragmentDensityMapLayers

instance Storable PipelineFragmentDensityMapLayeredCreateInfoVALVE where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineFragmentDensityMapLayeredCreateInfoVALVE where
  zero = PipelineFragmentDensityMapLayeredCreateInfoVALVE
           zero


type VALVE_FRAGMENT_DENSITY_MAP_LAYERED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_VALVE_FRAGMENT_DENSITY_MAP_LAYERED_SPEC_VERSION"
pattern VALVE_FRAGMENT_DENSITY_MAP_LAYERED_SPEC_VERSION :: forall a . Integral a => a
pattern VALVE_FRAGMENT_DENSITY_MAP_LAYERED_SPEC_VERSION = 1


type VALVE_FRAGMENT_DENSITY_MAP_LAYERED_EXTENSION_NAME = "VK_VALVE_fragment_density_map_layered"

-- No documentation found for TopLevel "VK_VALVE_FRAGMENT_DENSITY_MAP_LAYERED_EXTENSION_NAME"
pattern VALVE_FRAGMENT_DENSITY_MAP_LAYERED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern VALVE_FRAGMENT_DENSITY_MAP_LAYERED_EXTENSION_NAME = "VK_VALVE_fragment_density_map_layered"

