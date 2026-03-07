{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_replicated_composites - device extension
--
-- = VK_EXT_shader_replicated_composites
--
-- [__Name String__]
--     @VK_EXT_shader_replicated_composites@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     565
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_replicated_composites.html SPV_EXT_replicated_composites>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_replicated_composites] @kpet%0A*Here describe the issue or question you have about the VK_EXT_shader_replicated_composites extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_replicated_composites.adoc VK_EXT_shader_replicated_composites>
--
-- [__Last Modified Date__]
--     2024-02-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
-- This extension adds support for creating composites from a single value
-- in SPIR-V modules, as defined by SPV_EXT_replicated_composites.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderReplicatedCompositesFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_REPLICATED_COMPOSITES_EXTENSION_NAME'
--
-- -   'EXT_SHADER_REPLICATED_COMPOSITES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_REPLICATED_COMPOSITES_FEATURES_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-ReplicatedCompositesEXT ReplicatedCompositesEXT>
--
-- == Version History
--
-- -   Revision 1, 2024-02-08 (Kévin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_replicated_composites Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_replicated_composites  ( PhysicalDeviceShaderReplicatedCompositesFeaturesEXT(..)
                                                              , EXT_SHADER_REPLICATED_COMPOSITES_SPEC_VERSION
                                                              , pattern EXT_SHADER_REPLICATED_COMPOSITES_SPEC_VERSION
                                                              , EXT_SHADER_REPLICATED_COMPOSITES_EXTENSION_NAME
                                                              , pattern EXT_SHADER_REPLICATED_COMPOSITES_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_REPLICATED_COMPOSITES_FEATURES_EXT))
-- | VkPhysicalDeviceShaderReplicatedCompositesFeaturesEXT - Structure
-- describing whether support for replicated composites in SPIR-V is
-- enabled
--
-- = Description
--
-- If the 'PhysicalDeviceShaderReplicatedCompositesFeaturesEXT' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderReplicatedCompositesFeaturesEXT', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_replicated_composites VK_EXT_shader_replicated_composites>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderReplicatedCompositesFeaturesEXT = PhysicalDeviceShaderReplicatedCompositesFeaturesEXT
  { -- | #features-shaderReplicatedComposites# @shaderReplicatedComposites@
    -- specifies whether shader modules /can/ declare the
    -- @ReplicatedCompositesEXT@ capability.
    shaderReplicatedComposites :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderReplicatedCompositesFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderReplicatedCompositesFeaturesEXT

instance ToCStruct PhysicalDeviceShaderReplicatedCompositesFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderReplicatedCompositesFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_REPLICATED_COMPOSITES_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderReplicatedComposites))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_REPLICATED_COMPOSITES_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderReplicatedCompositesFeaturesEXT where
  peekCStruct p = do
    shaderReplicatedComposites <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderReplicatedCompositesFeaturesEXT
             (bool32ToBool shaderReplicatedComposites)

instance Storable PhysicalDeviceShaderReplicatedCompositesFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderReplicatedCompositesFeaturesEXT where
  zero = PhysicalDeviceShaderReplicatedCompositesFeaturesEXT
           zero


type EXT_SHADER_REPLICATED_COMPOSITES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_REPLICATED_COMPOSITES_SPEC_VERSION"
pattern EXT_SHADER_REPLICATED_COMPOSITES_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_REPLICATED_COMPOSITES_SPEC_VERSION = 1


type EXT_SHADER_REPLICATED_COMPOSITES_EXTENSION_NAME = "VK_EXT_shader_replicated_composites"

-- No documentation found for TopLevel "VK_EXT_SHADER_REPLICATED_COMPOSITES_EXTENSION_NAME"
pattern EXT_SHADER_REPLICATED_COMPOSITES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_REPLICATED_COMPOSITES_EXTENSION_NAME = "VK_EXT_shader_replicated_composites"

