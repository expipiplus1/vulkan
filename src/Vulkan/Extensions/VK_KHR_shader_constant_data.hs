{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_constant_data - device extension
--
-- = VK_KHR_shader_constant_data
--
-- [__Name String__]
--     @VK_KHR_shader_constant_data@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     232
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
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_constant_data.html SPV_KHR_constant_data>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_constant_data] @tobski%0A*Here describe the issue or question you have about the VK_KHR_shader_constant_data extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_constant_data.adoc VK_KHR_shader_constant_data>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Piers Daniell, Nvidia
--
--     -   Craig Graham, Samsung
--
--     -   Vikram Tarikere, IMG
--
-- == Description
--
-- This extension allows the use of the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_constant_data.html SPV_KHR_constant_data>
-- extension in SPIR-V shader modules which enables the specification and
-- specialization of arrays of constant data.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderConstantDataFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_CONSTANT_DATA_EXTENSION_NAME'
--
-- -   'KHR_SHADER_CONSTANT_DATA_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CONSTANT_DATA_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-ConstantDataKHR ConstantDataKHR>
--
-- == Version History
--
-- -   Revision 1, 2024-10-30 (Tobias Hector)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_constant_data Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_constant_data  ( PhysicalDeviceShaderConstantDataFeaturesKHR(..)
                                                      , KHR_SHADER_CONSTANT_DATA_SPEC_VERSION
                                                      , pattern KHR_SHADER_CONSTANT_DATA_SPEC_VERSION
                                                      , KHR_SHADER_CONSTANT_DATA_EXTENSION_NAME
                                                      , pattern KHR_SHADER_CONSTANT_DATA_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CONSTANT_DATA_FEATURES_KHR))
-- | VkPhysicalDeviceShaderConstantDataFeaturesKHR - Structure describing
-- support for VK_KHR_shader_constant_data in an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderConstantDataFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderConstantDataFeaturesKHR', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_constant_data VK_KHR_shader_constant_data>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderConstantDataFeaturesKHR = PhysicalDeviceShaderConstantDataFeaturesKHR
  { -- | #features-shaderConstantData# @shaderConstantData@ specifies whether the
    -- implementation supports SPIR-V modules that use the
    -- @VK_KHR_shader_constant_data@ extension.
    shaderConstantData :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderConstantDataFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderConstantDataFeaturesKHR

instance ToCStruct PhysicalDeviceShaderConstantDataFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderConstantDataFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CONSTANT_DATA_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderConstantData))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CONSTANT_DATA_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderConstantDataFeaturesKHR where
  peekCStruct p = do
    shaderConstantData <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderConstantDataFeaturesKHR
             (bool32ToBool shaderConstantData)

instance Storable PhysicalDeviceShaderConstantDataFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderConstantDataFeaturesKHR where
  zero = PhysicalDeviceShaderConstantDataFeaturesKHR
           zero


type KHR_SHADER_CONSTANT_DATA_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_CONSTANT_DATA_SPEC_VERSION"
pattern KHR_SHADER_CONSTANT_DATA_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_CONSTANT_DATA_SPEC_VERSION = 1


type KHR_SHADER_CONSTANT_DATA_EXTENSION_NAME = "VK_KHR_shader_constant_data"

-- No documentation found for TopLevel "VK_KHR_SHADER_CONSTANT_DATA_EXTENSION_NAME"
pattern KHR_SHADER_CONSTANT_DATA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_CONSTANT_DATA_EXTENSION_NAME = "VK_KHR_shader_constant_data"

