{-# language CPP #-}
-- | = Name
--
-- VK_NV_raw_access_chains - device extension
--
-- = VK_NV_raw_access_chains
--
-- [__Name String__]
--     @VK_NV_raw_access_chains@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     556
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
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_raw_access_chains.html SPV_NV_raw_access_chains>
--
-- [__Contact__]
--
--     -   Rodrigo Locatti
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_raw_access_chains] @rlocatti%0A*Here describe the issue or question you have about the VK_NV_raw_access_chains extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-12-04
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_raw_access_chains.html SPV_NV_raw_access_chains>
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Rodrigo Locatti, NVIDIA
--
-- == Description
--
-- This extension allows the use of the @SPV_NV_raw_access_chains@
-- extension in SPIR-V shader modules. This enables SPIR-V producers to
-- efficiently implement interfaces similar to Direct3D structured buffers
-- and byte address buffers, allowing shaders compiled from an HLSL source
-- to generate more efficient code.
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-RawAccessChainsNV RawAccessChainsNV>
--
-- == Version History
--
-- -   Revision 1, 2023-12-04 (Rodrigo Locatti)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_raw_access_chains Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_raw_access_chains  ( PhysicalDeviceRawAccessChainsFeaturesNV(..)
                                                  , NV_RAW_ACCESS_CHAINS_SPEC_VERSION
                                                  , pattern NV_RAW_ACCESS_CHAINS_SPEC_VERSION
                                                  , NV_RAW_ACCESS_CHAINS_EXTENSION_NAME
                                                  , pattern NV_RAW_ACCESS_CHAINS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAW_ACCESS_CHAINS_FEATURES_NV))
-- | VkPhysicalDeviceRawAccessChainsFeaturesNV - Structure describing shader
-- raw access chains features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceRawAccessChainsFeaturesNV' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceRawAccessChainsFeaturesNV', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_raw_access_chains VK_NV_raw_access_chains>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRawAccessChainsFeaturesNV = PhysicalDeviceRawAccessChainsFeaturesNV
  { -- | #features-shaderRawAccessChains# @shaderRawAccessChains@ specifies
    -- whether shader modules /can/ declare the @RawAccessChainsNV@ capability.
    shaderRawAccessChains :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRawAccessChainsFeaturesNV)
#endif
deriving instance Show PhysicalDeviceRawAccessChainsFeaturesNV

instance ToCStruct PhysicalDeviceRawAccessChainsFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRawAccessChainsFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAW_ACCESS_CHAINS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderRawAccessChains))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAW_ACCESS_CHAINS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRawAccessChainsFeaturesNV where
  peekCStruct p = do
    shaderRawAccessChains <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRawAccessChainsFeaturesNV
             (bool32ToBool shaderRawAccessChains)

instance Storable PhysicalDeviceRawAccessChainsFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRawAccessChainsFeaturesNV where
  zero = PhysicalDeviceRawAccessChainsFeaturesNV
           zero


type NV_RAW_ACCESS_CHAINS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_RAW_ACCESS_CHAINS_SPEC_VERSION"
pattern NV_RAW_ACCESS_CHAINS_SPEC_VERSION :: forall a . Integral a => a
pattern NV_RAW_ACCESS_CHAINS_SPEC_VERSION = 1


type NV_RAW_ACCESS_CHAINS_EXTENSION_NAME = "VK_NV_raw_access_chains"

-- No documentation found for TopLevel "VK_NV_RAW_ACCESS_CHAINS_EXTENSION_NAME"
pattern NV_RAW_ACCESS_CHAINS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_RAW_ACCESS_CHAINS_EXTENSION_NAME = "VK_NV_raw_access_chains"

