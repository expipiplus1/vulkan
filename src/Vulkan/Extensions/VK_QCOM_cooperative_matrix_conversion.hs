{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_cooperative_matrix_conversion - device extension
--
-- = VK_QCOM_cooperative_matrix_conversion
--
-- [__Name String__]
--     @VK_QCOM_cooperative_matrix_conversion@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     173
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_cooperative_matrix_conversion.html SPV_QCOM_cooperative_matrix_conversion>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_cooperative_matrix_conversion] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_cooperative_matrix_conversion extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_cooperative_matrix_conversion.adoc VK_QCOM_cooperative_matrix_conversion>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-01-28
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/qcom/GLSL_QCOM_cooperative_matrix_conversion.txt GLSL_QCOM_cooperative_matrix_conversion>
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
--     -   Elina Kamenetskaya, Qualcomm Technologies, Inc
--
--     -   Alex Bourd, Qualcomm Technologies, Inc
--
--     -   Ruihao Zhang, Qualcomm Technologies, Inc
--
--     -   Wooyoung Kim, Qualcomm Technologies, Inc
--
-- == Description
--
-- This extension adds support for new SPIR-V shader instructions that
-- allow loading and storing a cooperative matrix without needing to stage
-- through shared memory and to allow bit casting arrays.
--
-- These instructions are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_cooperative_matrix_conversion.html SPV_QCOM_cooperative_matrix_conversion>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/qcom/GLSL_QCOM_cooperative_matrix_conversion.txt GLSL_QCOM_cooperative_matrix_conversion>
-- GLSL extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_COOPERATIVE_MATRIX_CONVERSION_EXTENSION_NAME'
--
-- -   'QCOM_COOPERATIVE_MATRIX_CONVERSION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_CONVERSION_FEATURES_QCOM'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixConversionQCOM CooperativeMatrixConversionQCOM>
--
-- == Version History
--
-- -   Revision 1, 2026-01-28 (Matthew Netsch)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_cooperative_matrix_conversion Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_cooperative_matrix_conversion  ( PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM(..)
                                                                , QCOM_COOPERATIVE_MATRIX_CONVERSION_SPEC_VERSION
                                                                , pattern QCOM_COOPERATIVE_MATRIX_CONVERSION_SPEC_VERSION
                                                                , QCOM_COOPERATIVE_MATRIX_CONVERSION_EXTENSION_NAME
                                                                , pattern QCOM_COOPERATIVE_MATRIX_CONVERSION_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_CONVERSION_FEATURES_QCOM))
-- | VkPhysicalDeviceCooperativeMatrixConversionFeaturesQCOM - Structure
-- describing cooperative matrix conversion features that can be supported
-- by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM', it /must/ add
-- an instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_cooperative_matrix_conversion VK_QCOM_cooperative_matrix_conversion>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM = PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM
  { -- | #features-cooperativeMatrixConversion# @cooperativeMatrixConversion@
    -- indicates that the implementation supports the
    -- @CooperativeMatrixConversionQCOM@ SPIR-V capability.
    cooperativeMatrixConversion :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM

instance ToCStruct PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_CONVERSION_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixConversion))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_CONVERSION_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM where
  peekCStruct p = do
    cooperativeMatrixConversion <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM
             (bool32ToBool cooperativeMatrixConversion)

instance Storable PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM where
  zero = PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM
           zero


type QCOM_COOPERATIVE_MATRIX_CONVERSION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_COOPERATIVE_MATRIX_CONVERSION_SPEC_VERSION"
pattern QCOM_COOPERATIVE_MATRIX_CONVERSION_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_COOPERATIVE_MATRIX_CONVERSION_SPEC_VERSION = 1


type QCOM_COOPERATIVE_MATRIX_CONVERSION_EXTENSION_NAME = "VK_QCOM_cooperative_matrix_conversion"

-- No documentation found for TopLevel "VK_QCOM_COOPERATIVE_MATRIX_CONVERSION_EXTENSION_NAME"
pattern QCOM_COOPERATIVE_MATRIX_CONVERSION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_COOPERATIVE_MATRIX_CONVERSION_EXTENSION_NAME = "VK_QCOM_cooperative_matrix_conversion"

