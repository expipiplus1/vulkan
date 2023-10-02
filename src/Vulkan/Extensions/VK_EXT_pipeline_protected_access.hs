{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_protected_access - device extension
--
-- == VK_EXT_pipeline_protected_access
--
-- [__Name String__]
--     @VK_EXT_pipeline_protected_access@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     467
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_pipeline_protected_access] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_pipeline_protected_access extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_pipeline_protected_access.adoc VK_EXT_pipeline_protected_access>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-28
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Jörg Wagner, Arm
--
--     -   Ralph Potter, Samsung
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension allows protected memory access to be specified per
-- pipeline as opposed to per device. Through the usage of this extension,
-- any performance penalty paid due to access to protected memory will be
-- limited to the specific pipelines that make such accesses.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineProtectedAccessFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME'
--
-- -   'EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-07-28 (Shahbaz Youssefi)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDevicePipelineProtectedAccessFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pipeline_protected_access Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_protected_access  ( PhysicalDevicePipelineProtectedAccessFeaturesEXT(..)
                                                           , EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION
                                                           , pattern EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION
                                                           , EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME
                                                           , pattern EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT))
-- | VkPhysicalDevicePipelineProtectedAccessFeaturesEXT - Structure
-- describing support for specifying protected access on individual
-- pipelines
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePipelineProtectedAccessFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePipelineProtectedAccessFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_protected_access VK_EXT_pipeline_protected_access>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineProtectedAccessFeaturesEXT = PhysicalDevicePipelineProtectedAccessFeaturesEXT
  { -- | #features-pipelineProtectedAccess# @pipelineProtectedAccess@ indicates
    -- whether the implementation supports specifying protected access on
    -- individual pipelines.
    pipelineProtectedAccess :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineProtectedAccessFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePipelineProtectedAccessFeaturesEXT

instance ToCStruct PhysicalDevicePipelineProtectedAccessFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineProtectedAccessFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineProtectedAccess))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineProtectedAccessFeaturesEXT where
  peekCStruct p = do
    pipelineProtectedAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineProtectedAccessFeaturesEXT
             (bool32ToBool pipelineProtectedAccess)

instance Storable PhysicalDevicePipelineProtectedAccessFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineProtectedAccessFeaturesEXT where
  zero = PhysicalDevicePipelineProtectedAccessFeaturesEXT
           zero


type EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION"
pattern EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION = 1


type EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME = "VK_EXT_pipeline_protected_access"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME"
pattern EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME = "VK_EXT_pipeline_protected_access"

