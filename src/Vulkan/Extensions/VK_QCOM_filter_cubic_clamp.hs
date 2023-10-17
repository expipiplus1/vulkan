{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_filter_cubic_clamp - device extension
--
-- == VK_QCOM_filter_cubic_clamp
--
-- [__Name String__]
--     @VK_QCOM_filter_cubic_clamp@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     522
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Version 1.2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_sampler_filter_minmax VK_EXT_sampler_filter_minmax>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_filter_cubic_clamp] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_filter_cubic_clamp extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-08-02
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension extends cubic filtering by adding the ability to enable
-- an anti-ringing clamp. Cubic filtering samples from a 4x4 region of
-- texels and computes a cubic weighted average of the region. In some
-- cases, the resulting value is outside the range of any of the texels in
-- the 4x4 region. This is sometimes referred to as “filter overshoot” or
-- “filter ringing” and can occur when there is a sharp discontinuity in
-- the 4x4 region being filtered. For some use cases this “ringing” can
-- produces unacceptable artifacts.
--
-- The solution to the ringing problem is to clamp the post-cubic-filtered
-- value to be within the max and min of texel values in the 4x4 region.
-- While such “range clamping” can be performed in shader code, the
-- additional texture fetches and clamping ALU operations can be costly.
--
-- Certain Adreno GPUs are able to perform the range clamp in the texture
-- unit during cubic filtering at significant performance\/power savings
-- versus a shader-based clamping approach. This extension exposes such
-- hardware functionality.
--
-- This extension extends
-- 'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode', adding
-- 'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
-- which enables the range clamp operation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCubicClampFeaturesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_FILTER_CUBIC_CLAMP_EXTENSION_NAME'
--
-- -   'QCOM_FILTER_CUBIC_CLAMP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode':
--
--     -   'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_CLAMP_FEATURES_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2023-08-02 (jleger)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceCubicClampFeaturesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_filter_cubic_clamp Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_filter_cubic_clamp  ( PhysicalDeviceCubicClampFeaturesQCOM(..)
                                                     , QCOM_FILTER_CUBIC_CLAMP_SPEC_VERSION
                                                     , pattern QCOM_FILTER_CUBIC_CLAMP_SPEC_VERSION
                                                     , QCOM_FILTER_CUBIC_CLAMP_EXTENSION_NAME
                                                     , pattern QCOM_FILTER_CUBIC_CLAMP_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_CLAMP_FEATURES_QCOM))
-- | VkPhysicalDeviceCubicClampFeaturesQCOM - Structure describing cubic
-- clamp features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCubicClampFeaturesQCOM' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceCubicClampFeaturesQCOM' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_filter_cubic_clamp VK_QCOM_filter_cubic_clamp>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCubicClampFeaturesQCOM = PhysicalDeviceCubicClampFeaturesQCOM
  { -- | #features-filter-cubic-range-clamp# @cubicRangeClamp@ indicates that the
    -- implementation supports cubic filtering in combination with a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-texel-range-clamp texel range clamp>.
    cubicRangeClamp :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCubicClampFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceCubicClampFeaturesQCOM

instance ToCStruct PhysicalDeviceCubicClampFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCubicClampFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_CLAMP_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cubicRangeClamp))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_CLAMP_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCubicClampFeaturesQCOM where
  peekCStruct p = do
    cubicRangeClamp <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCubicClampFeaturesQCOM
             (bool32ToBool cubicRangeClamp)

instance Storable PhysicalDeviceCubicClampFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCubicClampFeaturesQCOM where
  zero = PhysicalDeviceCubicClampFeaturesQCOM
           zero


type QCOM_FILTER_CUBIC_CLAMP_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_FILTER_CUBIC_CLAMP_SPEC_VERSION"
pattern QCOM_FILTER_CUBIC_CLAMP_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_FILTER_CUBIC_CLAMP_SPEC_VERSION = 1


type QCOM_FILTER_CUBIC_CLAMP_EXTENSION_NAME = "VK_QCOM_filter_cubic_clamp"

-- No documentation found for TopLevel "VK_QCOM_FILTER_CUBIC_CLAMP_EXTENSION_NAME"
pattern QCOM_FILTER_CUBIC_CLAMP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_FILTER_CUBIC_CLAMP_EXTENSION_NAME = "VK_QCOM_filter_cubic_clamp"

