{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_filter_cubic_weights - device extension
--
-- == VK_QCOM_filter_cubic_weights
--
-- [__Name String__]
--     @VK_QCOM_filter_cubic_weights@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     520
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_filter_cubic_weights] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_filter_cubic_weights extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-06-23
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension extends cubic filtering by adding the ability to select a
-- set of weights. Without this extension, the weights used in cubic
-- filtering are limited to those corresponding to a Catmull-Rom spline.
-- This extension adds support for 3 additional spline weights.
--
-- This extension adds a new structure that /can/ be added to the @pNext@
-- chain of 'Vulkan.Core10.Sampler.SamplerCreateInfo' that /can/ be used to
-- specify which set of cubic weights are used in cubic filtering. A
-- similar structure can be added to the @pNext@ chain of
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BlitImageInfo2' to
-- specify cubic weights used in a blit operation.
--
-- With this extension weights corresponding to the following additional
-- splines can be selected for cubic filtered sampling and blits:
--
-- -   Zero Tangent Cardinal
--
-- -   B-Spline
--
-- -   Mitchell-Netravali
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BlitImageInfo2':
--
--     -   'BlitImageCubicWeightsInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCubicWeightsFeaturesQCOM'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerCubicWeightsCreateInfoQCOM'
--
-- == New Enums
--
-- -   'CubicFilterWeightsQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_FILTER_CUBIC_WEIGHTS_EXTENSION_NAME'
--
-- -   'QCOM_FILTER_CUBIC_WEIGHTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BLIT_IMAGE_CUBIC_WEIGHTS_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_WEIGHTS_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CUBIC_WEIGHTS_CREATE_INFO_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2023-06-23 (jleger)
--
--     -   Initial version
--
-- == See Also
--
-- 'BlitImageCubicWeightsInfoQCOM', 'CubicFilterWeightsQCOM',
-- 'PhysicalDeviceCubicWeightsFeaturesQCOM',
-- 'SamplerCubicWeightsCreateInfoQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_filter_cubic_weights Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_filter_cubic_weights  ( PhysicalDeviceCubicWeightsFeaturesQCOM(..)
                                                       , SamplerCubicWeightsCreateInfoQCOM(..)
                                                       , BlitImageCubicWeightsInfoQCOM(..)
                                                       , CubicFilterWeightsQCOM( CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM
                                                                               , CUBIC_FILTER_WEIGHTS_ZERO_TANGENT_CARDINAL_QCOM
                                                                               , CUBIC_FILTER_WEIGHTS_B_SPLINE_QCOM
                                                                               , CUBIC_FILTER_WEIGHTS_MITCHELL_NETRAVALI_QCOM
                                                                               , ..
                                                                               )
                                                       , QCOM_FILTER_CUBIC_WEIGHTS_SPEC_VERSION
                                                       , pattern QCOM_FILTER_CUBIC_WEIGHTS_SPEC_VERSION
                                                       , QCOM_FILTER_CUBIC_WEIGHTS_EXTENSION_NAME
                                                       , pattern QCOM_FILTER_CUBIC_WEIGHTS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BLIT_IMAGE_CUBIC_WEIGHTS_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_WEIGHTS_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_CUBIC_WEIGHTS_CREATE_INFO_QCOM))
-- | VkPhysicalDeviceCubicWeightsFeaturesQCOM - Structure describing cubic
-- weight selection features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceCubicWeightsFeaturesQCOM' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceCubicWeightsFeaturesQCOM' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_filter_cubic_weights VK_QCOM_filter_cubic_weights>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCubicWeightsFeaturesQCOM = PhysicalDeviceCubicWeightsFeaturesQCOM
  { -- | #features-filter-cubic-weight-selection# @selectableCubicWeights@
    -- indicates that the implementation supports the selection of filter cubic
    -- weights.
    selectableCubicWeights :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCubicWeightsFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceCubicWeightsFeaturesQCOM

instance ToCStruct PhysicalDeviceCubicWeightsFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCubicWeightsFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_WEIGHTS_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (selectableCubicWeights))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_WEIGHTS_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCubicWeightsFeaturesQCOM where
  peekCStruct p = do
    selectableCubicWeights <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCubicWeightsFeaturesQCOM
             (bool32ToBool selectableCubicWeights)

instance Storable PhysicalDeviceCubicWeightsFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCubicWeightsFeaturesQCOM where
  zero = PhysicalDeviceCubicWeightsFeaturesQCOM
           zero


-- | VkSamplerCubicWeightsCreateInfoQCOM - Structure specifying sampler cubic
-- weights
--
-- = Description
--
-- If the @pNext@ chain of 'Vulkan.Core10.Sampler.SamplerCreateInfo'
-- includes a 'SamplerCubicWeightsCreateInfoQCOM' structure, then that
-- structure specifies which cubic weights are used.
--
-- If that structure is not present, @cubicWeights@ is considered to be
-- 'CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_filter_cubic_weights VK_QCOM_filter_cubic_weights>,
-- 'CubicFilterWeightsQCOM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerCubicWeightsCreateInfoQCOM = SamplerCubicWeightsCreateInfoQCOM
  { -- | @cubicWeights@ is a 'CubicFilterWeightsQCOM' value controlling which
    -- cubic weights are used.
    --
    -- #VUID-VkSamplerCubicWeightsCreateInfoQCOM-cubicWeights-parameter#
    -- @cubicWeights@ /must/ be a valid 'CubicFilterWeightsQCOM' value
    cubicWeights :: CubicFilterWeightsQCOM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerCubicWeightsCreateInfoQCOM)
#endif
deriving instance Show SamplerCubicWeightsCreateInfoQCOM

instance ToCStruct SamplerCubicWeightsCreateInfoQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerCubicWeightsCreateInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CUBIC_WEIGHTS_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CubicFilterWeightsQCOM)) (cubicWeights)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CUBIC_WEIGHTS_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CubicFilterWeightsQCOM)) (zero)
    f

instance FromCStruct SamplerCubicWeightsCreateInfoQCOM where
  peekCStruct p = do
    cubicWeights <- peek @CubicFilterWeightsQCOM ((p `plusPtr` 16 :: Ptr CubicFilterWeightsQCOM))
    pure $ SamplerCubicWeightsCreateInfoQCOM
             cubicWeights

instance Storable SamplerCubicWeightsCreateInfoQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SamplerCubicWeightsCreateInfoQCOM where
  zero = SamplerCubicWeightsCreateInfoQCOM
           zero


-- | VkBlitImageCubicWeightsInfoQCOM - Structure specifying image blit cubic
-- weight info
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_filter_cubic_weights VK_QCOM_filter_cubic_weights>,
-- 'CubicFilterWeightsQCOM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BlitImageCubicWeightsInfoQCOM = BlitImageCubicWeightsInfoQCOM
  { -- | @cubicWeights@ is a 'CubicFilterWeightsQCOM' value controlling cubic
    -- filter weights for the blit.
    --
    -- #VUID-VkBlitImageCubicWeightsInfoQCOM-cubicWeights-parameter#
    -- @cubicWeights@ /must/ be a valid 'CubicFilterWeightsQCOM' value
    cubicWeights :: CubicFilterWeightsQCOM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BlitImageCubicWeightsInfoQCOM)
#endif
deriving instance Show BlitImageCubicWeightsInfoQCOM

instance ToCStruct BlitImageCubicWeightsInfoQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BlitImageCubicWeightsInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BLIT_IMAGE_CUBIC_WEIGHTS_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CubicFilterWeightsQCOM)) (cubicWeights)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BLIT_IMAGE_CUBIC_WEIGHTS_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CubicFilterWeightsQCOM)) (zero)
    f

instance FromCStruct BlitImageCubicWeightsInfoQCOM where
  peekCStruct p = do
    cubicWeights <- peek @CubicFilterWeightsQCOM ((p `plusPtr` 16 :: Ptr CubicFilterWeightsQCOM))
    pure $ BlitImageCubicWeightsInfoQCOM
             cubicWeights

instance Storable BlitImageCubicWeightsInfoQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BlitImageCubicWeightsInfoQCOM where
  zero = BlitImageCubicWeightsInfoQCOM
           zero


-- | VkCubicFilterWeightsQCOM - Specify cubic weights for texture filtering
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_filter_cubic_weights VK_QCOM_filter_cubic_weights>,
-- 'BlitImageCubicWeightsInfoQCOM', 'SamplerCubicWeightsCreateInfoQCOM'
newtype CubicFilterWeightsQCOM = CubicFilterWeightsQCOM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM' specifies Catmull-Rom weights.
pattern CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM = CubicFilterWeightsQCOM 0

-- | 'CUBIC_FILTER_WEIGHTS_ZERO_TANGENT_CARDINAL_QCOM' specifies Zero Tangent
-- Cardinal weights.
pattern CUBIC_FILTER_WEIGHTS_ZERO_TANGENT_CARDINAL_QCOM = CubicFilterWeightsQCOM 1

-- | 'CUBIC_FILTER_WEIGHTS_B_SPLINE_QCOM' specifies B-Spline weights.
pattern CUBIC_FILTER_WEIGHTS_B_SPLINE_QCOM = CubicFilterWeightsQCOM 2

-- | 'CUBIC_FILTER_WEIGHTS_MITCHELL_NETRAVALI_QCOM' specifies
-- Mitchell-Netravali weights.
pattern CUBIC_FILTER_WEIGHTS_MITCHELL_NETRAVALI_QCOM = CubicFilterWeightsQCOM 3

{-# COMPLETE
  CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM
  , CUBIC_FILTER_WEIGHTS_ZERO_TANGENT_CARDINAL_QCOM
  , CUBIC_FILTER_WEIGHTS_B_SPLINE_QCOM
  , CUBIC_FILTER_WEIGHTS_MITCHELL_NETRAVALI_QCOM ::
    CubicFilterWeightsQCOM
  #-}

conNameCubicFilterWeightsQCOM :: String
conNameCubicFilterWeightsQCOM = "CubicFilterWeightsQCOM"

enumPrefixCubicFilterWeightsQCOM :: String
enumPrefixCubicFilterWeightsQCOM = "CUBIC_FILTER_WEIGHTS_"

showTableCubicFilterWeightsQCOM :: [(CubicFilterWeightsQCOM, String)]
showTableCubicFilterWeightsQCOM =
  [
    ( CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM
    , "CATMULL_ROM_QCOM"
    )
  ,
    ( CUBIC_FILTER_WEIGHTS_ZERO_TANGENT_CARDINAL_QCOM
    , "ZERO_TANGENT_CARDINAL_QCOM"
    )
  ,
    ( CUBIC_FILTER_WEIGHTS_B_SPLINE_QCOM
    , "B_SPLINE_QCOM"
    )
  ,
    ( CUBIC_FILTER_WEIGHTS_MITCHELL_NETRAVALI_QCOM
    , "MITCHELL_NETRAVALI_QCOM"
    )
  ]

instance Show CubicFilterWeightsQCOM where
  showsPrec =
    enumShowsPrec
      enumPrefixCubicFilterWeightsQCOM
      showTableCubicFilterWeightsQCOM
      conNameCubicFilterWeightsQCOM
      (\(CubicFilterWeightsQCOM x) -> x)
      (showsPrec 11)

instance Read CubicFilterWeightsQCOM where
  readPrec =
    enumReadPrec
      enumPrefixCubicFilterWeightsQCOM
      showTableCubicFilterWeightsQCOM
      conNameCubicFilterWeightsQCOM
      CubicFilterWeightsQCOM

type QCOM_FILTER_CUBIC_WEIGHTS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_FILTER_CUBIC_WEIGHTS_SPEC_VERSION"
pattern QCOM_FILTER_CUBIC_WEIGHTS_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_FILTER_CUBIC_WEIGHTS_SPEC_VERSION = 1


type QCOM_FILTER_CUBIC_WEIGHTS_EXTENSION_NAME = "VK_QCOM_filter_cubic_weights"

-- No documentation found for TopLevel "VK_QCOM_FILTER_CUBIC_WEIGHTS_EXTENSION_NAME"
pattern QCOM_FILTER_CUBIC_WEIGHTS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_FILTER_CUBIC_WEIGHTS_EXTENSION_NAME = "VK_QCOM_filter_cubic_weights"

