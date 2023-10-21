{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_image_processing2 - device extension
--
-- == VK_QCOM_image_processing2
--
-- [__Name String__]
--     @VK_QCOM_image_processing2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     519
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing VK_QCOM_image_processing>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_image_processing2] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_image_processing2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-10
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_image_processing2.html SPV_QCOM_image_processing2>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/qcom/GLSL_QCOM_image_processing2.txt GL_QCOM_image_processing2>
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension enables support for the SPIR-V @TextureBlockMatch2QCOM@
-- capability. It builds on the functionality of QCOM_image_processing with
-- the addition of 4 new image processing operations.
--
-- -   The @opImageBlockMatchWindowSADQCOM@\` SPIR-V instruction builds
--     upon the functionality of @opImageBlockMatchSADQCOM@\` by repeatedly
--     performing block match operations across a 2D window. The “2D
--     windowExtent” and “compareMode” are are specified by
--     'SamplerBlockMatchWindowCreateInfoQCOM' in the sampler used to
--     create the /target image/. Like @OpImageBlockMatchSADQCOM@,
--     @opImageBlockMatchWindowSADQCOM@ computes an error metric, that
--     describes whether a block of texels in the /target image/ matches a
--     corresponding block of texels in the /reference image/. Unlike
--     @OpImageBlockMatchSADQCOM@, this instruction computes an error
--     metric at each (X,Y) location within the 2D window and returns
--     either the minimum or maximum error. The instruction only supports
--     single-component formats. Refer to the pseudocode below for details.
--
-- -   The @opImageBlockMatchWindowSSDQCOM@ follows the same pattern,
--     computing the SSD error metric at each location within the 2D
--     window.
--
-- -   The @opImageBlockMatchGatherSADQCOM@ builds upon
--     @OpImageBlockMatchSADQCOM@. This instruction computes an error
--     metric, that describes whether a block of texels in the /target
--     image/ matches a corresponding block of texels in the /reference
--     image/. The instruction computes the SAD error metric at 4 texel
--     offsets and returns the error metric for each offset in the
--     X,Y,Z,and W components. The instruction only supports
--     single-component texture formats. Refer to the pseudocode below for
--     details.
--
-- -   The @opImageBlockMatchGatherSSDQCOM@ follows the same pattern,
--     computing the SSD error metric for 4 offsets.
--
-- Each of the above 4 image processing instructions are limited to
-- single-component formats.
--
-- Below is the pseudocode for GLSL built-in function
-- @textureWindowBlockMatchSADQCOM@. The pseudocode for
-- @textureWindowBlockMatchSSD@ is identical other than replacing all
-- instances of @\"SAD\"@ with @\"SSD\"@.
--
-- > vec4 textureBlockMatchWindowSAD( sampler2D target,
-- >                                  uvec2 targetCoord,
-- >                                  samler2D reference,
-- >                                  uvec2 refCoord,
-- >                                  uvec2 blocksize) {
-- >     // compareMode (MIN or MAX) comes from the vkSampler associated with `target`
-- >     // uvec2 window  comes from the vkSampler associated with `target`
-- >     minSAD = INF;
-- >     maxSAD = -INF;
-- >     uvec2 minCoord;
-- >     uvec2 maxCoord;
-- >
-- >     for (uint x=0, x < window.width; x++) {
-- >         for (uint y=0; y < window.height; y++) {
-- >             float SAD = textureBlockMatchSAD(target,
-- >                                             targetCoord + uvec2(x, y),
-- >                                             reference,
-- >                                             refCoord,
-- >                                             blocksize).x;
-- >             // Note: the below comparison operator will produce undefined results
-- >             // if SAD is a denorm value.
-- >             if (SAD < minSAD) {
-- >                 minSAD = SAD;
-- >                 minCoord = uvec2(x,y);
-- >             }
-- >             if (SAD > maxSAD) {
-- >                 maxSAD = SAD;
-- >                 maxCoord = uvec2(x,y);
-- >             }
-- >         }
-- >     }
-- >     if (compareMode=MIN) {
-- >         return vec4(minSAD, minCoord.x, minCoord.y, 0.0);
-- >     } else {
-- >         return vec4(maxSAD, maxCoord.x, maxCoord.y, 0.0);
-- >     }
-- > }
--
-- Below is the pseudocode for @textureBlockMatchGatherSADQCOM@. The
-- pseudocode for @textureBlockMatchGatherSSD@ follows an identical
-- pattern.
--
-- > vec4 textureBlockMatchGatherSAD( sampler2D target,
-- >                                  uvec2 targetCoord,
-- >                                  samler2D reference,
-- >                                  uvec2 refCoord,
-- >                                  uvec2 blocksize) {
-- >     vec4 out;
-- >     for (uint x=0, x<4; x++) {
-- >             float SAD = textureBlockMatchSAD(target,
-- >                                             targetCoord + uvec2(x, 0),
-- >                                             reference,
-- >                                             refCoord,
-- >                                             blocksize).x;
-- >             out[x] = SAD;
-- >     }
-- >     return out;
-- > }
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageProcessing2FeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceImageProcessing2PropertiesQCOM'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerBlockMatchWindowCreateInfoQCOM'
--
-- == New Enums
--
-- -   'BlockMatchWindowCompareModeQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_IMAGE_PROCESSING_2_EXTENSION_NAME'
--
-- -   'QCOM_IMAGE_PROCESSING_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_PROPERTIES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_BLOCK_MATCH_WINDOW_CREATE_INFO_QCOM'
--
-- == Issues
--
-- 1) What is the precision of the min\/max comparison checks?
--
-- __RESOLVED__: Intermediate computations for the new operations are
-- performed at 16-bit floating point precision. If the value of
-- @\"float SAD\"@ in the above code sample is a 16-bit denorm value, then
-- behavior of the MIN\/MAX comparison is undefined.
--
-- == Version History
--
-- -   Revision 1, 2023-03-10 (Jeff Leger)
--
-- == See Also
--
-- 'BlockMatchWindowCompareModeQCOM',
-- 'PhysicalDeviceImageProcessing2FeaturesQCOM',
-- 'PhysicalDeviceImageProcessing2PropertiesQCOM',
-- 'SamplerBlockMatchWindowCreateInfoQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_image_processing2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_image_processing2  ( PhysicalDeviceImageProcessing2FeaturesQCOM(..)
                                                    , PhysicalDeviceImageProcessing2PropertiesQCOM(..)
                                                    , SamplerBlockMatchWindowCreateInfoQCOM(..)
                                                    , BlockMatchWindowCompareModeQCOM( BLOCK_MATCH_WINDOW_COMPARE_MODE_MIN_QCOM
                                                                                     , BLOCK_MATCH_WINDOW_COMPARE_MODE_MAX_QCOM
                                                                                     , ..
                                                                                     )
                                                    , QCOM_IMAGE_PROCESSING_2_SPEC_VERSION
                                                    , pattern QCOM_IMAGE_PROCESSING_2_SPEC_VERSION
                                                    , QCOM_IMAGE_PROCESSING_2_EXTENSION_NAME
                                                    , pattern QCOM_IMAGE_PROCESSING_2_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_PROPERTIES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_BLOCK_MATCH_WINDOW_CREATE_INFO_QCOM))
-- | VkPhysicalDeviceImageProcessing2FeaturesQCOM - Structure describing
-- image processing features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceImageProcessing2FeaturesQCOM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImageProcessing2FeaturesQCOM' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing2 VK_QCOM_image_processing2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageProcessing2FeaturesQCOM = PhysicalDeviceImageProcessing2FeaturesQCOM
  { -- | #features-textureBlockMatch2# @textureBlockMatch2@ indicates that the
    -- implementation supports shader modules that declare the
    -- @TextureBlockMatch2QCOM@ capability.
    textureBlockMatch2 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageProcessing2FeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceImageProcessing2FeaturesQCOM

instance ToCStruct PhysicalDeviceImageProcessing2FeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageProcessing2FeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (textureBlockMatch2))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageProcessing2FeaturesQCOM where
  peekCStruct p = do
    textureBlockMatch2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImageProcessing2FeaturesQCOM
             (bool32ToBool textureBlockMatch2)

instance Storable PhysicalDeviceImageProcessing2FeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageProcessing2FeaturesQCOM where
  zero = PhysicalDeviceImageProcessing2FeaturesQCOM
           zero


-- | VkPhysicalDeviceImageProcessing2PropertiesQCOM - Structure containing
-- image processing2 properties
--
-- = Description
--
-- If the 'PhysicalDeviceImageProcessing2PropertiesQCOM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These are properties of the image processing2 information of a physical
-- device.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing2 VK_QCOM_image_processing2>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageProcessing2PropertiesQCOM = PhysicalDeviceImageProcessing2PropertiesQCOM
  { -- | #limits-blockmatch-maxWindowExtent# @maxBlockMatchWindow@ is a
    -- 'Vulkan.Core10.FundamentalTypes.Extent2D' describing the largest
    -- dimensions (@width@ and @height@) that /can/ be specified for the block
    -- match window.
    maxBlockMatchWindow :: Extent2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageProcessing2PropertiesQCOM)
#endif
deriving instance Show PhysicalDeviceImageProcessing2PropertiesQCOM

instance ToCStruct PhysicalDeviceImageProcessing2PropertiesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageProcessing2PropertiesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (maxBlockMatchWindow)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PhysicalDeviceImageProcessing2PropertiesQCOM where
  peekCStruct p = do
    maxBlockMatchWindow <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    pure $ PhysicalDeviceImageProcessing2PropertiesQCOM
             maxBlockMatchWindow

instance Storable PhysicalDeviceImageProcessing2PropertiesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageProcessing2PropertiesQCOM where
  zero = PhysicalDeviceImageProcessing2PropertiesQCOM
           zero


-- | VkSamplerBlockMatchWindowCreateInfoQCOM - Structure specifying the block
-- match window parameters
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing2 VK_QCOM_image_processing2>,
-- 'BlockMatchWindowCompareModeQCOM',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerBlockMatchWindowCreateInfoQCOM = SamplerBlockMatchWindowCreateInfoQCOM
  { -- | @windowExtent@ is a 'Vulkan.Core10.FundamentalTypes.Extent2D' specifying
    -- a the width and height of the block match window.
    windowExtent :: Extent2D
  , -- | @windowCompareMode@ is a 'BlockMatchWindowCompareModeQCOM' specifying
    -- the compare mode.
    --
    -- #VUID-VkSamplerBlockMatchWindowCreateInfoQCOM-windowCompareMode-parameter#
    -- @windowCompareMode@ /must/ be a valid 'BlockMatchWindowCompareModeQCOM'
    -- value
    windowCompareMode :: BlockMatchWindowCompareModeQCOM
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerBlockMatchWindowCreateInfoQCOM)
#endif
deriving instance Show SamplerBlockMatchWindowCreateInfoQCOM

instance ToCStruct SamplerBlockMatchWindowCreateInfoQCOM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerBlockMatchWindowCreateInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_BLOCK_MATCH_WINDOW_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (windowExtent)
    poke ((p `plusPtr` 24 :: Ptr BlockMatchWindowCompareModeQCOM)) (windowCompareMode)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_BLOCK_MATCH_WINDOW_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr BlockMatchWindowCompareModeQCOM)) (zero)
    f

instance FromCStruct SamplerBlockMatchWindowCreateInfoQCOM where
  peekCStruct p = do
    windowExtent <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    windowCompareMode <- peek @BlockMatchWindowCompareModeQCOM ((p `plusPtr` 24 :: Ptr BlockMatchWindowCompareModeQCOM))
    pure $ SamplerBlockMatchWindowCreateInfoQCOM
             windowExtent windowCompareMode

instance Storable SamplerBlockMatchWindowCreateInfoQCOM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SamplerBlockMatchWindowCreateInfoQCOM where
  zero = SamplerBlockMatchWindowCreateInfoQCOM
           zero
           zero


-- | VkBlockMatchWindowCompareModeQCOM - Block match window compare modes
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing2 VK_QCOM_image_processing2>,
-- 'SamplerBlockMatchWindowCreateInfoQCOM'
newtype BlockMatchWindowCompareModeQCOM = BlockMatchWindowCompareModeQCOM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'BLOCK_MATCH_WINDOW_COMPARE_MODE_MIN_QCOM' specifies that windowed block
-- match operations return the minimum error within the window.
pattern BLOCK_MATCH_WINDOW_COMPARE_MODE_MIN_QCOM = BlockMatchWindowCompareModeQCOM 0

-- | 'BLOCK_MATCH_WINDOW_COMPARE_MODE_MAX_QCOM' specifies that windowed block
-- match operations return the maximum error within the window.
pattern BLOCK_MATCH_WINDOW_COMPARE_MODE_MAX_QCOM = BlockMatchWindowCompareModeQCOM 1

{-# COMPLETE
  BLOCK_MATCH_WINDOW_COMPARE_MODE_MIN_QCOM
  , BLOCK_MATCH_WINDOW_COMPARE_MODE_MAX_QCOM ::
    BlockMatchWindowCompareModeQCOM
  #-}

conNameBlockMatchWindowCompareModeQCOM :: String
conNameBlockMatchWindowCompareModeQCOM = "BlockMatchWindowCompareModeQCOM"

enumPrefixBlockMatchWindowCompareModeQCOM :: String
enumPrefixBlockMatchWindowCompareModeQCOM = "BLOCK_MATCH_WINDOW_COMPARE_MODE_M"

showTableBlockMatchWindowCompareModeQCOM :: [(BlockMatchWindowCompareModeQCOM, String)]
showTableBlockMatchWindowCompareModeQCOM =
  [
    ( BLOCK_MATCH_WINDOW_COMPARE_MODE_MIN_QCOM
    , "IN_QCOM"
    )
  ,
    ( BLOCK_MATCH_WINDOW_COMPARE_MODE_MAX_QCOM
    , "AX_QCOM"
    )
  ]

instance Show BlockMatchWindowCompareModeQCOM where
  showsPrec =
    enumShowsPrec
      enumPrefixBlockMatchWindowCompareModeQCOM
      showTableBlockMatchWindowCompareModeQCOM
      conNameBlockMatchWindowCompareModeQCOM
      (\(BlockMatchWindowCompareModeQCOM x) -> x)
      (showsPrec 11)

instance Read BlockMatchWindowCompareModeQCOM where
  readPrec =
    enumReadPrec
      enumPrefixBlockMatchWindowCompareModeQCOM
      showTableBlockMatchWindowCompareModeQCOM
      conNameBlockMatchWindowCompareModeQCOM
      BlockMatchWindowCompareModeQCOM

type QCOM_IMAGE_PROCESSING_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_IMAGE_PROCESSING_2_SPEC_VERSION"
pattern QCOM_IMAGE_PROCESSING_2_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_IMAGE_PROCESSING_2_SPEC_VERSION = 1


type QCOM_IMAGE_PROCESSING_2_EXTENSION_NAME = "VK_QCOM_image_processing2"

-- No documentation found for TopLevel "VK_QCOM_IMAGE_PROCESSING_2_EXTENSION_NAME"
pattern QCOM_IMAGE_PROCESSING_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_IMAGE_PROCESSING_2_EXTENSION_NAME = "VK_QCOM_image_processing2"

