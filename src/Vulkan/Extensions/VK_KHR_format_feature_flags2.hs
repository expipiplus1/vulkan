{-# language CPP #-}
-- | = Name
--
-- VK_KHR_format_feature_flags2 - device extension
--
-- == VK_KHR_format_feature_flags2
--
-- [__Name String__]
--     @VK_KHR_format_feature_flags2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     361
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Lionel Landwerlin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_format_feature_flags2] @llandwerlin%0A<<Here describe the issue or question you have about the VK_KHR_format_feature_flags2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-07-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Lionel Landwerlin, Intel
--
--     -   Jason Ekstrand, Intel
--
--     -   Tobias Hector, AMD
--
--     -   Spencer Fricke, Samsung Electronics
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, ARM
--
-- == Description
--
-- This extension adds a new
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlagBits2KHR'
-- 64bits format feature flag type to extend the existing
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits' which
-- is limited to 31 flags. At the time of this writing 29 bits of
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits' are
-- already used.
--
-- Because
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2'
-- is already defined to extend the Vulkan 1.0
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
-- entry point, this extension defines a new 'FormatProperties3KHR' to
-- extend the 'Vulkan.Core10.DeviceInitialization.FormatProperties'.
--
-- On top of replicating all the bits from
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlagBits2KHR'
-- adds the following bits :
--
-- -   'Vulkan.Extensions.VK_KHR_acceleration_structure.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT_KHR'
--     and
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT_KHR'
--     indicate that an implementation supports respectively reading and
--     writing a given 'Vulkan.Core10.Enums.Format.Format' through storage
--     operations without specifying the format in the shader.
--
-- -   'Vulkan.Extensions.VK_KHR_acceleration_structure.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT_KHR'
--     indicates that an implementation supports depth comparison performed
--     by @OpImage*Dref@ instructions on a given
--     'Vulkan.Core10.Enums.Format.Format'. Previously the result of
--     executing a @OpImage*Dref*@ instruction on an image view, where the
--     @format@ was not one of the depth\/stencil formats with a depth
--     component, was undefined. This bit clarifies on which formats such
--     instructions can be used.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2':
--
--     -   'FormatProperties3KHR'
--
-- == New Enums
--
-- -   'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlagBits2KHR'
--
-- == New Bitmasks
--
-- -   'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlags2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME'
--
-- -   'KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FORMAT_PROPERTIES_3_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-07-21 (Lionel Landwerlin)
--
--     -   Initial draft
--
-- == See Also
--
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlagBits2KHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlags2KHR',
-- 'FormatProperties3KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_format_feature_flags2  ( FormatProperties3KHR(..)
                                                       , KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION
                                                       , pattern KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION
                                                       , KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME
                                                       , pattern KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME
                                                       , FormatFeatureFlagBits2KHR(..)
                                                       , FormatFeatureFlags2KHR
                                                       , Flags64
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
import Vulkan.Extensions.VK_KHR_acceleration_structure (FormatFeatureFlags2KHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FORMAT_PROPERTIES_3_KHR))
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Extensions.VK_KHR_acceleration_structure (FormatFeatureFlagBits2KHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (FormatFeatureFlags2KHR)
-- | VkFormatProperties3KHR - Structure specifying image format properties
--
-- = Description
--
-- The bits reported in @linearTilingFeatures@, @optimalTilingFeatures@ and
-- @bufferFeatures@ /must/ include the bits reported in the corresponding
-- fields of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2'::@formatProperties@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlags2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data FormatProperties3KHR = FormatProperties3KHR
  { -- | @linearTilingFeatures@ is a bitmask of
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlagBits2KHR'
    -- specifying features supported by images created with a @tiling@
    -- parameter of 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'.
    linearTilingFeatures :: FormatFeatureFlags2KHR
  , -- | @optimalTilingFeatures@ is a bitmask of
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlagBits2KHR'
    -- specifying features supported by images created with a @tiling@
    -- parameter of 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
    optimalTilingFeatures :: FormatFeatureFlags2KHR
  , -- | @bufferFeatures@ is a bitmask of
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlagBits2KHR'
    -- specifying features supported by buffers.
    bufferFeatures :: FormatFeatureFlags2KHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FormatProperties3KHR)
#endif
deriving instance Show FormatProperties3KHR

instance ToCStruct FormatProperties3KHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FormatProperties3KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FORMAT_PROPERTIES_3_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FormatFeatureFlags2KHR)) (linearTilingFeatures)
    poke ((p `plusPtr` 24 :: Ptr FormatFeatureFlags2KHR)) (optimalTilingFeatures)
    poke ((p `plusPtr` 32 :: Ptr FormatFeatureFlags2KHR)) (bufferFeatures)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FORMAT_PROPERTIES_3_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct FormatProperties3KHR where
  peekCStruct p = do
    linearTilingFeatures <- peek @FormatFeatureFlags2KHR ((p `plusPtr` 16 :: Ptr FormatFeatureFlags2KHR))
    optimalTilingFeatures <- peek @FormatFeatureFlags2KHR ((p `plusPtr` 24 :: Ptr FormatFeatureFlags2KHR))
    bufferFeatures <- peek @FormatFeatureFlags2KHR ((p `plusPtr` 32 :: Ptr FormatFeatureFlags2KHR))
    pure $ FormatProperties3KHR
             linearTilingFeatures optimalTilingFeatures bufferFeatures

instance Storable FormatProperties3KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FormatProperties3KHR where
  zero = FormatProperties3KHR
           zero
           zero
           zero


type KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION"
pattern KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION = 1


type KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME = "VK_KHR_format_feature_flags2"

-- No documentation found for TopLevel "VK_KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME"
pattern KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME = "VK_KHR_format_feature_flags2"

