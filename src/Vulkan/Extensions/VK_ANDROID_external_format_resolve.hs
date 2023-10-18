{-# language CPP #-}
-- | = Name
--
-- VK_ANDROID_external_format_resolve - device extension
--
-- == VK_ANDROID_external_format_resolve
--
-- [__Name String__]
--     @VK_ANDROID_external_format_resolve@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     469
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ANDROID_external_memory_android_hardware_buffer VK_ANDROID_external_memory_android_hardware_buffer>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Chris Forbes
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ANDROID_external_format_resolve] @chrisforbes%0A*Here describe the issue or question you have about the VK_ANDROID_external_format_resolve extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ANDROID_external_format_resolve.adoc VK_ANDROID_external_format_resolve>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Chris Forbes, Google
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Shahbaz Youssefi, Google
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Tony Zlatsinki, Nvidia
--
--     -   Daniel Koch, Nvidia
--
--     -   Jeff Leger, Qualcomm
--
--     -   Alex Walters, Imagination
--
--     -   Andrew Garrard, Imagination
--
--     -   Ralph Potter, Samsung
--
--     -   Ian Elliott, Google
--
-- == Description
--
-- This extension enables rendering to Android Hardware Buffers with
-- external formats which cannot be directly represented as renderable in
-- Vulkan, including Y′CBCR formats.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferPropertiesANDROID':
--
--     -   'AndroidHardwareBufferFormatResolvePropertiesANDROID'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExternalFormatResolveFeaturesANDROID'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceExternalFormatResolvePropertiesANDROID'
--
-- == New Enum Constants
--
-- -   'ANDROID_EXTERNAL_FORMAT_RESOLVE_EXTENSION_NAME'
--
-- -   'ANDROID_EXTERNAL_FORMAT_RESOLVE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_RESOLVE_PROPERTIES_ANDROID'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_FEATURES_ANDROID'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_PROPERTIES_ANDROID'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits':
--
--     -   'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--
-- == Version History
--
-- -   Revision 1, 2023-05-34 (Tobias Hector)
--
--     -   Initial version
--
-- == See Also
--
-- 'AndroidHardwareBufferFormatResolvePropertiesANDROID',
-- 'PhysicalDeviceExternalFormatResolveFeaturesANDROID',
-- 'PhysicalDeviceExternalFormatResolvePropertiesANDROID'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ANDROID_external_format_resolve Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ANDROID_external_format_resolve  ( PhysicalDeviceExternalFormatResolveFeaturesANDROID(..)
                                                             , PhysicalDeviceExternalFormatResolvePropertiesANDROID(..)
                                                             , AndroidHardwareBufferFormatResolvePropertiesANDROID(..)
                                                             , ANDROID_EXTERNAL_FORMAT_RESOLVE_SPEC_VERSION
                                                             , pattern ANDROID_EXTERNAL_FORMAT_RESOLVE_SPEC_VERSION
                                                             , ANDROID_EXTERNAL_FORMAT_RESOLVE_EXTENSION_NAME
                                                             , pattern ANDROID_EXTERNAL_FORMAT_RESOLVE_EXTENSION_NAME
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
import Vulkan.Core11.Enums.ChromaLocation (ChromaLocation)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_RESOLVE_PROPERTIES_ANDROID))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_FEATURES_ANDROID))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_PROPERTIES_ANDROID))
-- | VkPhysicalDeviceExternalFormatResolveFeaturesANDROID - Structure
-- describing whether external format resolves are supported
--
-- = Description
--
-- If the 'PhysicalDeviceExternalFormatResolveFeaturesANDROID' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceExternalFormatResolveFeaturesANDROID' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ANDROID_external_format_resolve VK_ANDROID_external_format_resolve>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExternalFormatResolveFeaturesANDROID = PhysicalDeviceExternalFormatResolveFeaturesANDROID
  { -- | #features-externalFormatResolve# @externalFormatResolve@ specifies
    -- whether external format resolves are supported.
    externalFormatResolve :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalFormatResolveFeaturesANDROID)
#endif
deriving instance Show PhysicalDeviceExternalFormatResolveFeaturesANDROID

instance ToCStruct PhysicalDeviceExternalFormatResolveFeaturesANDROID where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalFormatResolveFeaturesANDROID{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_FEATURES_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (externalFormatResolve))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_FEATURES_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExternalFormatResolveFeaturesANDROID where
  peekCStruct p = do
    externalFormatResolve <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExternalFormatResolveFeaturesANDROID
             (bool32ToBool externalFormatResolve)

instance Storable PhysicalDeviceExternalFormatResolveFeaturesANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalFormatResolveFeaturesANDROID where
  zero = PhysicalDeviceExternalFormatResolveFeaturesANDROID
           zero


-- | VkPhysicalDeviceExternalFormatResolvePropertiesANDROID - Structure
-- describing external format resolve supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceExternalFormatResolvePropertiesANDROID' structure
-- is included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ANDROID_external_format_resolve VK_ANDROID_external_format_resolve>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExternalFormatResolvePropertiesANDROID = PhysicalDeviceExternalFormatResolvePropertiesANDROID
  { -- | #limits-nullColorAttachmentWithExternalFormatResolve#
    -- @nullColorAttachmentWithExternalFormatResolve@ indicates that there
    -- /must/ be no color attachment image when performing external format
    -- resolves if it is 'Vulkan.Core10.FundamentalTypes.TRUE'.
    nullColorAttachmentWithExternalFormatResolve :: Bool
  , -- | #limits-externalFormatResolveChromaOffsetX#
    -- @externalFormatResolveChromaOffsetX@ indicates the
    -- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' that an
    -- implementation uses in the X axis for accesses to an external format
    -- image as a resolve attachment. This /must/ be consistent between
    -- external format resolves and load operations from external format
    -- resolve attachments to color attachments when
    -- @nullColorAttachmentWithExternalFormatResolve@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE'.
    externalFormatResolveChromaOffsetX :: ChromaLocation
  , -- | #limits-externalFormatResolveChromaOffsetY#
    -- @externalFormatResolveChromaOffsetY@ indicates the
    -- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' that an
    -- implementation uses in the Y axis for accesses to an external format
    -- image as a resolve attachment. This /must/ be consistent between
    -- external format resolves and load operations from external format
    -- resolve attachments to color attachments when
    -- @nullColorAttachmentWithExternalFormatResolve@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE'.
    externalFormatResolveChromaOffsetY :: ChromaLocation
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalFormatResolvePropertiesANDROID)
#endif
deriving instance Show PhysicalDeviceExternalFormatResolvePropertiesANDROID

instance ToCStruct PhysicalDeviceExternalFormatResolvePropertiesANDROID where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalFormatResolvePropertiesANDROID{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_PROPERTIES_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (nullColorAttachmentWithExternalFormatResolve))
    poke ((p `plusPtr` 20 :: Ptr ChromaLocation)) (externalFormatResolveChromaOffsetX)
    poke ((p `plusPtr` 24 :: Ptr ChromaLocation)) (externalFormatResolveChromaOffsetY)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_PROPERTIES_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr ChromaLocation)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ChromaLocation)) (zero)
    f

instance FromCStruct PhysicalDeviceExternalFormatResolvePropertiesANDROID where
  peekCStruct p = do
    nullColorAttachmentWithExternalFormatResolve <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    externalFormatResolveChromaOffsetX <- peek @ChromaLocation ((p `plusPtr` 20 :: Ptr ChromaLocation))
    externalFormatResolveChromaOffsetY <- peek @ChromaLocation ((p `plusPtr` 24 :: Ptr ChromaLocation))
    pure $ PhysicalDeviceExternalFormatResolvePropertiesANDROID
             (bool32ToBool nullColorAttachmentWithExternalFormatResolve)
             externalFormatResolveChromaOffsetX
             externalFormatResolveChromaOffsetY

instance Storable PhysicalDeviceExternalFormatResolvePropertiesANDROID where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalFormatResolvePropertiesANDROID where
  zero = PhysicalDeviceExternalFormatResolvePropertiesANDROID
           zero
           zero
           zero


-- | VkAndroidHardwareBufferFormatResolvePropertiesANDROID - Structure
-- defining properties of resolves using an external format
--
-- = Description
--
-- Any Android hardware buffer created with the @GRALLOC_USAGE_HW_RENDER@
-- flag /must/ be renderable in some way in Vulkan, either:
--
-- -   'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID'::@format@
--     /must/ be a format that supports
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     in
--     'Vulkan.Core10.DeviceInitialization.FormatProperties'::@optimalTilingFeatures@;
--     or
--
-- -   @colorAttachmentFormat@ /must/ be a format that supports
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     in
--     'Vulkan.Core10.DeviceInitialization.FormatProperties'::@optimalTilingFeatures@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAndroidHardwareBufferFormatResolvePropertiesANDROID-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_RESOLVE_PROPERTIES_ANDROID'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ANDROID_external_format_resolve VK_ANDROID_external_format_resolve>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AndroidHardwareBufferFormatResolvePropertiesANDROID = AndroidHardwareBufferFormatResolvePropertiesANDROID
  { -- | @colorAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format'
    -- specifying the format of color attachment images that /must/ be used for
    -- color attachments when resolving to the specified external format. If
    -- the implementation supports external format resolves for the specified
    -- external format, this value will be set to a color format supporting the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
    -- in
    -- 'Vulkan.Core10.DeviceInitialization.FormatProperties'::@optimalTilingFeatures@
    -- as returned by
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
    -- with @format@ equal to @colorAttachmentFormat@ If external format
    -- resolves are not supported, this value will be set to
    -- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'.
    colorAttachmentFormat :: Format }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AndroidHardwareBufferFormatResolvePropertiesANDROID)
#endif
deriving instance Show AndroidHardwareBufferFormatResolvePropertiesANDROID

instance ToCStruct AndroidHardwareBufferFormatResolvePropertiesANDROID where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AndroidHardwareBufferFormatResolvePropertiesANDROID{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_RESOLVE_PROPERTIES_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (colorAttachmentFormat)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_RESOLVE_PROPERTIES_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    f

instance FromCStruct AndroidHardwareBufferFormatResolvePropertiesANDROID where
  peekCStruct p = do
    colorAttachmentFormat <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    pure $ AndroidHardwareBufferFormatResolvePropertiesANDROID
             colorAttachmentFormat

instance Storable AndroidHardwareBufferFormatResolvePropertiesANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AndroidHardwareBufferFormatResolvePropertiesANDROID where
  zero = AndroidHardwareBufferFormatResolvePropertiesANDROID
           zero


type ANDROID_EXTERNAL_FORMAT_RESOLVE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_FORMAT_RESOLVE_SPEC_VERSION"
pattern ANDROID_EXTERNAL_FORMAT_RESOLVE_SPEC_VERSION :: forall a . Integral a => a
pattern ANDROID_EXTERNAL_FORMAT_RESOLVE_SPEC_VERSION = 1


type ANDROID_EXTERNAL_FORMAT_RESOLVE_EXTENSION_NAME = "VK_ANDROID_external_format_resolve"

-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_FORMAT_RESOLVE_EXTENSION_NAME"
pattern ANDROID_EXTERNAL_FORMAT_RESOLVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ANDROID_EXTERNAL_FORMAT_RESOLVE_EXTENSION_NAME = "VK_ANDROID_external_format_resolve"

