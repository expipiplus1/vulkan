{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_compression_control - device extension
--
-- == VK_EXT_image_compression_control
--
-- [__Name String__]
--     @VK_EXT_image_compression_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     339
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_compression_control] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_image_compression_control extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_image_compression_control.adoc VK_EXT_image_compression_control>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Graeme Leese, Broadcom
--
--     -   Andrew Garrard, Imagination
--
--     -   Lisa Wu, Arm
--
--     -   Peter Kohaut, Arm
--
-- == Description
--
-- This extension enables fixed-rate image compression and adds the ability
-- to control when this kind of compression can be applied. Many
-- implementations support some form of framebuffer compression. This is
-- typically transparent to applications as lossless compression schemes
-- are used. With fixed-rate compression, the compression is done at a
-- defined bitrate. Such compression algorithms generally produce results
-- that are visually lossless, but the results are typically not bit-exact
-- when compared to a non-compressed result. The implementation may not be
-- able to use the requested compression rate in all cases. This extension
-- adds a query that can be used to determine the compression scheme and
-- rate that was applied to an image.
--
-- == New Commands
--
-- -   'Vulkan.Extensions.VK_EXT_host_image_copy.getImageSubresourceLayout2EXT'
--
-- == New Structures
--
-- -   'Vulkan.Extensions.VK_EXT_host_image_copy.ImageSubresource2EXT'
--
-- -   'Vulkan.Extensions.VK_EXT_host_image_copy.SubresourceLayout2EXT'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2':
--
--     -   'ImageCompressionControlEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2',
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceFormat2KHR',
--     'Vulkan.Extensions.VK_KHR_maintenance5.SubresourceLayout2KHR':
--
--     -   'ImageCompressionPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageCompressionControlFeaturesEXT'
--
-- == New Enums
--
-- -   'ImageCompressionFixedRateFlagBitsEXT'
--
-- -   'ImageCompressionFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'ImageCompressionFixedRateFlagsEXT'
--
-- -   'ImageCompressionFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_COMPRESSION_EXHAUSTED_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-05-02 (Jan-Harald Fredriksen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'ImageCompressionControlEXT', 'ImageCompressionFixedRateFlagBitsEXT',
-- 'ImageCompressionFixedRateFlagsEXT', 'ImageCompressionFlagBitsEXT',
-- 'ImageCompressionFlagsEXT', 'ImageCompressionPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_host_image_copy.ImageSubresource2EXT',
-- 'PhysicalDeviceImageCompressionControlFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_host_image_copy.SubresourceLayout2EXT',
-- 'Vulkan.Extensions.VK_EXT_host_image_copy.getImageSubresourceLayout2EXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_compression_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_compression_control  ( pattern STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT
                                                           , pattern STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_EXT
                                                           , ImageCompressionControlEXT(..)
                                                           , PhysicalDeviceImageCompressionControlFeaturesEXT(..)
                                                           , ImageCompressionPropertiesEXT(..)
                                                           , ImageCompressionFlagsEXT
                                                           , ImageCompressionFlagBitsEXT( IMAGE_COMPRESSION_DEFAULT_EXT
                                                                                        , IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT
                                                                                        , IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT
                                                                                        , IMAGE_COMPRESSION_DISABLED_EXT
                                                                                        , ..
                                                                                        )
                                                           , ImageCompressionFixedRateFlagsEXT
                                                           , ImageCompressionFixedRateFlagBitsEXT( IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_1BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_3BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_5BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_6BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_7BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_8BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_9BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_10BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_11BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_12BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_13BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_14BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_15BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_16BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_17BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_18BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_19BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_20BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_21BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_22BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_23BPC_BIT_EXT
                                                                                                 , IMAGE_COMPRESSION_FIXED_RATE_24BPC_BIT_EXT
                                                                                                 , ..
                                                                                                 )
                                                           , EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION
                                                           , pattern EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION
                                                           , EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME
                                                           , pattern EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME
                                                           , ImageSubresource2KHR(..)
                                                           , SubresourceLayout2KHR(..)
                                                           , getImageSubresourceLayout2KHR
                                                           , ImageSubresource2EXT
                                                           , SubresourceLayout2EXT
                                                           , getImageSubresourceLayout2EXT
                                                           ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
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
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_KHR))
import Vulkan.Extensions.VK_EXT_host_image_copy (getImageSubresourceLayout2EXT)
import Vulkan.Extensions.VK_KHR_maintenance5 (getImageSubresourceLayout2KHR)
import Vulkan.Extensions.VK_EXT_host_image_copy (ImageSubresource2EXT)
import Vulkan.Extensions.VK_KHR_maintenance5 (ImageSubresource2KHR(..))
import Vulkan.Extensions.VK_EXT_host_image_copy (SubresourceLayout2EXT)
import Vulkan.Extensions.VK_KHR_maintenance5 (SubresourceLayout2KHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT"
pattern STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT = STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_EXT"
pattern STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_EXT = STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_KHR


-- | VkImageCompressionControlEXT - Specify image compression properties
--
-- = Description
--
-- If enabled, fixed-rate compression is done in an implementation-defined
-- manner and /may/ be applied at block granularity. In that case, a write
-- to an individual texel /may/ modify the value of other texels in the
-- same block.
--
-- == Valid Usage
--
-- -   #VUID-VkImageCompressionControlEXT-flags-06747# @flags@ /must/ be
--     one of 'IMAGE_COMPRESSION_DEFAULT_EXT',
--     'IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT',
--     'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT', or
--     'IMAGE_COMPRESSION_DISABLED_EXT'
--
-- -   #VUID-VkImageCompressionControlEXT-flags-06748# If @flags@ includes
--     'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT', @pFixedRateFlags@
--     /must/ not be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageCompressionControlEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT'
--
-- Note
--
-- Some combinations of compression properties may not be supported. For
-- example, some implementations may not support different fixed-rate
-- compression rates per plane of a multi-planar format and will not be
-- able to enable fixed-rate compression for any plane if the requested
-- rates differ.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'ImageCompressionFixedRateFlagsEXT', 'ImageCompressionFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageCompressionControlEXT = ImageCompressionControlEXT
  { -- | @flags@ is a bitmask of 'ImageCompressionFlagBitsEXT' describing
    -- compression controls for the image.
    flags :: ImageCompressionFlagsEXT
  , -- | @compressionControlPlaneCount@ is the number of entries in the
    -- @pFixedRateFlags@ array.
    compressionControlPlaneCount :: Word32
  , -- | @pFixedRateFlags@ is @NULL@ or a pointer to an array of
    -- 'ImageCompressionFixedRateFlagsEXT' bitfields describing allowed
    -- fixed-rate compression rates of each image plane. It is ignored if
    -- @flags@ does not include 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT'.
    fixedRateFlags :: Ptr ImageCompressionFixedRateFlagsEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCompressionControlEXT)
#endif
deriving instance Show ImageCompressionControlEXT

instance ToCStruct ImageCompressionControlEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCompressionControlEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (compressionControlPlaneCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ImageCompressionFixedRateFlagsEXT))) (fixedRateFlags)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ImageCompressionFixedRateFlagsEXT))) (zero)
    f

instance FromCStruct ImageCompressionControlEXT where
  peekCStruct p = do
    flags <- peek @ImageCompressionFlagsEXT ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT))
    compressionControlPlaneCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pFixedRateFlags <- peek @(Ptr ImageCompressionFixedRateFlagsEXT) ((p `plusPtr` 24 :: Ptr (Ptr ImageCompressionFixedRateFlagsEXT)))
    pure $ ImageCompressionControlEXT
             flags compressionControlPlaneCount pFixedRateFlags

instance Storable ImageCompressionControlEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageCompressionControlEXT where
  zero = ImageCompressionControlEXT
           zero
           zero
           zero


-- | VkPhysicalDeviceImageCompressionControlFeaturesEXT - Structure
-- describing whether image compression controls can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceImageCompressionControlFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImageCompressionControlFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageCompressionControlFeaturesEXT = PhysicalDeviceImageCompressionControlFeaturesEXT
  { -- | #features-imageCompressionControl# @imageCompressionControl@ indicates
    -- that the implementation supports providing controls for image
    -- compression at image creation time.
    imageCompressionControl :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageCompressionControlFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceImageCompressionControlFeaturesEXT

instance ToCStruct PhysicalDeviceImageCompressionControlFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageCompressionControlFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (imageCompressionControl))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageCompressionControlFeaturesEXT where
  peekCStruct p = do
    imageCompressionControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImageCompressionControlFeaturesEXT
             (bool32ToBool imageCompressionControl)

instance Storable PhysicalDeviceImageCompressionControlFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageCompressionControlFeaturesEXT where
  zero = PhysicalDeviceImageCompressionControlFeaturesEXT
           zero


-- | VkImageCompressionPropertiesEXT - Compression properties of an image
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'ImageCompressionFixedRateFlagsEXT', 'ImageCompressionFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageCompressionPropertiesEXT = ImageCompressionPropertiesEXT
  { -- | @imageCompressionFlags@ returns a value describing the compression
    -- controls that apply to the image. The value will be either
    -- 'IMAGE_COMPRESSION_DEFAULT_EXT' to indicate no fixed-rate compression,
    -- 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT' to indicate fixed-rate
    -- compression, or 'IMAGE_COMPRESSION_DISABLED_EXT' to indicate no
    -- compression.
    imageCompressionFlags :: ImageCompressionFlagsEXT
  , -- | @imageCompressionFixedRateFlags@ returns a
    -- 'ImageCompressionFixedRateFlagsEXT' value describing the compression
    -- rates that apply to the specified aspect of the image.
    imageCompressionFixedRateFlags :: ImageCompressionFixedRateFlagsEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCompressionPropertiesEXT)
#endif
deriving instance Show ImageCompressionPropertiesEXT

instance ToCStruct ImageCompressionPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCompressionPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT)) (imageCompressionFlags)
    poke ((p `plusPtr` 20 :: Ptr ImageCompressionFixedRateFlagsEXT)) (imageCompressionFixedRateFlags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ImageCompressionFixedRateFlagsEXT)) (zero)
    f

instance FromCStruct ImageCompressionPropertiesEXT where
  peekCStruct p = do
    imageCompressionFlags <- peek @ImageCompressionFlagsEXT ((p `plusPtr` 16 :: Ptr ImageCompressionFlagsEXT))
    imageCompressionFixedRateFlags <- peek @ImageCompressionFixedRateFlagsEXT ((p `plusPtr` 20 :: Ptr ImageCompressionFixedRateFlagsEXT))
    pure $ ImageCompressionPropertiesEXT
             imageCompressionFlags imageCompressionFixedRateFlags

instance Storable ImageCompressionPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageCompressionPropertiesEXT where
  zero = ImageCompressionPropertiesEXT
           zero
           zero


type ImageCompressionFlagsEXT = ImageCompressionFlagBitsEXT

-- | VkImageCompressionFlagBitsEXT - Bitmask specifying image compression
-- controls
--
-- = Description
--
-- If 'ImageCompressionControlEXT'::@flags@ is
-- 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT', then the @i@th member of
-- the @pFixedRateFlags@ array specifies the allowed compression rates for
-- the image’s @i@th plane.
--
-- Note
--
-- If 'IMAGE_COMPRESSION_DISABLED_EXT' is included in
-- 'ImageCompressionControlEXT'::@flags@, both lossless and fixed-rate
-- compression will be disabled. This is likely to have a negative impact
-- on performance and is only intended to be used for debugging purposes.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'ImageCompressionFlagsEXT'
newtype ImageCompressionFlagBitsEXT = ImageCompressionFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_COMPRESSION_DEFAULT_EXT' specifies that the default image
-- compression setting is used. Implementations /must/ not apply fixed-rate
-- compression.
pattern IMAGE_COMPRESSION_DEFAULT_EXT = ImageCompressionFlagBitsEXT 0x00000000

-- | 'IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT' specifies that the
-- implementation /may/ choose any supported fixed-rate compression setting
-- in an implementation-defined manner based on the properties of the
-- image.
pattern IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT = ImageCompressionFlagBitsEXT 0x00000001

-- | 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT' specifies that fixed-rate
-- compression /may/ be used and that the allowed compression rates are
-- specified by 'ImageCompressionControlEXT'::@pFixedRateFlags@.
pattern IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT = ImageCompressionFlagBitsEXT 0x00000002

-- | 'IMAGE_COMPRESSION_DISABLED_EXT' specifies that all lossless and
-- fixed-rate compression /should/ be disabled.
pattern IMAGE_COMPRESSION_DISABLED_EXT = ImageCompressionFlagBitsEXT 0x00000004

conNameImageCompressionFlagBitsEXT :: String
conNameImageCompressionFlagBitsEXT = "ImageCompressionFlagBitsEXT"

enumPrefixImageCompressionFlagBitsEXT :: String
enumPrefixImageCompressionFlagBitsEXT = "IMAGE_COMPRESSION_"

showTableImageCompressionFlagBitsEXT :: [(ImageCompressionFlagBitsEXT, String)]
showTableImageCompressionFlagBitsEXT =
  [
    ( IMAGE_COMPRESSION_DEFAULT_EXT
    , "DEFAULT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_DEFAULT_EXT
    , "FIXED_RATE_DEFAULT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT
    , "FIXED_RATE_EXPLICIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_DISABLED_EXT
    , "DISABLED_EXT"
    )
  ]

instance Show ImageCompressionFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixImageCompressionFlagBitsEXT
      showTableImageCompressionFlagBitsEXT
      conNameImageCompressionFlagBitsEXT
      (\(ImageCompressionFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ImageCompressionFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixImageCompressionFlagBitsEXT
      showTableImageCompressionFlagBitsEXT
      conNameImageCompressionFlagBitsEXT
      ImageCompressionFlagBitsEXT

type ImageCompressionFixedRateFlagsEXT = ImageCompressionFixedRateFlagBitsEXT

-- | VkImageCompressionFixedRateFlagBitsEXT - Bitmask specifying fixed rate
-- image compression rates
--
-- = Description
--
-- If the format has a different bit rate for different components,
-- 'ImageCompressionControlEXT'::@pFixedRateFlags@ describes the rate of
-- the component with the largest number of bits assigned to it, scaled pro
-- rata. For example, to request that a
-- 'Vulkan.Core10.Enums.Format.FORMAT_A2R10G10B10_UNORM_PACK32' format be
-- stored at a rate of 8 bits per pixel, use
-- 'IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT' (10 bits for the largest
-- component, stored at quarter the original size, 2.5 bits, rounded down).
--
-- If @flags@ includes 'IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT', and
-- multiple bits are set in 'ImageCompressionControlEXT'::@pFixedRateFlags@
-- for a plane, implementations /should/ apply the lowest allowed bitrate
-- that is supported.
--
-- Note
--
-- The choice of “bits per component” terminology was chosen so that the
-- same compression rate describes the same degree of compression applied
-- to formats that differ only in the number of components. For example,
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8G8_UNORM' compressed to half its
-- original size is a rate of 4 bits per component, 8 bits per pixel.
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM' compressed to half
-- /its/ original size is 4 bits per component, 16 bits per pixel. Both of
-- these cases can be requested with
-- 'IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- 'ImageCompressionFixedRateFlagsEXT'
newtype ImageCompressionFixedRateFlagBitsEXT = ImageCompressionFixedRateFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT' specifies that fixed-rate
-- compression /must/ not be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000000

-- | 'IMAGE_COMPRESSION_FIXED_RATE_1BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [1,2) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_1BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000001

-- | 'IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [2,3) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000002

-- | 'IMAGE_COMPRESSION_FIXED_RATE_3BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [3,4) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_3BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000004

-- | 'IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [4,5) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000008

-- | 'IMAGE_COMPRESSION_FIXED_RATE_5BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [5,6) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_5BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000010

-- | 'IMAGE_COMPRESSION_FIXED_RATE_6BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [6,7) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_6BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000020

-- | 'IMAGE_COMPRESSION_FIXED_RATE_7BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [7,8) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_7BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000040

-- | 'IMAGE_COMPRESSION_FIXED_RATE_8BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [8,9) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_8BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000080

-- | 'IMAGE_COMPRESSION_FIXED_RATE_9BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [9,10) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_9BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000100

-- | 'IMAGE_COMPRESSION_FIXED_RATE_10BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [10,11) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_10BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000200

-- | 'IMAGE_COMPRESSION_FIXED_RATE_11BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of [11,12) bits per component /may/ be used.
pattern IMAGE_COMPRESSION_FIXED_RATE_11BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000400

-- | 'IMAGE_COMPRESSION_FIXED_RATE_12BPC_BIT_EXT' specifies that fixed-rate
-- compression with a bitrate of at least 12 bits per component /may/ be
-- used.
pattern IMAGE_COMPRESSION_FIXED_RATE_12BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00000800

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_13BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_13BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00001000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_14BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_14BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00002000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_15BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_15BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00004000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_16BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_16BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00008000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_17BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_17BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00010000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_18BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_18BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00020000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_19BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_19BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00040000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_20BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_20BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00080000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_21BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_21BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00100000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_22BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_22BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00200000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_23BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_23BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00400000

-- No documentation found for Nested "VkImageCompressionFixedRateFlagBitsEXT" "VK_IMAGE_COMPRESSION_FIXED_RATE_24BPC_BIT_EXT"
pattern IMAGE_COMPRESSION_FIXED_RATE_24BPC_BIT_EXT = ImageCompressionFixedRateFlagBitsEXT 0x00800000

conNameImageCompressionFixedRateFlagBitsEXT :: String
conNameImageCompressionFixedRateFlagBitsEXT = "ImageCompressionFixedRateFlagBitsEXT"

enumPrefixImageCompressionFixedRateFlagBitsEXT :: String
enumPrefixImageCompressionFixedRateFlagBitsEXT = "IMAGE_COMPRESSION_FIXED_RATE_"

showTableImageCompressionFixedRateFlagBitsEXT :: [(ImageCompressionFixedRateFlagBitsEXT, String)]
showTableImageCompressionFixedRateFlagBitsEXT =
  [
    ( IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT
    , "NONE_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_1BPC_BIT_EXT
    , "1BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_2BPC_BIT_EXT
    , "2BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_3BPC_BIT_EXT
    , "3BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_4BPC_BIT_EXT
    , "4BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_5BPC_BIT_EXT
    , "5BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_6BPC_BIT_EXT
    , "6BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_7BPC_BIT_EXT
    , "7BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_8BPC_BIT_EXT
    , "8BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_9BPC_BIT_EXT
    , "9BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_10BPC_BIT_EXT
    , "10BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_11BPC_BIT_EXT
    , "11BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_12BPC_BIT_EXT
    , "12BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_13BPC_BIT_EXT
    , "13BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_14BPC_BIT_EXT
    , "14BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_15BPC_BIT_EXT
    , "15BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_16BPC_BIT_EXT
    , "16BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_17BPC_BIT_EXT
    , "17BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_18BPC_BIT_EXT
    , "18BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_19BPC_BIT_EXT
    , "19BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_20BPC_BIT_EXT
    , "20BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_21BPC_BIT_EXT
    , "21BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_22BPC_BIT_EXT
    , "22BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_23BPC_BIT_EXT
    , "23BPC_BIT_EXT"
    )
  ,
    ( IMAGE_COMPRESSION_FIXED_RATE_24BPC_BIT_EXT
    , "24BPC_BIT_EXT"
    )
  ]

instance Show ImageCompressionFixedRateFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixImageCompressionFixedRateFlagBitsEXT
      showTableImageCompressionFixedRateFlagBitsEXT
      conNameImageCompressionFixedRateFlagBitsEXT
      (\(ImageCompressionFixedRateFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ImageCompressionFixedRateFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixImageCompressionFixedRateFlagBitsEXT
      showTableImageCompressionFixedRateFlagBitsEXT
      conNameImageCompressionFixedRateFlagBitsEXT
      ImageCompressionFixedRateFlagBitsEXT

type EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION"
pattern EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION = 1


type EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME = "VK_EXT_image_compression_control"

-- No documentation found for TopLevel "VK_EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME"
pattern EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME = "VK_EXT_image_compression_control"

