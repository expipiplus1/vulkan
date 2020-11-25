{-# language CPP #-}
-- | = Name
--
-- VK_EXT_custom_border_color - device extension
--
-- = Registered Extension Number
--
-- 288
--
-- = Revision
--
-- 12
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-04-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Philip Rebohle, Valve
--
--     -   Liam Middlebrook, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Tobias Hector, AMD
--
--     -   Jason Ekstrand, Intel
--
--     -   Spencer Fricke, Samsung Electronics
--
--     -   Graeme Leese, Broadcom
--
--     -   Jesse Hall, Google
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Tom Olson, ARM
--
--     -   Stuart Smith, Imagination Technologies
--
--     -   Donald Scorgie, Imagination Technologies
--
--     -   Alex Walters, Imagination Technologies
--
--     -   Peter Quayle, Imagination Technologies
--
-- == Description
--
-- This extension provides cross-vendor functionality to specify a custom
-- border color for use when the sampler address mode
-- 'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'
-- is used.
--
-- To create a sampler which uses a custom border color set
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'::@borderColor@ to one of:
--
-- -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT'
--
-- -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- When 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
-- 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT' is used,
-- applications must provide a 'SamplerCustomBorderColorCreateInfoEXT' in
-- the pNext chain for 'Vulkan.Core10.Sampler.SamplerCreateInfo'.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCustomBorderColorFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCustomBorderColorPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerCustomBorderColorCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME'
--
-- -   'EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.BorderColor.BorderColor':
--
--     -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT'
--
--     -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) Should VkClearColorValue be used for the border color value, or
-- should we have our own struct\/union? Do we need to specify the type of
-- the input values for the components? This is more of a concern if
-- VkClearColorValue is used here because it provides a union of
-- float,int,uint types.
--
-- RESOLVED: Will re-use existing VkClearColorValue structure in order to
-- easily take advantage of float,int,uint borderColor types.
--
-- 2) For hardware which supports a limited number of border colors what
-- happens if that number is exceeded? Should this be handled by the driver
-- unbeknownst to the application? In Revision 1 we had solved this issue
-- using a new Object type, however that may have lead to additional system
-- resource consumption which would otherwise not be required.
--
-- RESOLVED: Added
-- 'PhysicalDeviceCustomBorderColorPropertiesEXT'::@maxCustomBorderColorSamplers@
-- for tracking implementation-specific limit, and Valid Usage statement
-- handling overflow.
--
-- 3) Should this be supported for immutable samplers at all, or by a
-- feature bit? Some implementations may not be able to support custom
-- border colors on immutable samplers — is it worthwhile enabling this to
-- work on them for implementations that can support it, or forbidding it
-- entirely.
--
-- RESOLVED: Samplers created with a custom border color are forbidden from
-- being immutable. This resolves concerns for implementations where the
-- custom border color is an index to a LUT instead of being directly
-- embedded into sampler state.
--
-- 4) Should UINT and SINT (unsigned integer and signed integer) border
-- color types be separated or should they be combined into one generic INT
-- (integer) type?
--
-- RESOLVED: Separating these doesn’t make much sense as the existing fixed
-- border color types don’t have this distinction, and there is no reason
-- in hardware to do so. This separation would also create unnecessary work
-- and considerations for the application.
--
-- == Version History
--
-- -   Revision 1, 2019-10-10 (Joshua Ashton)
--
--     -   Internal revisions.
--
-- -   Revision 2, 2019-10-11 (Liam Middlebrook)
--
--     -   Remove VkCustomBorderColor object and associated functions
--
--     -   Add issues concerning HW limitations for custom border color
--         count
--
-- -   Revision 3, 2019-10-12 (Joshua Ashton)
--
--     -   Re-expose the limits for the maximum number of unique border
--         colors
--
--     -   Add extra details about border color tracking
--
--     -   Fix typos
--
-- -   Revision 4, 2019-10-12 (Joshua Ashton)
--
--     -   Changed maxUniqueCustomBorderColors to a uint32_t from a
--         VkDeviceSize
--
-- -   Revision 5, 2019-10-14 (Liam Middlebrook)
--
--     -   Added features bit
--
-- -   Revision 6, 2019-10-15 (Joshua Ashton)
--
--     -   Type-ize VK_BORDER_COLOR_CUSTOM
--
--     -   Fix const-ness on pNext of
--         VkSamplerCustomBorderColorCreateInfoEXT
--
-- -   Revision 7, 2019-11-26 (Liam Middlebrook)
--
--     -   Renamed maxUniqueCustomBorderColors to maxCustomBorderColors
--
-- -   Revision 8, 2019-11-29 (Joshua Ashton)
--
--     -   Renamed borderColor member of
--         VkSamplerCustomBorderColorCreateInfoEXT to customBorderColor
--
-- -   Revision 9, 2020-02-19 (Joshua Ashton)
--
--     -   Renamed maxCustomBorderColors to maxCustomBorderColorSamplers
--
-- -   Revision 10, 2020-02-21 (Joshua Ashton)
--
--     -   Added format to VkSamplerCustomBorderColorCreateInfoEXT and
--         feature bit
--
-- -   Revision 11, 2020-04-07 (Joshua Ashton)
--
--     -   Dropped UINT\/SINT border color differences, consolidated types
--
-- -   Revision 12, 2020-04-16 (Joshua Ashton)
--
--     -   Renamed VK_BORDER_COLOR_CUSTOM_FLOAT_EXT to
--         VK_BORDER_COLOR_FLOAT_CUSTOM_EXT for consistency
--
-- = See Also
--
-- 'PhysicalDeviceCustomBorderColorFeaturesEXT',
-- 'PhysicalDeviceCustomBorderColorPropertiesEXT',
-- 'SamplerCustomBorderColorCreateInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_border_color Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_custom_border_color  ( SamplerCustomBorderColorCreateInfoEXT(..)
                                                     , PhysicalDeviceCustomBorderColorPropertiesEXT(..)
                                                     , PhysicalDeviceCustomBorderColorFeaturesEXT(..)
                                                     , EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION
                                                     , pattern EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION
                                                     , EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME
                                                     , pattern EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME
                                                     ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.CommandBufferBuilding (ClearColorValue)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT))
-- | VkSamplerCustomBorderColorCreateInfoEXT - Structure specifying custom
-- border color
--
-- == Valid Usage
--
-- -   #VUID-VkSamplerCustomBorderColorCreateInfoEXT-format-04013# If
--     provided @format@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' then the
--     'Vulkan.Core10.Sampler.SamplerCreateInfo'::@borderColor@ type /must/
--     match the sampled type of the provided @format@, as shown in the
--     /SPIR-V Sampled Type/ column of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat>
--     table
--
-- -   #VUID-VkSamplerCustomBorderColorCreateInfoEXT-format-04014# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-customBorderColorWithoutFormat customBorderColorWithoutFormat>
--     feature is not enabled then @format@ /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-VkSamplerCustomBorderColorCreateInfoEXT-format-04015# If the
--     sampler is used to sample an image view of
--     'Vulkan.Core10.Enums.Format.FORMAT_B4G4R4A4_UNORM_PACK16',
--     'Vulkan.Core10.Enums.Format.FORMAT_B5G6R5_UNORM_PACK16', or
--     'Vulkan.Core10.Enums.Format.FORMAT_B5G5R5A1_UNORM_PACK16' format
--     then @format@ /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSamplerCustomBorderColorCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT'
--
-- -   #VUID-VkSamplerCustomBorderColorCreateInfoEXT-format-parameter#
--     @format@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- = See Also
--
-- 'Vulkan.Core10.CommandBufferBuilding.ClearColorValue',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerCustomBorderColorCreateInfoEXT = SamplerCustomBorderColorCreateInfoEXT
  { -- | @customBorderColor@ is a
    -- 'Vulkan.Core10.CommandBufferBuilding.ClearColorValue' representing the
    -- desired custom sampler border color.
    customBorderColor :: ClearColorValue
  , -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' representing the
    -- format of the sampled image view(s). This field may be
    -- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-customBorderColorWithoutFormat customBorderColorWithoutFormat>
    -- feature is enabled.
    format :: Format
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerCustomBorderColorCreateInfoEXT)
#endif
deriving instance Show SamplerCustomBorderColorCreateInfoEXT

instance ToCStruct SamplerCustomBorderColorCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerCustomBorderColorCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr ClearColorValue)) (customBorderColor) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (format)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr ClearColorValue)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    lift $ f

instance Zero SamplerCustomBorderColorCreateInfoEXT where
  zero = SamplerCustomBorderColorCreateInfoEXT
           zero
           zero


-- | VkPhysicalDeviceCustomBorderColorPropertiesEXT - Structure describing
-- whether custom border colors can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceCustomBorderColorPropertiesEXT'
-- structure describe the following features:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCustomBorderColorPropertiesEXT = PhysicalDeviceCustomBorderColorPropertiesEXT
  { -- | #limits-maxCustomBorderColorSamplers# @maxCustomBorderColorSamplers@
    -- indicates the maximum number of samplers with custom border colors which
    -- /can/ simultaneously exist on a device.
    maxCustomBorderColorSamplers :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCustomBorderColorPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceCustomBorderColorPropertiesEXT

instance ToCStruct PhysicalDeviceCustomBorderColorPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCustomBorderColorPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxCustomBorderColorSamplers)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceCustomBorderColorPropertiesEXT where
  peekCStruct p = do
    maxCustomBorderColorSamplers <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceCustomBorderColorPropertiesEXT
             maxCustomBorderColorSamplers

instance Storable PhysicalDeviceCustomBorderColorPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCustomBorderColorPropertiesEXT where
  zero = PhysicalDeviceCustomBorderColorPropertiesEXT
           zero


-- | VkPhysicalDeviceCustomBorderColorFeaturesEXT - Structure describing
-- whether custom border colors can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceCustomBorderColorFeaturesEXT'
-- structure describe the following features:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCustomBorderColorFeaturesEXT = PhysicalDeviceCustomBorderColorFeaturesEXT
  { -- | #features-customBorderColors# @customBorderColors@ indicates that the
    -- implementation supports providing a @borderColor@ value with one of the
    -- following values at sampler creation time:
    --
    -- -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT'
    --
    -- -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
    customBorderColors :: Bool
  , -- | #features-customBorderColorWithoutFormat#
    -- @customBorderColorWithoutFormat@ indicates that explicit formats are not
    -- required for custom border colors and the value of the @format@ member
    -- of the 'SamplerCustomBorderColorCreateInfoEXT' structure /may/ be
    -- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'. If this feature bit is
    -- not set, applications /must/ provide the
    -- 'Vulkan.Core10.Enums.Format.Format' of the image view(s) being sampled
    -- by this sampler in the @format@ member of the
    -- 'SamplerCustomBorderColorCreateInfoEXT' structure.
    customBorderColorWithoutFormat :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCustomBorderColorFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceCustomBorderColorFeaturesEXT

instance ToCStruct PhysicalDeviceCustomBorderColorFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCustomBorderColorFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (customBorderColors))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (customBorderColorWithoutFormat))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCustomBorderColorFeaturesEXT where
  peekCStruct p = do
    customBorderColors <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    customBorderColorWithoutFormat <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceCustomBorderColorFeaturesEXT
             (bool32ToBool customBorderColors) (bool32ToBool customBorderColorWithoutFormat)

instance Storable PhysicalDeviceCustomBorderColorFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCustomBorderColorFeaturesEXT where
  zero = PhysicalDeviceCustomBorderColorFeaturesEXT
           zero
           zero


type EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION = 12

-- No documentation found for TopLevel "VK_EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION"
pattern EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION = 12


type EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME = "VK_EXT_custom_border_color"

-- No documentation found for TopLevel "VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME"
pattern EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME = "VK_EXT_custom_border_color"

