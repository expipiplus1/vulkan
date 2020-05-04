{-# language CPP #-}
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
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.SharedTypes (ClearColorValue)
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
-- -   If provided @format@ is not
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' then the
--     'Vulkan.Core10.Sampler.SamplerCreateInfo'::@borderColor@ type /must/
--     match the sampled type of the provided @format@, as shown in the
--     /SPIR-V Sampled Type/ column of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat>
--     table
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-customBorderColorWithoutFormat customBorderColorWithoutFormat>
--     feature is not enabled then @format@ /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   If the sampler is used to sample an image view of
--     'Vulkan.Core10.Enums.Format.FORMAT_B4G4R4A4_UNORM_PACK16' format
--     then @format@ /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT'
--
-- -   @format@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- = See Also
--
-- 'Vulkan.Core10.SharedTypes.ClearColorValue',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerCustomBorderColorCreateInfoEXT = SamplerCustomBorderColorCreateInfoEXT
  { -- | @customBorderColor@ is a 'Vulkan.Core10.SharedTypes.ClearColorValue'
    -- representing the desired custom sampler border color.
    customBorderColor :: ClearColorValue
  , -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' representing the
    -- format of the sampled image view(s). This field may be
    -- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-customBorderColorWithoutFormat customBorderColorWithoutFormat>
    -- feature is enabled.
    format :: Format
  }
  deriving (Typeable)
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
  { -- | @maxCustomBorderColorSamplers@ indicates the maximum number of samplers
    -- with custom border colors which /can/ simultaneously exist on a device.
    maxCustomBorderColorSamplers :: Word32 }
  deriving (Typeable)
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
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCustomBorderColorFeaturesEXT = PhysicalDeviceCustomBorderColorFeaturesEXT
  { -- | @customBorderColors@ indicates that the implementation supports
    -- providing a @borderColor@ value with one of the following values at
    -- sampler creation time:
    --
    -- -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT'
    --
    -- -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
    customBorderColors :: Bool
  , -- | @customBorderColorWithoutFormat@ indicates that explicit formats are not
    -- required for custom border colors and the value of the @format@ member
    -- of the 'SamplerCustomBorderColorCreateInfoEXT' structure /may/ be
    -- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'. If this feature bit is
    -- not set, applications /must/ provide the
    -- 'Vulkan.Core10.Enums.Format.Format' of the image view(s) being sampled
    -- by this sampler in the @format@ member of the
    -- 'SamplerCustomBorderColorCreateInfoEXT' structure.
    customBorderColorWithoutFormat :: Bool
  }
  deriving (Typeable)
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

