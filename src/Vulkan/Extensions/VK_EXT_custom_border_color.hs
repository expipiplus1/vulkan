{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_custom_border_color"
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

-- No documentation found for TopLevel "VkSamplerCustomBorderColorCreateInfoEXT"
data SamplerCustomBorderColorCreateInfoEXT = SamplerCustomBorderColorCreateInfoEXT
  { -- No documentation found for Nested "VkSamplerCustomBorderColorCreateInfoEXT" "customBorderColor"
    customBorderColor :: ClearColorValue
  , -- No documentation found for Nested "VkSamplerCustomBorderColorCreateInfoEXT" "format"
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



-- No documentation found for TopLevel "VkPhysicalDeviceCustomBorderColorPropertiesEXT"
data PhysicalDeviceCustomBorderColorPropertiesEXT = PhysicalDeviceCustomBorderColorPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceCustomBorderColorPropertiesEXT" "maxCustomBorderColorSamplers"
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



-- No documentation found for TopLevel "VkPhysicalDeviceCustomBorderColorFeaturesEXT"
data PhysicalDeviceCustomBorderColorFeaturesEXT = PhysicalDeviceCustomBorderColorFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceCustomBorderColorFeaturesEXT" "customBorderColors"
    customBorderColors :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceCustomBorderColorFeaturesEXT" "customBorderColorWithoutFormat"
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

