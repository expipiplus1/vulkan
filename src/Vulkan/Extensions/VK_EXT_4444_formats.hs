{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_4444_formats"
module Vulkan.Extensions.VK_EXT_4444_formats  ( PhysicalDevice4444FormatsFeaturesEXT(..)
                                              , EXT_4444_FORMATS_SPEC_VERSION
                                              , pattern EXT_4444_FORMATS_SPEC_VERSION
                                              , EXT_4444_FORMATS_EXTENSION_NAME
                                              , pattern EXT_4444_FORMATS_EXTENSION_NAME
                                              ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT))

-- No documentation found for TopLevel "VkPhysicalDevice4444FormatsFeaturesEXT"
data PhysicalDevice4444FormatsFeaturesEXT = PhysicalDevice4444FormatsFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDevice4444FormatsFeaturesEXT" "formatA4R4G4B4"
    formatA4R4G4B4 :: Bool
  , -- No documentation found for Nested "VkPhysicalDevice4444FormatsFeaturesEXT" "formatA4B4G4R4"
    formatA4B4G4R4 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevice4444FormatsFeaturesEXT)
#endif
deriving instance Show PhysicalDevice4444FormatsFeaturesEXT

instance ToCStruct PhysicalDevice4444FormatsFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevice4444FormatsFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (formatA4R4G4B4))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (formatA4B4G4R4))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevice4444FormatsFeaturesEXT where
  peekCStruct p = do
    formatA4R4G4B4 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    formatA4B4G4R4 <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDevice4444FormatsFeaturesEXT
             (bool32ToBool formatA4R4G4B4) (bool32ToBool formatA4B4G4R4)


instance Storable PhysicalDevice4444FormatsFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevice4444FormatsFeaturesEXT where
  zero = PhysicalDevice4444FormatsFeaturesEXT
           zero
           zero


type EXT_4444_FORMATS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_4444_FORMATS_SPEC_VERSION"
pattern EXT_4444_FORMATS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_4444_FORMATS_SPEC_VERSION = 1


type EXT_4444_FORMATS_EXTENSION_NAME = "VK_EXT_4444_formats"

-- No documentation found for TopLevel "VK_EXT_4444_FORMATS_EXTENSION_NAME"
pattern EXT_4444_FORMATS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_4444_FORMATS_EXTENSION_NAME = "VK_EXT_4444_formats"

