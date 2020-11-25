{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_image_robustness"
module Vulkan.Extensions.VK_EXT_image_robustness  ( PhysicalDeviceImageRobustnessFeaturesEXT(..)
                                                  , EXT_IMAGE_ROBUSTNESS_SPEC_VERSION
                                                  , pattern EXT_IMAGE_ROBUSTNESS_SPEC_VERSION
                                                  , EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME
                                                  , pattern EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceImageRobustnessFeaturesEXT"
data PhysicalDeviceImageRobustnessFeaturesEXT = PhysicalDeviceImageRobustnessFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceImageRobustnessFeaturesEXT" "robustImageAccess"
    robustImageAccess :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageRobustnessFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceImageRobustnessFeaturesEXT

instance ToCStruct PhysicalDeviceImageRobustnessFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageRobustnessFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (robustImageAccess))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageRobustnessFeaturesEXT where
  peekCStruct p = do
    robustImageAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImageRobustnessFeaturesEXT
             (bool32ToBool robustImageAccess)


instance Storable PhysicalDeviceImageRobustnessFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageRobustnessFeaturesEXT where
  zero = PhysicalDeviceImageRobustnessFeaturesEXT
           zero


type EXT_IMAGE_ROBUSTNESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_ROBUSTNESS_SPEC_VERSION"
pattern EXT_IMAGE_ROBUSTNESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_ROBUSTNESS_SPEC_VERSION = 1


type EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_image_robustness"

-- No documentation found for TopLevel "VK_EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME"
pattern EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_image_robustness"

