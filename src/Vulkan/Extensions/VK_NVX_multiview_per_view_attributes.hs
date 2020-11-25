{-# language CPP #-}
-- No documentation found for Chapter "VK_NVX_multiview_per_view_attributes"
module Vulkan.Extensions.VK_NVX_multiview_per_view_attributes  ( PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
                                                               , NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
                                                               , pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
                                                               , NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
                                                               , pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX))

-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX"
data PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "perViewPositionAllComponents"
    perViewPositionAllComponents :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)
#endif
deriving instance Show PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX

instance ToCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (perViewPositionAllComponents))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  peekCStruct p = do
    perViewPositionAllComponents <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             (bool32ToBool perViewPositionAllComponents)


instance Storable PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  zero = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
           zero


type NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION :: forall a . Integral a => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1


type NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = "VK_NVX_multiview_per_view_attributes"

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = "VK_NVX_multiview_per_view_attributes"

