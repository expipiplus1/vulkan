{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_fragment_density_map"
module Vulkan.Extensions.VK_EXT_fragment_density_map  ( PhysicalDeviceFragmentDensityMapFeaturesEXT(..)
                                                      , PhysicalDeviceFragmentDensityMapPropertiesEXT(..)
                                                      , RenderPassFragmentDensityMapCreateInfoEXT(..)
                                                      , EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
                                                      , pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
                                                      , EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
                                                      , pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
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
import Vulkan.Core10.Pass (AttachmentReference)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMapFeaturesEXT"
data PhysicalDeviceFragmentDensityMapFeaturesEXT = PhysicalDeviceFragmentDensityMapFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMap"
    fragmentDensityMap :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapDynamic"
    fragmentDensityMapDynamic :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapNonSubsampledImages"
    fragmentDensityMapNonSubsampledImages :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMap))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMapDynamic))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMapNonSubsampledImages))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT where
  peekCStruct p = do
    fragmentDensityMap <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    fragmentDensityMapDynamic <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    fragmentDensityMapNonSubsampledImages <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentDensityMapFeaturesEXT
             (bool32ToBool fragmentDensityMap) (bool32ToBool fragmentDensityMapDynamic) (bool32ToBool fragmentDensityMapNonSubsampledImages)


instance Storable PhysicalDeviceFragmentDensityMapFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapFeaturesEXT where
  zero = PhysicalDeviceFragmentDensityMapFeaturesEXT
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMapPropertiesEXT"
data PhysicalDeviceFragmentDensityMapPropertiesEXT = PhysicalDeviceFragmentDensityMapPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "minFragmentDensityTexelSize"
    minFragmentDensityTexelSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "maxFragmentDensityTexelSize"
    maxFragmentDensityTexelSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "fragmentDensityInvocations"
    fragmentDensityInvocations :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapPropertiesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (minFragmentDensityTexelSize)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (maxFragmentDensityTexelSize)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (fragmentDensityInvocations))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT where
  peekCStruct p = do
    minFragmentDensityTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    maxFragmentDensityTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    fragmentDensityInvocations <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentDensityMapPropertiesEXT
             minFragmentDensityTexelSize maxFragmentDensityTexelSize (bool32ToBool fragmentDensityInvocations)


instance Storable PhysicalDeviceFragmentDensityMapPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapPropertiesEXT where
  zero = PhysicalDeviceFragmentDensityMapPropertiesEXT
           zero
           zero
           zero



-- No documentation found for TopLevel "VkRenderPassFragmentDensityMapCreateInfoEXT"
data RenderPassFragmentDensityMapCreateInfoEXT = RenderPassFragmentDensityMapCreateInfoEXT
  { -- No documentation found for Nested "VkRenderPassFragmentDensityMapCreateInfoEXT" "fragmentDensityMapAttachment"
    fragmentDensityMapAttachment :: AttachmentReference }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassFragmentDensityMapCreateInfoEXT)
#endif
deriving instance Show RenderPassFragmentDensityMapCreateInfoEXT

instance ToCStruct RenderPassFragmentDensityMapCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassFragmentDensityMapCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AttachmentReference)) (fragmentDensityMapAttachment)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AttachmentReference)) (zero)
    f

instance FromCStruct RenderPassFragmentDensityMapCreateInfoEXT where
  peekCStruct p = do
    fragmentDensityMapAttachment <- peekCStruct @AttachmentReference ((p `plusPtr` 16 :: Ptr AttachmentReference))
    pure $ RenderPassFragmentDensityMapCreateInfoEXT
             fragmentDensityMapAttachment


instance Storable RenderPassFragmentDensityMapCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassFragmentDensityMapCreateInfoEXT where
  zero = RenderPassFragmentDensityMapCreateInfoEXT
           zero


type EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION"
pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = 1


type EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME = "VK_EXT_fragment_density_map"

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME"
pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME = "VK_EXT_fragment_density_map"

