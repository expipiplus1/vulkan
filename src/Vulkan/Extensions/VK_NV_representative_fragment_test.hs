{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_representative_fragment_test"
module Vulkan.Extensions.VK_NV_representative_fragment_test  ( PhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
                                                             , PipelineRepresentativeFragmentTestStateCreateInfoNV(..)
                                                             , NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
                                                             , pattern NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
                                                             , NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME
                                                             , pattern NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV))

-- No documentation found for TopLevel "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV"
data PhysicalDeviceRepresentativeFragmentTestFeaturesNV = PhysicalDeviceRepresentativeFragmentTestFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV" "representativeFragmentTest"
    representativeFragmentTest :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
#endif
deriving instance Show PhysicalDeviceRepresentativeFragmentTestFeaturesNV

instance ToCStruct PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRepresentativeFragmentTestFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (representativeFragmentTest))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  peekCStruct p = do
    representativeFragmentTest <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRepresentativeFragmentTestFeaturesNV
             (bool32ToBool representativeFragmentTest)


instance Storable PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  zero = PhysicalDeviceRepresentativeFragmentTestFeaturesNV
           zero



-- No documentation found for TopLevel "VkPipelineRepresentativeFragmentTestStateCreateInfoNV"
data PipelineRepresentativeFragmentTestStateCreateInfoNV = PipelineRepresentativeFragmentTestStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineRepresentativeFragmentTestStateCreateInfoNV" "representativeFragmentTestEnable"
    representativeFragmentTestEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRepresentativeFragmentTestStateCreateInfoNV)
#endif
deriving instance Show PipelineRepresentativeFragmentTestStateCreateInfoNV

instance ToCStruct PipelineRepresentativeFragmentTestStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRepresentativeFragmentTestStateCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (representativeFragmentTestEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineRepresentativeFragmentTestStateCreateInfoNV where
  peekCStruct p = do
    representativeFragmentTestEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PipelineRepresentativeFragmentTestStateCreateInfoNV
             (bool32ToBool representativeFragmentTestEnable)


instance Storable PipelineRepresentativeFragmentTestStateCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRepresentativeFragmentTestStateCreateInfoNV where
  zero = PipelineRepresentativeFragmentTestStateCreateInfoNV
           zero


type NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION"
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION :: forall a . Integral a => a
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION = 2


type NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME = "VK_NV_representative_fragment_test"

-- No documentation found for TopLevel "VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME"
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME = "VK_NV_representative_fragment_test"

