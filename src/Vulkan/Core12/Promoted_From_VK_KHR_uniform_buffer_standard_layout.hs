{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_uniform_buffer_standard_layout"
module Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout  ( PhysicalDeviceUniformBufferStandardLayoutFeatures(..)
                                                                          , StructureType(..)
                                                                          ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDeviceUniformBufferStandardLayoutFeatures"
data PhysicalDeviceUniformBufferStandardLayoutFeatures = PhysicalDeviceUniformBufferStandardLayoutFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceUniformBufferStandardLayoutFeatures" "uniformBufferStandardLayout"
    uniformBufferStandardLayout :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceUniformBufferStandardLayoutFeatures)
#endif
deriving instance Show PhysicalDeviceUniformBufferStandardLayoutFeatures

instance ToCStruct PhysicalDeviceUniformBufferStandardLayoutFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceUniformBufferStandardLayoutFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (uniformBufferStandardLayout))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceUniformBufferStandardLayoutFeatures where
  peekCStruct p = do
    uniformBufferStandardLayout <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceUniformBufferStandardLayoutFeatures
             (bool32ToBool uniformBufferStandardLayout)


instance Storable PhysicalDeviceUniformBufferStandardLayoutFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceUniformBufferStandardLayoutFeatures where
  zero = PhysicalDeviceUniformBufferStandardLayoutFeatures
           zero

