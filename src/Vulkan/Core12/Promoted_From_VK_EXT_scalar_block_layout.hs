{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_scalar_block_layout"
module Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout  ( PhysicalDeviceScalarBlockLayoutFeatures(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDeviceScalarBlockLayoutFeatures"
data PhysicalDeviceScalarBlockLayoutFeatures = PhysicalDeviceScalarBlockLayoutFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceScalarBlockLayoutFeatures" "scalarBlockLayout"
    scalarBlockLayout :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceScalarBlockLayoutFeatures)
#endif
deriving instance Show PhysicalDeviceScalarBlockLayoutFeatures

instance ToCStruct PhysicalDeviceScalarBlockLayoutFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceScalarBlockLayoutFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (scalarBlockLayout))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceScalarBlockLayoutFeatures where
  peekCStruct p = do
    scalarBlockLayout <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceScalarBlockLayoutFeatures
             (bool32ToBool scalarBlockLayout)


instance Storable PhysicalDeviceScalarBlockLayoutFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceScalarBlockLayoutFeatures where
  zero = PhysicalDeviceScalarBlockLayoutFeatures
           zero

