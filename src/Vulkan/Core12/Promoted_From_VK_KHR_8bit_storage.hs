{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_8bit_storage"
module Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage  ( PhysicalDevice8BitStorageFeatures(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDevice8BitStorageFeatures"
data PhysicalDevice8BitStorageFeatures = PhysicalDevice8BitStorageFeatures
  { -- No documentation found for Nested "VkPhysicalDevice8BitStorageFeatures" "storageBuffer8BitAccess"
    storageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "VkPhysicalDevice8BitStorageFeatures" "uniformAndStorageBuffer8BitAccess"
    uniformAndStorageBuffer8BitAccess :: Bool
  , -- No documentation found for Nested "VkPhysicalDevice8BitStorageFeatures" "storagePushConstant8"
    storagePushConstant8 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevice8BitStorageFeatures)
#endif
deriving instance Show PhysicalDevice8BitStorageFeatures

instance ToCStruct PhysicalDevice8BitStorageFeatures where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevice8BitStorageFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (storageBuffer8BitAccess))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (uniformAndStorageBuffer8BitAccess))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (storagePushConstant8))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevice8BitStorageFeatures where
  peekCStruct p = do
    storageBuffer8BitAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    uniformAndStorageBuffer8BitAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    storagePushConstant8 <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDevice8BitStorageFeatures
             (bool32ToBool storageBuffer8BitAccess) (bool32ToBool uniformAndStorageBuffer8BitAccess) (bool32ToBool storagePushConstant8)


instance Storable PhysicalDevice8BitStorageFeatures where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevice8BitStorageFeatures where
  zero = PhysicalDevice8BitStorageFeatures
           zero
           zero
           zero

