{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_AMD_device_coherent_memory  ( PhysicalDeviceCoherentMemoryFeaturesAMD(..)
                                                                 , AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION
                                                                 , pattern AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION
                                                                 , AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME
                                                                 , pattern AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME
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
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD))
-- | VkPhysicalDeviceCoherentMemoryFeaturesAMD - Structure describing whether
-- device coherent memory can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceCoherentMemoryFeaturesAMD' structure
-- describe the following features:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCoherentMemoryFeaturesAMD = PhysicalDeviceCoherentMemoryFeaturesAMD
  { -- | @deviceCoherentMemory@ indicates that the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkMemoryPropertyFlagBits device coherent memory>.
    deviceCoherentMemory :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceCoherentMemoryFeaturesAMD

instance ToCStruct PhysicalDeviceCoherentMemoryFeaturesAMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCoherentMemoryFeaturesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceCoherentMemory))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCoherentMemoryFeaturesAMD where
  peekCStruct p = do
    deviceCoherentMemory <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCoherentMemoryFeaturesAMD
             (bool32ToBool deviceCoherentMemory)

instance Storable PhysicalDeviceCoherentMemoryFeaturesAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCoherentMemoryFeaturesAMD where
  zero = PhysicalDeviceCoherentMemoryFeaturesAMD
           zero


type AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION"
pattern AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION = 1


type AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME = "VK_AMD_device_coherent_memory"

-- No documentation found for TopLevel "VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME"
pattern AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME = "VK_AMD_device_coherent_memory"

