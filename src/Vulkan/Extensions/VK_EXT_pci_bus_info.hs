{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_pci_bus_info  ( PhysicalDevicePCIBusInfoPropertiesEXT(..)
                                              , EXT_PCI_BUS_INFO_SPEC_VERSION
                                              , pattern EXT_PCI_BUS_INFO_SPEC_VERSION
                                              , EXT_PCI_BUS_INFO_EXTENSION_NAME
                                              , pattern EXT_PCI_BUS_INFO_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT))
-- | VkPhysicalDevicePCIBusInfoPropertiesEXT - Structure containing PCI bus
-- information of a physical device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePCIBusInfoPropertiesEXT = PhysicalDevicePCIBusInfoPropertiesEXT
  { -- | @pciDomain@ is the PCI bus domain.
    pciDomain :: Word32
  , -- | @pciBus@ is the PCI bus identifier.
    pciBus :: Word32
  , -- | @pciDevice@ is the PCI device identifier.
    pciDevice :: Word32
  , -- | @pciFunction@ is the PCI device function identifier.
    pciFunction :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePCIBusInfoPropertiesEXT)
#endif
deriving instance Show PhysicalDevicePCIBusInfoPropertiesEXT

instance ToCStruct PhysicalDevicePCIBusInfoPropertiesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePCIBusInfoPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (pciDomain)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (pciBus)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (pciDevice)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (pciFunction)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDevicePCIBusInfoPropertiesEXT where
  peekCStruct p = do
    pciDomain <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pciBus <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pciDevice <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pciFunction <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ PhysicalDevicePCIBusInfoPropertiesEXT
             pciDomain pciBus pciDevice pciFunction

instance Storable PhysicalDevicePCIBusInfoPropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePCIBusInfoPropertiesEXT where
  zero = PhysicalDevicePCIBusInfoPropertiesEXT
           zero
           zero
           zero
           zero


type EXT_PCI_BUS_INFO_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_PCI_BUS_INFO_SPEC_VERSION"
pattern EXT_PCI_BUS_INFO_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PCI_BUS_INFO_SPEC_VERSION = 2


type EXT_PCI_BUS_INFO_EXTENSION_NAME = "VK_EXT_pci_bus_info"

-- No documentation found for TopLevel "VK_EXT_PCI_BUS_INFO_EXTENSION_NAME"
pattern EXT_PCI_BUS_INFO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PCI_BUS_INFO_EXTENSION_NAME = "VK_EXT_pci_bus_info"

