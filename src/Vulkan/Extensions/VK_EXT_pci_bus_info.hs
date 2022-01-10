{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pci_bus_info - device extension
--
-- == VK_EXT_pci_bus_info
--
-- [__Name String__]
--     @VK_EXT_pci_bus_info@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     213
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_pci_bus_info] @anteru%0A<<Here describe the issue or question you have about the VK_EXT_pci_bus_info extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension adds a new query to obtain PCI bus information about a
-- physical device.
--
-- Not all physical devices have PCI bus information, either due to the
-- device not being connected to the system through a PCI interface or due
-- to platform specific restrictions and policies. Thus this extension is
-- only expected to be supported by physical devices which can provide the
-- information.
--
-- As a consequence, applications should always check for the presence of
-- the extension string for each individual physical device for which they
-- intend to issue the new query for and should not have any assumptions
-- about the availability of the extension on any given platform.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePCIBusInfoPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PCI_BUS_INFO_EXTENSION_NAME'
--
-- -   'EXT_PCI_BUS_INFO_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 2, 2018-12-10 (Daniel Rakos)
--
--     -   Changed all members of the new structure to have the uint32_t
--         type
--
-- -   Revision 1, 2018-10-11 (Daniel Rakos)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDevicePCIBusInfoPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pci_bus_info Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pci_bus_info  ( PhysicalDevicePCIBusInfoPropertiesEXT(..)
                                              , EXT_PCI_BUS_INFO_SPEC_VERSION
                                              , pattern EXT_PCI_BUS_INFO_SPEC_VERSION
                                              , EXT_PCI_BUS_INFO_EXTENSION_NAME
                                              , pattern EXT_PCI_BUS_INFO_EXTENSION_NAME
                                              ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT))
-- | VkPhysicalDevicePCIBusInfoPropertiesEXT - Structure containing PCI bus
-- information of a physical device
--
-- = Description
--
-- If the 'PhysicalDevicePCIBusInfoPropertiesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These are properties of the PCI bus information of a physical device.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pci_bus_info VK_EXT_pci_bus_info>,
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
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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

