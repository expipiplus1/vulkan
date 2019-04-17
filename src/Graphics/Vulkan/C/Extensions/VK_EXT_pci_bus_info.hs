{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_pci_bus_info
  ( VkPhysicalDevicePCIBusInfoPropertiesEXT(..)
  , pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME
  , pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )


-- | VkPhysicalDevicePCIBusInfoPropertiesEXT - Structure containing PCI bus
-- information of a physical device
--
-- = Description
--
-- Unresolved directive in VkPhysicalDevicePCIBusInfoPropertiesEXT.txt -
-- include::..\/validity\/structs\/VkPhysicalDevicePCIBusInfoPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDevicePCIBusInfoPropertiesEXT = VkPhysicalDevicePCIBusInfoPropertiesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pciDomain@ is the PCI bus domain.
  vkPciDomain :: Word32
  , -- | @pciBus@ is the PCI bus identifier.
  vkPciBus :: Word32
  , -- | @pciDevice@ is the PCI device identifier.
  vkPciDevice :: Word32
  , -- | @pciFunction@ is the PCI device function identifier.
  vkPciFunction :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDevicePCIBusInfoPropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDevicePCIBusInfoPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 20)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkPciDomain (poked :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkPciBus (poked :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkPciDevice (poked :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkPciFunction (poked :: VkPhysicalDevicePCIBusInfoPropertiesEXT))

instance Zero VkPhysicalDevicePCIBusInfoPropertiesEXT where
  zero = VkPhysicalDevicePCIBusInfoPropertiesEXT zero
                                                 zero
                                                 zero
                                                 zero
                                                 zero
                                                 zero
-- No documentation found for TopLevel "VK_EXT_PCI_BUS_INFO_EXTENSION_NAME"
pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME = "VK_EXT_pci_bus_info"
-- No documentation found for TopLevel "VK_EXT_PCI_BUS_INFO_SPEC_VERSION"
pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION :: Integral a => a
pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION = 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT = VkStructureType 1000212000
