{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_pci_bus_info
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDevicePCIBusInfoPropertiesEXT(..)
  , 
#endif
  pattern EXT_PCI_BUS_INFO_EXTENSION_NAME
  , pattern EXT_PCI_BUS_INFO_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_pci_bus_info
  ( pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME
  , pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDevicePCIBusInfoPropertiesEXT"
data PhysicalDevicePCIBusInfoPropertiesEXT = PhysicalDevicePCIBusInfoPropertiesEXT
  { -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pciDomain"
  pciDomain :: Word32
  , -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pciBus"
  pciBus :: Word32
  , -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pciDevice"
  pciDevice :: Word32
  , -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pciFunction"
  pciFunction :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDevicePCIBusInfoPropertiesEXT where
  zero = PhysicalDevicePCIBusInfoPropertiesEXT Nothing
                                               zero
                                               zero
                                               zero
                                               zero

#endif

-- No documentation found for TopLevel "VK_EXT_PCI_BUS_INFO_EXTENSION_NAME"
pattern EXT_PCI_BUS_INFO_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_PCI_BUS_INFO_EXTENSION_NAME = VK_EXT_PCI_BUS_INFO_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_PCI_BUS_INFO_SPEC_VERSION"
pattern EXT_PCI_BUS_INFO_SPEC_VERSION :: Integral a => a
pattern EXT_PCI_BUS_INFO_SPEC_VERSION = VK_EXT_PCI_BUS_INFO_SPEC_VERSION
