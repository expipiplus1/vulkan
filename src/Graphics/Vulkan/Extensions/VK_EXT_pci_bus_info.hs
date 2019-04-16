{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_pci_bus_info
  ( withCStructPhysicalDevicePCIBusInfoPropertiesEXT
  , fromCStructPhysicalDevicePCIBusInfoPropertiesEXT
  , PhysicalDevicePCIBusInfoPropertiesEXT(..)
  , pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION
  , pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_pci_bus_info
  ( VkPhysicalDevicePCIBusInfoPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_pci_bus_info
  ( pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME
  , pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDevicePCIBusInfoPropertiesEXT"
data PhysicalDevicePCIBusInfoPropertiesEXT = PhysicalDevicePCIBusInfoPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pciDomain"
  vkPciDomain :: Word32
  , -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pciBus"
  vkPciBus :: Word32
  , -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pciDevice"
  vkPciDevice :: Word32
  , -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pciFunction"
  vkPciFunction :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDevicePCIBusInfoPropertiesEXT :: PhysicalDevicePCIBusInfoPropertiesEXT -> (VkPhysicalDevicePCIBusInfoPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDevicePCIBusInfoPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDevicePCIBusInfoPropertiesEXT)) (\pPNext -> cont (VkPhysicalDevicePCIBusInfoPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT pPNext (vkPciDomain (from :: PhysicalDevicePCIBusInfoPropertiesEXT)) (vkPciBus (from :: PhysicalDevicePCIBusInfoPropertiesEXT)) (vkPciDevice (from :: PhysicalDevicePCIBusInfoPropertiesEXT)) (vkPciFunction (from :: PhysicalDevicePCIBusInfoPropertiesEXT))))
fromCStructPhysicalDevicePCIBusInfoPropertiesEXT :: VkPhysicalDevicePCIBusInfoPropertiesEXT -> IO PhysicalDevicePCIBusInfoPropertiesEXT
fromCStructPhysicalDevicePCIBusInfoPropertiesEXT c = PhysicalDevicePCIBusInfoPropertiesEXT <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT)))
                                                                                           <*> pure (vkPciDomain (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                                                                                           <*> pure (vkPciBus (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                                                                                           <*> pure (vkPciDevice (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                                                                                           <*> pure (vkPciFunction (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
