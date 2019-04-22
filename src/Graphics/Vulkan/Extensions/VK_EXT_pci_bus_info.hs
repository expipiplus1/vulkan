{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_pci_bus_info
  ( withCStructPhysicalDevicePCIBusInfoPropertiesEXT
  , fromCStructPhysicalDevicePCIBusInfoPropertiesEXT
  , PhysicalDevicePCIBusInfoPropertiesEXT(..)
  , pattern EXT_PCI_BUS_INFO_EXTENSION_NAME
  , pattern EXT_PCI_BUS_INFO_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  ) where

import Data.String
  ( IsString
  )
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_pci_bus_info
  ( VkPhysicalDevicePCIBusInfoPropertiesEXT(..)
  , pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME
  , pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  )



-- | VkPhysicalDevicePCIBusInfoPropertiesEXT - Structure containing PCI bus
-- information of a physical device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDevicePCIBusInfoPropertiesEXT = PhysicalDevicePCIBusInfoPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDevicePCIBusInfoPropertiesEXT" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDevicePCIBusInfoPropertiesEXT' and
-- marshal a 'PhysicalDevicePCIBusInfoPropertiesEXT' into it. The 'VkPhysicalDevicePCIBusInfoPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDevicePCIBusInfoPropertiesEXT :: PhysicalDevicePCIBusInfoPropertiesEXT -> (VkPhysicalDevicePCIBusInfoPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDevicePCIBusInfoPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDevicePCIBusInfoPropertiesEXT)) (\pPNext -> cont (VkPhysicalDevicePCIBusInfoPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT pPNext (pciDomain (marshalled :: PhysicalDevicePCIBusInfoPropertiesEXT)) (pciBus (marshalled :: PhysicalDevicePCIBusInfoPropertiesEXT)) (pciDevice (marshalled :: PhysicalDevicePCIBusInfoPropertiesEXT)) (pciFunction (marshalled :: PhysicalDevicePCIBusInfoPropertiesEXT))))

-- | A function to read a 'VkPhysicalDevicePCIBusInfoPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDevicePCIBusInfoPropertiesEXT'.
fromCStructPhysicalDevicePCIBusInfoPropertiesEXT :: VkPhysicalDevicePCIBusInfoPropertiesEXT -> IO PhysicalDevicePCIBusInfoPropertiesEXT
fromCStructPhysicalDevicePCIBusInfoPropertiesEXT c = PhysicalDevicePCIBusInfoPropertiesEXT <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT)))
                                                                                           <*> pure (vkPciDomain (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                                                                                           <*> pure (vkPciBus (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                                                                                           <*> pure (vkPciDevice (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT))
                                                                                           <*> pure (vkPciFunction (c :: VkPhysicalDevicePCIBusInfoPropertiesEXT))

instance Zero PhysicalDevicePCIBusInfoPropertiesEXT where
  zero = PhysicalDevicePCIBusInfoPropertiesEXT Nothing
                                               zero
                                               zero
                                               zero
                                               zero


-- No documentation found for TopLevel "VK_EXT_PCI_BUS_INFO_EXTENSION_NAME"
pattern EXT_PCI_BUS_INFO_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_PCI_BUS_INFO_EXTENSION_NAME = VK_EXT_PCI_BUS_INFO_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_PCI_BUS_INFO_SPEC_VERSION"
pattern EXT_PCI_BUS_INFO_SPEC_VERSION :: Integral a => a
pattern EXT_PCI_BUS_INFO_SPEC_VERSION = VK_EXT_PCI_BUS_INFO_SPEC_VERSION
