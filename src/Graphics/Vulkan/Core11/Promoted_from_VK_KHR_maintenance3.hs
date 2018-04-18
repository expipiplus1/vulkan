{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , vkGetDescriptorSetLayoutSupport
  , VkPhysicalDeviceMaintenance3Properties(..)
  , VkDescriptorSetLayoutSupport(..)
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorSetLayoutCreateInfo(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkDevice
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES = VkStructureType 1000168000
-- | Nothing
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT = VkStructureType 1000168001
-- | 
foreign import ccall "vkGetDescriptorSetLayoutSupport" vkGetDescriptorSetLayoutSupport :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()
-- | TODO: Struct comments
data VkPhysicalDeviceMaintenance3Properties = VkPhysicalDeviceMaintenance3Properties
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMaxPerSetDescriptors :: Word32
  , vkMaxMemoryAllocationSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMaintenance3Properties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMaintenance3Properties <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMaintenance3Properties))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceMaintenance3Properties))
                *> poke (ptr `plusPtr` 16) (vkMaxPerSetDescriptors (poked :: VkPhysicalDeviceMaintenance3Properties))
                *> poke (ptr `plusPtr` 24) (vkMaxMemoryAllocationSize (poked :: VkPhysicalDeviceMaintenance3Properties))
-- | TODO: Struct comments
data VkDescriptorSetLayoutSupport = VkDescriptorSetLayoutSupport
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSupported :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutSupport where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutSupport <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutSupport))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDescriptorSetLayoutSupport))
                *> poke (ptr `plusPtr` 16) (vkSupported (poked :: VkDescriptorSetLayoutSupport))
