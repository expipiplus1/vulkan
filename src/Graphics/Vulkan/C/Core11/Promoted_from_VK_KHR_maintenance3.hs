{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  , VkPhysicalDeviceMaintenance3Properties(..)
  , FN_vkGetDescriptorSetLayoutSupport
  , PFN_vkGetDescriptorSetLayoutSupport
  , vkGetDescriptorSetLayoutSupport
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorSetLayoutCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDescriptorSetLayoutSupport"
data VkDescriptorSetLayoutSupport = VkDescriptorSetLayoutSupport
  { -- No documentation found for Nested "VkDescriptorSetLayoutSupport" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorSetLayoutSupport" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorSetLayoutSupport" "supported"
  vkSupported :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutSupport where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutSupport <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutSupport))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetLayoutSupport))
                *> poke (ptr `plusPtr` 16) (vkSupported (poked :: VkDescriptorSetLayoutSupport))

instance Zero VkDescriptorSetLayoutSupport where
  zero = VkDescriptorSetLayoutSupport VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
                                      zero
                                      zero

-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance3Properties"
data VkPhysicalDeviceMaintenance3Properties = VkPhysicalDeviceMaintenance3Properties
  { -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "maxPerSetDescriptors"
  vkMaxPerSetDescriptors :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "maxMemoryAllocationSize"
  vkMaxMemoryAllocationSize :: VkDeviceSize
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMaintenance3Properties))
                *> poke (ptr `plusPtr` 16) (vkMaxPerSetDescriptors (poked :: VkPhysicalDeviceMaintenance3Properties))
                *> poke (ptr `plusPtr` 24) (vkMaxMemoryAllocationSize (poked :: VkPhysicalDeviceMaintenance3Properties))

instance Zero VkPhysicalDeviceMaintenance3Properties where
  zero = VkPhysicalDeviceMaintenance3Properties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
                                                zero
                                                zero
                                                zero

-- No documentation found for TopLevel "vkGetDescriptorSetLayoutSupport"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDescriptorSetLayoutSupport" vkGetDescriptorSetLayoutSupport :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()
#else
vkGetDescriptorSetLayoutSupport :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()
vkGetDescriptorSetLayoutSupport deviceCmds = mkVkGetDescriptorSetLayoutSupport (pVkGetDescriptorSetLayoutSupport deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorSetLayoutSupport
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ())
#endif

type FN_vkGetDescriptorSetLayoutSupport = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()
type PFN_vkGetDescriptorSetLayoutSupport = FunPtr FN_vkGetDescriptorSetLayoutSupport

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT = VkStructureType 1000168001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES = VkStructureType 1000168000
