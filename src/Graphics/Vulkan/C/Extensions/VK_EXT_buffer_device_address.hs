{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressCreateInfoEXT(..)
  , VkBufferDeviceAddressInfoEXT(..)
  , VkDeviceAddress
  , VkPhysicalDeviceBufferAddressFeaturesEXT
  , pattern VkPhysicalDeviceBufferAddressFeaturesEXT
  , VkPhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetBufferDeviceAddressEXT
#endif
  , FN_vkGetBufferDeviceAddressEXT
  , PFN_vkGetBufferDeviceAddressEXT
  , pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  , pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits(..)
  , VkBufferUsageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkBufferDeviceAddressCreateInfoEXT"
data VkBufferDeviceAddressCreateInfoEXT = VkBufferDeviceAddressCreateInfoEXT
  { -- No documentation found for Nested "VkBufferDeviceAddressCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBufferDeviceAddressCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBufferDeviceAddressCreateInfoEXT" "deviceAddress"
  vkDeviceAddress :: VkDeviceAddress
  }
  deriving (Eq, Show)

instance Storable VkBufferDeviceAddressCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferDeviceAddressCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferDeviceAddressCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferDeviceAddressCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDeviceAddress (poked :: VkBufferDeviceAddressCreateInfoEXT))

instance Zero VkBufferDeviceAddressCreateInfoEXT where
  zero = VkBufferDeviceAddressCreateInfoEXT zero
                                            zero
                                            zero
-- No documentation found for TopLevel "VkBufferDeviceAddressInfoEXT"
data VkBufferDeviceAddressInfoEXT = VkBufferDeviceAddressInfoEXT
  { -- No documentation found for Nested "VkBufferDeviceAddressInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBufferDeviceAddressInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBufferDeviceAddressInfoEXT" "buffer"
  vkBuffer :: VkBuffer
  }
  deriving (Eq, Show)

instance Storable VkBufferDeviceAddressInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferDeviceAddressInfoEXT <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferDeviceAddressInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferDeviceAddressInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBufferDeviceAddressInfoEXT))

instance Zero VkBufferDeviceAddressInfoEXT where
  zero = VkBufferDeviceAddressInfoEXT zero
                                      zero
                                      zero
-- No documentation found for TopLevel "VkDeviceAddress"
type VkDeviceAddress = Word64
-- No documentation found for TopLevel "VkPhysicalDeviceBufferAddressFeaturesEXT"
type VkPhysicalDeviceBufferAddressFeaturesEXT = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT


-- No documentation found for TopLevel "VkPhysicalDeviceBufferAddressFeaturesEXT"
pattern VkPhysicalDeviceBufferAddressFeaturesEXT :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("bufferDeviceAddress" ::: VkBool32) -> ("bufferDeviceAddressCaptureReplay" ::: VkBool32) -> ("bufferDeviceAddressMultiDevice" ::: VkBool32) -> VkPhysicalDeviceBufferAddressFeaturesEXT
pattern VkPhysicalDeviceBufferAddressFeaturesEXT vkSType vkPNext vkBufferDeviceAddress vkBufferDeviceAddressCaptureReplay vkBufferDeviceAddressMultiDevice = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT vkSType vkPNext vkBufferDeviceAddress vkBufferDeviceAddressCaptureReplay vkBufferDeviceAddressMultiDevice
-- No documentation found for TopLevel "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT"
data VkPhysicalDeviceBufferDeviceAddressFeaturesEXT = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddress"
  vkBufferDeviceAddress :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressCaptureReplay"
  vkBufferDeviceAddressCaptureReplay :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressMultiDevice"
  vkBufferDeviceAddressMultiDevice :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceBufferDeviceAddressFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 20)
                                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkBufferDeviceAddress (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkBufferDeviceAddressCaptureReplay (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
                *> poke (ptr `plusPtr` 24) (vkBufferDeviceAddressMultiDevice (poked :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))

instance Zero VkPhysicalDeviceBufferDeviceAddressFeaturesEXT where
  zero = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetBufferDeviceAddressEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetBufferDeviceAddressEXT" vkGetBufferDeviceAddressEXT :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferDeviceAddressInfoEXT) -> IO VkDeviceAddress

#endif
type FN_vkGetBufferDeviceAddressEXT = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferDeviceAddressInfoEXT) -> IO VkDeviceAddress
type PFN_vkGetBufferDeviceAddressEXT = FunPtr FN_vkGetBufferDeviceAddressEXT
-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT"
pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT = VkBufferCreateFlagBits 0x00000010
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT"
pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT = VkBufferUsageFlagBits 0x00020000
-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_DEVICE_ADDRESS_EXT"
pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT :: VkResult
pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT = VkResult (-1000244000)
-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME"
pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = "VK_EXT_buffer_device_address"
-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION"
pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT = VkStructureType 1000244002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT"
pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT = VkStructureType 1000244001
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT = VkStructureType 1000244000
