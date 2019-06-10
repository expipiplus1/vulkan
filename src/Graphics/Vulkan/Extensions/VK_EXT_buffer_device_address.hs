{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  BufferDeviceAddressCreateInfoEXT(..)
  , 
  BufferDeviceAddressInfoEXT(..)
#endif
  , DeviceAddress
  , PhysicalDeviceBufferAddressFeaturesEXT
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
#endif
  , getBufferDeviceAddressEXT
  , pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  , pattern EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  , pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern ERROR_INVALID_DEVICE_ADDRESS_EXT
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Utils
  ( with
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkDeviceAddress
  , vkGetBufferDeviceAddressEXT
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Buffer
  ( pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_INVALID_DEVICE_ADDRESS_EXT
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBufferDeviceAddressCreateInfoEXT"
data BufferDeviceAddressCreateInfoEXT = BufferDeviceAddressCreateInfoEXT
  { -- No documentation found for Nested "BufferDeviceAddressCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferDeviceAddressCreateInfoEXT" "deviceAddress"
  deviceAddress :: DeviceAddress
  }
  deriving (Show, Eq)

instance Zero BufferDeviceAddressCreateInfoEXT where
  zero = BufferDeviceAddressCreateInfoEXT Nothing
                                          zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBufferDeviceAddressInfoEXT"
data BufferDeviceAddressInfoEXT = BufferDeviceAddressInfoEXT
  { -- No documentation found for Nested "BufferDeviceAddressInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferDeviceAddressInfoEXT" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

instance Zero BufferDeviceAddressInfoEXT where
  zero = BufferDeviceAddressInfoEXT Nothing
                                    zero

#endif

-- No documentation found for TopLevel "DeviceAddress"
type DeviceAddress = VkDeviceAddress
  

type PhysicalDeviceBufferAddressFeaturesEXT = PhysicalDeviceBufferDeviceAddressFeaturesEXT
-- TODO: Pattern constructor alias)


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT"
data PhysicalDeviceBufferDeviceAddressFeaturesEXT = PhysicalDeviceBufferDeviceAddressFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddress"
  bufferDeviceAddress :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressCaptureReplay"
  bufferDeviceAddressCaptureReplay :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressMultiDevice"
  bufferDeviceAddressMultiDevice :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceBufferDeviceAddressFeaturesEXT where
  zero = PhysicalDeviceBufferDeviceAddressFeaturesEXT Nothing
                                                      False
                                                      False
                                                      False

#endif


-- No documentation found for TopLevel "vkGetBufferDeviceAddressEXT"
getBufferDeviceAddressEXT :: Device ->  BufferDeviceAddressInfoEXT ->  IO (VkDeviceAddress)
getBufferDeviceAddressEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME"
pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION"
pattern EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION :: Integral a => a
pattern EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
