{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address
  ( withCStructBufferDeviceAddressCreateInfoEXT
  , fromCStructBufferDeviceAddressCreateInfoEXT
  , BufferDeviceAddressCreateInfoEXT(..)
  , withCStructBufferDeviceAddressInfoEXT
  , fromCStructBufferDeviceAddressInfoEXT
  , BufferDeviceAddressInfoEXT(..)
  , DeviceAddress
  , PhysicalDeviceBufferAddressFeaturesEXT
  , withCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT
  , fromCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT
  , PhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  , getBufferDeviceAddressEXT
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  , pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getBufferDeviceAddressEXT
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressCreateInfoEXT(..)
  , VkBufferDeviceAddressInfoEXT(..)
  , VkPhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  , VkDeviceAddress
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  , pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  , pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT
  )


-- No documentation found for TopLevel "BufferDeviceAddressCreateInfoEXT"
data BufferDeviceAddressCreateInfoEXT = BufferDeviceAddressCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "BufferDeviceAddressCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferDeviceAddressCreateInfoEXT" "deviceAddress"
  vkDeviceAddress :: DeviceAddress
  }
  deriving (Show, Eq)
withCStructBufferDeviceAddressCreateInfoEXT :: BufferDeviceAddressCreateInfoEXT -> (VkBufferDeviceAddressCreateInfoEXT -> IO a) -> IO a
withCStructBufferDeviceAddressCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: BufferDeviceAddressCreateInfoEXT)) (\pPNext -> cont (VkBufferDeviceAddressCreateInfoEXT VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT pPNext (vkDeviceAddress (from :: BufferDeviceAddressCreateInfoEXT))))
fromCStructBufferDeviceAddressCreateInfoEXT :: VkBufferDeviceAddressCreateInfoEXT -> IO BufferDeviceAddressCreateInfoEXT
fromCStructBufferDeviceAddressCreateInfoEXT c = BufferDeviceAddressCreateInfoEXT <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferDeviceAddressCreateInfoEXT)))
                                                                                 <*> pure (vkDeviceAddress (c :: VkBufferDeviceAddressCreateInfoEXT))
-- No documentation found for TopLevel "BufferDeviceAddressInfoEXT"
data BufferDeviceAddressInfoEXT = BufferDeviceAddressInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "BufferDeviceAddressInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferDeviceAddressInfoEXT" "buffer"
  vkBuffer :: Buffer
  }
  deriving (Show, Eq)
withCStructBufferDeviceAddressInfoEXT :: BufferDeviceAddressInfoEXT -> (VkBufferDeviceAddressInfoEXT -> IO a) -> IO a
withCStructBufferDeviceAddressInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: BufferDeviceAddressInfoEXT)) (\pPNext -> cont (VkBufferDeviceAddressInfoEXT VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT pPNext (vkBuffer (from :: BufferDeviceAddressInfoEXT))))
fromCStructBufferDeviceAddressInfoEXT :: VkBufferDeviceAddressInfoEXT -> IO BufferDeviceAddressInfoEXT
fromCStructBufferDeviceAddressInfoEXT c = BufferDeviceAddressInfoEXT <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferDeviceAddressInfoEXT)))
                                                                     <*> pure (vkBuffer (c :: VkBufferDeviceAddressInfoEXT))
-- No documentation found for TopLevel "DeviceAddress"
type DeviceAddress = VkDeviceAddress
  
type PhysicalDeviceBufferAddressFeaturesEXT = PhysicalDeviceBufferDeviceAddressFeaturesEXT
-- TODO: Pattern constructor alias)
-- No documentation found for TopLevel "PhysicalDeviceBufferDeviceAddressFeaturesEXT"
data PhysicalDeviceBufferDeviceAddressFeaturesEXT = PhysicalDeviceBufferDeviceAddressFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddress"
  vkBufferDeviceAddress :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressCaptureReplay"
  vkBufferDeviceAddressCaptureReplay :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBufferDeviceAddressFeaturesEXT" "bufferDeviceAddressMultiDevice"
  vkBufferDeviceAddressMultiDevice :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT :: PhysicalDeviceBufferDeviceAddressFeaturesEXT -> (VkPhysicalDeviceBufferDeviceAddressFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceBufferDeviceAddressFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceBufferDeviceAddressFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT pPNext (boolToBool32 (vkBufferDeviceAddress (from :: PhysicalDeviceBufferDeviceAddressFeaturesEXT))) (boolToBool32 (vkBufferDeviceAddressCaptureReplay (from :: PhysicalDeviceBufferDeviceAddressFeaturesEXT))) (boolToBool32 (vkBufferDeviceAddressMultiDevice (from :: PhysicalDeviceBufferDeviceAddressFeaturesEXT)))))
fromCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT -> IO PhysicalDeviceBufferDeviceAddressFeaturesEXT
fromCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT c = PhysicalDeviceBufferDeviceAddressFeaturesEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkBufferDeviceAddress (c :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkBufferDeviceAddressCaptureReplay (c :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkBufferDeviceAddressMultiDevice (c :: VkPhysicalDeviceBufferDeviceAddressFeaturesEXT)))

-- | Wrapper for 'vkGetBufferDeviceAddressEXT'
getBufferDeviceAddressEXT :: Device ->  BufferDeviceAddressInfoEXT ->  IO (VkDeviceAddress)
getBufferDeviceAddressEXT = \(Device device commandTable) -> \info -> (\a -> withCStructBufferDeviceAddressInfoEXT a . flip with) info (\pInfo -> Graphics.Vulkan.C.Dynamic.getBufferDeviceAddressEXT commandTable device pInfo >>= (\r -> pure r))
