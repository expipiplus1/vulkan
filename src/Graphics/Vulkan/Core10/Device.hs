{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Device
  ( DeviceCreateFlags
  , withCStructDeviceCreateInfo
  , fromCStructDeviceCreateInfo
  , DeviceCreateInfo(..)
  , DeviceQueueCreateFlagBits
  , DeviceQueueCreateFlags
  , withCStructDeviceQueueCreateInfo
  , fromCStructDeviceQueueCreateInfo
  , DeviceQueueCreateInfo(..)
  , createDevice
  , destroyDevice
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.ByteString
  ( ByteString
  , useAsCString
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createDevice
  , destroyDevice
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateFlags(..)
  , VkDeviceCreateInfo(..)
  , VkDeviceQueueCreateFlagBits(..)
  , VkDeviceQueueCreateInfo(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( initDeviceCmds
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , PhysicalDevice(..)
  , PhysicalDeviceFeatures(..)
  , fromCStructPhysicalDeviceFeatures
  , withCStructAllocationCallbacks
  , withCStructPhysicalDeviceFeatures
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( packCStringElemOff
  , withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "DeviceCreateFlags"
type DeviceCreateFlags = VkDeviceCreateFlags
-- No documentation found for TopLevel "DeviceCreateInfo"
data DeviceCreateInfo = DeviceCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceCreateInfo" "flags"
  vkFlags :: DeviceCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceCreateInfo" "pQueueCreateInfos"
  vkPQueueCreateInfos :: Vector DeviceQueueCreateInfo
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceCreateInfo" "ppEnabledLayerNames"
  vkPpEnabledLayerNames :: Vector ByteString
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceCreateInfo" "ppEnabledExtensionNames"
  vkPpEnabledExtensionNames :: Vector ByteString
  , -- No documentation found for Nested "DeviceCreateInfo" "pEnabledFeatures"
  vkPEnabledFeatures :: Maybe PhysicalDeviceFeatures
  }
  deriving (Show, Eq)
withCStructDeviceCreateInfo :: DeviceCreateInfo -> (VkDeviceCreateInfo -> IO a) -> IO a
withCStructDeviceCreateInfo from cont = maybeWith (\a -> withCStructPhysicalDeviceFeatures a . flip with) (vkPEnabledFeatures (from :: DeviceCreateInfo)) (\pEnabledFeatures -> withVec useAsCString (vkPpEnabledExtensionNames (from :: DeviceCreateInfo)) (\pPEnabledExtensionNames -> withVec useAsCString (vkPpEnabledLayerNames (from :: DeviceCreateInfo)) (\pPEnabledLayerNames -> withVec withCStructDeviceQueueCreateInfo (vkPQueueCreateInfos (from :: DeviceCreateInfo)) (\pQueueCreateInfos -> maybeWith withSomeVkStruct (vkPNext (from :: DeviceCreateInfo)) (\pPNext -> cont (VkDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO pPNext (vkFlags (from :: DeviceCreateInfo)) (fromIntegral (Data.Vector.length (vkPQueueCreateInfos (from :: DeviceCreateInfo)))) pQueueCreateInfos (fromIntegral (Data.Vector.length (vkPpEnabledLayerNames (from :: DeviceCreateInfo)))) pPEnabledLayerNames (fromIntegral (Data.Vector.length (vkPpEnabledExtensionNames (from :: DeviceCreateInfo)))) pPEnabledExtensionNames pEnabledFeatures))))))
fromCStructDeviceCreateInfo :: VkDeviceCreateInfo -> IO DeviceCreateInfo
fromCStructDeviceCreateInfo c = DeviceCreateInfo <$> -- Univalued Member elided
                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceCreateInfo)))
                                                 <*> pure (vkFlags (c :: VkDeviceCreateInfo))
                                                 -- Length valued member elided
                                                 <*> (Data.Vector.generateM (fromIntegral (vkQueueCreateInfoCount (c :: VkDeviceCreateInfo))) (((fromCStructDeviceQueueCreateInfo <=<) . peekElemOff) (vkPQueueCreateInfos (c :: VkDeviceCreateInfo))))
                                                 -- Length valued member elided
                                                 <*> (Data.Vector.generateM (fromIntegral (vkEnabledLayerCount (c :: VkDeviceCreateInfo))) (packCStringElemOff (vkPPEnabledLayerNames (c :: VkDeviceCreateInfo))))
                                                 -- Length valued member elided
                                                 <*> (Data.Vector.generateM (fromIntegral (vkEnabledExtensionCount (c :: VkDeviceCreateInfo))) (packCStringElemOff (vkPPEnabledExtensionNames (c :: VkDeviceCreateInfo))))
                                                 <*> maybePeek (fromCStructPhysicalDeviceFeatures <=< peek) (vkPEnabledFeatures (c :: VkDeviceCreateInfo))
-- No documentation found for TopLevel "DeviceQueueCreateFlagBits"
type DeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits
-- No documentation found for TopLevel "DeviceQueueCreateFlags"
type DeviceQueueCreateFlags = DeviceQueueCreateFlagBits
-- No documentation found for TopLevel "DeviceQueueCreateInfo"
data DeviceQueueCreateInfo = DeviceQueueCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceQueueCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceQueueCreateInfo" "flags"
  vkFlags :: DeviceQueueCreateFlags
  , -- No documentation found for Nested "DeviceQueueCreateInfo" "queueFamilyIndex"
  vkQueueFamilyIndex :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceQueueCreateInfo" "pQueuePriorities"
  vkPQueuePriorities :: Vector CFloat
  }
  deriving (Show, Eq)
withCStructDeviceQueueCreateInfo :: DeviceQueueCreateInfo -> (VkDeviceQueueCreateInfo -> IO a) -> IO a
withCStructDeviceQueueCreateInfo from cont = withVec (&) (vkPQueuePriorities (from :: DeviceQueueCreateInfo)) (\pQueuePriorities -> maybeWith withSomeVkStruct (vkPNext (from :: DeviceQueueCreateInfo)) (\pPNext -> cont (VkDeviceQueueCreateInfo VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO pPNext (vkFlags (from :: DeviceQueueCreateInfo)) (vkQueueFamilyIndex (from :: DeviceQueueCreateInfo)) (fromIntegral (Data.Vector.length (vkPQueuePriorities (from :: DeviceQueueCreateInfo)))) pQueuePriorities)))
fromCStructDeviceQueueCreateInfo :: VkDeviceQueueCreateInfo -> IO DeviceQueueCreateInfo
fromCStructDeviceQueueCreateInfo c = DeviceQueueCreateInfo <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceQueueCreateInfo)))
                                                           <*> pure (vkFlags (c :: VkDeviceQueueCreateInfo))
                                                           <*> pure (vkQueueFamilyIndex (c :: VkDeviceQueueCreateInfo))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkQueueCount (c :: VkDeviceQueueCreateInfo))) (peekElemOff (vkPQueuePriorities (c :: VkDeviceQueueCreateInfo))))

-- | Wrapper for 'vkCreateDevice'
createDevice :: PhysicalDevice ->  DeviceCreateInfo ->  Maybe AllocationCallbacks ->  IO ( Device )
createDevice = \(PhysicalDevice physicalDevice commandTable) -> \createInfo -> \allocator -> alloca (\pDevice -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDeviceCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createDevice commandTable physicalDevice pCreateInfo pAllocator pDevice >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pDevice >>= (\deviceH -> Device deviceH <$> initDeviceCmds commandTable deviceH))))))

-- | Wrapper for 'vkDestroyDevice'
destroyDevice :: Device ->  Maybe AllocationCallbacks ->  IO ()
destroyDevice = \(Device device commandTable) -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyDevice commandTable device pAllocator *> (pure ()))
