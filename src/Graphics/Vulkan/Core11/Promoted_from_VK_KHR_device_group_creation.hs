{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( withCStructDeviceGroupDeviceCreateInfo
  , fromCStructDeviceGroupDeviceCreateInfo
  , DeviceGroupDeviceCreateInfo(..)
  , withCStructPhysicalDeviceGroupProperties
  , fromCStructPhysicalDeviceGroupProperties
  , PhysicalDeviceGroupProperties(..)
  , getNumPhysicalDeviceGroups
  , enumeratePhysicalDeviceGroups
  , enumerateAllPhysicalDeviceGroups
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_MAX_DEVICE_GROUP_SIZE
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
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
  , take
  )
import qualified Data.Vector.Generic
  ( convert
  )
import qualified Data.Vector.Generic.Sized
  ( convert
  )
import qualified Data.Vector.Storable.Sized
  ( fromSized
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( enumeratePhysicalDeviceGroups
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkDeviceGroupDeviceCreateInfo(..)
  , VkPhysicalDeviceGroupProperties(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Instance(..)
  , PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( padSized
  , withArray
  , withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( pattern VK_MAX_DEVICE_GROUP_SIZE
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  )


-- No documentation found for TopLevel "DeviceGroupDeviceCreateInfo"
data DeviceGroupDeviceCreateInfo = DeviceGroupDeviceCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGroupDeviceCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupDeviceCreateInfo" "pPhysicalDevices"
  vkPPhysicalDevices :: Vector PhysicalDevice
  }
  deriving (Show, Eq)
withCStructDeviceGroupDeviceCreateInfo :: DeviceGroupDeviceCreateInfo -> (VkDeviceGroupDeviceCreateInfo -> IO a) -> IO a
withCStructDeviceGroupDeviceCreateInfo from cont = withVec ((&) . physicalDeviceHandle) (vkPPhysicalDevices (from :: DeviceGroupDeviceCreateInfo)) (\pPhysicalDevices -> maybeWith withSomeVkStruct (vkPNext (from :: DeviceGroupDeviceCreateInfo)) (\pPNext -> cont (VkDeviceGroupDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO pPNext (fromIntegral (Data.Vector.length (vkPPhysicalDevices (from :: DeviceGroupDeviceCreateInfo)))) pPhysicalDevices)))
fromCStructDeviceGroupDeviceCreateInfo :: InstanceCmds -> VkDeviceGroupDeviceCreateInfo -> IO DeviceGroupDeviceCreateInfo
fromCStructDeviceGroupDeviceCreateInfo commandTable c = DeviceGroupDeviceCreateInfo <$> -- Univalued Member elided
                                                                                    maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupDeviceCreateInfo)))
                                                                                    -- Length valued member elided
                                                                                    <*> (Data.Vector.generateM (fromIntegral (vkPhysicalDeviceCount (c :: VkDeviceGroupDeviceCreateInfo))) ((\p i -> flip PhysicalDevice commandTable <$> peekElemOff p i) (vkPPhysicalDevices (c :: VkDeviceGroupDeviceCreateInfo))))
-- No documentation found for TopLevel "PhysicalDeviceGroupProperties"
data PhysicalDeviceGroupProperties = PhysicalDeviceGroupProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceGroupProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Fixed array valid count member elided
  , -- No documentation found for Nested "PhysicalDeviceGroupProperties" "physicalDevices"
  vkPhysicalDevices :: Vector PhysicalDevice
  , -- No documentation found for Nested "PhysicalDeviceGroupProperties" "subsetAllocation"
  vkSubsetAllocation :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceGroupProperties :: PhysicalDeviceGroupProperties -> (VkPhysicalDeviceGroupProperties -> IO a) -> IO a
withCStructPhysicalDeviceGroupProperties from cont = withArray ((&) . physicalDeviceHandle) (vkPhysicalDevices (from :: PhysicalDeviceGroupProperties)) (\hysicalDevices -> maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceGroupProperties)) (\pPNext -> cont (VkPhysicalDeviceGroupProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES pPNext (fromIntegral (Data.Vector.length (vkPhysicalDevices (from :: PhysicalDeviceGroupProperties)))) (Data.Vector.Generic.Sized.convert (padSized nullPtr hysicalDevices)) (boolToBool32 (vkSubsetAllocation (from :: PhysicalDeviceGroupProperties))))))
fromCStructPhysicalDeviceGroupProperties :: InstanceCmds -> VkPhysicalDeviceGroupProperties -> IO PhysicalDeviceGroupProperties
fromCStructPhysicalDeviceGroupProperties commandTable c = PhysicalDeviceGroupProperties <$> -- Univalued Member elided
                                                                                        maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceGroupProperties)))
                                                                                        -- Fixed array valid count member elided
                                                                                        <*> traverse (\p -> pure $ PhysicalDevice p commandTable) (Data.Vector.take (fromIntegral (vkPhysicalDeviceCount (c :: VkPhysicalDeviceGroupProperties))) (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkPhysicalDevices (c :: VkPhysicalDeviceGroupProperties)))))
                                                                                        <*> pure (bool32ToBool (vkSubsetAllocation (c :: VkPhysicalDeviceGroupProperties)))

-- | Wrapper for vkEnumeratePhysicalDeviceGroups
getNumPhysicalDeviceGroups :: Instance ->  IO (VkResult, Word32)
getNumPhysicalDeviceGroups = \(Instance instance' commandTable) -> alloca (\pPhysicalDeviceGroupCount -> Graphics.Vulkan.C.Dynamic.enumeratePhysicalDeviceGroups commandTable instance' pPhysicalDeviceGroupCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPhysicalDeviceGroupCount)))

-- | Wrapper for vkEnumeratePhysicalDeviceGroups
enumeratePhysicalDeviceGroups :: Instance ->  Word32 ->  IO (VkResult, Vector PhysicalDeviceGroupProperties)
enumeratePhysicalDeviceGroups = \(Instance instance' commandTable) -> \physicalDeviceGroupCount -> allocaArray (fromIntegral physicalDeviceGroupCount) (\pPhysicalDeviceGroupProperties -> with physicalDeviceGroupCount (\pPhysicalDeviceGroupCount -> Graphics.Vulkan.C.Dynamic.enumeratePhysicalDeviceGroups commandTable instance' pPhysicalDeviceGroupCount pPhysicalDeviceGroupProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructPhysicalDeviceGroupProperties commandTable <=< peekElemOff p) pPhysicalDeviceGroupProperties) =<< (fromIntegral <$> (peek pPhysicalDeviceGroupCount)))))))
-- | Call 'getNumPhysicalDeviceGroups' to get the number of return values, then use that
-- number to call 'enumeratePhysicalDeviceGroups' to get all the values.
enumerateAllPhysicalDeviceGroups :: Instance ->  IO (Vector PhysicalDeviceGroupProperties)
enumerateAllPhysicalDeviceGroups instance' =
  snd <$> getNumPhysicalDeviceGroups instance'
    >>= \num -> snd <$> enumeratePhysicalDeviceGroups instance' num

