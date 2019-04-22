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
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern MEMORY_HEAP_MULTI_INSTANCE_BIT
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
  ( empty
  , generateM
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkDeviceGroupDeviceCreateInfo(..)
  , VkPhysicalDeviceGroupProperties(..)
  , vkEnumeratePhysicalDeviceGroups
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern MEMORY_HEAP_MULTI_INSTANCE_BIT
  )



-- | VkDeviceGroupDeviceCreateInfo - Create a logical device from multiple
-- physical devices
--
-- = Description
--
-- The elements of the @pPhysicalDevices@ array are an ordered list of the
-- physical devices that the logical device represents. These /must/ be a
-- subset of a single device group, and need not be in the same order as
-- they were enumerated. The order of the physical devices in the
-- @pPhysicalDevices@ array determines the /device index/ of each physical
-- device, with element i being assigned a device index of i. Certain
-- commands and structures refer to one or more physical devices by using
-- device indices or /device masks/ formed using device indices.
--
-- A logical device created without using
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo',
-- or with @physicalDeviceCount@ equal to zero, is equivalent to a
-- @physicalDeviceCount@ of one and @pPhysicalDevices@ pointing to the
-- @physicalDevice@ parameter to
-- 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice'. In particular, the
-- device index of that physical device is zero.
--
-- == Valid Usage
--
-- -   Each element of @pPhysicalDevices@ /must/ be unique
--
-- -   All elements of @pPhysicalDevices@ /must/ be in the same device
--     group as enumerated by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.vkEnumeratePhysicalDeviceGroups'
--
-- -   If @physicalDeviceCount@ is not @0@, the @physicalDevice@ parameter
--     of 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice' /must/ be an
--     element of @pPhysicalDevices@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO'
--
-- -   If @physicalDeviceCount@ is not @0@, @pPhysicalDevices@ /must/ be a
--     valid pointer to an array of @physicalDeviceCount@ valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handles
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DeviceGroupDeviceCreateInfo = DeviceGroupDeviceCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGroupDeviceCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupDeviceCreateInfo" "pPhysicalDevices"
  physicalDevices :: Vector PhysicalDevice
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceGroupDeviceCreateInfo' and
-- marshal a 'DeviceGroupDeviceCreateInfo' into it. The 'VkDeviceGroupDeviceCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGroupDeviceCreateInfo :: DeviceGroupDeviceCreateInfo -> (VkDeviceGroupDeviceCreateInfo -> IO a) -> IO a
withCStructDeviceGroupDeviceCreateInfo marshalled cont = withVec ((&) . physicalDeviceHandle) (physicalDevices (marshalled :: DeviceGroupDeviceCreateInfo)) (\pPPhysicalDevices -> maybeWith withSomeVkStruct (next (marshalled :: DeviceGroupDeviceCreateInfo)) (\pPNext -> cont (VkDeviceGroupDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO pPNext (fromIntegral (Data.Vector.length (physicalDevices (marshalled :: DeviceGroupDeviceCreateInfo)))) pPPhysicalDevices)))

-- | A function to read a 'VkDeviceGroupDeviceCreateInfo' and all additional
-- structures in the pointer chain into a 'DeviceGroupDeviceCreateInfo'.
fromCStructDeviceGroupDeviceCreateInfo :: InstanceCmds -> VkDeviceGroupDeviceCreateInfo -> IO DeviceGroupDeviceCreateInfo
fromCStructDeviceGroupDeviceCreateInfo commandTable c = DeviceGroupDeviceCreateInfo <$> -- Univalued Member elided
                                                                                    maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupDeviceCreateInfo)))
                                                                                    -- Length valued member elided
                                                                                    <*> (Data.Vector.generateM (fromIntegral (vkPhysicalDeviceCount (c :: VkDeviceGroupDeviceCreateInfo))) ((\p i -> flip PhysicalDevice commandTable <$> peekElemOff p i) (vkPPhysicalDevices (c :: VkDeviceGroupDeviceCreateInfo))))

instance Zero DeviceGroupDeviceCreateInfo where
  zero = DeviceGroupDeviceCreateInfo Nothing
                                     Data.Vector.empty



-- | VkPhysicalDeviceGroupProperties - Structure specifying physical device
-- group properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.vkEnumeratePhysicalDeviceGroups',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group_creation.vkEnumeratePhysicalDeviceGroupsKHR'
data PhysicalDeviceGroupProperties = PhysicalDeviceGroupProperties
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceGroupProperties" "pNext"
  next :: Maybe SomeVkStruct
  -- Fixed array valid count member elided
  , -- No documentation found for Nested "PhysicalDeviceGroupProperties" "physicalDevices"
  physicalDevices :: Vector PhysicalDevice
  , -- No documentation found for Nested "PhysicalDeviceGroupProperties" "subsetAllocation"
  subsetAllocation :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceGroupProperties' and
-- marshal a 'PhysicalDeviceGroupProperties' into it. The 'VkPhysicalDeviceGroupProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceGroupProperties :: PhysicalDeviceGroupProperties -> (VkPhysicalDeviceGroupProperties -> IO a) -> IO a
withCStructPhysicalDeviceGroupProperties marshalled cont = withArray ((&) . physicalDeviceHandle) (physicalDevices (marshalled :: PhysicalDeviceGroupProperties)) (\pPhysicalDevices' -> maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceGroupProperties)) (\pPNext -> cont (VkPhysicalDeviceGroupProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES pPNext (fromIntegral (Data.Vector.length (physicalDevices (marshalled :: PhysicalDeviceGroupProperties)))) (Data.Vector.Generic.Sized.convert (padSized nullPtr pPhysicalDevices')) (boolToBool32 (subsetAllocation (marshalled :: PhysicalDeviceGroupProperties))))))

-- | A function to read a 'VkPhysicalDeviceGroupProperties' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceGroupProperties'.
fromCStructPhysicalDeviceGroupProperties :: InstanceCmds -> VkPhysicalDeviceGroupProperties -> IO PhysicalDeviceGroupProperties
fromCStructPhysicalDeviceGroupProperties commandTable c = PhysicalDeviceGroupProperties <$> -- Univalued Member elided
                                                                                        maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceGroupProperties)))
                                                                                        -- Fixed array valid count member elided
                                                                                        <*> traverse (\p -> pure $ PhysicalDevice p commandTable) (Data.Vector.take (fromIntegral (vkPhysicalDeviceCount (c :: VkPhysicalDeviceGroupProperties))) (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkPhysicalDevices (c :: VkPhysicalDeviceGroupProperties)))))
                                                                                        <*> pure (bool32ToBool (vkSubsetAllocation (c :: VkPhysicalDeviceGroupProperties)))

instance Zero PhysicalDeviceGroupProperties where
  zero = PhysicalDeviceGroupProperties Nothing
                                       Data.Vector.empty
                                       False



-- | vkEnumeratePhysicalDeviceGroups - Enumerates groups of physical devices
-- that can be used to create a single logical device
--
-- = Parameters
--
-- -   @instance@ is a handle to a Vulkan instance previously created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance'.
--
-- -   @pPhysicalDeviceGroupCount@ is a pointer to an integer related to
--     the number of device groups available or queried, as described
--     below.
--
-- -   @pPhysicalDeviceGroupProperties@ is either @NULL@ or a pointer to an
--     array of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties'
--     structures.
--
-- = Description
--
-- If @pPhysicalDeviceGroupProperties@ is @NULL@, then the number of device
-- groups available is returned in @pPhysicalDeviceGroupCount@. Otherwise,
-- @pPhysicalDeviceGroupCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pPhysicalDeviceGroupProperties@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pPhysicalDeviceGroupProperties@. If
-- @pPhysicalDeviceGroupCount@ is less than the number of device groups
-- available, at most @pPhysicalDeviceGroupCount@ structures will be
-- written. If @pPhysicalDeviceGroupCount@ is smaller than the number of
-- device groups available, 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
-- will be returned instead of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS',
-- to indicate that not all the available device groups were returned.
--
-- Every physical device /must/ be in exactly one device group.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pPhysicalDeviceGroupCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pPhysicalDeviceGroupCount@ is not @0@,
--     and @pPhysicalDeviceGroupProperties@ is not @NULL@,
--     @pPhysicalDeviceGroupProperties@ /must/ be a valid pointer to an
--     array of @pPhysicalDeviceGroupCount@
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties'
getNumPhysicalDeviceGroups :: Instance ->  IO (VkResult, Word32)
getNumPhysicalDeviceGroups = \(Instance instance' commandTable) -> alloca (\pPhysicalDeviceGroupCount' -> vkEnumeratePhysicalDeviceGroups commandTable instance' pPhysicalDeviceGroupCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPhysicalDeviceGroupCount')))

-- | vkEnumeratePhysicalDeviceGroups - Enumerates groups of physical devices
-- that can be used to create a single logical device
--
-- = Parameters
--
-- -   @instance@ is a handle to a Vulkan instance previously created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance'.
--
-- -   @pPhysicalDeviceGroupCount@ is a pointer to an integer related to
--     the number of device groups available or queried, as described
--     below.
--
-- -   @pPhysicalDeviceGroupProperties@ is either @NULL@ or a pointer to an
--     array of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties'
--     structures.
--
-- = Description
--
-- If @pPhysicalDeviceGroupProperties@ is @NULL@, then the number of device
-- groups available is returned in @pPhysicalDeviceGroupCount@. Otherwise,
-- @pPhysicalDeviceGroupCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pPhysicalDeviceGroupProperties@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pPhysicalDeviceGroupProperties@. If
-- @pPhysicalDeviceGroupCount@ is less than the number of device groups
-- available, at most @pPhysicalDeviceGroupCount@ structures will be
-- written. If @pPhysicalDeviceGroupCount@ is smaller than the number of
-- device groups available, 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
-- will be returned instead of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS',
-- to indicate that not all the available device groups were returned.
--
-- Every physical device /must/ be in exactly one device group.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pPhysicalDeviceGroupCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pPhysicalDeviceGroupCount@ is not @0@,
--     and @pPhysicalDeviceGroupProperties@ is not @NULL@,
--     @pPhysicalDeviceGroupProperties@ /must/ be a valid pointer to an
--     array of @pPhysicalDeviceGroupCount@
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties'
enumeratePhysicalDeviceGroups :: Instance ->  Word32 ->  IO (VkResult, Vector PhysicalDeviceGroupProperties)
enumeratePhysicalDeviceGroups = \(Instance instance' commandTable) -> \physicalDeviceGroupCount' -> allocaArray (fromIntegral physicalDeviceGroupCount') (\pPhysicalDeviceGroupProperties' -> with physicalDeviceGroupCount' (\pPhysicalDeviceGroupCount' -> vkEnumeratePhysicalDeviceGroups commandTable instance' pPhysicalDeviceGroupCount' pPhysicalDeviceGroupProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructPhysicalDeviceGroupProperties commandTable <=< peekElemOff p) pPhysicalDeviceGroupProperties') =<< (fromIntegral <$> (peek pPhysicalDeviceGroupCount')))))))
-- | Returns all the values available from 'enumeratePhysicalDeviceGroups'.
enumerateAllPhysicalDeviceGroups :: Instance ->  IO (Vector PhysicalDeviceGroupProperties)
enumerateAllPhysicalDeviceGroups instance' =
  snd <$> getNumPhysicalDeviceGroups instance'
    >>= \num -> snd <$> enumeratePhysicalDeviceGroups instance' num

