{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Device
  ( VkDeviceCreateFlags(..)
  , VkDeviceQueueCreateFlagBits(..)
  , vkCreateDevice
  , vkDestroyDevice
  , VkDeviceQueueCreateInfo(..)
  , VkDeviceCreateInfo(..)
  , VkDeviceQueueCreateFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDeviceFeatures(..)
  , VkDevice
  , VkAllocationCallbacks(..)
  , VkPhysicalDevice
  )


-- ** VkDeviceCreateFlags

-- | VkDeviceCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkDeviceCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkDeviceCreateInfo'
newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDeviceCreateFlags where
  
  showsPrec p (VkDeviceCreateFlags x) = showParen (p >= 11) (showString "VkDeviceCreateFlags " . showsPrec 11 x)

instance Read VkDeviceCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDeviceCreateFlags")
                        v <- step readPrec
                        pure (VkDeviceCreateFlags v)
                        )
                    )


-- ** VkDeviceQueueCreateFlagBits

-- | VkDeviceQueueCreateFlagBits - Bitmask specifying behavior of the queue
--
-- = See Also
-- #_see_also#
--
-- 'VkDeviceQueueCreateFlags'
newtype VkDeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDeviceQueueCreateFlagBits where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDeviceQueueCreateFlagBits 0x00000001) = showString "VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT"
  showsPrec p (VkDeviceQueueCreateFlagBits x) = showParen (p >= 11) (showString "VkDeviceQueueCreateFlagBits " . showsPrec 11 x)

instance Read VkDeviceQueueCreateFlagBits where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT", pure (VkDeviceQueueCreateFlagBits 0x00000001))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDeviceQueueCreateFlagBits")
                        v <- step readPrec
                        pure (VkDeviceQueueCreateFlagBits v)
                        )
                    )


-- | vkCreateDevice - Create a new device instance
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ /must/ be one of the device handles returned from a
--     call to @vkEnumeratePhysicalDevices@ (see
--     <{html_spec_relative}#devsandqueues-physical-device-enumeration Physical Device Enumeration>).
--
-- -   @pCreateInfo@ is a pointer to a 'VkDeviceCreateInfo' structure
--     containing information about how to create the device.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pDevice@ points to a handle in which the created @VkDevice@ is
--     returned.
--
-- = Description
-- #_description#
--
-- @vkCreateDevice@ verifies that extensions and features requested in the
-- @ppEnabledExtensionNames@ and @pEnabledFeatures@ members of
-- @pCreateInfo@, respectively, are supported by the implementation. If any
-- requested extension is not supported, @vkCreateDevice@ /must/ return
-- @VK_ERROR_EXTENSION_NOT_PRESENT@. If any requested feature is not
-- supported, @vkCreateDevice@ /must/ return
-- @VK_ERROR_FEATURE_NOT_PRESENT@. Support for extensions /can/ be checked
-- before creating a device by querying
-- 'Graphics.Vulkan.Core10.ExtensionDiscovery.vkEnumerateDeviceExtensionProperties'.
-- Support for features /can/ similarly be checked by querying
-- 'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceFeatures'.
--
-- After verifying and enabling the extensions the @VkDevice@ object is
-- created and returned to the application. If a requested extension is
-- only supported by a layer, both the layer and the extension need to be
-- specified at @vkCreateInstance@ time for the creation to succeed.
--
-- Multiple logical devices /can/ be created from the same physical device.
-- Logical device creation /may/ fail due to lack of device-specific
-- resources (in addition to the other errors). If that occurs,
-- @vkCreateDevice@ will return @VK_ERROR_TOO_MANY_OBJECTS@.
--
-- == Valid Usage
--
-- -   All
--     <{html_spec_relative}#extended-functionality-extensions-dependencies required extensions>
--     for each extension in the
--     'VkDeviceCreateInfo'::@ppEnabledExtensionNames@ list /must/ also be
--     present in that list.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkDeviceCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pDevice@ /must/ be a valid pointer to a @VkDevice@ handle
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INITIALIZATION_FAILED@
--
--     -   @VK_ERROR_EXTENSION_NOT_PRESENT@
--
--     -   @VK_ERROR_FEATURE_NOT_PRESENT@
--
--     -   @VK_ERROR_TOO_MANY_OBJECTS@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkDeviceCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall "vkCreateDevice" vkCreateDevice :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult
-- | vkDestroyDevice - Destroy a logical device
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- = Description
-- #_description#
--
-- To ensure that no work is active on the device,
-- 'Graphics.Vulkan.Core10.Queue.vkDeviceWaitIdle' /can/ be used to gate
-- the destruction of the device. Prior to destroying a device, an
-- application is responsible for destroying\/freeing any Vulkan objects
-- that were created using that device as the first parameter of the
-- corresponding @vkCreate*@ or @vkAllocate*@ command.
--
-- __Note__
--
-- The lifetime of each of these objects is bound by the lifetime of the
-- @VkDevice@ object. Therefore, to avoid resource leaks, it is critical
-- that an application explicitly free all of these resources prior to
-- calling @vkDestroyDevice@.
--
-- == Valid Usage
--
-- -   All child objects created on @device@ /must/ have been destroyed
--     prior to destroying @device@
--
-- -   If @VkAllocationCallbacks@ were provided when @device@ was created,
--     a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @device@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   If @device@ is not @NULL@, @device@ /must/ be a valid @VkDevice@
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- == Host Synchronization
--
-- -   Host access to @device@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall "vkDestroyDevice" vkDestroyDevice :: ("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkDeviceQueueCreateInfo - Structure specifying parameters of a newly
-- created device queue
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by @vkGetPhysicalDeviceQueueFamilyProperties@
--
-- -   @queueCount@ /must/ be less than or equal to the @queueCount@ member
--     of the @VkQueueFamilyProperties@ structure, as returned by
--     @vkGetPhysicalDeviceQueueFamilyProperties@ in the
--     @pQueueFamilyProperties@[@queueFamilyIndex@]
--
-- -   Each element of @pQueuePriorities@ /must/ be between @0.0@ and @1.0@
--     inclusive
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_global_priority.VkDeviceQueueGlobalPriorityCreateInfoEXT'
--
-- -   @flags@ /must/ be a valid combination of
--     'VkDeviceQueueCreateFlagBits' values
--
-- -   @pQueuePriorities@ /must/ be a valid pointer to an array of
--     @queueCount@ @float@ values
--
-- -   @queueCount@ /must/ be greater than @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkDeviceCreateInfo', 'VkDeviceQueueCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkDeviceQueueCreateInfo = VkDeviceQueueCreateInfo
  { -- No documentation found for Nested "VkDeviceQueueCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "vkFlags"
  vkFlags :: VkDeviceQueueCreateFlags
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "vkQueueFamilyIndex"
  vkQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "vkQueueCount"
  vkQueueCount :: Word32
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "vkPQueuePriorities"
  vkPQueuePriorities :: Ptr CFloat
  }
  deriving (Eq, Show)

instance Storable VkDeviceQueueCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDeviceQueueCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkQueueCount (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPQueuePriorities (poked :: VkDeviceQueueCreateInfo))
-- | VkDeviceCreateInfo - Structure specifying parameters of a newly created
-- device
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   The @queueFamilyIndex@ member of each element of @pQueueCreateInfos@
--     /must/ be unique within @pQueueCreateInfos@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo',
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage.VkPhysicalDevice16BitStorageFeatures',
--     'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT',
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewFeatures',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryFeatures',
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkPhysicalDeviceSamplerYcbcrConversionFeatures',
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers.VkPhysicalDeviceVariablePointerFeatures'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @pQueueCreateInfos@ /must/ be a valid pointer to an array of
--     @queueCreateInfoCount@ valid @VkDeviceQueueCreateInfo@ structures
--
-- -   If @enabledLayerCount@ is not @0@, @ppEnabledLayerNames@ /must/ be a
--     valid pointer to an array of @enabledLayerCount@ null-terminated
--     UTF-8 strings
--
-- -   If @enabledExtensionCount@ is not @0@, @ppEnabledExtensionNames@
--     /must/ be a valid pointer to an array of @enabledExtensionCount@
--     null-terminated UTF-8 strings
--
-- -   If @pEnabledFeatures@ is not @NULL@, @pEnabledFeatures@ /must/ be a
--     valid pointer to a valid @VkPhysicalDeviceFeatures@ structure
--
-- -   @queueCreateInfoCount@ /must/ be greater than @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkDeviceCreateFlags', 'VkDeviceQueueCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceFeatures',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateDevice'
data VkDeviceCreateInfo = VkDeviceCreateInfo
  { -- No documentation found for Nested "VkDeviceCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceCreateInfo" "vkFlags"
  vkFlags :: VkDeviceCreateFlags
  , -- No documentation found for Nested "VkDeviceCreateInfo" "vkQueueCreateInfoCount"
  vkQueueCreateInfoCount :: Word32
  , -- No documentation found for Nested "VkDeviceCreateInfo" "vkPQueueCreateInfos"
  vkPQueueCreateInfos :: Ptr VkDeviceQueueCreateInfo
  , -- No documentation found for Nested "VkDeviceCreateInfo" "vkEnabledLayerCount"
  vkEnabledLayerCount :: Word32
  , -- No documentation found for Nested "VkDeviceCreateInfo" "vkPPEnabledLayerNames"
  vkPPEnabledLayerNames :: Ptr (Ptr CChar)
  , -- No documentation found for Nested "VkDeviceCreateInfo" "vkEnabledExtensionCount"
  vkEnabledExtensionCount :: Word32
  , -- No documentation found for Nested "VkDeviceCreateInfo" "vkPPEnabledExtensionNames"
  vkPPEnabledExtensionNames :: Ptr (Ptr CChar)
  , -- No documentation found for Nested "VkDeviceCreateInfo" "vkPEnabledFeatures"
  vkPEnabledFeatures :: Ptr VkPhysicalDeviceFeatures
  }
  deriving (Eq, Show)

instance Storable VkDeviceCreateInfo where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkDeviceCreateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 20)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
                                <*> peek (ptr `plusPtr` 56)
                                <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueCreateInfoCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPQueueCreateInfos (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkEnabledLayerCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPPEnabledLayerNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkEnabledExtensionCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPPEnabledExtensionNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkPEnabledFeatures (poked :: VkDeviceCreateInfo))
-- | VkDeviceQueueCreateFlags - Bitmask of VkDeviceQueueCreateFlagBits
--
-- = Description
-- #_description#
--
-- @VkDeviceQueueCreateFlags@ is a bitmask type for setting a mask of zero
-- or more 'VkDeviceQueueCreateFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkDeviceQueueCreateFlagBits', 'VkDeviceQueueCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkDeviceQueueInfo2'
type VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlagBits
