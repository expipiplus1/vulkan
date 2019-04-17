{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateFlags(..)
  , VkDeviceCreateInfo(..)
  , VkDeviceQueueCreateFlagBits(..)
  , VkDeviceQueueCreateFlags
  , VkDeviceQueueCreateInfo(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateDevice
#endif
  , FN_vkCreateDevice
  , PFN_vkCreateDevice
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyDevice
#endif
  , FN_vkDestroyDevice
  , PFN_vkDestroyDevice
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
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkPhysicalDeviceFeatures(..)
  , VkDevice
  , VkPhysicalDevice
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkDeviceCreateFlags

-- | VkDeviceCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkDeviceCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkDeviceCreateInfo'
newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- | VkDeviceCreateInfo - Structure specifying parameters of a newly created
-- device
--
-- == Valid Usage
--
-- -   The @queueFamilyIndex@ member of each element of @pQueueCreateInfos@
--     /must/ be unique within @pQueueCreateInfos@
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2'
--     structure, then @pEnabledFeatures@ /must/ be @NULL@
--
-- -   @ppEnabledExtensionNames@ /must/ not contain both
--     @{html_spec_relative}#VK_KHR_maintenance1@ and
--     @{html_spec_relative}#VK_AMD_negative_viewport_height@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_16bit_storage.VkPhysicalDevice16BitStorageFeatures',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewFeatures',
--     'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryFeatures',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkPhysicalDeviceSamplerYcbcrConversionFeatures',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters.VkPhysicalDeviceShaderDrawParametersFeatures',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers.VkPhysicalDeviceVariablePointersFeatures'
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
--
-- 'VkDeviceCreateFlags', 'VkDeviceQueueCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceFeatures',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkCreateDevice'
data VkDeviceCreateInfo = VkDeviceCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkDeviceCreateFlags
  , -- | @queueCreateInfoCount@ is the unsigned integer size of the
  -- @pQueueCreateInfos@ array. Refer to the
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-queue-creation Queue Creation>
  -- section below for further details.
  vkQueueCreateInfoCount :: Word32
  , -- | @pQueueCreateInfos@ is a pointer to an array of
  -- 'VkDeviceQueueCreateInfo' structures describing the queues that are
  -- requested to be created along with the logical device. Refer to the
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-queue-creation Queue Creation>
  -- section below for further details.
  vkPQueueCreateInfos :: Ptr VkDeviceQueueCreateInfo
  , -- | @enabledLayerCount@ is deprecated and ignored.
  vkEnabledLayerCount :: Word32
  , -- | @ppEnabledLayerNames@ is deprecated and ignored. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#extendingvulkan-layers-devicelayerdeprecation {html_spec_relative}#extendingvulkan-layers-devicelayerdeprecation>.
  vkPPEnabledLayerNames :: Ptr (Ptr CChar)
  , -- | @enabledExtensionCount@ is the number of device extensions to enable.
  vkEnabledExtensionCount :: Word32
  , -- | @ppEnabledExtensionNames@ is a pointer to an array of
  -- @enabledExtensionCount@ null-terminated UTF-8 strings containing the
  -- names of extensions to enable for the created device. See the
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#extendingvulkan-extensions {html_spec_relative}#extendingvulkan-extensions>
  -- section for further details.
  vkPPEnabledExtensionNames :: Ptr (Ptr CChar)
  , -- | @pEnabledFeatures@ is @NULL@ or a pointer to a
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceFeatures'
  -- structure that contains boolean indicators of all the features to be
  -- enabled. Refer to the
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features Features>
  -- section for further details.
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

instance Zero VkDeviceCreateInfo where
  zero = VkDeviceCreateInfo zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
-- ** VkDeviceQueueCreateFlagBits

-- | VkDeviceQueueCreateFlagBits - Bitmask specifying behavior of the queue
--
-- = See Also
--
-- 'VkDeviceQueueCreateFlags'
newtype VkDeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- | VkDeviceQueueCreateFlags - Bitmask of VkDeviceQueueCreateFlagBits
--
-- = Description
--
-- @VkDeviceQueueCreateFlags@ is a bitmask type for setting a mask of zero
-- or more 'VkDeviceQueueCreateFlagBits'.
--
-- = See Also
--
-- 'VkDeviceQueueCreateFlagBits', 'VkDeviceQueueCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkDeviceQueueInfo2'
type VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlagBits
-- | VkDeviceQueueCreateInfo - Structure specifying parameters of a newly
-- created device queue
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
-- -   @pNext@ /must/ be @NULL@
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
--
-- 'VkDeviceCreateInfo', 'VkDeviceQueueCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkDeviceQueueCreateInfo = VkDeviceQueueCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkDeviceQueueCreateFlags
  , -- | @queueFamilyIndex@ is an unsigned integer indicating the index of the
  -- queue family to create on this device. This index corresponds to the
  -- index of an element of the @pQueueFamilyProperties@ array that was
  -- returned by @vkGetPhysicalDeviceQueueFamilyProperties@.
  vkQueueFamilyIndex :: Word32
  , -- | @queueCount@ is an unsigned integer specifying the number of queues to
  -- create in the queue family indicated by @queueFamilyIndex@.
  vkQueueCount :: Word32
  , -- | @pQueuePriorities@ is an array of @queueCount@ normalized floating point
  -- values, specifying priorities of work that will be submitted to each
  -- created queue. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-priority Queue Priority>
  -- for more information.
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

instance Zero VkDeviceQueueCreateInfo where
  zero = VkDeviceQueueCreateInfo zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkCreateDevice - Create a new device instance
--
-- = Parameters
--
-- -   @physicalDevice@ /must/ be one of the device handles returned from a
--     call to @vkEnumeratePhysicalDevices@ (see
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-physical-device-enumeration Physical Device Enumeration>).
--
-- -   @pCreateInfo@ is a pointer to a 'VkDeviceCreateInfo' structure
--     containing information about how to create the device.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pDevice@ points to a handle in which the created
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' is
--     returned.
--
-- = Description
--
-- @vkCreateDevice@ verifies that extensions and features requested in the
-- @ppEnabledExtensionNames@ and @pEnabledFeatures@ members of
-- @pCreateInfo@, respectively, are supported by the implementation. If any
-- requested extension is not supported, @vkCreateDevice@ /must/ return
-- @VK_ERROR_EXTENSION_NOT_PRESENT@. If any requested feature is not
-- supported, @vkCreateDevice@ /must/ return
-- @VK_ERROR_FEATURE_NOT_PRESENT@. Support for extensions /can/ be checked
-- before creating a device by querying
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.vkEnumerateDeviceExtensionProperties'.
-- Support for features /can/ similarly be checked by querying
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFeatures'.
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
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#extendingvulkan-extensions-extensiondependencies required extensions>
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
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
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkDeviceCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDevice" vkCreateDevice :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult

#endif
type FN_vkCreateDevice = ("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult
type PFN_vkCreateDevice = FunPtr FN_vkCreateDevice
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkDestroyDevice - Destroy a logical device
--
-- = Parameters
--
-- -   @device@ is the logical device to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- = Description
--
-- To ensure that no work is active on the device,
-- 'Graphics.Vulkan.C.Core10.Queue.vkDeviceWaitIdle' /can/ be used to gate
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
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDevice" vkDestroyDevice :: ("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyDevice = ("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDevice = FunPtr FN_vkDestroyDevice
