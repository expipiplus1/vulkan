{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.ExtensionDiscovery
  ( VK_MAX_EXTENSION_NAME_SIZE
  , pattern VK_MAX_EXTENSION_NAME_SIZE
  , vkEnumerateInstanceExtensionProperties
  , vkEnumerateDeviceExtensionProperties
  , VkExtensionProperties(..)
  ) where

import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CChar(..)
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )


-- No documentation found for TopLevel "VK_MAX_EXTENSION_NAME_SIZE"
type VK_MAX_EXTENSION_NAME_SIZE = 256
-- No documentation found for Nested "Integral a => a" "VK_MAX_EXTENSION_NAME_SIZE"
pattern VK_MAX_EXTENSION_NAME_SIZE :: Integral a => a
pattern VK_MAX_EXTENSION_NAME_SIZE = 256
-- | vkEnumerateInstanceExtensionProperties - Returns up to requested number
-- of global extension properties
--
-- = Parameters
--
-- -   @pLayerName@ is either @NULL@ or a pointer to a null-terminated
--     UTF-8 string naming the layer to retrieve extensions from.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     extension properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'VkExtensionProperties' structures.
--
-- = Description
--
-- When @pLayerName@ parameter is @NULL@, only extensions provided by the
-- Vulkan implementation or by implicitly enabled layers are returned. When
-- @pLayerName@ is the name of a layer, the instance extensions provided by
-- that layer are returned.
--
-- If @pProperties@ is @NULL@, then the number of extensions properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of extension properties
-- available, at most @pPropertyCount@ structures will be written. If
-- @pPropertyCount@ is smaller than the number of extensions available,
-- @VK_INCOMPLETE@ will be returned instead of @VK_SUCCESS@, to indicate
-- that not all the available properties were returned.
--
-- Because the list of available layers may change externally between calls
-- to @vkEnumerateInstanceExtensionProperties@, two calls may retrieve
-- different results if a @pLayerName@ is available in one call but not in
-- another. The extensions supported by a layer may also change between two
-- calls, e.g. if the layer implementation is replaced by a different
-- version between those calls.
--
-- == Valid Usage (Implicit)
--
-- -   If @pLayerName@ is not @NULL@, @pLayerName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ @VkExtensionProperties@ structures
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_LAYER_NOT_PRESENT@
--
-- = See Also
--
-- 'VkExtensionProperties'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateInstanceExtensionProperties" vkEnumerateInstanceExtensionProperties :: ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
-- | vkEnumerateDeviceExtensionProperties - Returns properties of available
-- physical device extensions
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be queried.
--
-- -   @pLayerName@ is either @NULL@ or a pointer to a null-terminated
--     UTF-8 string naming the layer to retrieve extensions from.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     extension properties available or queried, and is treated in the
--     same fashion as the
--     'vkEnumerateInstanceExtensionProperties'::@pPropertyCount@
--     parameter.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'VkExtensionProperties' structures.
--
-- = Description
--
-- When @pLayerName@ parameter is @NULL@, only extensions provided by the
-- Vulkan implementation or by implicitly enabled layers are returned. When
-- @pLayerName@ is the name of a layer, the device extensions provided by
-- that layer are returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   If @pLayerName@ is not @NULL@, @pLayerName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ @VkExtensionProperties@ structures
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_LAYER_NOT_PRESENT@
--
-- = See Also
--
-- 'VkExtensionProperties',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateDeviceExtensionProperties" vkEnumerateDeviceExtensionProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
-- | VkExtensionProperties - Structure specifying a extension properties
--
-- = See Also
--
-- 'vkEnumerateDeviceExtensionProperties',
-- 'vkEnumerateInstanceExtensionProperties'
data VkExtensionProperties = VkExtensionProperties
  { -- | @extensionName@ is a null-terminated string specifying the name of the
  -- extension.
  vkExtensionName :: Vector VK_MAX_EXTENSION_NAME_SIZE CChar
  , -- | @specVersion@ is the version of this extension. It is an integer,
  -- incremented with backward compatible changes.
  vkSpecVersion :: Word32
  }
  deriving (Eq, Show)

instance Storable VkExtensionProperties where
  sizeOf ~_ = 260
  alignment ~_ = 4
  peek ptr = VkExtensionProperties <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 256)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkExtensionName (poked :: VkExtensionProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkExtensionProperties))
